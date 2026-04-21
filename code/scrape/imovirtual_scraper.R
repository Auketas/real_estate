library(httr)
library(rvest)
library(assertthat)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidygeocoder)
library(DBI)
library(RSQLite)
library(httr2)
library(RPostgres)

get_con <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("NEON_DBNAME"),
    host = Sys.getenv("NEON_HOST"),
    user = Sys.getenv("NEON_USER"),
    password = Sys.getenv("NEON_PASSWORD"),
    #dbname=dbname,
    #host=host,
    #user=user,
    #password=password,
    port = 5432,
    sslmode = "require"
  )
}

safe_con <- function(con) {
  tryCatch({
    dbExecute(con, "SELECT 1")
    return(con)
  }, error = function(e) {
    message("Connection lost, reconnecting...")
    return(get_con())
  })
}

fetch_with_retry <- function(url, max_retries = 3, backoff = c(5, 20, 60)) {
  for (attempt in 1:(max_retries + 1)) {
    page <- tryCatch({
      resp <- GET(url, add_headers("User-Agent" = "Mozilla/5.0"))
      read_html(resp)
    }, error = function(e) e)

    if (!inherits(page, "error")) return(page)

    if (attempt <= max_retries) {
      wait <- backoff[min(attempt, length(backoff))]
      message(sprintf("Request failed (attempt %d/%d): %s — retrying in %ds...",
                      attempt, max_retries + 1, conditionMessage(page), wait))
      Sys.sleep(wait)
    }
  }
  stop(sprintf("Gave up after %d attempts: %s", max_retries + 1, conditionMessage(page)))
}

get_listing_info <- function(page_num,base_url) {
  url <- paste0(
    base_url,
    page_num
  )

  page <- fetch_with_retry(url)
  
  cards <- page %>%
    html_elements("section[data-sentry-component='BaseCard']")
  
  nads <- length(cards)
  
  results <- lapply(cards, function(card) {
    
    link <- card %>%
      html_element("a[data-cy='listing-item-link']") %>%
      html_attr("href")
    
    id <- str_extract(link, "ID[[:alnum:]]+")
    
    description <- card %>%
      html_element("p[data-cy='listing-item-title']") %>%
      html_text(trim = TRUE)
    
    price <- card %>%
      html_elements("span[data-sentry-element='MainPrice']") %>%
      html_text(trim = TRUE)
    
    # Handle multiple prices
    if (length(price) != 1) {
      return(NULL)  # skip messy listings
    }
    
    data.frame(
      id = id,
      link = link,
      description = description,
      price = price,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine and remove NULLs
  results <- do.call(rbind, results)
  return(list(results=results,nads=nads))  
}

scrape_listings <- function(base_url){
  all_links <- c()
  all_descriptions <- c()
  all_prices <- c()
  all_ids <- c()
  page <- 1
  nzeros <- 0
  zsame <- 0
  olddescriptions <- 0
  repeat {
    cat("Scraping page", page, "\n")
    
    pageresults <- tryCatch(
      get_listing_info(page, base_url),
      error = function(e) {
        message(sprintf("Skipping page %d after all retries: %s", page, e$message))
        NULL
      }
    )
    if (is.null(pageresults)) { page <- page + 1; next }
    results <- pageresults$results
    nads <- pageresults$nads
    if(length(results$link)==0){
      nzeros <- nzeros+1
    }else{
      nzeros <- 0
    }
    links <- paste0("https://www.imovirtual.com",results$link)
    descriptions <- results$description
    prices <- results$price
    ids <- results$id
    ids <- ifelse(substr(ids,1,2)=="ID",substr(ids,3,nchar(ids)),ids)
    assert_that(length(links)==length(descriptions),msg=paste0("Different number of links and descriptions on page ",page))
    assert_that(length(links)==length(prices),msg=paste0("Different number of links and prices on page ",page))
    assert_that(length(links)==length(ids),msg=paste0("Different number of links and ids on page ",page))
    if(identical(descriptions,olddescriptions)){
      nsame <- nsame+1
    }else{
      nsame <-  0
    }
    if(nsame == 5){
      print("Same listings found on five subsequent pages - Stopping")
      break
    }
    if(nzeros==5){
      print("No listings found on five subsequent pages - Stopping")
      break
    }
    
    if (nads==0) {
      cat("No listings found — stopping.\n")
      break
    }
    
    print(paste0(length(links)," listings scraped"))
    
    all_links <- c(all_links, links)
    all_descriptions <- c(all_descriptions, descriptions)
    all_prices <- c(all_prices,prices)
    all_ids <- c(all_ids,ids)
    olddescriptions <- descriptions
    
    page <- page + 1
    
    Sys.sleep(1.5)  # be polite
  }
  result <- data.frame(id=all_ids,link=all_links,description=all_descriptions,price=all_prices)
  result <- result[!duplicated(result$id),]
  result <- result[substr(result$price,nchar(result$price),nchar(result$price))=="€",]
  result$price <- as.numeric(gsub("[^0-9]", "", result$price))
  return(result)
}

scrape_ad <- function(url){
  page <- fetch_with_retry(url)
  features <- page %>%
    html_elements("div.css-1okys8k.e178zspo0") %>%
    html_text(trim = TRUE)
  
  area <- features[which(features=="Área:")+1]
  area <- ifelse(length(area)==0,NA,ifelse(substr(area,nchar(area),nchar(area))==":",NA,area))
  tipologia <- features[which(features=="Tipologia:")+1]
  tipologia <- ifelse(length(tipologia)==0,NA,ifelse(substr(tipologia,nchar(tipologia),nchar(tipologia))==":",NA,tipologia))
  andar <- features[which(features=="Andar:")+1]
  andar <- ifelse(length(andar)==0,NA,ifelse(substr(andar,nchar(andar),nchar(andar))==":",NA,andar))
  anunciante <- features[which(features=="Tipo de anunciante:")+1]
  anunciante <- ifelse(length(anunciante)==0,NA,ifelse(substr(anunciante,nchar(anunciante),nchar(anunciante))==":",NA,anunciante))
  tipo <- features[which(features=="Tipo de imóvel:")+1]
  tipo <- ifelse(length(tipo)==0,NA,ifelse(substr(tipo,nchar(tipo),nchar(tipo))==":",NA,tipo))
  novo  <- features[which(features=="Nova construção:")+1]
  novo <- ifelse(length(novo)==0,NA,ifelse(substr(novo,nchar(novo),nchar(novo))==":",NA,novo))
  jardim <- ifelse(any(grepl("jardim", features)), 1, 0)
  energia <- features[which(features=="Certificado energético:")+1]
  energia <- ifelse(length(energia)==0,NA,ifelse(substr(energia,nchar(energia),nchar(energia))==":",NA,energia))
  elevador <- features[which(features=="Elevador:")+1]
  elevador <- ifelse(length(elevador)==0,NA,ifelse(substr(elevador,nchar(elevador),nchar(elevador))==":",NA,elevador))
  garagem <- ifelse(any(grepl("garagem", features)), 1, 0)
  terraco <- ifelse(any(grepl("terraço", features)), 1, 0)
  varanda <- ifelse(any(grepl("varanda", features)), 1, 0)
  
  script <- page %>%
    html_element("script#__NEXT_DATA__") %>%
    html_text()
  
  data <- fromJSON(script)
  lat <- data$props$pageProps$ad$location$coordinates$latitude
  lon <- data$props$pageProps$ad$location$coordinates$longitude
  
  id <- data$props$pageProps$id
  if(substr(id,1,2)=="ID"){
    id <- substr(id,3,nchar(id))
  }
  
  neighbourhood <- convert_coordinates_to_neighbourhood(lon,lat)
  
  
  return(c(id,area,tipologia,andar,anunciante,tipo,novo,jardim,energia,elevador,garagem,terraco,varanda,lat,lon,neighbourhood))
}

update <- function(type, city,runstats) {
  
  con <- get_con()
  on.exit(dbDisconnect(con))  # ensures connection is closed even if error occurs
  
  today <- Sys.Date()
  price_changes <- 0
  cityname <- strsplit(city, "/")[[1]][2]
  
  table_ads    <- ifelse(type == "buy", "ads_buy", "ads_rent")
  table_prices <- ifelse(type == "buy", "price_changes_buy", "price_changes_rent")
  
  # 1. Scrape
  base_url <- paste0(
    ifelse(type == "buy",
           "https://www.imovirtual.com/pt/resultados/comprar/apartamento/",
           "https://www.imovirtual.com/pt/resultados/arrendar/apartamento/"
    ), city, "?page="
  )
  current_ads <- scrape_listings(base_url)
  print("current ads scraped")
  
  # 2. Read DB - now a simple dataframe, no parsing needed
  con <- safe_con(con)
  db_ads <- read_ads(con, cityname, type)
  print("database read")
  
  # 3. ID logic
  new_ids      <- setdiff(current_ads$id, db_ads$id)
  existing_ids <- intersect(current_ads$id, db_ads$id)
  inactive_ids <- setdiff(
    db_ads$id[db_ads$is_active == 1],
    current_ads$id
  )
  print("ids extracted")
  
  # 4. New ads
  new_listings <- current_ads[current_ads$id %in% new_ids, ]
  if(nrow(new_listings)>0){
    new_data <- scrape_new_ads(new_listings, today, cityname)
    con <- safe_con(con)
    insert_ads(new_data, con, type, cityname)
  }
  print("new ads inserted into database")
  
  con <- safe_con(con)
  # 5. Update existing
  if (length(existing_ids) > 0) {
    
    # Bulk update last_seen for all existing ids
    ids_sql <- paste0("('", paste(existing_ids, collapse = "','"), "')")
    dbExecute(con, sprintf(
      "UPDATE %s SET last_seen = '%s' WHERE id IN %s AND city = '%s' AND platform = '%s';",
      table_ads, today, ids_sql, cityname, "imovirtual"
    ))
    
    # Find price changes by joining current_ads with db_ads in R
    current_subset <- current_ads[current_ads$id %in% existing_ids, c("id", "price")]
    db_subset      <- db_ads[db_ads$id %in% existing_ids, c("id", "price")]
    merged         <- merge(current_subset, db_subset, by = "id", suffixes = c("_current", "_db"))
    changed        <- merged[merged$price_current != merged$price_db, ]
    
    if (nrow(changed) > 0) {
      # Bulk insert price changes
      price_changes_df <- data.frame(
        id        = changed$id,
        city      = cityname,
        old_price = as.numeric(changed$price_db),
        new_price = as.numeric(changed$price_current),
        date      = today,
        platform  = "imovirtual"
      )
      dbWriteTable(con, table_prices, price_changes_df, append = TRUE, row.names = FALSE)
      
      # Bulk update prices - loop is still needed here but only for changed rows
      # which should be a small subset
      for (i in 1:nrow(changed)) {
        dbExecute(con, sprintf(
          "UPDATE %s SET price = %f WHERE id = '%s' AND city = '%s' AND platform = '%s';",
          table_ads, as.numeric(changed$price_current[i]), changed$id[i], cityname, "imovirtual"
        ))
      }
      
      price_changes <- nrow(changed)
    }
  }
  print("existing ads updated")
  
  # 6. Inactive ads - one bulk update
  con <- safe_con(con)
  if (length(inactive_ids) > 0) {
    ids_sql <- paste0("('", paste(inactive_ids, collapse = "','"), "')")
    dbExecute(con, sprintf(
      "UPDATE %s SET is_active = 0 WHERE id IN %s AND city = '%s' AND platform = '%s';",
      table_ads, ids_sql, cityname, "imovirtual"
    ))
  }
  print("inactive ads updated")
  
  runstats$new_listings <- runstats$new_listings+length(new_ids)
  runstats$existing_listings <- runstats$existing_listings+length(existing_ids)
  runstats$inactive_listings <- runstats$inactive_listings+length(inactive_ids)
  runstats$price_changes <- runstats$price_changes+price_changes
  
  return(runstats)
}

scrape_new_ads <- function(new_listings,date,cityname,maxads=4000){
  nads <- min(maxads,nrow(new_listings))
  new_listings  <- new_listings[1:nads,]
  newdata <- matrix(nrow=nads,ncol=16)
  for(i in 1:nads){
    print(i)
    Sys.sleep(2)
    
    result <- tryCatch(
      scrape_ad(new_listings$link[i]),
      error = function(e) {
        message(paste("Error at row", i, ":", e$message))
        return(rep(NA, 16))  
      }
    )
    
    newdata[i,] <- result
  }
  newdata <- data.frame(newdata)
  colnames(newdata) <- c("id","area","tipologia","andar","anunciante","tipo","novo","jardim","energia","elevador","garagem","terraco","varanda","lat","lon","neighbourhood")
  newdata$price <- new_listings$price
  newdata$is_active <- 1
  newdata$first_seen <- date
  newdata$last_seen <- date
  newdata$city <- cityname
  newdata$platform <- "imovirtual"
  newdata <- newdata[!is.na(newdata[,1]),]
  return(newdata)
}

update_price_table <- function(id,dbprice,currentprice,today,pricetable){
  newdata <- c(id,dbprice,currentprice,today)
  newdata <- data.frame(t(newdata))
  colnames(newdata) <- c("id","old_price","new_price","date")
  pricetable <- rbind(pricetable,newdata)
  return(pricetable)
}

convert_coordinates_to_neighbourhood <- function(lon, lat) {
  df <- data.frame(lon = lon, lat = lat)
  result <- reverse_geocode(
    df,
    lat = lat,
    long = lon,
    method = "osm",
    full_results = TRUE
  )
  
  # Ensure columns exist before coalescing (OSM doesn't always return all fields)
  expected_cols <- c("neighbourhood", "suburb")
  for (col in expected_cols) {
    if (!col %in% names(result)) {
      result[[col]] <- NA_character_
    }
  }
  
  neighbourhood <- result %>%
    transmute(
      neighbourhood = coalesce(neighbourhood, suburb)
    ) %>%
    pull(neighbourhood)
  
  return(neighbourhood)
}

turso_query <- function(sql, url, token) {
  res <- POST(
    url = paste0(url, "/v2/pipeline"),
    add_headers(Authorization = paste("Bearer", token)),
    body = toJSON(list(
      requests = list(list(type = "execute", stmt = list(sql = sql)))
    ), auto_unbox = TRUE),
    encode = "json"
  )
  
  content(res, as = "parsed", simplifyVector = TRUE)
}

read_ads <- function(con, city, type) {
  table_name <- ifelse(type == "buy", "ads_buy", "ads_rent")
  dbGetQuery(con, sprintf(
    "SELECT id, price FROM %s WHERE city = '%s';",
    table_name, city
  ))
}

read_prices <- function(url, token, name) {
  res <- turso_query(paste0("SELECT * FROM ",name,";"), url, token)
  res$results$response$result$rows
}

insert_ads <- function(df, con, type, city) {
  table_name <- ifelse(type == "buy", "ads_buy", "ads_rent")
  dbWriteTable(con, table_name, df, append = TRUE, row.names = FALSE)
}

update_database <- function(){
  runstats <- list("date"=Sys.Date(),"new_listings"=0,"existing_listings"=0,"inactive_listings"=0,"price_changes"=0,"platform"="imovirtual")
  cities <- c("porto/porto","faro/albufeira","faro/loule","faro/portimao","faro/lagos","faro/lagoa","faro/faro","lisboa/lisboa")
  for(city in cities){
    print(paste0("Scraping ",city))
    runstats <- update("rent",city,runstats)
    runstats <- update("buy",city,runstats)
  }
  print(paste0("new log data: ", runstats))
  old_log_data <- read.csv("log/scraper_log.csv")
  old_log_data <- rbind(old_log_data, runstats)
  write.csv(old_log_data, "log/scraper_log.csv", row.names = FALSE)
  print("log data written")
}
