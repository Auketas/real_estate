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

get_listing_info <- function(page_num,base_url) {
  url <- paste0(
    base_url,
    page_num
  )
  
  resp <- GET(
    url,
    add_headers("User-Agent" = "Mozilla/5.0")
  )
  
  page <- read_html(resp)
  
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
    
    pageresults <- get_listing_info(page,base_url)
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
  resp <- GET(
    url,
    add_headers("User-Agent" = "Mozilla/5.0")
  )
  
  page <- read_html(resp)
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

update_porto <- function(type) {
  
  url <- Sys.getenv("TURSO_URL")
  token <- Sys.getenv("TURSO_TOKEN")
  today <- Sys.Date()
  price_changes <- 0
  
  adsdbname <- ifelse(type=="buy","ads_porto_buy","ads_porto_rent")
  pricedbname <- ifelse(type=="buy","price_changes_porto_buy","price_changes_porto_rent")
  
  # 1. scrape
  base_url <- ifelse(type=="buy","https://www.imovirtual.com/pt/resultados/comprar/apartamento/porto/porto?page=","https://www.imovirtual.com/pt/resultados/comprar/apartamento/porto/porto?page=")
  current_ads <- scrape_listings(base_url)
  print("current ads scraped")
  # 2. read DB
  db_ads <- read_ads(url, token, adsdbname)
  #price_table <- read_prices(url, token, pricedbname)
  print("database read")
  
  # Convert to dataframes
  parse_turso_rows <- function(rows, col_names) {
    df_list <- lapply(rows, function(row) {
      values <- sapply(row, function(cell) cell$value)
      
      # IMPORTANT: transpose to make it a single row
      as.data.frame(t(values), stringsAsFactors = FALSE)
    })
    
    df <- do.call(rbind, df_list)
    colnames(df) <- col_names
    
    return(df)
  }
  db_ads <- parse_turso_rows(db_ads,c("id","price"))
  #price_table <- parse_turso_rows(price_table,c("id","old_price","new_price","date"))
  print("databases converted to dataframes")
  # 3. ID logic
  new_ids <- setdiff(current_ads$id, db_ads$id)
  existing_ids <- intersect(current_ads$id, db_ads$id)
  inactive_ids <- setdiff(
    db_ads$id[db_ads$is_active == "Yes"],
    current_ads$id
  )
  print("ids extracted")
  
  # 4. NEW ADS
  new_listings <- current_ads[current_ads$id %in% new_ids, ]
  new_data <- scrape_new_ads(new_listings, today)
  
  insert_ads(new_data, url, token, adsdbname)
  print("new ads inserted into database")
  
  # 5. UPDATE EXISTING
  for(id in existing_ids) {
    
    dbprice <- db_ads$price[db_ads$id == id]
    currentprice <- current_ads$price[current_ads$id == id]
    
    # update last_seen
    
    sql_update_seen <- sprintf(paste0(
      "UPDATE ",adsdbname," SET last_seen = '%s' WHERE id = '%s';")
      ,
      today, id
    )
    turso_query(sql_update_seen, url, token)
    
    # price change
    if(length(currentprice) > 0 && dbprice != currentprice) {
      
      sql_price <- sprintf(paste0(
        "INSERT INTO ",pricedbname," (id, old_price, new_price, date)
         VALUES ('%s', %f, %f, '%s');")
        ,
        id, as.numeric(dbprice), currentprice, today
      )
      
      turso_query(sql_price, url, token)
      
      sql_update_price <- sprintf(paste0(
        "UPDATE ",adsdbname," SET price = %f WHERE id = '%s';")
        ,
        currentprice, id
      )
      
      turso_query(sql_update_price, url, token)
      
      price_changes <- price_changes+1
    }
  }
  print("existing ads updated")
  # 6. INACTIVE ADS
  for(id in inactive_ids) {
    sql_inactive <- sprintf(paste0(
      "UPDATE ",adsdbname," SET is_active = 'No' WHERE id = '%s';")
      ,
      id
    )
    turso_query(sql_inactive, url, token)
  }
  print("inactive ads updated")
  new_log_data <- list(date=today,new_listings=length(new_ids),existing_listings=length(existing_ids),inactive_listings=length(inactive_ids),price_changes=price_changes)
  print(paste0("new log data:",new_log_data))
  old_log_data <- read.csv("log/scraper_log.csv")
  old_log_data <- rbind(old_log_data,new_log_data)
  write.csv(old_log_data,"log/scraper_log.csv",row.names=FALSE)
  print("log data written")
}

scrape_new_ads <- function(new_listings,date,maxads=4000){
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
  newdata$is_active <- "Yes"
  newdata$first_seen <- date
  newdata$last_seen <- date
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

read_ads <- function(url, token, name) {
  res <- turso_query(paste0("SELECT id,price FROM ",name,";"), url, token)
  res$results$response$result$rows
}

read_prices <- function(url, token, name) {
  res <- turso_query(paste0("SELECT * FROM ",name,";"), url, token)
  res$results$response$result$rows
}

insert_ads <- function(df, url, token, name) {
  for(i in 1:nrow(df)) {
    print(i)
    row <- df[i, ]
    
    sql <- sprintf(
      paste0(
        "INSERT INTO ",name," (
          id, area, tipologia, andar, anunciante, tipo, novo,
          jardim, energia, elevador, garagem, terraco, varanda,
          lat, lon, neighbourhood, price, is_active, first_seen, last_seen
        ) VALUES (
          '%s','%s','%s','%s','%s','%s','%s',
          %d,'%s','%s',%d,%d,%d,
          %f,%f,'%s',%f,'%s','%s','%s'
        );"
      ),
      row$id,
      row$area,
      row$tipologia,
      row$andar,
      row$anunciante,
      row$tipo,
      row$novo,
      as.numeric(row$jardim),
      row$energia,
      row$elevador,
      as.numeric(row$garagem),
      as.numeric(row$terraco),
      as.numeric(row$varanda),
      as.numeric(row$lat),
      as.numeric(row$lon),
      row$neighbourhood,
      row$price,
      row$is_active,
      row$first_seen,
      row$last_seen
    )
    
    turso_query(sql, url, token)
  }
}

update_database <- function(){
  update_porto("rent")
  update_porto("buy")
}
