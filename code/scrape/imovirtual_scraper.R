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
library(sf)

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

# Load GeoJSON files for point-in-polygon neighbourhood matching
load_geojson_neighbourhoods <- function() {
  geojson_dir <- "dashboard/static"
  geojsons <- list()

  tryCatch({
    porto_path <- file.path(geojson_dir, "porto_region.geojson")
    if (file.exists(porto_path)) {
      geojsons[["porto"]] <- st_read(porto_path, quiet = TRUE)
    }

    lisboa_path <- file.path(geojson_dir, "lisboa_region.geojson")
    if (file.exists(lisboa_path)) {
      geojsons[["lisboa"]] <- st_read(lisboa_path, quiet = TRUE)
    }

    algarve_path <- file.path(geojson_dir, "algarve.geojson")
    if (file.exists(algarve_path)) {
      geojsons[["algarve"]] <- st_read(algarve_path, quiet = TRUE)
    }

    setubal_path <- file.path(geojson_dir, "setubal.geojson")
    if (file.exists(setubal_path)) {
      geojsons[["setubal"]] <- st_read(setubal_path, quiet = TRUE)
    }

    return(geojsons)
  }, error = function(e) {
    message("Could not load GeoJSON files: ", e$message)
    return(list())
  })
}

# Global GeoJSON data (loaded once at startup)
GEOJSON_NEIGHBOURHOODS <- load_geojson_neighbourhoods()

# Match point to polygon
get_neighbourhood_from_polygon <- function(lon, lat, city) {
  if (is.na(lon) || is.na(lat) || length(GEOJSON_NEIGHBOURHOODS) == 0) {
    return(NA_character_)
  }

  tryCatch({
    city_norm <- gsub("-", " ", tolower(city))
    geojson <- NULL

    if (city_norm %in% c("porto", "vila nova de gaia", "matosinhos", "maia")) {
      geojson <- GEOJSON_NEIGHBOURHOODS[["porto"]]
    } else if (city_norm %in% c("lisboa", "cascais", "sintra")) {
      geojson <- GEOJSON_NEIGHBOURHOODS[["lisboa"]]
    } else if (city_norm %in% c("almada", "costa da caparica", "caparica e trafaria")) {
      geojson <- GEOJSON_NEIGHBOURHOODS[["setubal"]]
    } else if (city_norm %in% c("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")) {
      geojson <- GEOJSON_NEIGHBOURHOODS[["algarve"]]
    }

    if (is.null(geojson)) {
      return(NA_character_)
    }

    point <- st_point(c(lon, lat))
    point_sf <- st_sf(geometry = st_sfc(point), crs = 4326)

    intersects <- st_intersects(point_sf, geojson, sparse = TRUE)[[1]]

    if (length(intersects) == 0) {
      return(NA_character_)
    }

    feature_idx <- intersects[1]
    neighbourhood <- geojson$NAME_3[feature_idx]
    if (is.na(neighbourhood)) {
      neighbourhood <- geojson$NAME_2[feature_idx]
    }

    return(neighbourhood)
  }, error = function(e) {
    return(NA_character_)
  })
}

# Parse price from raw HTML text — handles ranges like "50 000 € - 55 000 €"
# by taking the lower (first) bound before non-digit stripping joins them.
parse_price_raw <- function(raw_price) {
  if (grepl("\\d.*-.*\\d", raw_price)) {
    first <- as.numeric(gsub("[^0-9]", "", strsplit(raw_price, "-")[[1]][1]))
    if (!is.na(first) && first > 0) return(list(price = first, was_range = TRUE))
  }
  list(price = as.numeric(gsub("[^0-9]", "", raw_price)), was_range = FALSE)
}

write_rejection_log <- function(entries) {
  log_file <- "log/rejected_listings.csv"
  if (nrow(entries) == 0) return(invisible(NULL))
  if (file.exists(log_file)) {
    prior <- read.csv(log_file, stringsAsFactors = FALSE)
    write.csv(rbind(prior, entries), log_file, row.names = FALSE)
  } else {
    write.csv(entries, log_file, row.names = FALSE)
  }
}

validate_new_listings <- function(df, platform, type) {
  keep    <- rep(TRUE, nrow(df))
  reasons <- rep(NA_character_, nrow(df))

  price_num <- suppressWarnings(as.numeric(df$price))
  area_num  <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", df$area)))

  for (i in seq_len(nrow(df))) {
    r <- character(0)
    p <- price_num[i]; a <- area_num[i]

    if (is.na(p) || p <= 0)          r <- c(r, "price_zero_or_negative")
    else if (p > 50000000)           r <- c(r, "price_above_50M")
    else if (type == "rent" && p > 10000) r <- c(r, "rent_price_above_10000_likely_buy")

    if (!is.na(a)) {
      if (a <= 0)             r <- c(r, "area_zero_or_negative")
      else if (a > 2000)      r <- c(r, "area_above_2000m2")
      else if (!is.na(p) && p > 0) {
        ppm2 <- p / a
        if (ppm2 < 200)       r <- c(r, "price_per_m2_below_200")
        else if (ppm2 > 50000) r <- c(r, "price_per_m2_above_50000")
      }
    }

    if (is.na(df$tipologia[i]) || df$tipologia[i] == "") r <- c(r, "missing_tipologia")
    if (is.na(df$city[i])      || df$city[i] == "")      r <- c(r, "missing_city")

    if (length(r) > 0) { keep[i] <- FALSE; reasons[i] <- paste(r, collapse = "; ") }
  }

  rejected <- df[!keep, ]
  log_entries <- if (nrow(rejected) > 0) {
    data.frame(date = Sys.Date(), id = rejected$id, city = rejected$city,
               platform = platform, reason = reasons[!keep], stringsAsFactors = FALSE)
  } else {
    data.frame(date = character(), id = character(), city = character(),
               platform = character(), reason = character(), stringsAsFactors = FALSE)
  }
  list(valid = df[keep, ], rejected_log = log_entries)
}

mark_duplicates <- function(con) {
  dedup_sql <- "
    UPDATE %s
    SET duplicate_flag = true
    WHERE id IN (
      SELECT a.id FROM %s a
      JOIN %s b
        ON a.id <> b.id
       AND a.platform <> b.platform
       AND a.is_active = 1 AND b.is_active = 1
       AND a.lat IS NOT NULL AND b.lat IS NOT NULL
       AND a.lon IS NOT NULL AND b.lon IS NOT NULL
       AND a.tipologia = b.tipologia
       AND ABS(a.lat::numeric - b.lat::numeric) < 0.0001
       AND ABS(a.lon::numeric - b.lon::numeric) < 0.0001
       AND a.price > 0 AND b.price > 0
       AND ABS(a.price - b.price) / LEAST(a.price, b.price) <= 0.05
      WHERE NOT a.duplicate_flag
    )
  "
  for (tbl in c("ads_buy", "ads_rent")) {
    tryCatch({
      n <- dbExecute(con, sprintf(dedup_sql, tbl, tbl, tbl))
      if (n > 0) message(sprintf("Marked %d duplicate(s) in %s", n, tbl))
    }, error = function(e) message("mark_duplicates failed (", tbl, "): ", e$message))
  }
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

  parsed    <- lapply(result$price, parse_price_raw)
  result$price  <- sapply(parsed, `[[`, "price")
  range_rows    <- sapply(parsed, `[[`, "was_range")
  if (any(range_rows))
    message(sprintf("%d listing(s) had range prices — lower bound used: %s",
                    sum(range_rows), paste(result$id[range_rows], collapse = ", ")))

  return(result)
}

scrape_ad <- function(url, cityname = NA){
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

  neighbourhood <- convert_coordinates_to_neighbourhood(lon, lat, cityname)

  return(c(id,area,tipologia,andar,anunciante,tipo,novo,jardim,energia,elevador,garagem,terraco,varanda,lat,lon,neighbourhood))
}

update <- function(type, city,runstats) {

  today <- Sys.Date()
  price_changes <- 0
  parts <- strsplit(city, "/")[[1]]
  cityname <- parts[length(parts)]

  table_ads    <- ifelse(type == "buy", "ads_buy", "ads_rent")
  table_prices <- ifelse(type == "buy", "price_changes_buy", "price_changes_rent")

  # 1. Scrape first, before opening a DB connection, so the connection is always fresh
  base_url <- paste0(
    ifelse(type == "buy",
           "https://www.imovirtual.com/pt/resultados/comprar/apartamento/",
           "https://www.imovirtual.com/pt/resultados/arrendar/apartamento/"
    ), city, "?page="
  )
  current_ads <- scrape_listings(base_url)
  current_ads <- current_ads[!is.na(current_ads$id) & current_ads$id != "", ]
  print("current ads scraped")

  con <- get_con()
  on.exit(dbDisconnect(con))

  # 2. Read DB - now a simple dataframe, no parsing needed
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
    new_data <- scrape_new_ads(new_listings, today, cityname, type)
  } else {
    new_data <- NULL
  }
  print("new ads inserted into database")

  # Disconnect before the long scraping phase; reconnect for all DB writes to ensure fresh connection
  dbDisconnect(con)
  con <- get_con()
  on.exit(dbDisconnect(con))

  if (!is.null(new_data)) {
    insert_ads(new_data, con, type, cityname)
  }

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
    changed        <- merged[!is.na(merged$price_current) & !is.na(merged$price_db) &
                               merged$price_current != merged$price_db, ]

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

scrape_new_ads <- function(new_listings,date,cityname,type,maxads=4000){
  nads <- min(maxads,nrow(new_listings))
  new_listings  <- new_listings[1:nads,]
  newdata <- matrix(nrow=nads,ncol=16)
  for(i in 1:nads){
    print(i)
    Sys.sleep(2)
    
    result <- tryCatch(
      scrape_ad(new_listings$link[i], cityname),
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
  checked <- validate_new_listings(newdata, "imovirtual", type)
  write_rejection_log(checked$rejected_log)
  if (nrow(checked$rejected_log) > 0)
    message(sprintf("%d listing(s) rejected by sanity checks — see log/rejected_listings.csv",
                    nrow(checked$rejected_log)))
  return(checked$valid)
}

update_price_table <- function(id,dbprice,currentprice,today,pricetable){
  newdata <- c(id,dbprice,currentprice,today)
  newdata <- data.frame(t(newdata))
  colnames(newdata) <- c("id","old_price","new_price","date")
  pricetable <- rbind(pricetable,newdata)
  return(pricetable)
}

is_algarve_city <- function(city) {
  city_norm <- gsub("-", " ", tolower(city))
  city_norm %in% c("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")
}

convert_coordinates_to_neighbourhood <- function(lon, lat, city = NA) {
  # For Algarve cities, use OSM directly (GeoJSON only has city-level boundaries)
  if (!is.na(city) && is_algarve_city(city)) {
    return(get_neighbourhood_from_osm(lon, lat))
  }

  # For other regions, try GeoJSON point-in-polygon matching first
  if (!is.na(lon) && !is.na(lat) && length(GEOJSON_NEIGHBOURHOODS) > 0) {
    tryCatch({
      for (geojson in GEOJSON_NEIGHBOURHOODS) {
        point <- st_point(c(lon, lat))
        point_sf <- st_sf(geometry = st_sfc(point), crs = 4326)
        intersects <- st_intersects(point_sf, geojson, sparse = TRUE)[[1]]

        if (length(intersects) > 0) {
          feature_idx <- intersects[1]
          neighbourhood <- geojson$NAME_3[feature_idx]
          if (is.na(neighbourhood)) {
            neighbourhood <- geojson$NAME_2[feature_idx]
          }
          if (!is.na(neighbourhood)) {
            return(neighbourhood)
          }
        }
      }
    }, error = function(e) {
      # If GeoJSON matching fails, fall back to OSM
      NULL
    })
  }

  # Fall back to OpenStreetMap reverse geocoding
  return(get_neighbourhood_from_osm(lon, lat))
}

get_neighbourhood_from_osm <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) {
    return(NA_character_)
  }

  tryCatch({
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
  }, error = function(e) {
    return(NA_character_)
  })
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
    "SELECT id, price, is_active FROM %s WHERE city = '%s' AND platform = 'imovirtual';",
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
  cities <- c("porto/porto","porto/vila-nova-de-gaia","porto/matosinhos","porto/maia",
              "faro/albufeira","faro/loule","faro/portimao","faro/lagos","faro/lagoa","faro/faro",
              "lisboa/lisboa","lisboa/cascais","lisboa/sintra",
              "setubal/almada","setubal/almada/costa-da-caparica","setubal/almada/caparica-e-trafaria")
  for(city in cities){
    print(paste0("Scraping ",city))
    runstats <- update("rent",city,runstats)
    runstats <- update("buy",city,runstats)
  }
  con <- get_con()
  on.exit(dbDisconnect(con))
  mark_duplicates(con)
  print(paste0("new log data: ", runstats))
  old_log_data <- read.csv("log/scraper_log.csv")
  old_log_data <- rbind(old_log_data, runstats)
  write.csv(old_log_data, "log/scraper_log.csv", row.names = FALSE)
  print("log data written")
}
