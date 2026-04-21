library(httr)
library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidygeocoder)

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
# ---- Headers ----------------------------------------------------------------
# Casa Sapo blocks basic User-Agent strings; full browser headers are required.

sapo_headers <- function(referer = "https://casa.sapo.pt/") {
  add_headers(
    "User-Agent"      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
    "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    "Accept-Language" = "pt-PT,pt;q=0.9,en;q=0.8",
    "Accept-Encoding" = "identity",
    "Referer"         = referer
  )
}

# ---- Listing page -----------------------------------------------------------

# Scrape one page of search results.
# Returns list(results = data.frame, nads = integer)
get_listing_info_sapo <- function(page_num, base_url) {
  url  <- paste0(base_url, page_num)
  resp <- GET(url, sapo_headers("https://casa.sapo.pt/"))
  page <- read_html(resp)

  # Each card is a div whose id begins with "property_"
  cards <- page %>% html_elements("div[id^='property_']")
  nads  <- length(cards)

  results <- lapply(cards, function(card) {
    id <- card %>% html_attr("id") %>% str_replace("^property_", "")

    # The href is a gespub tracking redirect; real URL is in the l= parameter.
    href <- card %>%
      html_element("a.property-info") %>%
      html_attr("href")
    link <- str_match(href, "[?&]l=(https://casa\\.sapo\\.pt/[^?]+)")[, 2]

    description <- card %>%
      html_element("div.property-type") %>%
      html_text(trim = TRUE)

    price <- card %>%
      html_element("div.property-price-value") %>%
      html_text(trim = TRUE)

    if (is.na(id) || is.na(link) || is.na(price)) return(NULL)

    data.frame(id = id, link = link, description = description, price = price,
               stringsAsFactors = FALSE)
  })

  results <- do.call(rbind, Filter(Negate(is.null), results))
  list(results = results, nads = nads)
}

# Scrape all pages for a given base URL until no new listings appear.
scrape_listings_sapo <- function(base_url) {
  all_ids          <- character(0)
  all_links        <- character(0)
  all_descriptions <- character(0)
  all_prices       <- character(0)
  page             <- 1
  nzeros           <- 0
  nsame            <- 0
  old_descriptions <- character(0)

  repeat {
    cat("Scraping page", page, "\n")
    Sys.sleep(1.5)

    pageresults <- get_listing_info_sapo(page, base_url)
    results     <- pageresults$results
    nads        <- pageresults$nads

    if (nads == 0 || is.null(results) || nrow(results) == 0) {
      nzeros <- nzeros + 1
    } else {
      nzeros <- 0
    }

    if (nzeros >= 3) {
      cat("No listings found on three subsequent pages — stopping.\n")
      break
    }

    if (!is.null(results) && nrow(results) > 0) {
      descs <- results$description
      if (identical(descs, old_descriptions)) {
        nsame <- nsame + 1
      } else {
        nsame <- 0
      }
      if (nsame >= 5) {
        cat("Same listings repeated on five subsequent pages — stopping.\n")
        break
      }

      cat(nrow(results), "listings found\n")
      all_ids          <- c(all_ids, results$id)
      all_links        <- c(all_links, results$link)
      all_descriptions <- c(all_descriptions, results$description)
      all_prices       <- c(all_prices, results$price)
      old_descriptions <- descs
    }

    page <- page + 1
  }

  result <- data.frame(
    id          = all_ids,
    link        = all_links,
    description = all_descriptions,
    price       = all_prices,
    stringsAsFactors = FALSE
  )
  result <- result[!duplicated(result$id), ]
  result <- result[grepl("\u20ac", result$price), ]   # keep EUR prices only
  result$price <- as.numeric(gsub("[^0-9]", "", result$price))
  result
}

# ---- Individual ad page -----------------------------------------------------

# Extract a value from the "Dados do imóvel" key-value grid.
# title_text must match the label exactly (after html_text).
get_main_feature <- function(page, title_text) {
  items <- page %>% html_elements("div.detail-main-features-item")
  for (item in items) {
    title <- item %>%
      html_element("div.detail-main-features-item-title") %>%
      html_text(trim = TRUE)
    if (!is.na(title) && title == title_text) {
      val <- item %>%
        html_element("div.detail-main-features-item-value") %>%
        html_text(trim = TRUE)
      return(if (is.na(val) || val == "") NA_character_ else val)
    }
  }
  NA_character_
}

# Extract the value from a "Label: Value" style feature-list item.
get_feature_value <- function(feature_texts, label) {
  pattern <- paste0("^", label, ":\\s*(.+)$")
  matches <- str_match(feature_texts, pattern)
  idx     <- which(!is.na(matches[, 1]))[1]
  if (is.na(idx)) return(NA_character_)
  str_trim(matches[idx, 2])
}

# Scrape one individual property page.
# Returns a named character vector with 16 fields (same order as imovirtual).
scrape_ad_sapo <- function(url) {
  # Use listing page as referer to look like a natural navigation
  listing_base <- str_extract(url, "https://casa\\.sapo\\.pt/[^/]+/apartamentos/[^/]+/")
  referer <- ifelse(is.na(listing_base), "https://casa.sapo.pt/", listing_base)
  resp <- GET(url, sapo_headers(referer))
  page <- read_html(resp)

  # --- ID (UUID from data-uid attribute) ---
  id <- page %>%
    html_element("div.detail-media") %>%
    html_attr("data-uid")

  # --- Tipologia from h1 (e.g. "Apartamento T2 para comprar no Porto") ---
  tipologia <- page %>%
    html_element("h1") %>%
    html_text(trim = TRUE) %>%
    str_extract("T[0-9]+")

  # --- Main key-value features ---
  area <- get_main_feature(page, "\u00c1rea \u00fatil")  # "Área útil"
  area <- str_replace_all(area, "m\u00b2|\\s", "")       # strip m² and spaces

  estado <- get_main_feature(page, "Estado")

  # Energy certificate (e.g. "A", "B", "C+", "D", "F", "Isento")
  energia <- get_main_feature(page, "Certifica\u00e7\u00e3o Energ\u00e9tica")

  # --- Feature list (detail-features-item) ---
  feature_texts <- page %>%
    html_elements("div.detail-features-item") %>%
    html_text(trim = TRUE)

  andar    <- get_feature_value(feature_texts, "Piso")
  elevador <- get_feature_value(feature_texts, "Elevador")

  garagem_val <- get_feature_value(feature_texts, "Garagem")
  garagem     <- ifelse(!is.na(garagem_val) && grepl("Sim", garagem_val), 1, 0)

  varanda_val <- get_feature_value(feature_texts, "Varandas")
  varanda     <- ifelse(!is.na(varanda_val) &&
                          !is.na(suppressWarnings(as.numeric(varanda_val))) &&
                          suppressWarnings(as.numeric(varanda_val)) > 0, 1, 0)

  # "Terraço" appears as a plain label (no colon/value)
  terraco <- ifelse(any(grepl("^Terra\u00e7o$", feature_texts)), 1, 0)
  jardim  <- ifelse(any(grepl("Jardim", feature_texts, ignore.case = TRUE)), 1, 0)

  # Novo: Estado contains "Novo" or "construção"
  novo <- ifelse(!is.na(estado) &&
                   grepl("Novo|constru\u00e7\u00e3o", estado, ignore.case = TRUE),
                 "Sim", "N\u00e3o")

  tipo <- "Apartamento"

  # --- Advertiser type ---
  ami_text   <- page %>% html_element("div.detail-owner-ami") %>% html_text(trim = TRUE)
  anunciante <- ifelse(is.na(ami_text) || ami_text == "", "Particular", "Profissional")

  # --- Coordinates from JSON-LD Offer schema ---
  lat <- NA_real_; lon <- NA_real_
  ld_scripts <- page %>%
    html_elements("script[type='application/ld+json']") %>%
    html_text()
  for (s in ld_scripts) {
    tryCatch({
      d <- fromJSON(s)
      if (!is.null(d$availableAtOrFrom$geo$latitude)) {
        lat <- as.numeric(d$availableAtOrFrom$geo$latitude)
        lon <- as.numeric(d$availableAtOrFrom$geo$longitude)
        break
      }
    }, error = function(e) NULL)
  }

  # --- Neighbourhood via OSM reverse geocoding ---
  neighbourhood <- convert_coordinates_to_neighbourhood_sapo(lon, lat)

  c(id, area, tipologia, andar, anunciante, tipo, novo, jardim,
    energia, elevador, garagem, terraco, varanda, lat, lon, neighbourhood)
}

# ---- Reverse geocoding ------------------------------------------------------

convert_coordinates_to_neighbourhood_sapo <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) return(NA_character_)
  df     <- data.frame(lon = as.numeric(lon), lat = as.numeric(lat))
  result <- reverse_geocode(df, lat = lat, long = lon,
                             method = "osm", full_results = TRUE)
  for (col in c("neighbourhood", "suburb")) {
    if (!col %in% names(result)) result[[col]] <- NA_character_
  }
  result %>%
    transmute(neighbourhood = coalesce(neighbourhood, suburb)) %>%
    pull(neighbourhood)
}

# ---- Batch new-ads scraper --------------------------------------------------

scrape_new_ads_sapo <- function(new_listings, date, cityname, maxads = 4000) {
  nads         <- min(maxads, nrow(new_listings))
  new_listings <- new_listings[seq_len(nads), ]
  newdata      <- matrix(nrow = nads, ncol = 16)

  for (i in seq_len(nads)) {
    cat("Scraping ad", i, "of", nads, "\n")
    Sys.sleep(2)
    result <- tryCatch(
      scrape_ad_sapo(new_listings$link[i]),
      error = function(e) {
        message("Error at row ", i, ": ", e$message)
        rep(NA_character_, 16)
      }
    )
    newdata[i, ] <- result
  }

  newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  colnames(newdata) <- c("id", "area", "tipologia", "andar", "anunciante", "tipo",
                         "novo", "jardim", "energia", "elevador", "garagem",
                         "terraco", "varanda", "lat", "lon", "neighbourhood")
  newdata$price      <- new_listings$price
  newdata$is_active  <- 1
  newdata$first_seen <- date
  newdata$last_seen  <- date
  newdata$city       <- cityname
  newdata$platform   <- "casa_sapo"
  newdata[!is.na(newdata[, 1]), ]
}

# ---- Cities & entry point ---------------------------------------------------

# Same cities as imovirtual, using Casa Sapo's URL slugs.
cities_sapo <- c("porto", "lisboa", "albufeira", "loule",
                 "portimao", "lagos", "lagoa", "faro")

scrape_city_sapo <- function(type, city) {
  base_url <- paste0(
    "https://casa.sapo.pt/",
    ifelse(type == "buy", "comprar", "arrendar"),
    "/apartamentos/", city, "/?page="
  )
  cat("--- Scraping", type, city, "---\n")
  listings <- scrape_listings_sapo(base_url)
  cat("Total listings:", nrow(listings), "\n")
  listings
}

# ---- Test on a single listing page ------------------------------------------

test_sapo_single_page <- function(city = "porto", type = "buy") {
  base_url <- paste0(
    "https://casa.sapo.pt/",
    ifelse(type == "buy", "comprar", "arrendar"),
    "/apartamentos/", city, "/?page="
  )

  cat("=== Listing page test ===\n")
  page_result <- get_listing_info_sapo(1, base_url)
  cat("Cards found on page 1:", page_result$nads, "\n\n")
  print(head(page_result$results, 3))

  if (!is.null(page_result$results) && nrow(page_result$results) > 0) {
    cat("\n=== Detail page test (first listing) ===\n")
    detail_raw <- scrape_ad_sapo(page_result$results$link[1])
    detail <- setNames(as.list(detail_raw),
                       c("id", "area", "tipologia", "andar", "anunciante", "tipo",
                         "novo", "jardim", "energia", "elevador", "garagem",
                         "terraco", "varanda", "lat", "lon", "neighbourhood"))
    print(as.data.frame(detail))
  }
}

read_ads <- function(con, city, type) {
  table_name <- ifelse(type == "buy", "ads_buy", "ads_rent")
  dbGetQuery(con, sprintf(
    "SELECT id, price FROM %s WHERE city = '%s';",
    table_name, city
  ))
}

insert_ads <- function(df, con, type, city) {
  table_name <- ifelse(type == "buy", "ads_buy", "ads_rent")
  dbWriteTable(con, table_name, df, append = TRUE, row.names = FALSE)
}

scrape_new_ads <- function(new_listings,date,cityname,maxads=4000){
  nads <- min(maxads,nrow(new_listings$results))
  new_listings  <- new_listings[1:nads,]
  newdata <- matrix(nrow=nads,ncol=16)
  for(i in 1:nads){
    print(i)
    Sys.sleep(2)
    
    result <- tryCatch(
      scrape_ad(new_listings$results$link[i]),
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

update <- function(type,city,runstats){
  base_url <- paste0(
    "https://casa.sapo.pt/",
    ifelse(type == "buy", "comprar", "arrendar"),
    "/apartamentos/", city, "/?page="
  )
  con <- get_con()
  on.exit(dbDisconnect(con))  # ensures connection is closed even if error occurs
  
  today <- Sys.Date()
  price_changes <- 0
  
  table_ads    <- ifelse(type == "buy", "ads_buy", "ads_rent")
  table_prices <- ifelse(type == "buy", "price_changes_buy", "price_changes_rent")
  
  current_ads <- scrape_listings_sapo(base_url)
  print("listings scraped")
  
  db_ads <- read_ads(con, cityname, type)
  print("database read")
  
  new_ids      <- setdiff(current_ads$id, db_ads$id)
  existing_ids <- intersect(current_ads$id, db_ads$id)
  inactive_ids <- setdiff(
    db_ads$id[db_ads$is_active == 1],
    current_ads$id
  )
  print("ids extracted")
  
  new_listings <- current_ads[current_ads$id %in% new_ids, ]
  if(nrow(new_listings)>0){
    new_data <- scrape_new_ads(new_listings, today, city)
    con <- safe_con(con)
    insert_ads(new_data, con, type, cityname)
  }
  print("new ads inserted into database")
  
  if (length(existing_ids) > 0) {
    
    # Bulk update last_seen for all existing ids
    ids_sql <- paste0("('", paste(existing_ids, collapse = "','"), "')")
    dbExecute(con, sprintf(
      "UPDATE %s SET last_seen = '%s' WHERE id IN %s AND city = '%s' AND platform = '%s';",
      table_ads, today, ids_sql, cityname, "casa_sapo"
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
        platform  = "casa_sapo"
      )
      dbWriteTable(con, table_prices, price_changes_df, append = TRUE, row.names = FALSE)
      
      # Bulk update prices - loop is still needed here but only for changed rows
      # which should be a small subset
      for (i in 1:nrow(changed)) {
        dbExecute(con, sprintf(
          "UPDATE %s SET price = %f WHERE id = '%s' AND city = '%s' AND platform = '%s';",
          table_ads, as.numeric(changed$price_current[i]), changed$id[i], cityname, "casa_sapo"
        ))
      }
      
      price_changes <- nrow(changed)
    }
  }
  print("existing ads updated")
  
  con <- safe_con(con)
  if (length(inactive_ids) > 0) {
    ids_sql <- paste0("('", paste(inactive_ids, collapse = "','"), "')")
    dbExecute(con, sprintf(
      "UPDATE %s SET is_active = 0 WHERE id IN %s AND city = '%s' AND platform = '%s';",
      table_ads, ids_sql, cityname, "casa_sapo"
    ))
  }
  print("inactive ads updated")
  
  runstats$new_listings <- runstats$new_listings+length(new_ids)
  runstats$existing_listings <- runstats$existing_listings+length(existing_ids)
  runstats$inactive_listings <- runstats$inactive_listings+length(inactive_ids)
  runstats$price_changes <- runstats$price_changes+price_changes
  
  return(runstats)
}

update_database(){
  cities_sapo <- c("porto", "lisboa", "albufeira", "loule",
                   "portimao", "lagos", "lagoa", "faro")
  runstats <- list("date"=Sys.Date(),"new_listings"=0,"existing_listings"=0,"inactive_listings"=0,"price_changes"=0,"plattform"="casa sapo")
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
