library(DBI)
library(RPostgres)
library(httr)
library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidygeocoder)
library(sf)

get_con <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("NEON_DBNAME"),
    host = Sys.getenv("NEON_HOST"),
    user = Sys.getenv("NEON_USER"),
    password = Sys.getenv("NEON_PASSWORD"),
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

    almada_path <- file.path(geojson_dir, "almada.geojson")
    if (file.exists(almada_path)) {
      geojsons[["almada"]] <- st_read(almada_path, quiet = TRUE)
    }

    return(geojsons)
  }, error = function(e) {
    message("Could not load GeoJSON files: ", e$message)
    return(list())
  })
}

# Global GeoJSON data (loaded once at startup)
GEOJSON_NEIGHBOURHOODS <- load_geojson_neighbourhoods()

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

# ---- Headers ----------------------------------------------------------------
# Casa Sapo blocks basic User-Agent strings; full browser headers are required.

sapo_headers <- function(referer = "https://casa.sapo.pt/") {
  add_headers(
    "User-Agent"      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    "Accept-Language" = "pt-PT,pt;q=0.9,en-US;q=0.8,en;q=0.7",
    "Accept-Encoding" = "gzip, deflate, br",
    "Cache-Control"   = "max-age=0",
    "Sec-Fetch-Dest"  = "document",
    "Sec-Fetch-Mode"  = "navigate",
    "Sec-Fetch-Site"  = "none",
    "Sec-Fetch-User"  = "?1",
    "Referer"         = referer
  )
}

# ---- Rate-limit-aware GET ---------------------------------------------------

# Retries on HTTP 429 with exponential backoff (up to max_tries attempts).
safe_get <- function(url, headers, max_tries = 5) {
  wait <- 30
  for (i in seq_len(max_tries)) {
    resp <- GET(url, headers)
    if (status_code(resp) != 429) return(resp)
    message("429 Too Many Requests — waiting ", wait, "s before retry ", i, "/", max_tries)
    Sys.sleep(wait)
    wait <- wait * 2
  }
  resp  # return last response; caller will hit the read_html error naturally
}

# ---- Listing page -----------------------------------------------------------

# Scrape one page of search results.
# Returns list(results = data.frame, nads = integer)
get_listing_info_sapo <- function(page_num, base_url) {
  url  <- paste0(base_url, page_num)
  resp <- safe_get(url, sapo_headers("https://casa.sapo.pt/"))
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
    Sys.sleep(runif(1, 15, 25))

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
  result <- result[grepl("€", result$price), ]   # keep EUR prices only

  parsed     <- lapply(result$price, parse_price_raw)
  result$price   <- sapply(parsed, `[[`, "price")
  range_rows     <- sapply(parsed, `[[`, "was_range")
  if (any(range_rows))
    message(sprintf("%d listing(s) had range prices — lower bound used: %s",
                    sum(range_rows), paste(result$id[range_rows], collapse = ", ")))

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
  resp <- safe_get(url, sapo_headers(referer))
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
  area <- get_main_feature(page, "Área útil")  # "Área útil"
  area <- str_replace_all(area, "m²|\\s", "")       # strip m² and spaces

  estado <- get_main_feature(page, "Estado")

  # Energy certificate (e.g. "A", "B", "C+", "D", "F", "Isento")
  energia <- get_main_feature(page, "Certificação Energética")

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
  terraco <- ifelse(any(grepl("^Terraço$", feature_texts)), 1, 0)
  jardim  <- ifelse(any(grepl("Jardim", feature_texts, ignore.case = TRUE)), 1, 0)

  # Novo: Estado contains "Novo" or "construção"
  novo <- ifelse(!is.na(estado) &&
                   grepl("Novo|construção", estado, ignore.case = TRUE),
                 "Sim", "Não")

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

  # Try GeoJSON point-in-polygon matching first
  if (length(GEOJSON_NEIGHBOURHOODS) > 0) {
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

scrape_new_ads_sapo <- function(new_listings, date, cityname, type, maxads = 1500) {
  nads         <- min(maxads, nrow(new_listings))
  new_listings <- new_listings[seq_len(nads), ]
  newdata      <- matrix(nrow = nads, ncol = 16)

  for (i in seq_len(nads)) {
    cat("Scraping ad", i, "of", nads, "\n")
    Sys.sleep(runif(1, 10, 15))
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
  newdata <- newdata[!is.na(newdata[, 1]), ]
  checked <- validate_new_listings(newdata, "casa_sapo", type)
  write_rejection_log(checked$rejected_log)
  if (nrow(checked$rejected_log) > 0)
    message(sprintf("%d listing(s) rejected by sanity checks — see log/rejected_listings.csv",
                    nrow(checked$rejected_log)))
  checked$valid
}

# ---- Cities -----------------------------------------------------------------

cities_sapo <- c("porto", "vila-nova-de-gaia", "lisboa", "albufeira", "loule",
                 "portimao", "lagos", "lagoa", "faro")

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

# ---- Database helpers -------------------------------------------------------

read_ads <- function(con, city, type) {
  table_name <- ifelse(type == "buy", "ads_buy", "ads_rent")
  dbGetQuery(con, sprintf(
    "SELECT id, price FROM %s WHERE city = '%s' AND platform = 'casa_sapo';",
    table_name, city
  ))
}

insert_ads <- function(df, con, type, city) {
  table_name <- ifelse(type == "buy", "ads_buy", "ads_rent")
  dbWriteTable(con, table_name, df, append = TRUE, row.names = FALSE)
}

# ---- Per-city update --------------------------------------------------------

update <- function(type, city, runstats) {
  base_url <- paste0(
    "https://casa.sapo.pt/",
    ifelse(type == "buy", "comprar", "arrendar"),
    "/apartamentos/", city, "/?page="
  )

  today         <- Sys.Date()
  price_changes <- 0

  table_ads    <- ifelse(type == "buy", "ads_buy", "ads_rent")
  table_prices <- ifelse(type == "buy", "price_changes_buy", "price_changes_rent")

  # Phase 1: scrape listing pages (no DB connection held during slow work)
  current_ads <- scrape_listings_sapo(base_url)
  print("listings scraped")

  # Phase 2: quick read, then close connection before the slow per-ad scrape
  con    <- get_con()
  db_ads <- read_ads(con, city, type)
  dbDisconnect(con)
  print("database read")

  new_ids      <- setdiff(current_ads$id, db_ads$id)
  existing_ids <- intersect(current_ads$id, db_ads$id)
  inactive_ids <- setdiff(db_ads$id, current_ads$id)
  print("ids extracted")

  # Phase 3: slow per-ad scraping (no connection held)
  new_listings <- current_ads[current_ads$id %in% new_ids, ]
  new_data     <- if (nrow(new_listings) > 0) scrape_new_ads_sapo(new_listings, today, city, type) else NULL

  # Phase 4: fresh connection for all writes
  con <- get_con()
  on.exit(dbDisconnect(con))

  if (!is.null(new_data)) {
    insert_ads(new_data, con, type, city)
  }
  print("new ads inserted into database")

  if (length(existing_ids) > 0) {
    ids_sql <- paste0("('", paste(existing_ids, collapse = "','"), "')")
    dbExecute(con, sprintf(
      "UPDATE %s SET last_seen = '%s' WHERE id IN %s AND city = '%s' AND platform = 'casa_sapo';",
      table_ads, today, ids_sql, city
    ))

    current_subset <- current_ads[current_ads$id %in% existing_ids, c("id", "price")]
    db_subset      <- db_ads[db_ads$id %in% existing_ids, c("id", "price")]
    merged         <- merge(current_subset, db_subset, by = "id", suffixes = c("_current", "_db"))
    changed        <- merged[merged$price_current != merged$price_db, ]

    if (nrow(changed) > 0) {
      price_changes_df <- data.frame(
        id        = changed$id,
        city      = city,
        old_price = as.numeric(changed$price_db),
        new_price = as.numeric(changed$price_current),
        date      = today,
        platform  = "casa_sapo"
      )
      dbWriteTable(con, table_prices, price_changes_df, append = TRUE, row.names = FALSE)

      for (i in 1:nrow(changed)) {
        dbExecute(con, sprintf(
          "UPDATE %s SET price = %f WHERE id = '%s' AND city = '%s' AND platform = 'casa_sapo';",
          table_ads, as.numeric(changed$price_current[i]), changed$id[i], city
        ))
      }

      price_changes <- nrow(changed)
    }
  }
  print("existing ads updated")

  if (length(inactive_ids) > 0) {
    ids_sql <- paste0("('", paste(inactive_ids, collapse = "','"), "')")
    dbExecute(con, sprintf(
      "UPDATE %s SET is_active = 0 WHERE id IN %s AND city = '%s' AND platform = 'casa_sapo';",
      table_ads, ids_sql, city
    ))
  }
  print("inactive ads updated")

  runstats$new_listings      <- runstats$new_listings      + length(new_ids)
  runstats$existing_listings <- runstats$existing_listings + length(existing_ids)
  runstats$inactive_listings <- runstats$inactive_listings + length(inactive_ids)
  runstats$price_changes     <- runstats$price_changes     + price_changes

  return(runstats)
}

# ---- Main entry point -------------------------------------------------------

update_database <- function() {
  message("Waiting 60s before starting scrape to avoid rate limiting...")
  Sys.sleep(60)

  cities_sapo <- c("porto", "vila-nova-de-gaia", "matosinhos", "maia",
                   "albufeira", "loule", "portimao", "lagos", "lagoa", "faro",
                   "lisboa", "cascais", "sintra", "almada")
  runstats <- list(
    "date"               = Sys.Date(),
    "new_listings"       = 0,
    "existing_listings"  = 0,
    "inactive_listings"  = 0,
    "price_changes"      = 0,
    "platform"           = "casa_sapo"
  )
  for (city in cities_sapo) {
    print(paste0("Scraping ", city))
    runstats <- update("rent", city, runstats)
    Sys.sleep(runif(1, 15, 30))
    runstats <- update("buy",  city, runstats)
    Sys.sleep(runif(1, 15, 30))
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
