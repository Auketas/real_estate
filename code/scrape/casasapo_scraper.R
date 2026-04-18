library(httr)
library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidygeocoder)

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
