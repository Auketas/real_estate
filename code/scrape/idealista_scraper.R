library(httr)
library(rvest)
library(assertthat)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidygeocoder)

# NOTE: Idealista uses Cloudflare protection. A plain GET with a User-Agent
# header may return a 403 or a JS challenge page rather than real HTML.
# If requests consistently fail, a headless browser (e.g. chromote/RSelenium)
# will be required. The structure below is correct assuming the HTML is returned.

get_request <- function(url) {
  GET(
    url,
    add_headers(
      "User-Agent"      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
      "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "pt-PT,pt;q=0.9,en;q=0.8",
      "Accept-Encoding" = "gzip, deflate, br",
      "Connection"      = "keep-alive"
    )
  )
}

# ---------------------------------------------------------------------------
# 1.  Scrape a single results page
# ---------------------------------------------------------------------------
get_listing_info <- function(page_num, base_url) {
  url <- paste0(base_url, "?pagina=", page_num)

  resp <- get_request(url)

  if (status_code(resp) != 200) {
    warning(paste("HTTP", status_code(resp), "on page", page_num))
    return(list(results = NULL, nads = 0))
  }

  page <- read_html(resp)

  # Each listing is an <article class="item"> with a data-element-id attribute
  cards <- page %>% html_elements("article.item")
  nads  <- length(cards)

  if (nads == 0) return(list(results = NULL, nads = 0))

  results <- lapply(cards, function(card) {
    id <- card %>% html_attr("data-element-id")

    link <- card %>%
      html_element("a.item-link") %>%
      html_attr("href")

    description <- card %>%
      html_element("a.item-link") %>%
      html_text(trim = TRUE)

    price_raw <- card %>%
      html_element("span.item-price") %>%
      html_text(trim = TRUE)

    if (is.na(id) || is.na(link) || is.na(price_raw)) return(NULL)

    data.frame(
      id          = id,
      link        = link,
      description = description,
      price       = price_raw,
      stringsAsFactors = FALSE
    )
  })

  results <- do.call(rbind, Filter(Negate(is.null), results))
  list(results = results, nads = nads)
}

# ---------------------------------------------------------------------------
# 2.  Paginate through all results pages
# ---------------------------------------------------------------------------
scrape_listings <- function(base_url) {
  all_ids          <- character()
  all_links        <- character()
  all_descriptions <- character()
  all_prices       <- character()

  page           <- 1
  nzeros         <- 0
  nsame          <- 0
  olddescriptions <- character()

  repeat {
    cat("Scraping page", page, "\n")

    pageresults  <- get_listing_info(page, base_url)
    results      <- pageresults$results
    nads         <- pageresults$nads

    if (is.null(results) || nrow(results) == 0) {
      nzeros <- nzeros + 1
    } else {
      nzeros <- 0
    }

    if (nzeros == 5) {
      message("No listings on five consecutive pages — stopping.")
      break
    }

    descriptions <- results$description
    if (identical(descriptions, olddescriptions)) {
      nsame <- nsame + 1
    } else {
      nsame <- 0
    }
    if (nsame == 5) {
      message("Same listings on five consecutive pages — stopping.")
      break
    }

    ids   <- results$id
    links <- paste0("https://www.idealista.pt", results$link)
    prices <- results$price

    assert_that(length(ids)   == length(links),  msg = paste("ids/links mismatch on page", page))
    assert_that(length(ids)   == length(descriptions), msg = paste("ids/descriptions mismatch on page", page))
    assert_that(length(ids)   == length(prices), msg = paste("ids/prices mismatch on page", page))

    cat(length(ids), "listings scraped\n")

    all_ids          <- c(all_ids, ids)
    all_links        <- c(all_links, links)
    all_descriptions <- c(all_descriptions, descriptions)
    all_prices       <- c(all_prices, prices)
    olddescriptions  <- descriptions

    page <- page + 1
    Sys.sleep(2)  # be polite
  }

  result        <- data.frame(id = all_ids, link = all_links, description = all_descriptions, price = all_prices, stringsAsFactors = FALSE)
  result        <- result[!duplicated(result$id), ]
  # Keep only rows where price ends with €
  result        <- result[grepl("€\\s*$", result$price), ]
  result$price  <- as.numeric(gsub("[^0-9]", "", result$price))
  result
}

# ---------------------------------------------------------------------------
# 3.  Scrape an individual listing page
# ---------------------------------------------------------------------------
scrape_ad <- function(url) {
  resp <- get_request(url)

  if (status_code(resp) != 200) {
    warning(paste("HTTP", status_code(resp), "—", url))
    return(rep(NA, 16))
  }

  page <- read_html(resp)

  # --- ID from URL ---------------------------------------------------------
  id <- str_extract(url, "[0-9]{7,}")

  # --- Basic feature list --------------------------------------------------
  # Idealista puts typology/area/floor as <li> items in the main detail block.
  # The exact class depends on the page version; adjust if needed.
  features <- page %>%
    html_elements("div#details .details-property-feature-one li, ul.details-property_features li") %>%
    html_text(trim = TRUE)

  # Area: contains "m²"
  area_raw  <- features[grepl("m²", features)][1]
  area      <- ifelse(is.na(area_raw), NA, gsub("[^0-9,\\.]", "", area_raw))

  # Tipologia: T0, T1, T2 ... or number of quartos
  tipologia_raw <- features[grepl("T[0-9]|quarto", features, ignore.case = TRUE)][1]
  tipologia     <- ifelse(is.na(tipologia_raw), NA, str_extract(tipologia_raw, "T[0-9]|[0-9]+ quarto"))

  # Andar (floor)
  andar_raw <- features[grepl("andar|piso|rés", features, ignore.case = TRUE)][1]
  andar     <- ifelse(is.na(andar_raw), NA, andar_raw)

  # Tipo de imóvel (property type)
  tipo_raw <- features[grepl("apartamento|moradia|estúdio|loft|cobertura", features, ignore.case = TRUE)][1]
  tipo     <- ifelse(is.na(tipo_raw), NA, tipo_raw)

  # --- Advertiser type -----------------------------------------------------
  # Idealista shows "Particular" or agency name in a dedicated section
  anunciante_node <- page %>%
    html_element(".advertiser-name, .professional-name, span.txt-bold")
  anunciante <- ifelse(is.null(anunciante_node) || is.na(anunciante_node),
                       NA,
                       html_text(anunciante_node, trim = TRUE))

  # --- Extras / amenities --------------------------------------------------
  # Idealista lists extras in <ul class="details-property_features"> blocks,
  # typically labelled "Equipamento" / "Outras características"
  extras <- page %>%
    html_elements("div#details li, .details-property-feature-two li") %>%
    html_text(trim = TRUE) %>%
    tolower()

  novo    <- as.integer(any(grepl("nova construção|obra nova", extras)))
  jardim  <- as.integer(any(grepl("jardim", extras)))
  elevador <- as.integer(any(grepl("elevador", extras)))
  garagem  <- as.integer(any(grepl("garagem|estacionamento", extras)))
  terraco  <- as.integer(any(grepl("terraço", extras)))
  varanda  <- as.integer(any(grepl("varanda", extras)))

  # --- Energy certificate --------------------------------------------------
  energia_node <- page %>%
    html_element(".energy-certificate-badge span, .icon-energy-certificate + span, [class*='energy'] span")
  energia <- ifelse(is.null(energia_node) || is.na(energia_node),
                    NA,
                    html_text(energia_node, trim = TRUE))

  # --- Coordinates ---------------------------------------------------------
  # Idealista embeds lat/lon in a <script> block; the variable name can vary.
  # Common patterns: "lat", "lng" / "longitude" inside window._env_ or JSON-LD.
  lat <- NA
  lon <- NA

  # Try JSON-LD first
  jsonld <- page %>%
    html_element("script[type='application/ld+json']") %>%
    html_text()

  if (!is.null(jsonld) && !is.na(jsonld)) {
    tryCatch({
      jdata <- fromJSON(jsonld)
      lat   <- jdata$geo$latitude  %||% jdata$latitude
      lon   <- jdata$geo$longitude %||% jdata$longitude
    }, error = function(e) NULL)
  }

  # Fallback: raw script text pattern matching
  if (is.na(lat)) {
    scripts <- page %>%
      html_elements("script") %>%
      html_text()
    for (s in scripts) {
      lat_match <- str_match(s, '"latitude"\\s*:\\s*([0-9\\.\\-]+)')
      lon_match <- str_match(s, '"longitude"\\s*:\\s*([0-9\\.\\-]+)')
      if (!is.na(lat_match[1, 2])) {
        lat <- as.numeric(lat_match[1, 2])
        lon <- as.numeric(lon_match[1, 2])
        break
      }
    }
  }

  # --- Neighbourhood -------------------------------------------------------
  neighbourhood <- tryCatch(
    convert_coordinates_to_neighbourhood(as.numeric(lon), as.numeric(lat)),
    error = function(e) NA
  )

  c(id, area, tipologia, andar, anunciante, tipo, novo, jardim, energia, elevador, garagem, terraco, varanda, lat, lon, neighbourhood)
}

# ---------------------------------------------------------------------------
# Null-coalescing helper (mimics %||% from rlang if not loaded)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

# ---------------------------------------------------------------------------
# 4.  Scrape details for all new listings
# ---------------------------------------------------------------------------
scrape_new_ads <- function(new_listings, date, cityname, maxads = 4000) {
  nads         <- min(maxads, nrow(new_listings))
  new_listings <- new_listings[seq_len(nads), ]
  newdata      <- matrix(nrow = nads, ncol = 16)

  for (i in seq_len(nads)) {
    cat("Scraping ad", i, "of", nads, "\n")
    Sys.sleep(2)

    result <- tryCatch(
      scrape_ad(new_listings$link[i]),
      error = function(e) {
        message(paste("Error at row", i, ":", e$message))
        rep(NA, 16)
      }
    )

    newdata[i, ] <- result
  }

  newdata           <- data.frame(newdata, stringsAsFactors = FALSE)
  colnames(newdata) <- c("id", "area", "tipologia", "andar", "anunciante", "tipo",
                         "novo", "jardim", "energia", "elevador", "garagem",
                         "terraco", "varanda", "lat", "lon", "neighbourhood")
  newdata$price      <- new_listings$price
  newdata$is_active  <- 1
  newdata$first_seen <- date
  newdata$last_seen  <- date
  newdata$city       <- cityname
  newdata            <- newdata[!is.na(newdata$id), ]
  newdata
}

# ---------------------------------------------------------------------------
# 5.  Reverse-geocode coordinates → neighbourhood  (shared with imovirtual)
# ---------------------------------------------------------------------------
convert_coordinates_to_neighbourhood <- function(lon, lat) {
  df     <- data.frame(lon = lon, lat = lat)
  result <- reverse_geocode(df, lat = lat, long = lon, method = "osm", full_results = TRUE)

  for (col in c("neighbourhood", "suburb")) {
    if (!col %in% names(result)) result[[col]] <- NA_character_
  }

  result %>%
    transmute(neighbourhood = coalesce(neighbourhood, suburb)) %>%
    pull(neighbourhood)
}

# ---------------------------------------------------------------------------
# 6.  Top-level convenience function
#     type  : "buy" or "rent"
#     city  : idealista city slug, e.g. "porto", "lisboa", "algarve/albufeira"
# ---------------------------------------------------------------------------
scrape_idealista <- function(type, city) {
  base_url <- paste0(
    "https://www.idealista.pt/",
    ifelse(type == "buy", "comprar-casas", "arrendar-casas"),
    "/", city, "/"
  )

  cat("Starting idealista scrape:", base_url, "\n")
  listings <- scrape_listings(base_url)
  cat(nrow(listings), "listings collected from search pages\n")

  today   <- Sys.Date()
  cityname <- tail(strsplit(city, "/")[[1]], 1)

  details <- scrape_new_ads(listings, today, cityname)
  cat(nrow(details), "listings with full details\n")

  details
}

# ---------------------------------------------------------------------------
# Example usage (not run automatically):
#
#   porto_rent <- scrape_idealista("rent", "porto")
#   porto_buy  <- scrape_idealista("buy",  "porto")
# ---------------------------------------------------------------------------
