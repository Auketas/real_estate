library(DBI)
library(RPostgres)
library(dplyr)

get_con <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("NEON_DBNAME"),
    host     = Sys.getenv("NEON_HOST"),
    user     = Sys.getenv("NEON_USER"),
    password = Sys.getenv("NEON_PASSWORD"),
    port     = 5432,
    sslmode  = "require"
  )
}

# ---- Helpers ----------------------------------------------------------------

mode_val <- function(x) {
  t <- table(x[!is.na(x) & x != ""])
  if (length(t) == 0) return(NA_character_)
  names(which.max(t))
}

pull_active_listings <- function(con, table_name, listing_type) {
  dbGetQuery(con, sprintf(
    "SELECT city, neighbourhood, tipologia, area, price, first_seen, last_seen
     FROM %s
     WHERE is_active = 1
       AND (duplicate_flag = false OR duplicate_flag IS NULL)",
    table_name
  )) %>%
    mutate(
      listing_type   = listing_type,
      price          = suppressWarnings(as.numeric(price)),
      area_m2        = suppressWarnings(as.numeric(gsub("[^0-9.]", "", area))),
      days_on_market = pmax(0, as.numeric(as.Date(last_seen) - as.Date(first_seen)))
    ) %>%
    select(-area)
}

# ---- Main -------------------------------------------------------------------

run_daily_aggregation <- function() {
  snapshot_date <- Sys.Date()
  message("Running daily aggregation for ", snapshot_date)

  con <- get_con()
  on.exit(dbDisconnect(con))

  ads <- bind_rows(
    pull_active_listings(con, "ads_buy",  "buy"),
    pull_active_listings(con, "ads_rent", "rent")
  )
  ads$city[ads$city == "matosinhos"] <- "porto"
  message(nrow(ads), " active listings loaded")

  # Precompute price_per_m2 — used in both summaries
  ads <- ads %>%
    mutate(price_per_m2 = ifelse(!is.na(area_m2) & area_m2 > 0 & !is.na(price) & price > 0,
                                 price / area_m2, NA_real_))

  # ---- City summary ----------------------------------------------------------
  city_rows <- ads %>%
    filter(!is.na(price) & price > 0) %>%
    group_by(city, listing_type) %>%
    summarise(
      listing_count           = n(),
      median_price            = median(price, na.rm = TRUE),
      median_price_per_m2     = median(price_per_m2, na.rm = TRUE),
      avg_time_on_market_days = mean(days_on_market, na.rm = TRUE),
      p25_price               = quantile(price, 0.25, na.rm = TRUE),
      p75_price               = quantile(price, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(snapshot_date = snapshot_date)

  bad_city <- filter(city_rows,
    is.na(median_price_per_m2) |
    (listing_type == "buy"  & (median_price_per_m2 < 500  | median_price_per_m2 > 20000)) |
    (listing_type == "rent" & (median_price_per_m2 < 3    | median_price_per_m2 > 100)))
  if (nrow(bad_city) > 0) {
    message("WARNING — implausible median price/m² in city rows:")
    print(bad_city[, c("city", "listing_type", "median_price_per_m2")])
  }

  # ---- Neighbourhood summary -------------------------------------------------
  nbhd_rows <- ads %>%
    filter(!is.na(price) & price > 0 & !is.na(neighbourhood) & neighbourhood != "") %>%
    group_by(city, neighbourhood, listing_type) %>%
    summarise(
      listing_count             = n(),
      median_price              = median(price, na.rm = TRUE),
      median_price_per_m2       = median(price_per_m2, na.rm = TRUE),
      avg_time_on_market_days   = mean(days_on_market, na.rm = TRUE),
      most_common_property_type = mode_val(tipologia),
      .groups = "drop"
    ) %>%
    mutate(snapshot_date = snapshot_date)

  # ---- Neighbourhood threshold / city-level fallback ------------------------
  MIN_NBHD_LISTINGS <- 10

  nbhd_rows_filtered <- filter(nbhd_rows, listing_count >= MIN_NBHD_LISTINGS)

  # City+type combos where every neighbourhood fell below the threshold
  dropped_combos <- anti_join(
    distinct(nbhd_rows,          city, listing_type),
    distinct(nbhd_rows_filtered, city, listing_type),
    by = c("city", "listing_type")
  )

  if (nrow(dropped_combos) > 0) {
    message(nrow(dropped_combos), " city/type combo(s) had no neighbourhood meeting the ",
            MIN_NBHD_LISTINGS, "-listing threshold — adding city-level fallback rows:")
    print(dropped_combos)
    fallback_rows <- ads %>%
      filter(!is.na(price) & price > 0) %>%
      inner_join(dropped_combos, by = c("city", "listing_type")) %>%
      group_by(city, listing_type) %>%
      summarise(
        listing_count             = n(),
        median_price              = median(price, na.rm = TRUE),
        median_price_per_m2       = median(price_per_m2, na.rm = TRUE),
        avg_time_on_market_days   = mean(days_on_market, na.rm = TRUE),
        most_common_property_type = mode_val(tipologia),
        .groups = "drop"
      ) %>%
      mutate(neighbourhood = city, snapshot_date = snapshot_date)
    nbhd_rows <- bind_rows(nbhd_rows_filtered, fallback_rows)
  } else {
    nbhd_rows <- nbhd_rows_filtered
  }

  # ---- Write -----------------------------------------------------------------
  dbExecute(con, "DELETE FROM city_latest_summary")
  dbExecute(con, "DELETE FROM neighbourhood_latest_summary")

  dbWriteTable(con, "city_latest_summary",         city_rows, append = TRUE, row.names = FALSE)
  dbWriteTable(con, "neighbourhood_latest_summary", nbhd_rows, append = TRUE, row.names = FALSE)

  message(sprintf("Done — %d city rows, %d neighbourhood rows written",
                  nrow(city_rows), nrow(nbhd_rows)))
}
