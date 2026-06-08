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

run_monthly_aggregation <- function() {
  snapshot_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  message("Running monthly aggregation for ", snapshot_month)

  con <- get_con()
  on.exit(dbDisconnect(con))

  # Copy today's _latest_ snapshot into the monthly archive for this month
  message("Copying latest summaries into monthly archive for ", snapshot_month)
  city_latest <- dbGetQuery(con, "SELECT * FROM city_latest_summary")
  nbhd_latest <- dbGetQuery(con, "SELECT * FROM neighbourhood_latest_summary")

  # Replace snapshot_date with snapshot_month for archive
  city_latest$snapshot_month <- snapshot_month
  city_latest$snapshot_date <- NULL
  nbhd_latest$snapshot_month <- snapshot_month
  nbhd_latest$snapshot_date <- NULL

  # Attach prior-month prices for monthly_price_change_pct (neighbourhood only)
  prior_month <- as.Date(format(snapshot_month - 1, "%Y-%m-01"))
  prior <- dbGetQuery(con, sprintf(
    "SELECT city, neighbourhood, listing_type, median_price AS prior_price
     FROM neighbourhood_monthly_summary WHERE snapshot_month = '%s'",
    prior_month
  ))

  nbhd_latest <- nbhd_latest %>%
    left_join(prior, by = c("city", "neighbourhood", "listing_type")) %>%
    mutate(
      monthly_price_change_pct = ifelse(
        !is.na(prior_price) & prior_price > 0,
        round((median_price - prior_price) / prior_price * 100, 2),
        NA_real_
      )
    ) %>%
    select(-prior_price)

  # Delete existing month and insert archived snapshot
  dbExecute(con, sprintf(
    "DELETE FROM city_monthly_summary WHERE snapshot_month = '%s'", snapshot_month))
  dbExecute(con, sprintf(
    "DELETE FROM neighbourhood_monthly_summary WHERE snapshot_month = '%s'", snapshot_month))

  dbWriteTable(con, "city_monthly_summary",         city_latest, append = TRUE, row.names = FALSE)
  dbWriteTable(con, "neighbourhood_monthly_summary", nbhd_latest, append = TRUE, row.names = FALSE)

  message(sprintf("Archived %d city rows, %d neighbourhood rows for %s",
                  nrow(city_latest), nrow(nbhd_latest), snapshot_month))
}
