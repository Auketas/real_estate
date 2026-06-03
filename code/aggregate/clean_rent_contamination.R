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

PRICE_THRESHOLD <- 10000  # monthly rent cannot legitimately exceed this

run_clean_rent_contamination <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con))

  # 1. Count contaminated rows
  bad <- dbGetQuery(con, sprintf(
    "SELECT id, platform, price, city, is_active FROM ads_rent WHERE price > %d",
    PRICE_THRESHOLD
  ))

  if (nrow(bad) == 0) {
    message("No contaminated rows found in ads_rent — nothing to do")
    return(invisible(NULL))
  }

  message(sprintf(
    "Found %d contaminated rows in ads_rent (price > %d) — %d active, %d inactive",
    nrow(bad), PRICE_THRESHOLD,
    sum(bad$is_active == 1, na.rm = TRUE),
    sum(bad$is_active != 1 | is.na(bad$is_active))
  ))

  by_platform <- bad %>% count(platform)
  message("By platform:")
  for (i in seq_len(nrow(by_platform)))
    message(sprintf("  %s: %d rows", by_platform$platform[i], by_platform$n[i]))

  # 2. Find rows NOT already in ads_buy (to avoid losing data)
  to_move <- dbGetQuery(con, sprintf(
    "SELECT r.* FROM ads_rent r
     WHERE r.price > %d
       AND NOT EXISTS (
         SELECT 1 FROM ads_buy b WHERE b.id = r.id AND b.platform = r.platform
       )",
    PRICE_THRESHOLD
  ))

  already_in_buy <- nrow(bad) - nrow(to_move)
  message(sprintf(
    "\n%d already exist in ads_buy (will just delete from ads_rent)",
    already_in_buy
  ))
  message(sprintf(
    "%d not in ads_buy — moving to ads_buy before deleting",
    nrow(to_move)
  ))

  # 3. Move the ones not in ads_buy
  if (nrow(to_move) > 0) {
    dbWriteTable(con, "ads_buy", to_move, append = TRUE, row.names = FALSE)
    message(sprintf("  Moved %d rows to ads_buy", nrow(to_move)))
  }

  # 4. Remove referencing rows in price_changes_rent first (FK constraint)
  n_pc_deleted <- dbExecute(con, sprintf(
    "DELETE FROM price_changes_rent
     WHERE id IN (SELECT id FROM ads_rent WHERE price > %d)",
    PRICE_THRESHOLD
  ))
  if (n_pc_deleted > 0)
    message(sprintf("Deleted %d rows from price_changes_rent (FK cleanup)", n_pc_deleted))

  # 5. Delete all contaminated rows from ads_rent
  n_deleted <- dbExecute(con, sprintf(
    "DELETE FROM ads_rent WHERE price > %d", PRICE_THRESHOLD
  ))
  message(sprintf("\nDeleted %d rows from ads_rent", n_deleted))
  message("Done. Rerun monthly aggregation to reflect clean rent data.")
}
