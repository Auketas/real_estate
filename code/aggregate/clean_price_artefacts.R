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

# "50 000 - 55 000" was scraped as "5000055000" when the hyphen was stripped before
# non-numeric removal. Try to recover the lower bound by finding the unique split
# point where both halves are plausible prices and the left half < right half.
# Returns the lower bound if exactly one valid split exists, NA otherwise.
try_recover_price <- function(bad_price) {
  s <- sprintf("%.0f", bad_price)
  n <- nchar(s)
  valid <- list()
  for (i in seq_len(n - 1)) {
    p1 <- as.numeric(substr(s, 1, i))
    p2 <- as.numeric(substr(s, i + 1, n))
    if (!is.na(p1) && !is.na(p2) &&
        p1 >= 10000 && p1 <= 50000000 &&
        p2 >= 10000 && p2 <= 50000000 &&
        p1 < p2) {
      valid <- c(valid, list(c(p1, p2)))
    }
  }
  if (length(valid) == 1) valid[[1]][1] else NA_real_
}

clean_table <- function(con, table_name) {
  bad <- dbGetQuery(con, sprintf(
    "SELECT id, price, is_active FROM %s WHERE price > 50000000", table_name
  ))

  if (nrow(bad) == 0) {
    message(table_name, ": no bad prices found")
    return(invisible(data.frame()))
  }

  bad$recovered_price <- sapply(bad$price, try_recover_price)
  bad$action <- ifelse(is.na(bad$recovered_price), "nulled", "recovered")

  message(sprintf(
    "%s: %d bad rows (%d active) — %d recovered, %d nulled",
    table_name, nrow(bad), sum(bad$is_active == 1, na.rm = TRUE),
    sum(bad$action == "recovered"), sum(bad$action == "nulled")
  ))

  # Update recovered prices one row at a time (each has a different new value)
  recovered <- filter(bad, action == "recovered")
  for (i in seq_len(nrow(recovered))) {
    dbExecute(
      con,
      sprintf("UPDATE %s SET price = $1 WHERE id = $2", table_name),
      params = list(recovered$recovered_price[i], recovered$id[i])
    )
  }

  # Null out unrecoverable prices in one statement
  nulled <- filter(bad, action == "nulled")
  if (nrow(nulled) > 0) {
    ids_sql <- paste0("'", nulled$id, "'", collapse = ", ")
    dbExecute(con, sprintf(
      "UPDATE %s SET price = NULL WHERE id IN (%s)", table_name, ids_sql
    ))
  }

  print(bad[order(bad$action), c("id", "price", "recovered_price", "action", "is_active")])
  invisible(mutate(bad, table = table_name))
}

run_price_cleanup <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con))

  message("=== Price artefact cleanup ===")
  buy_log  <- clean_table(con, "ads_buy")
  rent_log <- clean_table(con, "ads_rent")

  combined <- bind_rows(buy_log, rent_log)
  if (nrow(combined) == 0) {
    message("No bad prices found — database is clean")
    return(invisible(NULL))
  }

  active_affected <-
    sum(combined$is_active == 1 & combined$action == "recovered", na.rm = TRUE) +
    sum(combined$is_active == 1 & combined$action == "nulled",    na.rm = TRUE)

  message(sprintf(
    "\nDone — %d total rows fixed (%d recovered, %d nulled). %d were active listings.",
    nrow(combined),
    sum(combined$action == "recovered"),
    sum(combined$action == "nulled"),
    active_affected
  ))
  if (active_affected > 0)
    message("Rerun monthly aggregation to reflect corrected prices.")
  else
    message("No active listings affected — monthly aggregation does not need rerunning.")
}
