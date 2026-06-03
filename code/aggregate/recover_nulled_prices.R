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

# Same fixed recovery function — ratio constraint added vs original run.
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
        p1 < p2 &&
        p2 / p1 <= 2) {
      valid <- c(valid, list(c(p1, p2)))
    }
  }
  if (length(valid) == 1) valid[[1]][1] else NA_real_
}

# ---------------------------------------------------------------------------
# FILL THIS IN from the [NULLED] lines in the clean_price_artefacts log.
# Copy every line that starts with [NULLED], and for each one add a row below:
#   table_name  = "ads_buy" or "ads_rent"
#   id          = the id value from the log line
#   orig_price  = the number before " -> NULL" in the log line
# ---------------------------------------------------------------------------
nulled_log <- data.frame(
  table_name = character(),
  id         = character(),
  orig_price = numeric(),
  stringsAsFactors = FALSE
)

# Example (delete these and replace with real rows from your log):
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="01acb7bd-dd6e-11ef-8add-060000000054", orig_price=250000260000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="5388eea6-0a9f-11f1-8ef4-060000000058", orig_price=700000760000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="461679d1-1e58-11f1-b3d3-060000000057", orig_price=349000370000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="87e92b6d-d9b1-11f0-9e61-060000000056", orig_price=510000560000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="d727909c-23f0-11ef-8add-060000000054", orig_price=203000250000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="b59afdc7-3827-11f1-90a1-060000000056", orig_price=769000800000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="951faf63-625e-11f0-b3d3-060000000057", orig_price=650000700000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="12234d50-ace1-11ef-8add-060000000054", orig_price=235000250000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="62f13096-f784-11f0-8add-060000000054", orig_price=250000260000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="7b91f38e-f6fa-11f0-8add-060000000054", orig_price=249000260000, stringsAsFactors=FALSE))
 nulled_log <- rbind(nulled_log, data.frame(table_name="ads_buy",  id="2f483248-421f-11f0-a463-060000000052", orig_price=489000500000, stringsAsFactors=FALSE))
 
# ---------------------------------------------------------------------------

run_recovery <- function() {
  if (nrow(nulled_log) == 0) stop("nulled_log is empty — fill it in first")

  nulled_log$recovered_price <- sapply(nulled_log$orig_price, try_recover_price)
  still_ambiguous <- filter(nulled_log, is.na(recovered_price))
  recoverable     <- filter(nulled_log, !is.na(recovered_price))

  message(sprintf("%d rows to recover, %d still ambiguous after ratio fix",
                  nrow(recoverable), nrow(still_ambiguous)))

  if (nrow(still_ambiguous) > 0) {
    message("Still ambiguous (will remain NULL):")
    for (i in seq_len(nrow(still_ambiguous)))
      message(sprintf("  %s id=%s  orig=%s", still_ambiguous$table_name[i],
                      still_ambiguous$id[i],
                      formatC(still_ambiguous$orig_price[i], format = "f", digits = 0)))
  }

  if (nrow(recoverable) == 0) {
    message("Nothing to update.")
    return(invisible(NULL))
  }

  con <- get_con()
  on.exit(dbDisconnect(con))

  for (i in seq_len(nrow(recoverable))) {
    row <- recoverable[i, ]
    dbExecute(
      con,
      sprintf("UPDATE %s SET price = $1 WHERE id = $2", row$table_name),
      params = list(row$recovered_price, row$id)
    )
    message(sprintf("  FIXED %s id=%-12s  %s -> %s",
      row$table_name, row$id,
      formatC(row$orig_price,      format = "f", digits = 0),
      formatC(row$recovered_price, format = "f", digits = 0)
    ))
  }

  message(sprintf("\nDone — %d prices restored. Rerun monthly aggregation if any were active listings.",
                  nrow(recoverable)))
}

run_recovery()
