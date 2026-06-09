library(DBI)
library(RPostgres)
library(jsonlite)
library(dplyr)

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

con <- get_con()
on.exit(dbDisconnect(con))

# Load current lookup
current_lookup <- fromJSON("dashboard/static/neighbourhood_lookup.json")
current_db_names <- names(current_lookup)

# Get all neighbourhoods with their listing counts
nbh_counts <- dbGetQuery(con, "
SELECT neighbourhood, city, COUNT(*) as count
FROM (
  SELECT neighbourhood, city FROM ads_buy WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
  UNION ALL
  SELECT neighbourhood, city FROM ads_rent WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
) t
GROUP BY neighbourhood, city
ORDER BY count DESC
")

# Find which ones are unmatched
nbh_counts$is_matched <- nbh_counts$neighbourhood %in% current_db_names

cat("=== TOP 30 NEIGHBOURHOODS BY LISTING COUNT ===\n")
cat("(matched=YES means it's in neighbourhood_lookup.json)\n\n")
print(head(nbh_counts[, c("neighbourhood", "city", "count", "is_matched")], 30))

cat("\n=== UNMATCHED NEIGHBOURHOODS WITH >20 LISTINGS ===\n\n")
high_impact <- nbh_counts[!nbh_counts$is_matched & nbh_counts$count > 20, ]
if (nrow(high_impact) > 0) {
  print(high_impact[, c("neighbourhood", "city", "count")])
  cat(sprintf("\nTotal listings in high-impact unmatched neighbourhoods: %d\n",
              sum(high_impact$count)))
} else {
  cat("None found!\n")
}

cat("\n=== UNMATCHED NEIGHBOURHOODS WITH >10 LISTINGS ===\n\n")
medium_impact <- nbh_counts[!nbh_counts$is_matched & nbh_counts$count > 10, ]
if (nrow(medium_impact) > 0) {
  print(medium_impact[, c("neighbourhood", "city", "count")])
  cat(sprintf("\nTotal listings in medium-impact unmatched neighbourhoods: %d\n",
              sum(medium_impact$count)))
} else {
  cat("None found!\n")
}

# Check Maia specifically
cat("\n=== MAIA NEIGHBOURHOODS ===\n")
maia_nbh <- nbh_counts[nbh_counts$city == "maia", ]
if (nrow(maia_nbh) > 0) {
  print(maia_nbh[, c("neighbourhood", "count", "is_matched")])
  maia_unmatched <- sum(maia_nbh$count[!maia_nbh$is_matched])
  maia_total <- sum(maia_nbh$count)
  cat(sprintf("\nMaia: %d unmatched out of %d total (%.1f%%)\n",
              maia_unmatched, maia_total, 100 * maia_unmatched / maia_total))
} else {
  cat("No Maia neighbourhoods found\n")
}
