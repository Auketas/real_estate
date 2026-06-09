library(DBI)
library(RPostgres)
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

# ---- Check overall unmatched neighbourhood percentage ----
cat("\n=== NEIGHBOURHOOD MATCHING STATUS ===\n")
result <- dbGetQuery(con, "
SELECT
  'Buy' as listing_type,
  COUNT(*) as total_listings,
  COUNT(CASE WHEN neighbourhood IS NULL OR neighbourhood = '' THEN 1 END) as unmatched,
  ROUND(100.0 * COUNT(CASE WHEN neighbourhood IS NULL OR neighbourhood = '' THEN 1 END) / COUNT(*), 2) as pct_unmatched
FROM ads_buy
WHERE is_active = 1
UNION ALL
SELECT
  'Rent' as listing_type,
  COUNT(*) as total_listings,
  COUNT(CASE WHEN neighbourhood IS NULL OR neighbourhood = '' THEN 1 END) as unmatched,
  ROUND(100.0 * COUNT(CASE WHEN neighbourhood IS NULL OR neighbourhood = '' THEN 1 END) / COUNT(*), 2) as pct_unmatched
FROM ads_rent
WHERE is_active = 1
")
print(result)

# ---- Check for Foz in Porto ----
cat("\n=== FOZ IN PORTO (BUY LISTINGS) ===\n")
foz_buy <- dbGetQuery(con, "
SELECT COUNT(*) as count
FROM ads_buy
WHERE is_active = 1 AND city = 'porto' AND (neighbourhood ILIKE '%Foz%' OR neighbourhood = 'FozDoDouro')
")
print(foz_buy)

cat("FOZ IN PORTO (RENT LISTINGS)\n")
foz_rent <- dbGetQuery(con, "
SELECT COUNT(*) as count
FROM ads_rent
WHERE is_active = 1 AND city = 'porto' AND (neighbourhood ILIKE '%Foz%' OR neighbourhood = 'FozDoDouro')
")
print(foz_rent)

# ---- Top neighbourhoods in Porto (to see if Foz exists at all) ----
cat("\n=== TOP 30 NEIGHBOURHOODS IN PORTO (BUY) ===\n")
porto_top <- dbGetQuery(con, "
SELECT neighbourhood, COUNT(*) as count
FROM ads_buy
WHERE is_active = 1 AND city = 'porto'
GROUP BY neighbourhood
ORDER BY count DESC
LIMIT 30
")
print(porto_top)

# ---- Check NULL neighbourhoods per city ----
cat("\n=== NULL/EMPTY NEIGHBOURHOODS BY CITY (BUY) ===\n")
null_by_city <- dbGetQuery(con, "
SELECT
  city,
  COUNT(*) as total,
  COUNT(CASE WHEN neighbourhood IS NULL OR neighbourhood = '' THEN 1 END) as null_count,
  ROUND(100.0 * COUNT(CASE WHEN neighbourhood IS NULL OR neighbourhood = '' THEN 1 END) / COUNT(*), 2) as pct_null
FROM ads_buy
WHERE is_active = 1
GROUP BY city
ORDER BY pct_null DESC
")
print(null_by_city)

# ---- Find neighbourhoods in database but missing from lookup ----
cat("\n=== UNMATCHED NEIGHBOURHOODS (not in lookup.json) ===\n")

# Read the lookup file
lookup_json <- jsonlite::read_json("dashboard/static/neighbourhood_lookup.json")
matched_names <- names(lookup_json)

# Get all unique neighbourhoods from DB
db_neighbourhoods <- dbGetQuery(con, "
SELECT DISTINCT neighbourhood, city
FROM (
  SELECT neighbourhood, city FROM ads_buy WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
  UNION
  SELECT neighbourhood, city FROM ads_rent WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
) t
ORDER BY city, neighbourhood
")

# Check which ones aren't in the lookup
unmatched <- db_neighbourhoods$neighbourhood[!db_neighbourhoods$neighbourhood %in% matched_names]
if (length(unmatched) > 0) {
  cat(sprintf("Found %d unmatched neighbourhoods:\n", length(unmatched)))
  unmatched_df <- db_neighbourhoods[db_neighbourhoods$neighbourhood %in% unmatched, ]
  unmatched_summary <- unmatched_df %>%
    group_by(city) %>%
    summarise(count = n(), neighbourhoods = paste(neighbourhood, collapse = ", "))
  print(unmatched_summary)
} else {
  cat("All neighbourhoods are matched!\n")
}
