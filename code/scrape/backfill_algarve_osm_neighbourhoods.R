#!/usr/bin/env Rscript
# Backfill neighbourhoods for Algarve listings using OSM reverse geocoding
#
# This script:
# 1. Loads all Algarve listings from the database
# 2. For each listing with valid coordinates, looks up the neighbourhood via OSM
# 3. Updates the database with the new neighbourhoods
# 4. Outputs diagnostics showing success rate and any failures
#
# Note: This is a one-time backfill operation. Ongoing listings will get
# neighbourhoods from the scraper (which now uses OSM for Algarve).

library(DBI)
library(RPostgres)
library(tidygeocoder)
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

# Get neighbourhood from OSM via reverse geocoding
get_osm_neighbourhood <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) {
    return(NA_character_)
  }

  tryCatch({
    df <- data.frame(lon = as.numeric(lon), lat = as.numeric(lat))
    result <- reverse_geocode(
      df,
      lat = lat,
      long = lon,
      method = "osm",
      full_results = TRUE
    )

    # Ensure columns exist before coalescing
    if (!("neighbourhood" %in% names(result))) result$neighbourhood <- NA_character_
    if (!("suburb" %in% names(result))) result$suburb <- NA_character_

    neighbourhood <- result %>%
      transmute(
        neighbourhood = coalesce(neighbourhood, suburb)
      ) %>%
      pull(neighbourhood)

    return(neighbourhood)
  }, error = function(e) {
    return(NA_character_)
  })
}

# ============================================================================
# Main backfill logic
# ============================================================================

cat("\n=== ALGARVE OSM NEIGHBOURHOOD BACKFILL ===\n\n")

con <- get_con()
on.exit(dbDisconnect(con))

algarve_cities <- c("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")

# Get all Algarve listings that need neighbourhood lookup
cat("Loading Algarve listings from database...\n")
algarve_listings <- dbGetQuery(con, sprintf(
  "SELECT id, city, lat, lon, neighbourhood FROM ads_buy
   WHERE city IN ('%s') AND lat IS NOT NULL AND lon IS NOT NULL
   ORDER BY city, id;",
  paste(algarve_cities, collapse = "','")
))

cat(sprintf("Found %d Algarve listings with coordinates\n\n", nrow(algarve_listings)))

if (nrow(algarve_listings) == 0) {
  cat("No listings to process.\n")
  quit(status = 0)
}

# Process each listing
cat("Looking up neighbourhoods via OSM...\n")
results <- data.frame(
  id = character(),
  city = character(),
  old_neighbourhood = character(),
  new_neighbourhood = character(),
  changed = logical(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(algarve_listings))) {
  if (i %% 100 == 0) {
    cat(sprintf("  Progress: %d/%d\n", i, nrow(algarve_listings)))
  }

  row <- algarve_listings[i, ]
  new_neighbourhood <- get_osm_neighbourhood(row$lon, row$lat)

  results <- rbind(results, data.frame(
    id = row$id,
    city = row$city,
    old_neighbourhood = row$neighbourhood,
    new_neighbourhood = ifelse(is.na(new_neighbourhood), NA_character_, new_neighbourhood),
    changed = !is.na(new_neighbourhood) && row$neighbourhood != new_neighbourhood,
    stringsAsFactors = FALSE
  ))

  # Small delay to be polite to OSM servers
  Sys.sleep(0.5)
}

cat(sprintf("\nCompleted OSM lookups for all %d listings\n\n", nrow(algarve_listings)))

# ============================================================================
# Diagnostics
# ============================================================================

cat("=== DIAGNOSTICS ===\n\n")

total_listings <- nrow(results)
found_neighbourhood <- sum(!is.na(results$new_neighbourhood))
not_found <- sum(is.na(results$new_neighbourhood))
found_pct <- (found_neighbourhood / total_listings) * 100
not_found_pct <- (not_found / total_listings) * 100

cat(sprintf("Total Algarve listings processed: %d\n", total_listings))
cat(sprintf("Found neighbourhood: %d (%.1f%%)\n", found_neighbourhood, found_pct))
cat(sprintf("NOT found: %d (%.1f%%)\n\n", not_found, not_found_pct))

# Summary by city
cat("--- By City ---\n")
by_city <- results %>%
  group_by(city) %>%
  summarise(
    total = n(),
    found = sum(!is.na(new_neighbourhood)),
    not_found = sum(is.na(new_neighbourhood)),
    pct_found = (sum(!is.na(new_neighbourhood)) / n()) * 100,
    .groups = "drop"
  ) %>%
  arrange(city)

for (i in seq_len(nrow(by_city))) {
  row <- by_city[i, ]
  cat(sprintf(
    "%s: %d total, %d found (%.1f%%), %d not found\n",
    row$city, row$total, row$found, row$pct_found, row$not_found
  ))
}

# List some examples of successful lookups
cat("\n--- Examples of New Neighbourhoods Found ---\n")
examples <- results %>%
  filter(!is.na(new_neighbourhood), changed) %>%
  slice_head(n = 10)

if (nrow(examples) > 0) {
  for (i in seq_len(nrow(examples))) {
    row <- examples[i, ]
    cat(sprintf(
      "%s (%s): %s → %s\n",
      row$id, row$city, row$old_neighbourhood, row$new_neighbourhood
    ))
  }
} else {
  cat("No neighbourhood changes to display\n")
}

# ============================================================================
# Apply updates to database
# ============================================================================

cat("\n=== UPDATING DATABASE ===\n\n")

# Only update rows where we found a new neighbourhood
to_update <- results %>%
  filter(!is.na(new_neighbourhood), changed)

if (nrow(to_update) > 0) {
  cat(sprintf("Updating %d listings with new neighbourhoods...\n", nrow(to_update)))

  for (i in seq_len(nrow(to_update))) {
    row <- to_update[i, ]
    dbExecute(con, sprintf(
      "UPDATE ads_buy SET neighbourhood = '%s' WHERE id = '%s' AND city = '%s';",
      gsub("'", "''", row$new_neighbourhood),
      row$id,
      row$city
    ))

    if (i %% 100 == 0) {
      cat(sprintf("  Updated: %d/%d\n", i, nrow(to_update)))
    }
  }

  cat(sprintf("✓ Successfully updated %d listings\n", nrow(to_update)))
} else {
  cat("No listings to update (no neighbourhood changes detected)\n")
}

# Also update ads_rent table if it has Algarve listings
cat("\nChecking ads_rent table for Algarve listings...\n")
algarve_rent <- dbGetQuery(con, sprintf(
  "SELECT COUNT(*) as cnt FROM ads_rent
   WHERE city IN ('%s') AND lat IS NOT NULL AND lon IS NOT NULL;",
  paste(algarve_cities, collapse = "','")
))

if (algarve_rent$cnt > 0) {
  cat(sprintf("Found %d Algarve rental listings. Running backfill on ads_rent...\n\n", algarve_rent$cnt))

  algarve_rent_listings <- dbGetQuery(con, sprintf(
    "SELECT id, city, lat, lon, neighbourhood FROM ads_rent
     WHERE city IN ('%s') AND lat IS NOT NULL AND lon IS NOT NULL
     ORDER BY city, id;",
    paste(algarve_cities, collapse = "','")
  ))

  rent_results <- data.frame(
    id = character(),
    city = character(),
    old_neighbourhood = character(),
    new_neighbourhood = character(),
    changed = logical(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(algarve_rent_listings))) {
    if (i %% 100 == 0) {
      cat(sprintf("  Progress: %d/%d\n", i, nrow(algarve_rent_listings)))
    }

    row <- algarve_rent_listings[i, ]
    new_neighbourhood <- get_osm_neighbourhood(row$lon, row$lat)

    rent_results <- rbind(rent_results, data.frame(
      id = row$id,
      city = row$city,
      old_neighbourhood = row$neighbourhood,
      new_neighbourhood = ifelse(is.na(new_neighbourhood), NA_character_, new_neighbourhood),
      changed = !is.na(new_neighbourhood) && row$neighbourhood != new_neighbourhood,
      stringsAsFactors = FALSE
    ))

    Sys.sleep(0.5)
  }

  cat("\n--- ads_rent DIAGNOSTICS ---\n\n")
  total_rent <- nrow(rent_results)
  found_rent <- sum(!is.na(rent_results$new_neighbourhood))
  not_found_rent <- sum(is.na(rent_results$new_neighbourhood))
  found_pct_rent <- (found_rent / total_rent) * 100

  cat(sprintf("Total Algarve rental listings: %d\n", total_rent))
  cat(sprintf("Found neighbourhood: %d (%.1f%%)\n", found_rent, found_pct_rent))
  cat(sprintf("NOT found: %d (%.1f%%)\n\n", not_found_rent, (not_found_rent / total_rent) * 100))

  # Update ads_rent
  rent_to_update <- rent_results %>%
    filter(!is.na(new_neighbourhood), changed)

  if (nrow(rent_to_update) > 0) {
    cat(sprintf("Updating %d rental listings with new neighbourhoods...\n", nrow(rent_to_update)))

    for (i in seq_len(nrow(rent_to_update))) {
      row <- rent_to_update[i, ]
      dbExecute(con, sprintf(
        "UPDATE ads_rent SET neighbourhood = '%s' WHERE id = '%s' AND city = '%s';",
        gsub("'", "''", row$new_neighbourhood),
        row$id,
        row$city
      ))

      if (i %% 100 == 0) {
        cat(sprintf("  Updated: %d/%d\n", i, nrow(rent_to_update)))
      }
    }

    cat(sprintf("✓ Successfully updated %d rental listings\n", nrow(rent_to_update)))
  }
} else {
  cat("No Algarve rental listings found\n")
}

cat("\n=== BACKFILL COMPLETE ===\n\n")
