#!/usr/bin/env Rscript
"""
Backfill neighbourhoods for all listings using spatial join (point-in-polygon).

This script goes through ALL listings in ads_buy and ads_rent and assigns them to
neighbourhoods by finding which GeoJSON polygon their coordinates fall inside.

Approach:
1. Load all GeoJSON files (porto, lisboa, algarve, almada)
2. For each listing with lon/lat, find the polygon it's inside
3. Use the polygon's NAME_3 (parishes) or NAME_2 (municipalities) as the neighbourhood
4. Update the database with the corrected neighbourhood

This is the same approach used by the scrapers for new listings.
"""

library(DBI)
library(RPostgres)
library(sf)
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

# ============================================================================
# Load GeoJSON files
# ============================================================================
cat("\n=== LOADING GEOJSON FILES ===\n")

geojson_dir <- "dashboard/static"
geojsons <- list()

# Porto region
porto_path <- file.path(geojson_dir, "porto_region.geojson")
if (file.exists(porto_path)) {
  geojsons[["porto"]] <- st_read(porto_path, quiet = TRUE)
  cat(sprintf("✓ Porto: %d features\n", nrow(geojsons[["porto"]])))
}

# Lisboa region
lisboa_path <- file.path(geojson_dir, "lisboa_region.geojson")
if (file.exists(lisboa_path)) {
  geojsons[["lisboa"]] <- st_read(lisboa_path, quiet = TRUE)
  cat(sprintf("✓ Lisboa: %d features\n", nrow(geojsons[["lisboa"]])))
}

# Algarve region
algarve_path <- file.path(geojson_dir, "algarve.geojson")
if (file.exists(algarve_path)) {
  geojsons[["algarve"]] <- st_read(algarve_path, quiet = TRUE)
  cat(sprintf("✓ Algarve: %d features\n", nrow(geojsons[["algarve"]])))
}

# Almada region
almada_path <- file.path(geojson_dir, "almada.geojson")
if (file.exists(almada_path)) {
  geojsons[["almada"]] <- st_read(almada_path, quiet = TRUE)
  cat(sprintf("✓ Almada: %d features\n", nrow(geojsons[["almada"]])))
}

# ============================================================================
# Function to assign neighbourhood via spatial join
# ============================================================================
assign_neighbourhood_spatial <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) return(NA_character_)

  tryCatch({
    for (geojson in geojsons) {
      point <- st_point(c(lon, lat))
      point_sf <- st_sf(geometry = st_sfc(point), crs = 4326)

      intersects <- st_intersects(point_sf, geojson, sparse = TRUE)[[1]]

      if (length(intersects) > 0) {
        feature_idx <- intersects[1]
        neighbourhood <- geojson$NAME_3[feature_idx]

        if (is.na(neighbourhood)) {
          neighbourhood <- geojson$NAME_2[feature_idx]
        }

        if (!is.na(neighbourhood)) {
          return(neighbourhood)
        }
      }
    }
    return(NA_character_)
  }, error = function(e) {
    return(NA_character_)
  })
}

# ============================================================================
# Get listings with valid coordinates
# ============================================================================
cat("\n=== PROCESSING LISTINGS ===\n")

con <- get_con()
on.exit(dbDisconnect(con))

# Get buy listings with coordinates
cat("Fetching buy listings with coordinates...\n")
buy_listings <- dbGetQuery(con, "
  SELECT id, lat, lon FROM ads_buy
  WHERE lat IS NOT NULL AND lon IS NOT NULL AND lat != 0 AND lon != 0
") %>%
  as_tibble()

cat(sprintf("  Found %d buy listings with coordinates\n", nrow(buy_listings)))

# Get rent listings with coordinates
cat("Fetching rent listings with coordinates...\n")
rent_listings <- dbGetQuery(con, "
  SELECT id, lat, lon FROM ads_rent
  WHERE lat IS NOT NULL AND lon IS NOT NULL AND lat != 0 AND lon != 0
") %>%
  as_tibble()

cat(sprintf("  Found %d rent listings with coordinates\n", nrow(rent_listings)))

# ============================================================================
# Backfill BUY listings
# ============================================================================
cat("\n=== BACKFILLING BUY LISTINGS ===\n")

buy_listings$neighbourhood <- NA_character_

for (i in seq_len(nrow(buy_listings))) {
  if (i %% 5000 == 0) {
    cat(sprintf("  Progress: %d / %d\n", i, nrow(buy_listings)))
  }

  neighbourhood <- assign_neighbourhood_spatial(
    buy_listings$lon[i],
    buy_listings$lat[i]
  )

  buy_listings$neighbourhood[i] <- neighbourhood
}

# Count results
buy_matched <- sum(!is.na(buy_listings$neighbourhood))
buy_unmatched <- sum(is.na(buy_listings$neighbourhood))

cat(sprintf("\nBuy listings neighbourhood assignment results:\n"))
cat(sprintf("  Matched: %d (%.1f%%)\n", buy_matched, 100 * buy_matched / nrow(buy_listings)))
cat(sprintf("  Unmatched: %d (%.1f%%)\n", buy_unmatched, 100 * buy_unmatched / nrow(buy_listings)))

# Update database
cat("Updating database with matched neighbourhoods...\n")

for (i in seq_len(nrow(buy_listings))) {
  if (!is.na(buy_listings$neighbourhood[i])) {
    dbExecute(con, "
      UPDATE ads_buy
      SET neighbourhood = $1
      WHERE id = $2
    ",
      params = list(buy_listings$neighbourhood[i], buy_listings$id[i])
    )
  }
}

cat("✓ Buy listings updated\n")

# ============================================================================
# Backfill RENT listings
# ============================================================================
cat("\n=== BACKFILLING RENT LISTINGS ===\n")

rent_listings$neighbourhood <- NA_character_

for (i in seq_len(nrow(rent_listings))) {
  if (i %% 5000 == 0) {
    cat(sprintf("  Progress: %d / %d\n", i, nrow(rent_listings)))
  }

  neighbourhood <- assign_neighbourhood_spatial(
    rent_listings$lon[i],
    rent_listings$lat[i]
  )

  rent_listings$neighbourhood[i] <- neighbourhood
}

# Count results
rent_matched <- sum(!is.na(rent_listings$neighbourhood))
rent_unmatched <- sum(is.na(rent_listings$neighbourhood))

cat(sprintf("\nRent listings neighbourhood assignment results:\n"))
cat(sprintf("  Matched: %d (%.1f%%)\n", rent_matched, 100 * rent_matched / nrow(rent_listings)))
cat(sprintf("  Unmatched: %d (%.1f%%)\n", rent_unmatched, 100 * rent_unmatched / nrow(rent_listings)))

# Update database
cat("Updating database with matched neighbourhoods...\n")

for (i in seq_len(nrow(rent_listings))) {
  if (!is.na(rent_listings$neighbourhood[i])) {
    dbExecute(con, "
      UPDATE ads_rent
      SET neighbourhood = $1
      WHERE id = $2
    ",
      params = list(rent_listings$neighbourhood[i], rent_listings$id[i])
    )
  }
}

cat("✓ Rent listings updated\n")

# ============================================================================
# Final summary
# ============================================================================
cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")

total_matched <- buy_matched + rent_matched
total_processed <- nrow(buy_listings) + nrow(rent_listings)

cat(sprintf("\nTotal listings processed: %d\n", total_processed))
cat(sprintf("Total matched to polygons: %d (%.1f%%)\n", total_matched, 100 * total_matched / total_processed))
cat(sprintf("Total still unmatched: %d (%.1f%%)\n", total_processed - total_matched, 100 * (total_processed - total_matched) / total_processed))

cat("\nBackfill complete! Re-run the neighbourhood coverage analysis to verify results.\n")
