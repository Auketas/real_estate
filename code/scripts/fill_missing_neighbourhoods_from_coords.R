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

convert_coordinates_to_neighbourhood <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) return(NA_character_)
  df <- data.frame(lon = as.numeric(lon), lat = as.numeric(lat))
  result <- reverse_geocode(
    df,
    lat = lat,
    long = lon,
    method = "osm",
    full_results = TRUE
  )

  expected_cols <- c("neighbourhood", "suburb")
  for (col in expected_cols) {
    if (!col %in% names(result)) {
      result[[col]] <- NA_character_
    }
  }

  neighbourhood <- result %>%
    transmute(
      neighbourhood = coalesce(neighbourhood, suburb)
    ) %>%
    pull(neighbourhood)

  return(neighbourhood)
}

con <- get_con()
on.exit(dbDisconnect(con))

total_assigned <- 0

safe_update <- function(con, table_name, nbh_id_pairs) {
  if (nrow(nbh_id_pairs) == 0) return(0)

  con <- safe_con(con)  # reconnect if needed
  updated <- 0
  query <- sprintf("UPDATE %s SET neighbourhood = $1 WHERE id = $2", table_name)

  for (i in seq_len(nrow(nbh_id_pairs))) {
    tryCatch({
      dbExecute(con, query,
        params = list(nbh_id_pairs$neighbourhood[i], nbh_id_pairs$id[i])
      )
      updated <- updated + 1
    }, error = function(e) {
      cat(sprintf("Error updating row %d: %s\n", i, e$message))
    })
  }
  updated
}

# ---- Find listings with coordinates but no neighbourhood ----
cat("Finding listings with coordinates but missing neighbourhoods...\n")

missing_nbh <- dbGetQuery(con, "
SELECT id, city, lat, lon
FROM ads_buy
WHERE is_active = 1
  AND (neighbourhood IS NULL OR neighbourhood = '')
  AND lat IS NOT NULL
  AND lon IS NOT NULL
LIMIT 500
")

cat(sprintf("Found %d listings to backfill\n", nrow(missing_nbh)))

if (nrow(missing_nbh) > 0) {
  # Reverse geocode each coordinate
  cat("Reverse geocoding coordinates (this may take a few minutes)...\n")
  missing_nbh$neighbourhood <- NA_character_

  for (i in seq_len(nrow(missing_nbh))) {
    if (i %% 50 == 0) {
      cat(sprintf("Progress: %d/%d\n", i, nrow(missing_nbh)))
    }

    tryCatch({
      nbh <- convert_coordinates_to_neighbourhood(
        missing_nbh$lon[i],
        missing_nbh$lat[i]
      )
      missing_nbh$neighbourhood[i] <- nbh
      Sys.sleep(1)  # be polite to OSM
    }, error = function(e) {
      cat(sprintf("Error geocoding row %d: %s\n", i, e$message))
    })
  }

  # Filter to rows that actually got a neighbourhood assigned
  filled <- missing_nbh[!is.na(missing_nbh$neighbourhood), ]

  if (nrow(filled) > 0) {
    cat(sprintf("\nSuccessfully geocoded %d listings\n", nrow(filled)))

    # Update database with parameterized queries
    updated <- safe_update(con, "ads_buy", filled[, c("neighbourhood", "id")])
    cat(sprintf("Updated %d rows in ads_buy\n", updated))
    total_assigned <- total_assigned + updated
  } else {
    cat("No listings successfully geocoded\n")
  }
} else {
  cat("No listings with missing neighbourhoods found\n")
}

# ---- Same for ads_rent ----
cat("\n--- Now checking ads_rent ---\n")

missing_rent <- dbGetQuery(con, "
SELECT id, city, lat, lon
FROM ads_rent
WHERE is_active = 1
  AND (neighbourhood IS NULL OR neighbourhood = '')
  AND lat IS NOT NULL
  AND lon IS NOT NULL
LIMIT 500
")

cat(sprintf("Found %d rental listings to backfill\n", nrow(missing_rent)))

if (nrow(missing_rent) > 0) {
  cat("Reverse geocoding coordinates (this may take a few minutes)...\n")
  missing_rent$neighbourhood <- NA_character_

  for (i in seq_len(nrow(missing_rent))) {
    if (i %% 50 == 0) {
      cat(sprintf("Progress: %d/%d\n", i, nrow(missing_rent)))
    }

    tryCatch({
      nbh <- convert_coordinates_to_neighbourhood(
        missing_rent$lon[i],
        missing_rent$lat[i]
      )
      missing_rent$neighbourhood[i] <- nbh
      Sys.sleep(1)  # be polite to OSM
    }, error = function(e) {
      cat(sprintf("Error geocoding row %d: %s\n", i, e$message))
    })
  }

  filled_rent <- missing_rent[!is.na(missing_rent$neighbourhood), ]

  if (nrow(filled_rent) > 0) {
    cat(sprintf("\nSuccessfully geocoded %d rental listings\n", nrow(filled_rent)))

    # Update database with parameterized queries
    updated <- safe_update(con, "ads_rent", filled_rent[, c("neighbourhood", "id")])
    cat(sprintf("Updated %d rows in ads_rent\n", updated))
    total_assigned <- total_assigned + updated
  } else {
    cat("No rental listings successfully geocoded\n")
  }
} else {
  cat("No rental listings with missing neighbourhoods found\n")
}

# ---- Final summary ----
cat("\n========== FINAL SUMMARY ==========\n")
cat(sprintf("Total neighbourhoods assigned: %d\n", total_assigned))

remaining_buy <- dbGetQuery(con, "
SELECT COUNT(*) as count FROM ads_buy
WHERE is_active = 1 AND (neighbourhood IS NULL OR neighbourhood = '')
")$count

remaining_rent <- dbGetQuery(con, "
SELECT COUNT(*) as count FROM ads_rent
WHERE is_active = 1 AND (neighbourhood IS NULL OR neighbourhood = '')
")$count

remaining_total <- remaining_buy + remaining_rent

cat(sprintf("Still unassigned (buy): %d\n", remaining_buy))
cat(sprintf("Still unassigned (rent): %d\n", remaining_rent))
cat(sprintf("Still unassigned (total): %d\n", remaining_total))
cat("===================================\n")
