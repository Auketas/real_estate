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

safe_con <- function(con) {
  tryCatch({
    dbExecute(con, "SELECT 1")
    return(con)
  }, error = function(e) {
    message("Connection lost, reconnecting...")
    return(get_con())
  })
}

# Load GeoJSON files
load_geojson_data <- function() {
  cat("Loading GeoJSON files...\n")

  geojson_dir <- "dashboard/static"
  cat(sprintf("Looking for GeoJSON files in: %s\n", geojson_dir))
  cat(sprintf("Directory exists: %s\n", dir.exists(geojson_dir)))

  if (dir.exists(geojson_dir)) {
    files <- list.files(geojson_dir, pattern = "\\.geojson$")
    cat(sprintf("Found %d .geojson files: %s\n", length(files), paste(files, collapse = ", ")))
  }

  geojsons <- list()

  # Porto region (parishes)
  porto_path <- file.path(geojson_dir, "porto.geojson")
  if (file.exists(porto_path)) {
    cat(sprintf("Reading %s...\n", porto_path))
    geojsons[["porto"]] <- st_read(porto_path, quiet = TRUE)
    cat(sprintf("Loaded porto.geojson with %d features, columns: %s\n",
                nrow(geojsons[["porto"]]), paste(names(geojsons[["porto"]]), collapse = ", ")))
  } else {
    cat(sprintf("porto.geojson not found at %s\n", porto_path))
  }

  # Lisboa region (parishes)
  lisboa_path <- file.path(geojson_dir, "lisboa.geojson")
  if (file.exists(lisboa_path)) {
    cat(sprintf("Reading %s...\n", lisboa_path))
    geojsons[["lisboa"]] <- st_read(lisboa_path, quiet = TRUE)
    cat(sprintf("Loaded lisboa.geojson with %d features, columns: %s\n",
                nrow(geojsons[["lisboa"]]), paste(names(geojsons[["lisboa"]]), collapse = ", ")))
  } else {
    cat(sprintf("lisboa.geojson not found at %s\n", lisboa_path))
  }

  # Algarve (municipalities)
  algarve_path <- file.path(geojson_dir, "algarve.geojson")
  if (file.exists(algarve_path)) {
    cat(sprintf("Reading %s...\n", algarve_path))
    geojsons[["algarve"]] <- st_read(algarve_path, quiet = TRUE)
    cat(sprintf("Loaded algarve.geojson with %d features, columns: %s\n",
                nrow(geojsons[["algarve"]]), paste(names(geojsons[["algarve"]]), collapse = ", ")))
  } else {
    cat(sprintf("algarve.geojson not found at %s\n", algarve_path))
  }

  # Almada
  almada_path <- file.path(geojson_dir, "almada.geojson")
  if (file.exists(almada_path)) {
    cat(sprintf("Reading %s...\n", almada_path))
    geojsons[["almada"]] <- st_read(almada_path, quiet = TRUE)
    cat(sprintf("Loaded almada.geojson with %d features, columns: %s\n",
                nrow(geojsons[["almada"]]), paste(names(geojsons[["almada"]]), collapse = ", ")))
  } else {
    cat(sprintf("almada.geojson not found at %s\n", almada_path))
  }

  geojsons
}

# Match a point to a GeoJSON feature and return the neighbourhood name
match_point_to_neighbourhood <- function(lon, lat, geojson) {
  if (is.na(lon) || is.na(lat)) return(NA_character_)

  tryCatch({
    point <- st_point(c(lon, lat))
    point_sf <- st_sf(geometry = st_sfc(point), crs = 4326)

    # Find which polygon(s) contain this point
    intersects <- st_intersects(point_sf, geojson, sparse = TRUE)[[1]]

    if (length(intersects) == 0) {
      return(NA_character_)  # point outside all polygons
    }

    # Use the first matching polygon
    feature_idx <- intersects[1]

    # Check which columns exist
    if ("NAME_3" %in% names(geojson)) {
      neighbourhood <- geojson$NAME_3[feature_idx]
    } else if ("NAME_2" %in% names(geojson)) {
      neighbourhood <- geojson$NAME_2[feature_idx]
    } else {
      cat(sprintf("Available columns in geojson: %s\n", paste(names(geojson), collapse = ", ")))
      return(NA_character_)
    }

    if (is.na(neighbourhood)) {
      # Try fallback column
      if ("NAME_2" %in% names(geojson) && "NAME_3" %in% names(geojson)) {
        neighbourhood <- geojson$NAME_2[feature_idx]
      }
    }

    return(neighbourhood)
  }, error = function(e) {
    cat(sprintf("Error matching point (%.4f, %.4f): %s\n", lon, lat, e$message))
    return(NA_character_)
  })
}

# Main backfill logic
con <- get_con()
on.exit(dbDisconnect(con))

geojsons <- load_geojson_data()

if (length(geojsons) == 0) {
  cat("ERROR: No GeoJSON files found in dashboard/static\n")
  stop("Cannot proceed without GeoJSON files")
}

# Verify GeoJSON files loaded and show basic info
cat("\nGeoJSON files loaded:\n")
for (name in names(geojsons)) {
  bounds <- st_bbox(geojsons[[name]])
  cat(sprintf("  %s: %d features, bounds (%.2f,%.2f) to (%.2f,%.2f)\n",
    name, nrow(geojsons[[name]]), bounds[1], bounds[2], bounds[3], bounds[4]))
}

safe_update <- function(con, table_name, nbh_id_pairs) {
  if (nrow(nbh_id_pairs) == 0) return(0)

  con <- safe_con(con)
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

total_assigned <- 0

# ---- Process buy listings ----
cat("\n=== Processing ads_buy ===\n")

missing_buy <- dbGetQuery(con, "
SELECT id, city, lat, lon
FROM ads_buy
WHERE is_active = 1
  AND (neighbourhood IS NULL OR neighbourhood = '')
  AND lat IS NOT NULL
  AND lon IS NOT NULL
ORDER BY city
")

cat(sprintf("Found %d buy listings with missing neighbourhoods\n", nrow(missing_buy)))

if (nrow(missing_buy) > 0) {
  missing_buy$neighbourhood <- NA_character_
  matched_count <- 0

  for (i in seq_len(nrow(missing_buy))) {
    if (i %% 50 == 0) {
      cat(sprintf("Progress: %d/%d\n", i, nrow(missing_buy)))
    }

    city <- tolower(missing_buy$city[i])
    lon <- missing_buy$lon[i]
    lat <- missing_buy$lat[i]

    # Select appropriate GeoJSON based on city
    geojson <- NULL
    if (city %in% c("porto", "vila nova de gaia", "matosinhos", "maia")) {
      geojson <- geojsons[["porto"]]
    } else if (city %in% c("lisboa", "cascais", "sintra")) {
      geojson <- geojsons[["lisboa"]]
    } else if (city == "almada") {
      geojson <- geojsons[["almada"]]
    } else if (city %in% c("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")) {
      geojson <- geojsons[["algarve"]]
    }

    if (!is.null(geojson)) {
      nbh <- match_point_to_neighbourhood(lon, lat, geojson)
      if (!is.na(nbh)) {
        missing_buy$neighbourhood[i] <- nbh
        matched_count <- matched_count + 1
      }
    }
  }

  cat(sprintf("Matched %d/%d coordinates to neighbourhoods\n", matched_count, nrow(missing_buy)))

  filled <- missing_buy[!is.na(missing_buy$neighbourhood), ]

  if (nrow(filled) > 0) {
    updated <- safe_update(con, "ads_buy", filled[, c("neighbourhood", "id")])
    cat(sprintf("Updated %d rows in ads_buy\n", updated))
    total_assigned <- total_assigned + updated
  }
}

# ---- Process rent listings ----
cat("\n=== Processing ads_rent ===\n")

missing_rent <- dbGetQuery(con, "
SELECT id, city, lat, lon
FROM ads_rent
WHERE is_active = 1
  AND (neighbourhood IS NULL OR neighbourhood = '')
  AND lat IS NOT NULL
  AND lon IS NOT NULL
ORDER BY city
")

cat(sprintf("Found %d rental listings with missing neighbourhoods\n", nrow(missing_rent)))

if (nrow(missing_rent) > 0) {
  missing_rent$neighbourhood <- NA_character_
  matched_count <- 0

  for (i in seq_len(nrow(missing_rent))) {
    if (i %% 50 == 0) {
      cat(sprintf("Progress: %d/%d\n", i, nrow(missing_rent)))
    }

    city <- tolower(missing_rent$city[i])
    lon <- missing_rent$lon[i]
    lat <- missing_rent$lat[i]

    geojson <- NULL
    if (city %in% c("porto", "vila nova de gaia", "matosinhos", "maia")) {
      geojson <- geojsons[["porto"]]
    } else if (city %in% c("lisboa", "cascais", "sintra")) {
      geojson <- geojsons[["lisboa"]]
    } else if (city == "almada") {
      geojson <- geojsons[["almada"]]
    } else if (city %in% c("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")) {
      geojson <- geojsons[["algarve"]]
    }

    if (!is.null(geojson)) {
      nbh <- match_point_to_neighbourhood(lon, lat, geojson)
      if (!is.na(nbh)) {
        missing_rent$neighbourhood[i] <- nbh
        matched_count <- matched_count + 1
      }
    }
  }

  cat(sprintf("Matched %d/%d coordinates to neighbourhoods\n", matched_count, nrow(missing_rent)))

  filled_rent <- missing_rent[!is.na(missing_rent$neighbourhood), ]

  if (nrow(filled_rent) > 0) {
    updated <- safe_update(con, "ads_rent", filled_rent[, c("neighbourhood", "id")])
    cat(sprintf("Updated %d rows in ads_rent\n", updated))
    total_assigned <- total_assigned + updated
  }
}

# ---- Final summary ----
cat("\n========== FINAL SUMMARY ==========\n")
cat(sprintf("Total neighbourhoods assigned: %d\n", total_assigned))

remaining_buy <- as.numeric(dbGetQuery(con, "
SELECT COUNT(*) as count FROM ads_buy
WHERE is_active = 1 AND (neighbourhood IS NULL OR neighbourhood = '')
")[1, 1])

remaining_rent <- as.numeric(dbGetQuery(con, "
SELECT COUNT(*) as count FROM ads_rent
WHERE is_active = 1 AND (neighbourhood IS NULL OR neighbourhood = '')
")[1, 1])

remaining_total <- remaining_buy + remaining_rent

cat(sprintf("Still unassigned (buy): %d\n", remaining_buy))
cat(sprintf("Still unassigned (rent): %d\n", remaining_rent))
cat(sprintf("Still unassigned (total): %d\n", remaining_total))
cat("===================================\n")
