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
  geojsons <- list()

  porto_path <- file.path(geojson_dir, "porto_region.geojson")
  if (file.exists(porto_path)) {
    geojsons[["porto"]] <- st_read(porto_path, quiet = TRUE)
    cat("Loaded porto_region.geojson\n")
  }

  lisboa_path <- file.path(geojson_dir, "lisboa_region.geojson")
  if (file.exists(lisboa_path)) {
    geojsons[["lisboa"]] <- st_read(lisboa_path, quiet = TRUE)
    cat("Loaded lisboa_region.geojson\n")
  }

  algarve_path <- file.path(geojson_dir, "algarve.geojson")
  if (file.exists(algarve_path)) {
    geojsons[["algarve"]] <- st_read(algarve_path, quiet = TRUE)
    cat("Loaded algarve.geojson\n")
  }

  almada_path <- file.path(geojson_dir, "almada.geojson")
  if (file.exists(almada_path)) {
    geojsons[["almada"]] <- st_read(almada_path, quiet = TRUE)
    cat("Loaded almada.geojson\n")
  }

  geojsons
}

# Match point to polygon and return neighbourhood
get_polygon_neighbourhood <- function(lon, lat, geojson) {
  if (is.na(lon) || is.na(lat)) return(NA_character_)

  tryCatch({
    point <- st_point(c(lon, lat))
    point_sf <- st_sf(geometry = st_sfc(point), crs = 4326)

    intersects <- st_intersects(point_sf, geojson, sparse = TRUE)[[1]]

    if (length(intersects) == 0) {
      return(NA_character_)
    }

    feature_idx <- intersects[1]

    if ("NAME_3" %in% names(geojson)) {
      neighbourhood <- geojson$NAME_3[feature_idx]
    } else if ("NAME_2" %in% names(geojson)) {
      neighbourhood <- geojson$NAME_2[feature_idx]
    } else {
      return(NA_character_)
    }

    return(neighbourhood)
  }, error = function(e) {
    return(NA_character_)
  })
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

# Main logic
con <- get_con()
on.exit(dbDisconnect(con))

geojsons <- load_geojson_data()

total_updated <- 0

# ---- Check ads_buy ----
cat("\n=== Validating ads_buy ===\n")

# Get all listings with non-null neighbourhood but is_active = 1
listings_buy <- dbGetQuery(con, "
SELECT id, city, neighbourhood, lat, lon
FROM ads_buy
WHERE is_active = 1
  AND neighbourhood IS NOT NULL
  AND neighbourhood != ''
  AND lat IS NOT NULL
  AND lon IS NOT NULL
")

cat(sprintf("Checking %d buy listings...\n", nrow(listings_buy)))

listings_buy$polygon_neighbourhood <- NA_character_
mismatches <- 0

for (i in seq_len(nrow(listings_buy))) {
  if (i %% 500 == 0) {
    cat(sprintf("Progress: %d/%d\n", i, nrow(listings_buy)))
  }

  city <- gsub("-", " ", tolower(listings_buy$city[i]))
  lon <- listings_buy$lon[i]
  lat <- listings_buy$lat[i]

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
    poly_nbh <- get_polygon_neighbourhood(lon, lat, geojson)
    listings_buy$polygon_neighbourhood[i] <- poly_nbh

    # Check for mismatch
    if (!is.na(poly_nbh) && poly_nbh != listings_buy$neighbourhood[i]) {
      mismatches <- mismatches + 1
    }
  }
}

cat(sprintf("Found %d listings with neighbourhood mismatches\n", mismatches))

# Update listings that have polygon match but wrong neighbourhood
needs_update <- listings_buy[
  !is.na(listings_buy$polygon_neighbourhood) &
  listings_buy$polygon_neighbourhood != listings_buy$neighbourhood,
  c("id", "polygon_neighbourhood")
]
colnames(needs_update)[2] <- "neighbourhood"

if (nrow(needs_update) > 0) {
  cat(sprintf("Updating %d listings with correct polygon neighbourhoods...\n", nrow(needs_update)))
  updated <- safe_update(con, "ads_buy", needs_update)
  cat(sprintf("Updated %d rows\n", updated))
  total_updated <- total_updated + updated
}

# ---- Check ads_rent ----
cat("\n=== Validating ads_rent ===\n")

listings_rent <- dbGetQuery(con, "
SELECT id, city, neighbourhood, lat, lon
FROM ads_rent
WHERE is_active = 1
  AND neighbourhood IS NOT NULL
  AND neighbourhood != ''
  AND lat IS NOT NULL
  AND lon IS NOT NULL
")

cat(sprintf("Checking %d rental listings...\n", nrow(listings_rent)))

listings_rent$polygon_neighbourhood <- NA_character_
mismatches_rent <- 0

for (i in seq_len(nrow(listings_rent))) {
  if (i %% 500 == 0) {
    cat(sprintf("Progress: %d/%d\n", i, nrow(listings_rent)))
  }

  city <- gsub("-", " ", tolower(listings_rent$city[i]))
  lon <- listings_rent$lon[i]
  lat <- listings_rent$lat[i]

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
    poly_nbh <- get_polygon_neighbourhood(lon, lat, geojson)
    listings_rent$polygon_neighbourhood[i] <- poly_nbh

    if (!is.na(poly_nbh) && poly_nbh != listings_rent$neighbourhood[i]) {
      mismatches_rent <- mismatches_rent + 1
    }
  }
}

cat(sprintf("Found %d rental listings with neighbourhood mismatches\n", mismatches_rent))

needs_update_rent <- listings_rent[
  !is.na(listings_rent$polygon_neighbourhood) &
  listings_rent$polygon_neighbourhood != listings_rent$neighbourhood,
  c("id", "polygon_neighbourhood")
]
colnames(needs_update_rent)[2] <- "neighbourhood"

if (nrow(needs_update_rent) > 0) {
  cat(sprintf("Updating %d rental listings with correct polygon neighbourhoods...\n", nrow(needs_update_rent)))
  updated <- safe_update(con, "ads_rent", needs_update_rent)
  cat(sprintf("Updated %d rows\n", updated))
  total_updated <- total_updated + updated
}

# Summary
cat("\n========== VALIDATION SUMMARY ==========\n")
cat(sprintf("Total listings with mismatches (buy): %d\n", mismatches))
cat(sprintf("Total listings with mismatches (rent): %d\n", mismatches_rent))
cat(sprintf("Total listings corrected: %d\n", total_updated))
cat("========================================\n")
