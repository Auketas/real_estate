library(sf)
library(dplyr)

geojson_files <- c(
  porto = "dashboard/static/porto_region.geojson",
  lisboa = "dashboard/static/lisboa_region.geojson",
  setubal = "dashboard/static/setubal.geojson",
  algarve = "dashboard/static/algarve.geojson",
  almada = "dashboard/static/almada.geojson"
)

all_neighbourhoods <- lapply(names(geojson_files), function(region) {
  gj <- st_read(geojson_files[[region]])

  gj %>%
    st_transform(4326) %>%
    st_make_valid() %>%
    mutate(
      city = region,
      neighbourhood = if ("NAME_3" %in% names(gj)) NAME_3 else NAME_2,  # use NAME_3 if exists, else NAME_2
      centroid = st_centroid(geometry),
      area_km2 = as.numeric(st_area(geometry)) / 1e6  # geodetic area via sf/s2, WGS84
    ) %>%
    select(city, neighbourhood, geometry, centroid, area_km2)
}) %>%
  bind_rows()

all_neighbourhoods <- all_neighbourhoods %>%
  rowwise() %>%
  mutate(
    b = list(st_bbox(geometry)),
    south = unname(b['ymin']),
    west = unname(b['xmin']),
    north = unname(b['ymax']),
    east = unname(b['xmax']),
    lat_center = st_coordinates(centroid)[1, 2],
    lon_center = st_coordinates(centroid)[1, 1]
  ) %>%
  ungroup() %>%
  select(city, neighbourhood, south, west, north, east, lat_center, lon_center, area_km2)

head(all_neighbourhoods)


# ---- Amenity + transit tag definitions -------------------------------------
# Each category maps to one *_count column. Categories are queried together
# in a single combined Overpass request per neighbourhood (see
# query_neighbourhood_osm() below), rather than one request per
# (neighbourhood, tag) pair. The old approach issued ~14 separate requests
# per neighbourhood (~4,200+ total across ~300 neighbourhoods) -- a volume
# that risks both GitHub Actions' 6h job timeout and Overpass's per-IP rate
# limiting (GitHub-hosted runners draw from a large, shared, non-dedicated
# IP pool, so query volume matters regardless of where this runs).
# Consolidating to one request per neighbourhood cuts that to ~300 total.
amenity_tag_pairs <- list(
  schools               = list(c("amenity", "school")),
  restaurants           = list(c("amenity", "restaurant"), c("amenity", "cafe")),
  bars                  = list(c("amenity", "bar"), c("amenity", "nightclub"), c("amenity", "pub")),
  parks                 = list(c("leisure", "park"), c("leisure", "garden"), c("leisure", "playground")),
  grocery_stores        = list(c("shop", "supermarket"), c("shop", "groceries"), c("amenity", "marketplace")),
  tourist_attractions   = list(c("tourism", "attraction"), c("tourism", "museum"))
)

# Public transport stops, split into rail (higher-capacity, fixed-guideway:
# train, metro, tram) and bus -- feeds transit_score below. `railway=station`
# also appears in query_stations() further down, which serves a different
# purpose (nearest-station distance for travel time, queried per-city over a
# wider bbox); this is a per-neighbourhood *count* for the density-based
# transit score.
transit_tag_pairs <- list(
  rail_stops = list(
    c("railway", "station"),
    c("railway", "halt"),
    c("railway", "tram_stop"),
    c("railway", "subway_entrance")
  ),
  bus_stops = list(c("highway", "bus_stop"))
)

all_tag_pairs <- c(amenity_tag_pairs, transit_tag_pairs)

count_cols <- paste0(names(all_tag_pairs), "_count")
results <- all_neighbourhoods
results[count_cols] <- 0L

library(httr)
library(jsonlite)
library(dplyr)

build_combined_query <- function(south, west, north, east, tag_pairs) {
  filters <- unlist(lapply(tag_pairs, function(category) {
    unlist(lapply(category, function(pair) {
      sprintf('node["%s"="%s"];way["%s"="%s"];relation["%s"="%s"];',
              pair[1], pair[2], pair[1], pair[2], pair[1], pair[2])
    }))
  }))
  sprintf('[bbox=%f,%f,%f,%f];\n(\n%s\n);\nout center;',
          south, west, north, east, paste(filters, collapse = "\n"))
}

classify_element <- function(tags, tag_pairs) {
  if (is.null(tags)) return(character(0))
  Filter(function(cat_name) {
    any(vapply(tag_pairs[[cat_name]], function(pair) {
      !is.null(tags[[pair[1]]]) && identical(tags[[pair[1]]], pair[2])
    }, logical(1)))
  }, names(tag_pairs))
}

# One combined query per neighbourhood, covering every amenity + transit
# category at once.
query_neighbourhood_osm <- function(south, west, north, east, tag_pairs, max_retries = 3) {

  overpass_url <- "https://overpass-api.de/api/interpreter"
  query <- build_combined_query(south, west, north, east, tag_pairs)
  zero_counts <- setNames(as.list(integer(length(tag_pairs))), names(tag_pairs))

  # Retry loop. Every failure path below returns NULL to retry -- the only
  # way out of the loop with real data is a 200 response. If all attempts
  # are exhausted, we fall through to the NA return after the loop, never
  # to zero_counts. That distinction matters: a query that genuinely found
  # no schools/bars/etc. should score 0, but a query that never got a valid
  # response must not be silently treated the same way -- it should be NA,
  # so a persistent failure surfaces as missing data instead of quietly
  # corrupting walk_score/transit_score/vibrancy_score with fake zeros.
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      response <- POST(overpass_url, body = query, timeout(300))  # 5 min timeout

      if (status_code(response) == 200) {
        data <- fromJSON(content(response, as = "text"), simplifyVector = FALSE)
        counts <- zero_counts
        for (el in data$elements) {
          for (cat_name in classify_element(el$tags, tag_pairs)) {
            counts[[cat_name]] <- counts[[cat_name]] + 1L
          }
        }
        counts
      } else if (status_code(response) == 429) {
        # Too many requests — wait and retry
        wait_time <- 2 ^ attempt  # exponential backoff: 2, 4, 8 sec
        cat(paste("Rate limited. Waiting", wait_time, "seconds...\n"))
        Sys.sleep(wait_time)
        NULL  # Continue to next retry
      } else {
        # Non-200, non-429 (e.g. transient 5xx) — retry rather than giving
        # up immediately, same backoff as the other failure paths.
        wait_time <- 2 ^ attempt
        warning(paste("Overpass error:", status_code(response), "- retrying in", wait_time, "sec"))
        Sys.sleep(wait_time)
        NULL  # Continue to next retry
      }
    }, error = function(e) {
      wait_time <- 2 ^ attempt
      cat(paste("Connection error (", e$message, "). Retrying in", wait_time, "seconds...\n"))
      Sys.sleep(wait_time)
      NULL  # Continue to next retry
    })

    if (!is.null(result)) return(result)
  }

  warning(paste("Query failed after", max_retries, "attempts — returning NA counts, not zero"))
  setNames(as.list(rep(NA_integer_, length(tag_pairs))), names(tag_pairs))
}

# Loop through each neighbourhood — one Overpass request per iteration now
for (i in 1:nrow(results)) {
  cat(paste0(i, "/", nrow(results), " - ", results$neighbourhood[i], "\n"))

  counts <- query_neighbourhood_osm(
    south = results$south[i],
    west = results$west[i],
    north = results$north[i],
    east = results$east[i],
    tag_pairs = all_tag_pairs
  )

  for (cat_name in names(counts)) {
    results[[paste0(cat_name, "_count")]][i] <- counts[[cat_name]]
  }

  Sys.sleep(1.5)  # delay between neighbourhoods (single request each now)
}

head(results)

# Hard stop on any unrecoverable query failure, rather than letting NA
# counts silently flow into vibrancy_score/walk_score/transit_score below
# (all of which divide by area_km2 and percentile-rank across the dataset --
# an NA there doesn't fail loudly, it just quietly produces an NA or skewed
# score for that row). This is unattended in GitHub Actions, so a failed
# neighbourhood needs to stop the run and show up as a failed job, not get
# written to Neon looking like valid data.
failed_neighbourhoods <- results$neighbourhood[apply(results[count_cols], 1, anyNA)]
if (length(failed_neighbourhoods) > 0) {
  stop(sprintf(
    "Overpass queries failed after retries for %d neighbourhood(s): %s — refusing to continue. Rerun the scraper (transient Overpass/rate-limit issues are the likely cause) rather than writing partial data to Neon.",
    length(failed_neighbourhoods), paste(failed_neighbourhoods, collapse = ", ")
  ))
}

# ---- Travel times: nearest train station + fixed airport landmark ---------
#
# NOTE ON UNITS: design.md's original pseudocode divided km by "1.4 km/min",
# but 1.4 is actually average walking speed in m/s (~5 km/h), not km/min.
# Dividing by 1.4 as if it were km/min would understate walking time by
# ~84x. Corrected here: 5 km/h walking -> 12 min/km; 50 km/h driving -> 1.2
# min/km. design.md should be corrected to match (done in this session).

haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  a <- sin(dLat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# Airports are effectively one-per-region, so a fixed landmark remains a fair
# proxy (unlike train stations, where the best-connected stop varies by
# neighbourhood — e.g. Porto: Campanha vs. São Bento). Setubal and Almada
# fold into the Lisboa region here, matching how they're grouped elsewhere
# in the dashboard.
airport_landmarks <- list(
  lisboa  = c(lat = 38.6749, lon = -9.1350),   # Humberto Delgado Lisbon
  porto   = c(lat = 41.2411, lon = -8.6761),   # Francisco de Sá Carneiro
  algarve = c(lat = 37.0144, lon = -7.9754)    # Faro Airport
)
airport_region_map <- list(
  lisboa = "lisboa", setubal = "lisboa", almada = "lisboa",
  porto = "porto", algarve = "algarve"
)

# Query all railway=station node/way within a city's combined bounding box,
# once per city, then take the nearest station per neighbourhood.
query_stations <- function(south, west, north, east, max_retries = 3) {
  overpass_url <- "https://overpass-api.de/api/interpreter"
  query <- sprintf(
    '[bbox=%f,%f,%f,%f];
     (node["railway"="station"];way["railway"="station"];);
     out center;',
    south, west, north, east
  )

  empty <- data.frame(lat = numeric(0), lon = numeric(0))

  for (attempt in 1:max_retries) {
    result <- tryCatch({
      response <- POST(overpass_url, body = query, timeout(300))
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, as = "text"))
        els <- data$elements
        if (is.null(els) || length(els) == 0 || nrow(els) == 0) return(empty)
        lat <- sapply(seq_len(nrow(els)), function(k) {
          if (!is.null(els$lat) && !is.na(els$lat[k])) els$lat[k] else els$center$lat[k]
        })
        lon <- sapply(seq_len(nrow(els)), function(k) {
          if (!is.null(els$lon) && !is.na(els$lon[k])) els$lon[k] else els$center$lon[k]
        })
        return(data.frame(lat = as.numeric(lat), lon = as.numeric(lon)))
      } else if (status_code(response) == 429) {
        wait_time <- 2 ^ attempt
        cat(paste("Rate limited. Waiting", wait_time, "seconds...\n"))
        Sys.sleep(wait_time)
        NULL
      } else {
        warning(paste("Overpass error:", status_code(response)))
        empty
      }
    }, error = function(e) {
      if (attempt < max_retries) {
        wait_time <- 2 ^ attempt
        cat(paste("Connection error (", e$message, "). Retrying in", wait_time, "seconds...\n"))
        Sys.sleep(wait_time)
        NULL
      } else {
        warning(paste("Station query failed after", max_retries, "attempts:", e$message))
        empty
      }
    })
    if (!is.null(result)) return(result)
  }
  empty
}

WALK_KMH <- 5
DRIVE_KMH <- 50

results$min_to_train_station <- NA_real_
results$min_to_airport <- NA_real_

for (region in unique(results$city)) {
  region_rows <- which(results$city == region)
  bbox <- results[region_rows, ]

  stations <- query_stations(
    south = min(bbox$south), west = min(bbox$west),
    north = max(bbox$north), east = max(bbox$east)
  )
  Sys.sleep(1.5)

  airport <- airport_landmarks[[airport_region_map[[region]]]]

  for (i in region_rows) {
    if (nrow(stations) > 0) {
      km_to_train <- min(haversine_km(results$lat_center[i], results$lon_center[i],
                                       stations$lat, stations$lon), na.rm = TRUE)
      min_walk <- km_to_train * (60 / WALK_KMH)
      results$min_to_train_station[i] <- if (min_walk > 60) NA_real_ else min_walk
    }

    km_to_airport <- haversine_km(results$lat_center[i], results$lon_center[i],
                                   airport["lat"], airport["lon"])
    results$min_to_airport[i] <- km_to_airport * (60 / DRIVE_KMH)
  }
}

# ---- Vibrancy Index --------------------------------------------------------
# Area-normalised: neighbourhood polygon sizes span ~21,600x in this dataset
# (0.035 km2 for a small Lisboa historic parish up to 765 km2 for Loule,
# which falls back to concelho/NAME_2 level in Algarve). Fixed absolute
# thresholds on raw counts would be meaningless at that range, so density
# (count per km2) plus data-driven tertile cutoffs are used instead of the
# original fixed 5/20 thresholds.
results <- results %>%
  mutate(
    vibrancy_score = (bars_count + tourist_attractions_count / 2) / area_km2,
    vibrancy_category = case_when(
      vibrancy_score <= quantile(vibrancy_score, 1/3, na.rm = TRUE) ~ "low",
      vibrancy_score <= quantile(vibrancy_score, 2/3, na.rm = TRUE) ~ "medium",
      TRUE ~ "high"
    )
  )

# ---- Walkability score (DIY, from already-scraped OSM amenity counts) -----
# Walk Score's free tier isn't actually usable (see design.md) — this
# approximates the same underlying idea ("can daily needs be met without a
# car") using data already collected above, rather than a new API.
#
# Uses amenity DENSITY (count / area_km2), not raw counts, for the same
# reason as the Vibrancy Index: Algarve entries are concelho-level (up to
# 765 km2) while Lisboa/Porto/Setubal are parish-level (as small as 0.035
# km2) — raw counts aren't comparable across that range. Each category is
# then percentile-ranked across the full dataset and combined with weights
# that mirror Walk Score's own category weighting: daily necessities
# (groceries, restaurants, schools) count for more than occasional-use
# amenities (bars, tourist attractions).
#
# NOTE — this only approximates "amenity mix." Real Walk Score does
# per-amenity distance-decay from a specific point (full credit within a
# ~5 min walk, decaying to zero by ~30 min) plus a street/intersection-
# density term for pedestrian connectivity. Neither is implemented here.
# Individual amenity coordinates are already present in the Overpass
# responses above (only the count is currently kept) — see design.md for
# the upgrade path if this proxy proves too crude once real data is in.

walk_weights <- c(
  grocery_stores_count      = 0.25,
  restaurants_count         = 0.20,
  schools_count              = 0.15,
  parks_count                 = 0.15,
  bars_count                   = 0.15,
  tourist_attractions_count    = 0.10
)

walk_density <- sapply(names(walk_weights), function(col) results[[col]] / results$area_km2)
walk_pct <- apply(walk_density, 2, dplyr::percent_rank)
results$walk_score <- round(as.numeric(walk_pct %*% walk_weights))

# ---- Public Transport (Transit) score --------------------------------------
# Same density + percentile-rank pattern as walk_score/vibrancy_score.
# rail_stops_count (train halts/stations, metro, tram — higher-capacity,
# fixed-guideway modes) is weighted above bus_stops_count, mirroring how
# Walk Score's own Transit Score weights by mode/frequency. Both counts come
# from the combined per-neighbourhood Overpass query above — no extra API
# calls needed.

transit_weights <- c(
  rail_stops_count = 0.65,
  bus_stops_count  = 0.35
)

transit_density <- sapply(names(transit_weights), function(col) results[[col]] / results$area_km2)
transit_pct <- apply(transit_density, 2, dplyr::percent_rank)
results$transit_score <- round(as.numeric(transit_pct %*% transit_weights))

head(results)

# ---- Write to Neon ----------------------------------------------------------
# `neighbourhood_metadata` exists in Neon (created Aug 2026). walk_score and
# transit_score are computed above; bike_score is left NA until a source is
# chosen (see design.md notes).

library(DBI)
library(RPostgres)

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

write_neighbourhood_metadata <- function(results) {
  con <- get_con()
  on.exit(dbDisconnect(con))

  out <- results %>%
    mutate(bike_score = NA_integer_,  # no data source chosen yet — see design.md
           updated_at = Sys.time()) %>%
    select(city, neighbourhood, area_km2, walk_score, transit_score, bike_score,
           schools_count, grocery_stores_count, restaurants_count, bars_count,
           parks_count, tourist_attractions_count, rail_stops_count, bus_stops_count,
           min_to_train_station, min_to_airport, vibrancy_score, vibrancy_category, updated_at)

  # Full quarterly rebuild — replace the whole table, matching the
  # delete-all-then-insert-all pattern used elsewhere in this repo.
  dbExecute(con, "DELETE FROM neighbourhood_metadata")
  dbWriteTable(con, "neighbourhood_metadata", out, append = TRUE, row.names = FALSE)
  message(sprintf("Wrote %d neighbourhood_metadata rows", nrow(out)))
}

write_neighbourhood_metadata(results)
