library(DBI)
library(RPostgres)
library(jsonlite)
library(dplyr)
library(stringr)

# Fuzzy string matching function
fuzzy_match <- function(db_name, geojson_names, threshold = 0.75) {
  if (length(geojson_names) == 0) return(NULL)

  # Normalize strings for matching
  norm_db <- tolower(str_replace_all(db_name, "[^a-z0-9]", ""))
  norm_geo <- tolower(str_replace_all(geojson_names, "[^a-z0-9]", ""))

  # Calculate Levenshtein distance (similarity)
  distances <- adist(norm_db, norm_geo) / pmax(nchar(norm_db), nchar(norm_geo))
  best_idx <- which.min(distances)
  best_score <- 1 - distances[best_idx]

  if (best_score >= threshold) {
    list(match = geojson_names[best_idx], score = best_score)
  } else {
    NULL
  }
}

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

# Load GeoJSON files
porto_geojson <- fromJSON("dashboard/static/porto_region.geojson")
lisboa_geojson <- fromJSON("dashboard/static/lisboa_region.geojson")
algarve_geojson <- fromJSON("dashboard/static/algarve.geojson")
almada_geojson <- fromJSON("dashboard/static/almada.geojson")

# Extract feature names from each
get_feature_names <- function(geojson) {
  sapply(geojson$features, function(f) f$properties$NAME_3 %||% f$properties$NAME_2 %||% "")
}

porto_features <- get_feature_names(porto_geojson)
lisboa_features <- get_feature_names(lisboa_geojson)
algarve_features <- get_feature_names(algarve_geojson)
almada_features <- get_feature_names(almada_geojson)

# Get all neighbourhoods from database
all_db_nbh <- dbGetQuery(con, "
SELECT DISTINCT neighbourhood, city
FROM (
  SELECT neighbourhood, city FROM ads_buy WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
  UNION
  SELECT neighbourhood, city FROM ads_rent WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
) t
ORDER BY city, neighbourhood
")

unmatched <- all_db_nbh[!all_db_nbh$neighbourhood %in% current_db_names, ]

cat(sprintf("Found %d unmatched neighbourhoods\n\n", nrow(unmatched)))

# Try to match each unmatched neighbourhood
suggestions <- list()

for (i in seq_len(nrow(unmatched))) {
  nbh <- unmatched$neighbourhood[i]
  city <- unmatched$city[i]

  # Pick appropriate GeoJSON based on city
  if (city %in% c("porto", "matosinhos", "maia", "vila-nova-de-gaia")) {
    features <- porto_features
  } else if (city %in% c("lisboa", "cascais", "sintra")) {
    features <- lisboa_features
  } else if (city %in% c("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")) {
    features <- algarve_features
  } else if (city == "almada") {
    features <- almada_features
  } else {
    features <- c()
  }

  if (length(features) > 0) {
    match <- fuzzy_match(nbh, features, threshold = 0.70)
    if (!is.null(match)) {
      suggestions[[length(suggestions) + 1]] <- list(
        db_name = nbh,
        city = city,
        suggested_match = match$match,
        score = round(match$score, 3)
      )
    }
  }
}

# Print suggestions grouped by city
if (length(suggestions) > 0) {
  cat("=== SUGGESTED MATCHES (confidence score >= 0.70) ===\n\n")

  suggestions_df <- bind_rows(lapply(suggestions, as.data.frame))

  for (city_val in unique(suggestions_df$city)) {
    city_matches <- suggestions_df[suggestions_df$city == city_val, ]
    cat(sprintf("CITY: %s (%d matches)\n", city_val, nrow(city_matches)))
    for (j in seq_len(nrow(city_matches))) {
      cat(sprintf('  "%s" -> "%s" (%.3f)\n',
                  city_matches$db_name[j],
                  city_matches$suggested_match[j],
                  city_matches$score[j]))
    }
    cat("\n")
  }

  cat(sprintf("=== SUMMARY ===\n"))
  cat(sprintf("Suggested %d matches out of %d unmatched neighbourhoods (%.1f%% coverage)\n",
              nrow(suggestions_df), nrow(unmatched),
              100 * nrow(suggestions_df) / nrow(unmatched)))

  # List neighbourhoods that couldn't be matched
  matched_names <- suggestions_df$db_name
  unmatched_names <- unmatched$neighbourhood[!unmatched$neighbourhood %in% matched_names]
  if (length(unmatched_names) > 0) {
    cat(sprintf("\n=== UNABLE TO MATCH (%d neighbourhoods) ===\n", length(unmatched_names)))
    cat("These have no close match in GeoJSON (score < 0.70):\n\n")
    unmatched_with_city <- unmatched[unmatched$neighbourhood %in% unmatched_names, ]
    print(unmatched_with_city)
  }
} else {
  cat("No matches found\n")
}
