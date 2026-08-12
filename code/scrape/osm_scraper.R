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
      centroid = st_centroid(geometry)
    ) %>%
    select(city, neighbourhood, geometry, centroid)
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
  select(city, neighbourhood, south, west, north, east, lat_center, lon_center)

head(all_neighbourhoods)



amenity_types <- list(
  schools = list(c("amenity", "school")),
  restaurants = list(c("amenity", "restaurant"), c("amenity", "cafe")),  # OR cafe
  bars = list(c("amenity", "bar"), c("amenity", "nightclub"), c("amenity", "pub")),  # OR nightclub or pub
  parks = list(c("leisure", "park"), c("leisure", "garden")),  # OR garden
  grocery_stores = list(c("shop", "supermarket"), c("shop", "groceries"), c("amenity", "marketplace")),  # OR groceries or marketplace
  tourist_attractions = list(c("tourism", "attraction"), c("tourism", "museum"))  # OR museum
)

results <- all_neighbourhoods %>%
  mutate(across(
    ends_with("_count"),
    ~0,
    .names = "{.col}"
  )) %>%
  mutate(
    schools_count = 0,
    restaurants_count = 0,
    bars_count = 0,
    parks_count = 0,
    grocery_stores_count = 0,
    tourist_attractions_count = 0
  )

library(httr)
library(jsonlite)
library(dplyr)

# Define the query function
query_amenity <- function(south, west, north, east, amenity_tag, amenity_value, max_retries = 3) {

  overpass_url <- "https://overpass-api.de/api/interpreter"

  # Build Overpass QL query
  query <- sprintf(
    '[bbox=%f,%f,%f,%f];
     (node["%s"="%s"];way["%s"="%s"];);
     out center;',
    south, west, north, east,
    amenity_tag, amenity_value,
    amenity_tag, amenity_value
  )

  # Retry loop
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      response <- POST(overpass_url, body = query, timeout(300))  # 5 min timeout

      if (status_code(response) == 200) {
        data <- fromJSON(content(response, as = "text"))
        return(length(data$elements))
      } else if (status_code(response) == 429) {
        # Too many requests — wait and retry
        wait_time <- 2 ^ attempt  # exponential backoff: 2, 4, 8 sec
        cat(paste("Rate limited. Waiting", wait_time, "seconds...\n"))
        Sys.sleep(wait_time)
        NULL  # Continue to next retry
      } else {
        warning(paste("Overpass error:", status_code(response)))
        0
      }
    }, error = function(e) {
      if (attempt < max_retries) {
        wait_time <- 2 ^ attempt
        cat(paste("Connection error (", e$message, "). Retrying in", wait_time, "seconds...\n"))
        Sys.sleep(wait_time)
        NULL  # Continue to next retry
      } else {
        warning(paste("Query failed after", max_retries, "attempts:", e$message))
        0
      }
    })

    if (!is.null(result)) return(result)
  }

  return(0)
}

# Loop through each neighbourhood
for (i in 1:nrow(results)) {
  cat(paste0(i, "/", nrow(results), " - ", results$neighbourhood[i], "\n"))
  
  # Loop through each amenity type
  for (amenity_name in names(amenity_types)) {
    print(amenity_name)
    total_count <- 0
    
    # Loop through each tag/value pair for this amenity
    for (j in 1:length(amenity_types[[amenity_name]])) {
      tag_pair <- amenity_types[[amenity_name]][[j]]
      tag <- tag_pair[1]
      value <- tag_pair[2]
      
      count <- query_amenity(
        south = results$south[i],
        west = results$west[i],
        north = results$north[i],
        east = results$east[i],
        amenity_tag = tag,
        amenity_value = value
      )
      
      total_count <- total_count + count
      Sys.sleep(0.5)  # Small delay between API calls
    }
    
    # Store total count in results
    col_name <- paste0(amenity_name, "_count")
    results[[col_name]][i] <- total_count
  }
  
  Sys.sleep(1)  # Delay between neighbourhoods
}

head(results)



