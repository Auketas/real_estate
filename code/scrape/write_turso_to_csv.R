library(httr)
library(jsonlite)

# Your existing connection details
url <- "your_url_here"
token <- "your_token_here"

# Your existing turso_query function
turso_query <- function(sql, url, token) {
  res <- POST(
    url = paste0(url, "/v2/pipeline"),
    add_headers(Authorization = paste("Bearer", token)),
    body = toJSON(list(
      requests = list(list(type = "execute", stmt = list(sql = sql)))
    ), auto_unbox = TRUE),
    encode = "json"
  )
  content(res, as = "parsed", simplifyVector = TRUE)
}

# Generic function to read any table fully
read_full_table <- function(url, token, table_name) {
  res <- turso_query(paste0("SELECT * FROM ", table_name, ";"), url, token)
  rows <- res$results$response$result$rows
  cols <- res$results$response$result$cols
  col_names <- sapply(cols, function(c) c$name)
  
  df_list <- lapply(rows, function(row) {
    values <- sapply(row, function(cell) cell$value)
    as.data.frame(t(values), stringsAsFactors = FALSE)
  })
  
  df <- do.call(rbind, df_list)
  colnames(df) <- col_names
  return(df)
}

# Export all four tables
tables <- c("ads_porto_buy", "price_changes_porto_buy", "ads_porto_rent", "price_changes_porto_rent")
output_names <- c("ads_buy", "price_changes_buy", "ads_rent", "price_changes_rent")

for (i in seq_along(tables)) {
  cat("Exporting", tables[i], "...\n")
  df <- read_full_table(url, token, tables[i])
  write.csv(df, file = paste0(output_names[i], ".csv"), row.names = FALSE)
  cat("Done:", nrow(df), "rows written to", paste0(output_names[i], ".csv\n"))
}
