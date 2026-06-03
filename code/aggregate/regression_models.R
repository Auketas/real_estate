library(DBI)
library(RPostgres)
library(dplyr)

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

MIN_LISTINGS <- 50   # minimum active listings to train a model for a city+type
MIN_CATEGORY <- 10   # minimum listings to keep a dummy category; smaller → "other"

# ---- Helpers ----------------------------------------------------------------

# Impute binary feature: NA treated as absent (0)
impute_binary <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x_num), 0L, as.integer(x_num != 0))
}

# Impute numeric with column median
impute_median <- function(x) {
  m <- median(x, na.rm = TRUE)
  ifelse(is.na(x), m, x)
}

# Parse floor (andar): handles "R/C", "Cave", plain integers
parse_andar <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("r/c", "rc", "r", "0", "rés do chão", "res-do-chão")] <- "0"
  x[x %in% c("cave", "sub-cave", "subcave", "-1")]                  <- "-1"
  suppressWarnings(as.numeric(x))
}

# Collapse factor levels with fewer than min_n observations into other_label
collapse_sparse <- function(x, min_n, other_label) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- other_label
  counts <- table(x)
  x[x %in% names(counts[counts < min_n])] <- other_label
  factor(x)
}

# Create summary tables if they don't exist yet
create_tables_if_missing <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_coefficients (
      id             SERIAL PRIMARY KEY,
      snapshot_month DATE,
      listing_type   VARCHAR(10),
      city           VARCHAR(100),
      variable_name  VARCHAR(100),
      coefficient    NUMERIC,
      std_error      NUMERIC,
      created_at     TIMESTAMP DEFAULT NOW()
    )")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_metadata (
      id                 SERIAL PRIMARY KEY,
      snapshot_month     DATE,
      listing_type       VARCHAR(10),
      city               VARCHAR(100),
      n_observations     INTEGER,
      r_squared          NUMERIC,
      residual_std_error NUMERIC,
      created_at         TIMESTAMP DEFAULT NOW()
    )")
}

# Fit one hedonic OLS model; returns list(coefficients, metadata) or NULL
fit_city_model <- function(df, listing_type, city, snapshot_month) {

  # Numeric imputation
  df$area_imp    <- impute_median(df$area_num)
  df$novo_imp    <- impute_binary(df$novo)
  df$jardim_imp  <- impute_binary(df$jardim)
  df$garagem_imp <- impute_binary(df$garagem)
  df$terraco_imp <- impute_binary(df$terraco)
  df$varanda_imp <- impute_binary(df$varanda)

  # Categorical: collapse sparse levels
  df$tipologia_f     <- collapse_sparse(df$tipologia,    MIN_CATEGORY, "other_tipo")
  df$neighbourhood_f <- collapse_sparse(df$neighbourhood, MIN_CATEGORY, "other_nbhd")
  df$energia_f       <- collapse_sparse(
    ifelse(is.na(df$energia) | df$energia == "", "unknown", df$energia),
    MIN_CATEGORY, "other_energia"
  )

  # Report imputation rates for key variables
  pct_area_imp <- round(100 * mean(is.na(df$area_num)), 1)
  if (pct_area_imp > 30)
    message(sprintf("    WARNING: area imputed for %.0f%% of listings in %s/%s",
                    pct_area_imp, listing_type, city))

  # Build predictor list — only include factors with > 1 level (otherwise collinear)
  vars <- c("area_imp", "novo_imp", "jardim_imp", "garagem_imp", "terraco_imp", "varanda_imp")
  if (nlevels(df$tipologia_f)     > 1) vars <- c(vars, "tipologia_f")
  if (nlevels(df$neighbourhood_f) > 1) vars <- c(vars, "neighbourhood_f")
  if (nlevels(df$energia_f)       > 1) vars <- c(vars, "energia_f")

  # Include andar only if at least 50% of values are non-missing
  andar_coverage <- mean(!is.na(df$andar_num))
  if (andar_coverage >= 0.5) {
    df$andar_imp <- impute_median(df$andar_num)
    vars <- c(vars, "andar_imp")
  }

  formula_obj <- as.formula(paste("log(price_num) ~", paste(vars, collapse = " + ")))

  fit <- tryCatch(
    lm(formula_obj, data = df),
    error = function(e) {
      message(sprintf("  ERROR %s/%-20s: %s", listing_type, city, e$message))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)

  s <- summary(fit)
  message(sprintf("  %s/%-20s  n=%4d  R²=%.3f  RSE=%.4f%s",
    listing_type, city, nrow(df), s$r.squared, s$sigma,
    if (s$r.squared < 0.3) "  *** LOW R² — check data" else ""
  ))

  coef_mat <- s$coefficients
  coefs_df <- data.frame(
    snapshot_month = snapshot_month,
    listing_type   = listing_type,
    city           = city,
    variable_name  = rownames(coef_mat),
    coefficient    = coef_mat[, "Estimate"],
    std_error      = coef_mat[, "Std. Error"],
    stringsAsFactors = FALSE
  )

  meta_df <- data.frame(
    snapshot_month     = snapshot_month,
    listing_type       = listing_type,
    city               = city,
    n_observations     = nrow(df),
    r_squared          = round(s$r.squared, 4),
    residual_std_error = round(s$sigma, 6),
    stringsAsFactors   = FALSE
  )

  list(coefficients = coefs_df, metadata = meta_df)
}

# ---- Main -------------------------------------------------------------------

run_regression_models <- function() {
  snapshot_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  message("Running hedonic regression models for ", snapshot_month)

  con <- get_con()
  on.exit(dbDisconnect(con))

  create_tables_if_missing(con)

  all_coefs <- list()
  all_meta  <- list()

  for (listing_type in c("buy", "rent")) {
    table_name <- ifelse(listing_type == "buy", "ads_buy", "ads_rent")
    message(sprintf("\n--- %s ---", toupper(listing_type)))

    ads <- dbGetQuery(con, sprintf("
      SELECT city, neighbourhood, tipologia, area, price, andar,
             novo, jardim, garagem, terraco, varanda, energia
      FROM %s
      WHERE is_active = 1
        AND (duplicate_flag = false OR duplicate_flag IS NULL)
        AND price IS NOT NULL AND price > 0",
      table_name
    ))

    ads$price_num <- suppressWarnings(as.numeric(ads$price))
    ads$area_num  <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(ads$area))))
    ads$andar_num <- parse_andar(ads$andar)

    # Remove price outliers: > 3 SD from city-level mean log(price/m²)
    # Only applied to listings where area is known — area-missing listings are kept
    ads_with_area <- ads %>%
      filter(!is.na(price_num) & price_num > 0 & !is.na(area_num) & area_num > 0) %>%
      mutate(log_ppm2 = log(price_num / area_num)) %>%
      group_by(city) %>%
      mutate(mean_lp = mean(log_ppm2), sd_lp = sd(log_ppm2)) %>%
      ungroup() %>%
      filter(abs(log_ppm2 - mean_lp) <= 3 * sd_lp) %>%
      select(-log_ppm2, -mean_lp, -sd_lp)

    ads_no_area <- filter(ads, is.na(area_num) | area_num <= 0,
                          !is.na(price_num), price_num > 0)

    ads_clean <- bind_rows(ads_with_area, ads_no_area)

    for (city in sort(unique(ads_clean$city))) {
      df <- filter(ads_clean, city == !!city)

      if (nrow(df) < MIN_LISTINGS) {
        message(sprintf("  SKIP %s/%-20s — %d listings (need %d)",
                        listing_type, city, nrow(df), MIN_LISTINGS))
        next
      }

      result <- fit_city_model(df, listing_type, city, snapshot_month)
      if (!is.null(result)) {
        all_coefs <- c(all_coefs, list(result$coefficients))
        all_meta  <- c(all_meta,  list(result$metadata))
      }
    }
  }

  if (length(all_coefs) == 0) {
    message("No models produced — nothing to write")
    return(invisible(NULL))
  }

  coefs_df <- bind_rows(all_coefs)
  meta_df  <- bind_rows(all_meta)

  dbExecute(con, sprintf("DELETE FROM model_coefficients WHERE snapshot_month = '%s'", snapshot_month))
  dbExecute(con, sprintf("DELETE FROM model_metadata     WHERE snapshot_month = '%s'", snapshot_month))

  dbWriteTable(con, "model_coefficients", coefs_df, append = TRUE, row.names = FALSE)
  dbWriteTable(con, "model_metadata",     meta_df,  append = TRUE, row.names = FALSE)

  message(sprintf(
    "\n=== Done — %d models, %d coefficients written for %s ===",
    nrow(meta_df), nrow(coefs_df), snapshot_month
  ))
}
