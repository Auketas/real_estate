library(DBI)
library(RPostgres)
library(dplyr)
library(glmnet)

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

ALGARVE_CITIES <- c("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")

# ---- Helpers ----------------------------------------------------------------

# Impute binary feature: NA treated as absent (0)
# Handles both numeric (1/0) and text ("sim"/"nao", "yes"/"no") values
impute_binary <- function(x) {
  x <- as.character(x)
  x <- tolower(trimws(x))
  # Convert text values to numeric
  x_binary <- ifelse(x %in% c("sim", "yes", "true", "1"), 1,
                     ifelse(x %in% c("nao", "no", "false", "0"), 0, NA))
  # Impute NAs as 0 (absent)
  ifelse(is.na(x_binary), 0L, as.integer(x_binary))
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

# Create summary tables if they don't exist yet; add missing columns to existing tables
create_tables_if_missing <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_coefficients (
      id             SERIAL PRIMARY KEY,
      snapshot_date  DATE,
      snapshot_month DATE,
      listing_type   VARCHAR(10),
      city           VARCHAR(100),
      variable_name  VARCHAR(100),
      coefficient    NUMERIC,
      std_error      NUMERIC,
      created_at     TIMESTAMP DEFAULT NOW()
    )")
  dbExecute(con, "ALTER TABLE model_coefficients ADD COLUMN IF NOT EXISTS snapshot_date DATE")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_metadata (
      id                 SERIAL PRIMARY KEY,
      snapshot_date      DATE,
      snapshot_month     DATE,
      listing_type       VARCHAR(10),
      city               VARCHAR(100),
      n_observations     INTEGER,
      r_squared          NUMERIC,
      residual_std_error NUMERIC,
      created_at         TIMESTAMP DEFAULT NOW()
    )")
  dbExecute(con, "ALTER TABLE model_metadata ADD COLUMN IF NOT EXISTS snapshot_date DATE")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_feature_stats (
      id             SERIAL PRIMARY KEY,
      snapshot_date  DATE,
      snapshot_month DATE,
      listing_type   VARCHAR(10),
      city           VARCHAR(100),
      variable_name  VARCHAR(100),
      feature_mean   NUMERIC,
      created_at     TIMESTAMP DEFAULT NOW()
    )")
  dbExecute(con, "ALTER TABLE model_feature_stats ADD COLUMN IF NOT EXISTS snapshot_date DATE")
}

# Fit one hedonic OLS model; returns list(coefficients, metadata) or NULL
fit_city_model <- function(df, listing_type, city, snapshot_date, snapshot_month, mode) {

  # Numeric imputation
  df$area_imp    <- impute_median(df$area_num)
  df$novo_imp    <- impute_binary(df$novo)
  df$jardim_imp  <- impute_binary(df$jardim)
  df$garagem_imp <- impute_binary(df$garagem)
  df$terraco_imp <- impute_binary(df$terraco)
  df$varanda_imp <- impute_binary(df$varanda)

  # Categorical: collapse sparse levels
  # Combine T5, T6, T7, T8, T9+ into T5+ to avoid overfitting rare property types
  df$tipologia <- as.character(df$tipologia)
  df$tipologia[df$tipologia %in% c("T5", "T6", "T7", "T8", "T9+")] <- "T5+"

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

  formula_obj <- as.formula(paste("log(price) ~", paste(vars, collapse = " + ")))

  # Build design matrix and response vector for glmnet (ridge regression)
  dm <- model.matrix(formula_obj, data = df)
  y  <- log(df$price)

  fit <- tryCatch(
    cv.glmnet(dm, y, alpha = 0, nfolds = 10, standardize = TRUE),
    error = function(e) {
      message(sprintf("  ERROR %s/%-20s: %s", listing_type, city, e$message))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)

  # Extract coefficients at optimal (1-SE) lambda
  # coef() returns a sparse matrix; convert to dense and extract as vector
  coef_matrix <- coef(fit, s = "lambda.1se")
  coefs <- as.numeric(coef_matrix)
  names(coefs) <- rownames(coef_matrix)

  # Deduplicate: keep only the first occurrence of each variable name
  # (glmnet sometimes includes both sparse and dense forms of coefficients)
  coefs <- coefs[!duplicated(names(coefs))]

  # Compute R² using predictions at optimal lambda
  y_pred <- as.numeric(predict(fit, newx = dm, s = "lambda.1se"))
  ss_res <- sum((y - y_pred) ^ 2)
  ss_tot <- sum((y - mean(y)) ^ 2)
  r_squared <- 1 - (ss_res / ss_tot)

  # Residual standard error
  residual_std_error <- sqrt(mean((y - y_pred) ^ 2))

  message(sprintf("  %s/%-20s  n=%4d  R²=%.3f  RSE=%.4f%s",
    listing_type, city, nrow(df), r_squared, residual_std_error,
    if (r_squared < 0.3) "  *** LOW R² — check data" else ""
  ))

  # Create coefficient matrix in lm-like format
  coef_mat <- data.frame(
    Estimate = coefs,
    "Std. Error" = rep(NA_real_, length(coefs)),  # glmnet doesn't directly provide SE
    check.names = FALSE
  )

  # Print structural coefficients (exclude noisy neighbourhood/energia dummies)
  coef_est <- coefs
  structural <- coef_est[!grepl("^neighbourhood_f|^energia_f|^other_", names(coef_est))]
  coef_lines <- paste(sprintf("    %-30s %+.4f", names(structural), structural), collapse = "\n")
  message(coef_lines)

  date_col_name <- ifelse(mode == "archive", "snapshot_month", "snapshot_date")
  date_col_value <- ifelse(mode == "archive", as.character(snapshot_month), as.character(snapshot_date))

  coefs_df <- data.frame(
    listing_type   = listing_type,
    city           = city,
    variable_name  = names(coefs),
    coefficient    = as.numeric(coefs),
    std_error      = NA_real_,  # Ridge regression: approximate with regularized SE
    stringsAsFactors = FALSE
  )
  coefs_df[[date_col_name]] <- date_col_value

  meta_df <- data.frame(
    listing_type       = listing_type,
    city               = city,
    n_observations     = nrow(df),
    r_squared          = round(r_squared, 4),
    residual_std_error = round(residual_std_error, 6),
    stringsAsFactors   = FALSE
  )
  meta_df[[date_col_name]] <- date_col_value

  # Feature means from the design matrix — used for marginalizing unspecified
  # inputs in the price calculator (β × mean gives expected contribution;
  # β² × p × (1−p) gives variance contribution for binary features)
  feat_means <- colMeans(dm)
  feat_means <- feat_means[names(feat_means) != "(Intercept)"]
  feat_stats_df <- data.frame(
    listing_type   = listing_type,
    city           = city,
    variable_name  = names(feat_means),
    feature_mean   = as.numeric(feat_means),
    stringsAsFactors = FALSE
  )
  feat_stats_df[[date_col_name]] <- date_col_value

  list(coefficients = coefs_df, metadata = meta_df, feature_stats = feat_stats_df)
}

# ---- Main -------------------------------------------------------------------

run_regression_models <- function(mode = "live") {
  # mode = "live": train daily live model (snapshot_date = today)
  # mode = "archive": train monthly snapshot (snapshot_month = 1st of month)

  snapshot_date <- Sys.Date()
  snapshot_month <- as.Date(format(snapshot_date, "%Y-%m-01"))

  if (mode == "archive") {
    snapshot_date_val <- NA
    message("Running hedonic regression models (ARCHIVE) for ", snapshot_month)
  } else {
    snapshot_date_val <- snapshot_date
    message("Running hedonic regression models (LIVE) for ", snapshot_date)

    # Safety check: if monthly snapshot for this month doesn't exist, create it
    con_temp <- get_con()
    existing_month <- dbGetQuery(con_temp, sprintf(
      "SELECT COUNT(*) as cnt FROM model_metadata WHERE snapshot_month = '%s'",
      snapshot_month
    ))
    dbDisconnect(con_temp)

    if (existing_month$cnt[1] == 0) {
      message("  NOTE: Monthly snapshot missing for ", snapshot_month,
              " — will create it alongside live model")
      should_also_archive <- TRUE
    } else {
      should_also_archive <- FALSE
    }
  }

  con <- get_con()
  on.exit(dbDisconnect(con))

  create_tables_if_missing(con)

  all_coefs      <- list()
  all_meta       <- list()
  all_feat_stats <- list()

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
    ads$city[ads$city == "matosinhos"] <- "porto"

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

      if (listing_type == "rent" && city %in% ALGARVE_CITIES) {
        message(sprintf("  SKIP rent/%-20s — Algarve rent data too sparse", city))
        next
      }

      if (nrow(df) < MIN_LISTINGS) {
        message(sprintf("  SKIP %s/%-20s — %d listings (need %d)",
                        listing_type, city, nrow(df), MIN_LISTINGS))
        next
      }

      result <- fit_city_model(df, listing_type, city, snapshot_date, snapshot_month, mode)
      if (!is.null(result)) {
        all_coefs      <- c(all_coefs,      list(result$coefficients))
        all_meta       <- c(all_meta,       list(result$metadata))
        all_feat_stats <- c(all_feat_stats, list(result$feature_stats))
      }
    }
  }

  if (length(all_coefs) == 0) {
    message("No models produced — nothing to write")
    return(invisible(NULL))
  }

  coefs_df      <- bind_rows(all_coefs)
  meta_df       <- bind_rows(all_meta)
  feat_stats_df <- bind_rows(all_feat_stats)

  # For live models: delete old live models (keep only today's)
  # For archive models: append without deleting (creates historical time series)
  if (mode == "live") {
    dbExecute(con, sprintf("DELETE FROM model_coefficients  WHERE snapshot_date = '%s'", snapshot_date_val))
    dbExecute(con, sprintf("DELETE FROM model_metadata      WHERE snapshot_date = '%s'", snapshot_date_val))
    dbExecute(con, sprintf("DELETE FROM model_feature_stats WHERE snapshot_date = '%s'", snapshot_date_val))
  }

  dbWriteTable(con, "model_coefficients",  coefs_df,      append = TRUE, row.names = FALSE)
  dbWriteTable(con, "model_metadata",      meta_df,       append = TRUE, row.names = FALSE)
  dbWriteTable(con, "model_feature_stats", feat_stats_df, append = TRUE, row.names = FALSE)

  mode_label <- ifelse(mode == "archive", "archive", "live")
  message(sprintf(
    "\n=== Done — %d models, %d coefficients, %d feature stats written (%s) ===",
    nrow(meta_df), nrow(coefs_df), nrow(feat_stats_df), mode_label
  ))

  # If running live mode and monthly snapshot is missing, also save it
  if (mode == "live" && exists("should_also_archive") && should_also_archive) {
    message("\nAlso saving as monthly snapshot for ", snapshot_month)
    coefs_archive <- coefs_df
    coefs_archive$snapshot_month <- as.character(snapshot_month)
    coefs_archive$snapshot_date <- NULL

    meta_archive <- meta_df
    meta_archive$snapshot_month <- as.character(snapshot_month)
    meta_archive$snapshot_date <- NULL

    feat_archive <- feat_stats_df
    feat_archive$snapshot_month <- as.character(snapshot_month)
    feat_archive$snapshot_date <- NULL

    dbWriteTable(con, "model_coefficients",  coefs_archive,  append = TRUE, row.names = FALSE)
    dbWriteTable(con, "model_metadata",      meta_archive,   append = TRUE, row.names = FALSE)
    dbWriteTable(con, "model_feature_stats", feat_archive,   append = TRUE, row.names = FALSE)
    message("Monthly snapshot saved for ", snapshot_month)
  }
}
