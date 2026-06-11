# GitHub Workflows — Quick Reference

This file describes what each workflow does and when it runs. All workflows use GitHub Actions secrets for database credentials.

## Data Collection & Aggregation

### 1. **run_imovirtual_scraper** (`r.yml`)
- **What:** Scrapes imovirtual.com for buy and rent listings across Portuguese cities
- **When:** Daily at 06:00 UTC, or manually via workflow_dispatch
- **Cities:** Porto, Matosinhos, Maia, Vila Nova de Gaia, Lisboa, Cascais, Sintra, Almada, Albufeira, Faro, Lagoa, Lagos, Loulé, Portimão
- **Output:** Inserts/updates rows in `ads_buy` and `ads_rent` with `platform = 'imovirtual'`
- **Notes:** Marks duplicate listings, sets `last_seen`, updates `is_active` flag

### 2. **run_casasapo_scraper** (`casasapo.yml`)
- **What:** Scrapes casasapo.pt for buy and rent listings across Portuguese cities
- **When:** Daily at 18:00 UTC (12 hours after imovirtual), or manually via workflow_dispatch
- **Cities:** Same as imovirtual
- **Output:** Inserts/updates rows in `ads_buy` and `ads_rent` with `platform = 'casa_sapo'`
- **Notes:** Intentionally delayed 12 hours to avoid database conflicts with imovirtual

### 3. **daily_aggregation** (`daily_aggregation.yml`)
- **What:** Builds daily snapshots of city and neighbourhood-level statistics from active listings
- **When:** Daily at 20:00 UTC (after both scrapers), or manually via workflow_dispatch
- **Populates:** `city_latest_summary` and `neighbourhood_latest_summary` tables
- **Scope:** Excludes inactive listings and duplicates; excludes neighbourhoods with < 10 listings
- **Output:** Used directly by the dashboard for current market data
- **Notes:** DELETE + INSERT pattern (safe to re-run); does not compute month-over-month changes

### 4. **monthly_aggregation** (`monthly_aggregation.yml`)
- **What:** Runs on the 1st of each month; archives latest snapshot and trains hedonic regression models
- **When:** 1st of each month at 06:00 UTC, or manually via workflow_dispatch
- **Steps:**
  1. Copies `city_latest_summary` and `neighbourhood_latest_summary` → `_monthly_` archive tables
  2. Computes `monthly_price_change_pct` by comparing to prior month
  3. Trains hedonic OLS regression models (buy & rent per city) and writes coefficients to `model_coefficients`
  4. Stores model metadata and feature statistics for price calculators
  5. Runs sanity checks on implausible values
- **Output:** `city_monthly_summary`, `neighbourhood_monthly_summary`, `model_coefficients`, `model_feature_stats`, `model_metadata`
- **Notes:** Required for calculator features and trend analysis

---

## Data Quality & Diagnostics

### 5. **keep_alive** (`keep_alive.yml`)
- **What:** Pings the Streamlit app every 10 minutes to prevent it sleeping on Streamlit Community Cloud
- **When:** Runs every 10 minutes, 24/7
- **Purpose:** Keeps the dashboard responsive and avoids cold-start delays
- **Notes:** No database access; purely a ping request to the live app

### 6. **Analyse Neighbourhood Coverage** (`analyse_neighbourhood_coverage.yml`)
- **What:** Daily snapshot of neighbourhood mapping quality
- **When:** Daily at 21:00 UTC (after daily aggregation), or manually via workflow_dispatch
- **Checks:**
  1. **NULL neighbourhood rate:** % of listings with missing/empty neighbourhood field
  2. **Unmapped neighbourhood rate:** % of listings whose neighbourhood can't be matched to any GeoJSON polygon on the dashboard
- **Scope:** Compares database neighbourhoods against:
  - `neighbourhood_lookup.json` (scraper → GeoJSON feature mapping)
  - Actual GeoJSON features in `porto_region.geojson`, `lisboa_region.geojson`, `algarve.geojson`, `almada.geojson`
- **Output:** Console report with:
  - Overall coverage % by city
  - Biggest problem neighbourhoods (by listing count)
  - Matched vs unmatched neighbourhood counts
- **Use case:** Identify which neighbourhoods need manual mapping to polygons before they appear on the dashboard maps

### 7. **Diagnose Neighbourhood Matching** (`diagnose_neighbourhoods.yml`)
- **What:** Detailed diagnostic of neighbourhood data completeness
- **When:** Manual trigger only (workflow_dispatch)
- **Checks:**
  - Overall NULL/empty neighbourhood percentage across buy/rent
  - NULL rate per city
  - Neighbourhoods in database but not in `neighbourhood_lookup.json`
  - Specific searches (e.g., looking for "Foz" listings in Porto)
  - Top neighbourhoods by listing count
- **Output:** Console tables with detailed breakdown
- **Use case:** Troubleshooting specific neighbourhood data issues

### 8. **Check Neighbourhood Coverage** (`check_neighbourhood_coverage.yml`)
- **What:** Simpler daily check of NULL neighbourhoods (legacy version)
- **When:** Manual trigger or 1st of month at 07:00 UTC
- **Notes:** Superseded by `Analyse Neighbourhood Coverage` — kept for reference
- **Use case:** Quick NULL rate check without GeoJSON matching details

---

## Neighbourhood Mapping (One-off / Diagnostic)

These workflows were created during Phase 7b (neighbourhood matching) and typically run once manually to diagnose or fix data issues. They are not scheduled.

### 9. **backfill_neighbourhoods_from_coords** (`backfill_neighbourhoods_from_coords.yml`)
- **What:** Attempts to fill NULL neighbourhoods by reverse-geocoding coordinates (lat/lon) to GeoJSON features
- **When:** Manual trigger only
- **Notes:** Experimental; depends on external geocoding service availability

### 10. **backfill_neighbourhoods_geojson** (`backfill_neighbourhoods_geojson.yml`)
- **What:** Another attempt to fill NULL neighbourhoods by spatial join against GeoJSON polygons
- **When:** Manual trigger only

### 11. **find_unmatched_neighbourhoods** (`find_unmatched_neighbourhoods.yml`)
- **What:** Identifies all neighbourhoods in the database that have no mapping in `neighbourhood_lookup.json`
- **When:** Manual trigger only
- **Output:** List of unmatched neighbourhoods by city for manual review

### 12. **suggest_neighbourhood_matches** (`suggest_neighbourhood_matches.yml`)
- **What:** Uses fuzzy string matching to suggest GeoJSON feature names for unmatched neighbourhoods
- **When:** Manual trigger only
- **Output:** Ranked list of suggestions for manual review before adding to `neighbourhood_lookup.json`

### 13. **validate_neighbourhoods_geojson** (`validate_neighbourhoods_geojson.yml`)
- **What:** Validates that `neighbourhood_lookup.json` mappings point to features that actually exist in GeoJSON files
- **When:** Manual trigger only
- **Output:** Reports broken mappings (neighbourhood → non-existent GeoJSON feature)

---

## Legacy / One-off Cleanup Workflows

### 14. **clean_price_artefacts** (`clean_price_artefacts.yml`)
- **What:** One-time cleanup to remove listings with obviously invalid prices (€0, negatives, >€50M)
- **When:** Manual trigger only (already run in Phase 1)
- **Notes:** Data is now validated at ingestion time; kept for reference

### 15. **recover_nulled_prices** (`recover_nulled_prices.yml`)
- **What:** Recover price data that was accidentally nulled
- **When:** Manual trigger only (used in Phase 1 debugging)

### 16. **clean_rent_contamination** (`clean_rent_contamination.yml`)
- **What:** Remove buy listings that accidentally appeared in `ads_rent` table
- **When:** Manual trigger only (already run in Phase 1)
- **Notes:** Rent price ceiling (€10,000) is now enforced at ingestion; kept for reference

---

## Regression Models (Alternative / Legacy)

### 17. **regression_models** (`regression_models.yml`)
- **What:** Standalone hedonic regression model training
- **When:** Manual trigger only
- **Notes:** Functionality is now included in `monthly_aggregation.yml` — this is kept for standalone runs if needed
- **Use case:** Re-train models outside of monthly schedule for testing

---

## Quick Reference: Normal Operation Schedule

| Time (UTC) | Workflow | Purpose |
|---|---|---|
| 06:00 | `run_imovirtual_scraper` | Collect listings from imovirtual |
| 18:00 | `run_casasapo_scraper` | Collect listings from casa sapo |
| 20:00 | `daily_aggregation` | Build daily summary tables for dashboard |
| 21:00 | `Analyse Neighbourhood Coverage` | Check mapping quality |
| Every 10 min | `keep_alive` | Keep Streamlit app responsive |
| **1st of month, 06:00** | `monthly_aggregation` | Archive month & train models |

**Manual triggers (workflow_dispatch):** Any workflow can be triggered manually via GitHub Actions UI for testing or emergency re-runs.

---

## Using These Workflows

### Triggering Manually
1. Go to https://github.com/your-repo/actions
2. Click the workflow name
3. Click "Run workflow" → select branch (usually `main`) → click "Run workflow"
4. Check the logs as it runs

### Checking Logs
1. Go to Actions tab
2. Click on the workflow run
3. Click on the job to see full output

### Common Tasks

**Re-run today's aggregation:**
- Trigger `daily_aggregation` manually

**Rebuild the entire month's summary:**
- Trigger `monthly_aggregation` manually (will overwrite current month's archive)

**Check neighbourhood mapping issues:**
- Trigger `Analyse Neighbourhood Coverage` for quick summary
- Trigger `Diagnose Neighbourhood Matching` for detailed breakdown
- Trigger `suggest_neighbourhood_matches` if you need fuzzy-match suggestions

**Emergency re-scrape:**
- Trigger `run_imovirtual_scraper` and `run_casasapo_scraper` manually in sequence
- Then trigger `daily_aggregation`
