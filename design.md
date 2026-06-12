# Portugal Real Estate Dashboard — Design & Architecture Brief

## Overview

This document defines the agreed design direction, page structure, visual language, and database architecture for the Portugal real estate dashboard. It is intended as a brief for Claude Code to implement.

---

## ⚠️ Important for Claude Code

**Whenever you add, modify, or remove a GitHub Actions workflow**, please update `.github/workflows/README.md` with a description of what the workflow does, when it runs, and what it touches. The README is the single source of truth for the list of all workflows in this repo.

---

## 🔴 Known Issues — START HERE FOR NEXT SESSION

None currently identified. See "Recent Changes & Current Status" for June 12-13 improvements.

---

---

## Target Audience

Primary: expats and foreigners considering relocating to or buying property in Portugal. They are making a high-stakes personal decision, not doing professional investment analysis. The tone and design should feel like a trusted local guide, not a financial terminal.

Secondary: property investors looking for yield data. Served by a dedicated page but not the primary design target.

---

## Visual Design Language

### Aesthetic

Warm and approachable, not slick and corporate. Think Portuguese tile, terracotta, warm stone — not Bloomberg or Zillow. The product should feel like it was made by someone who knows and loves Portugal.

### Colour Palette

- **Primary background:** warm off-white, e.g. `#FAF7F2`
- **Sidebar/surface:** warm light grey, e.g. `#F0EBE3`
- **Primary accent:** terracotta/rust, e.g. `#C4603A` — used for active nav items, key metrics, buttons
- **Secondary accent:** muted olive/sage, e.g. `#7A8C6E` — used for positive trends, secondary highlights
- **Text primary:** dark warm grey, e.g. `#2C2C2C`
- **Text secondary:** medium warm grey, e.g. `#6B6B6B`
- **Borders/dividers:** `#E0D9D0`
- **Map choropleth scale:** cream/sand `#F5E6C8` → terracotta `#C4603A` (low to high price per m²)

### Typography

- Use Streamlit's built-in font stack but configure via `st.set_page_config` and custom CSS
- Page titles: large, warm, slightly heavier weight
- Section headers: medium weight, not all-caps
- Metric labels: small, secondary colour, uppercase tracking
- Metric values: large, primary text colour

### Chart Style

- Minimal gridlines, light grey only
- No unnecessary borders on charts
- Consistent use of terracotta as primary chart colour
- Avoid default Plotly blue throughout
- Prefer horizontal bar charts over vertical when labels are long
- Error bars / confidence intervals in secondary accent colour
- All charts should have a subtitle explaining the data source or calculation method in small secondary text

### General UI

- Sidebar: city/region selector, currency toggle (EUR / GBP / USD / NOK / SEK / DKK), buy/rent toggle where relevant
- Currency toggle applies globally — all prices throughout the app convert on the fly using ECB daily rates
- No standalone Expat Tools page
- Navigation items: Explorer (free) | Neighbourhood Deep-Dive (paid) | Investment View (paid) | Contact | Legal
- Paywall: soft gate — show the section header and a blurred/placeholder visual with a subscribe prompt. Do not hard-redirect.

---

## Page Structure

### Page 1 — Explorer (free)

Entry point for all users. Shows country-level overview across all covered cities.

**Section 1: Hero metrics row**
Four stat cards in a horizontal row:
- Total active listings (with buy/rent toggle)
- Median price per m² across all covered cities
- Cities covered
- Data freshness ("Updated today" etc.)

Cards use terracotta accent for the metric value, secondary grey for the label.

**Section 2: Portugal overview map**
- Choropleth map coloured by median price per m² at city/region level
- Colour scale: cream → terracotta
- On hover: show city name, median price, median price per m², avg time on market, listing count
- This is the hero visual of the page — give it generous vertical space
- Use Plotly with a clean base map (no satellite, light grey geography)

**Section 3: City comparison summary table**
A clean, well-formatted table with one row per city. Columns:
- City
- Listings (buy)
- Median price (€)
- Median €/m²
- Avg time on market (days)
- Sunshine hrs/yr
- Avg summer temp (°C)
- Avg winter temp (°C)

City names should be properly capitalised (Lisboa not lisboa). Climate columns sourced from static data, not scraped. Include a small caption explaining this.

**Section 4: Market pulse (auto-generated text)**
Two or three plain-language sentences generated from the data. Example: "Lisbon has 8,968 active listings with a median asking price of €645,000. Prices across covered cities have held broadly flat over the past month. The Algarve continues to show the longest average time on market at X days." Template-driven, updated whenever data refreshes.

**Section 5: What's inside (paid features preview)**
A heading "Go deeper with a subscription" followed by a short demo video (20–30 seconds, silent, auto-loop) showcasing the paid features in action:
- Choropleth map showing neighbourhood price per m² with user clicking into a neighbourhood
- Charts and metrics updating dynamically
- Calculator input and output in motion

Below the video: brief descriptive text summarizing what users get ("Explore neighbourhood-level analysis, historical trends, and interactive calculators for any property specification"), plus a prominent Subscribe button linking to LemonSqueezy.

This section should feel like a natural part of the page, not an intrusive upsell. Keep the tone informative rather than salesy.

**Note:** Video production deferred to Phase 10 — will record and edit after page design is finalized and stable.

**Section 6: Feedback nudge**
A single subtle line near the bottom of the page, below the preview section. Suggested copy: "Have a specific market or feature in mind? We're actively expanding — [let us know](link to Contact page)." Style as secondary text, small font, not a banner or alert box. The goal is to invite input without signalling incompleteness.

---

### Page 2 — Neighbourhood Deep-Dive (paid)

City-level analysis with neighbourhood granularity.

**Controls (sidebar or top of page):**
- City selector (Lisboa / Porto / Algarve)
- Buy / Rent toggle — **Rent is only available for Lisboa and Porto.** Algarve is buy-only; the rental market there is too thin on these portals to produce meaningful data. Hide the rent toggle when an Algarve city is selected and show a one-line note: "Rental data is not available for the Algarve — the long-term rental market in this region is listed primarily on other platforms."

**Section 1: Neighbourhood choropleth map**
- Coloured by median price per m² at neighbourhood level
- Same cream → terracotta colour scale as country map for consistency
- On hover: neighbourhood name, median price per m², avg time on market, monthly price change (%), most common property type
- Do NOT show individual listing dots — aggregate only
- Use Plotly with GeoJSON neighbourhood boundaries

**Section 2: Price calculator**
The flagship paid feature.

Inputs (use Streamlit widgets in a clean horizontal or two-column layout):
- Neighbourhood (dropdown, populated by selected city)
- Property type (T0 / T1 / T2 / T3 / T4 / T4+)
- Size in m² (slider)
- Key features (toggles: parking, garden, terrace, new build) — **all optional**

All feature inputs are optional. Users set only what they know or care about. Unspecified features are marginalized out: their expected contribution to price is computed using the feature's prevalence in the training data (`β × prevalence`), and their uncertainty adds to the prediction interval width (`β² × p × (1−p)` per unspecified binary feature). The result is that specifying more features narrows the confidence band; leaving features blank widens it honestly rather than silently assuming a value. This should be communicated to the user with a short line like "Add more details to narrow the estimate."

Outputs:
- Predicted price with confidence interval, displayed as a prominent metric with range shown below (e.g. "€385,000 — estimated range €355,000–€420,000")
- **Confidence interval: 50% (not 95%)**. At 80% CI, rent predictions were useless (€1,000–€2,200 for a ~€1,500 rental). 50% CI gives actionable ranges while remaining statistically honest. Narrower than commercial tools but prevents false precision and encourages feature specification.
- Confidence indicator: High (±<25%), Medium (±25–40%), Low (±>40%). Shows when users add more details.
- Interval visibly widens as fewer features are specified — this is intentional and honest
- "Up X% from last month" / "Down X% over 6 months" as delta indicators (deferred until we have multiple monthly snapshots)
- Small bar or line chart showing predicted price for these specifications over available monthly history (deferred: will implement once we have >3 months of historical coefficients)
- Number of listings that informed the estimate, shown as a small caption for transparency

**Currency converter:** Located in sidebar (top of each page). Always accessible without scrolling, prominent enough for target audience (expats, international buyers), doesn't distract from core features. Label "Currency" is self-explanatory.

Implementation note: predictions are served from a precomputed monthly coefficients table in Neon (see Database section). Apply coefficients to user inputs at query time. Do not run regression live. Feature prevalences (needed for marginalization) are stored in `model_feature_stats` (see Database section).

---

### Page 3 — Investment View (paid)

Secondary page for investors. Renamed from "Rental Yield."

**Section 1: Gross yield by city**
- Clean horizontal bar chart, one bar per city
- Colour bars by yield level: below 3% red, 3–5% amber, above 5% green
- Add a dashed vertical line at 5% as benchmark
- Caption: "Gross yield = annualised median asking rent / median asking buy price. Asking prices only — not transaction data."
- Fix calculation before launch — current values are clearly erroneous

**Section 2: Price-to-rent ratio by city**
- Companion metric to yield
- Simple table or bar chart

**Section 3: Rental yield calculator**
Lisboa and Porto only — Algarve is excluded from rent analysis (see Data Coverage).

Same input structure as price calculator on Page 2 — all feature inputs optional, same marginalization approach. Outputs:
- Estimated buy price for specifications
- Estimated monthly rent for same specifications
- Gross yield percentage
- "Purchase prices for this type are up/down X% over 6 months"

Same coefficient table architecture as buy price calculator — run a parallel regression on rental listings monthly and store coefficients separately.

Honest caveat displayed prominently: "These are gross yield estimates based on asking prices and asking rents. Net yield will be lower after taxes, vacancy, maintenance and management costs."

---

## Database Architecture (Neon / PostgreSQL)

### Principle

Pre-aggregate everything. The dashboard should never run heavy calculations at query time. All expensive computations happen in scheduled jobs and results are stored in summary tables. The dashboard always reads from `_latest_` tables, which are refreshed daily; monthly `_monthly_` archive tables retain historical snapshots for trend analysis and regression model training.

### New tables to create

**`city_latest_summary`**
Daily snapshot of city-level stats, refreshed every night at 20:00 UTC. Feeds the dashboard with current data.
```sql
CREATE TABLE city_latest_summary (
    id SERIAL PRIMARY KEY,
    snapshot_date DATE,
    city VARCHAR(100),
    listing_type VARCHAR(10),      -- 'buy' or 'rent'
    listing_count INTEGER,
    median_price NUMERIC,
    median_price_per_m2 NUMERIC,
    avg_time_on_market_days NUMERIC,
    p25_price NUMERIC,
    p75_price NUMERIC,
    created_at TIMESTAMP DEFAULT NOW()
);
```

**`neighbourhood_latest_summary`**
Daily snapshot of neighbourhood-level stats, refreshed every night at 20:00 UTC. Feeds the dashboard with current data.
```sql
CREATE TABLE neighbourhood_latest_summary (
    id SERIAL PRIMARY KEY,
    snapshot_date DATE,
    city VARCHAR(100),
    neighbourhood VARCHAR(200),
    listing_type VARCHAR(10),
    listing_count INTEGER,
    median_price NUMERIC,
    median_price_per_m2 NUMERIC,
    avg_time_on_market_days NUMERIC,
    most_common_property_type VARCHAR(20),
    created_at TIMESTAMP DEFAULT NOW()
);
```

**`city_monthly_summary`**
Monthly archive of city-level stats. Populated on the 1st of each month by copying from `city_latest_summary`. Retains historical snapshots for trend analysis.
```sql
CREATE TABLE city_monthly_summary (
    id SERIAL PRIMARY KEY,
    snapshot_month DATE,           -- first day of month
    city VARCHAR(100),
    listing_type VARCHAR(10),      -- 'buy' or 'rent'
    listing_count INTEGER,
    median_price NUMERIC,
    median_price_per_m2 NUMERIC,
    avg_time_on_market_days NUMERIC,
    p25_price NUMERIC,
    p75_price NUMERIC,
    created_at TIMESTAMP DEFAULT NOW()
);
```

**`neighbourhood_monthly_summary`**
Monthly archive of neighbourhood-level stats. Populated on the 1st of each month by copying from `neighbourhood_latest_summary`. Includes `monthly_price_change_pct` computed from prior month.
```sql
CREATE TABLE neighbourhood_monthly_summary (
    id SERIAL PRIMARY KEY,
    snapshot_month DATE,
    city VARCHAR(100),
    neighbourhood VARCHAR(200),
    listing_type VARCHAR(10),
    listing_count INTEGER,
    median_price NUMERIC,
    median_price_per_m2 NUMERIC,
    avg_time_on_market_days NUMERIC,
    monthly_price_change_pct NUMERIC,   -- vs prior month
    most_common_property_type VARCHAR(20),
    created_at TIMESTAMP DEFAULT NOW()
);
```

**`model_coefficients`**
Stores hedonic regression coefficients per month per listing type. One row per predictor variable per month.
```sql
CREATE TABLE model_coefficients (
    id SERIAL PRIMARY KEY,
    snapshot_month DATE,
    listing_type VARCHAR(10),          -- 'buy' or 'rent'
    city VARCHAR(100),
    variable_name VARCHAR(100),        -- e.g. 'intercept', 'size_m2', 'type_T2', 'neighbourhood_Baixa'
    coefficient NUMERIC,
    std_error NUMERIC,
    created_at TIMESTAMP DEFAULT NOW()
);
```

**`model_feature_stats`**
Stores the mean/prevalence of each feature used in the model. Required for marginalizing over unspecified inputs in the price calculator — when a user leaves a feature blank, the calculator uses `β × prevalence` as the expected contribution and `β² × p × (1−p)` as the variance contribution to the confidence interval.
```sql
CREATE TABLE model_feature_stats (
    id SERIAL PRIMARY KEY,
    snapshot_month DATE,
    listing_type   VARCHAR(10),
    city           VARCHAR(100),
    variable_name  VARCHAR(100),   -- matches variable_name in model_coefficients
    feature_mean   NUMERIC,        -- prevalence for binary features, mean for numeric
    created_at     TIMESTAMP DEFAULT NOW()
);
```

**`model_metadata`**
One row per model run — stores model-level stats for confidence interval computation.
```sql
CREATE TABLE model_metadata (
    id SERIAL PRIMARY KEY,
    snapshot_month DATE,
    listing_type VARCHAR(10),
    city VARCHAR(100),
    n_observations INTEGER,
    r_squared NUMERIC,
    residual_std_error NUMERIC,        -- used for prediction intervals
    created_at TIMESTAMP DEFAULT NOW()
);
```

### Implementation order

**Important:** Create and verify summary tables before implementing any listing deletion logic. Run summary tables in parallel with the existing dashboard for at least one to two weeks, compare outputs, and fix any discrepancies before the scraper cleanup goes live. You want confidence the archive captures everything you need before raw data starts being removed.

### Creating the tables

The four tables above can be created directly in the Neon console by pasting the CREATE TABLE statements, or via a migration script run by Claude Code. For a one-time setup of four tables, the Neon console is fastest.

### Scraper updates

**Remove inactive listings:** The `is_active` flag and `last_seen` column already exist on `ads_buy` and `ads_rent`. The scraper should already be updating `last_seen` on every run. The cleanup logic to add is:
- Set `is_active = false` for any listing where `last_seen < NOW() - INTERVAL '7 days'`
- After summary tables are verified and stable, delete rows where `is_active = false` and `last_seen < NOW() - INTERVAL '7 days'`
- Before deleting, insert a summary row into a `listing_history` archive table with: `id`, `city`, `neighbourhood`, `tipologia`, `area`, `price`, `first_seen`, `last_seen`, computed days on market, and `platform`

The 7-day buffer handles properties that temporarily disappear and reappear on the platforms.

This keeps the active table lean and fast while preserving the historical data needed for time-on-market calculations and model training.

**Aggregation jobs — two cadences:**

**Daily aggregation** (every night at 20:00 UTC):
1. Populates `city_latest_summary` and `neighbourhood_latest_summary` from active listings
2. Overwrites previous day's snapshot (DELETE + INSERT)
3. Does not compute `monthly_price_change_pct` — that's for monthly archive only
4. Dashboard queries always read from `_latest_` tables for current data

**Monthly aggregation** (1st of each month at 06:00 UTC):
1. Copies today's `_latest_` snapshot into the monthly archive tables (`city_monthly_summary` and `neighbourhood_monthly_summary`)
2. Computes `monthly_price_change_pct` by comparing to prior month's archive
3. Runs the hedonic regression models (buy and rent, per city) and writes coefficients to `model_coefficients` and `model_metadata`
4. Runs sanity checks (flag any median price per m² outside plausible range, price changes >20% month-on-month, listing count changes >50%)

### Sanity checks — two layers

Filtering happens at two distinct stages for different reasons.

**Layer 1: Ingestion (in the scraper)**
Catch obvious scraping artefacts before they enter the database at all. Reject any listing where:
- `price` is €0, negative, or above €50,000,000
- `area` is 0, negative, or above 2,000 m²
- Price per m² (`price` / `area`, where both are present) is below €200 or above €50,000
- Required fields (`city`, `neighbourhood`, `tipologia`, `price`) are missing

Log rejected listings with reason — do not silently discard. This protects data quality at source.

**Layer 2: Summary table construction**
Catch subtler analytical issues when aggregating. Before writing any summary row, validate:
- Median price per m² should be between €500 and €20,000 for all cities
- Gross yield should be between 0% and 20%
- Price change month-on-month should be between -20% and +20%
- Listing counts should not change by more than 50% in a single month

For model training, additionally exclude listings outside 3 standard deviations from the city-level mean price per m² — these are genuine outliers that would bias regression coefficients even if they passed ingestion checks.

Log and alert on any violations rather than silently writing bad data. Do not overwrite a valid prior month's summary with a failed one.

---

## Geographical Matching: Listings to Neighbourhoods (GeoJSON)

**Method: Spatial join via lat/lon coordinates (NOT neighbourhood name matching)**

All listings are matched to GeoJSON parish polygons using their `lat`/`lon` coordinates via point-in-polygon spatial join. The `neighbourhood` column is populated with the GeoJSON feature name (`NAME_3` for Porto/Lisboa/Setúbal, `NAME_2` for Algarve) from the matched polygon. This approach:
- Avoids reliance on user-entered neighbourhood text (which is often misspelled or inconsistent across platforms)
- Matches ~99% of listings (only missing those with invalid coordinates)
- Makes `neighbourhood_lookup.json` obsolete — no name-based mapping needed

**In the dashboard:**
- Choropleth maps use the `neighbourhood` field directly as GeoJSON feature names
- No lookup table required; `neighbourhood` → `NAME_3` match is 1:1 and pre-computed at scrape time
- All neighbourhood data in `neighbourhood_latest_summary` and `neighbourhood_monthly_summary` is keyed by the correct GeoJSON feature name

**Backfill history:** Spatial join backfill completed June 2026; all ~35,500 listings with valid coordinates matched to GeoJSON polygons. Any unmatched listings have invalid/ocean coordinates.

---

## Dual Data Source Handling (Imovirtual + Casa Sapo)

Data comes from two platforms stored in `ads_buy` and `ads_rent`. The `platform` column already exists on both tables. Beyond deduplication, the following considerations apply.

### Current schema (ads_buy and ads_rent)

| Column | Description |
|--------|-------------|
| `id` | Primary key |
| `city` | City name |
| `area` | Size in m² |
| `tipologia` | Property type (T0/T1/T2/T3/T4 etc.) |
| `andar` | Floor level — affects price, include in regression model |
| `anunciante` | Listing agency/advertiser — useful for deduplication |
| `tipo` | Property category (apartment, house, etc.) |
| `novo` | New build flag |
| `jardim` | Garden (boolean) |
| `energia` | Energy rating — affects price, include in regression model |
| `elevador` | Lift (boolean) |
| `garagem` | Parking/garage (boolean) |
| `terraco` | Terrace (boolean) |
| `varanda` | Balcony (boolean) |
| `lat` / `lon` | Coordinates |
| `neighbourhood` | Neighbourhood name |
| `price` | Asking price (€) |
| `is_active` | Active listing flag — already maintained by scraper |
| `first_seen` | Date first scraped |
| `last_seen` | Date last seen in scrape |
| `platform` | Source platform (Imovirtual / Casa Sapo) |
| `price_change_buys` / `price_change_rent` | Foreign key to price_changes tables |

**Note on price_changes tables:** `price_changes_buy` and `price_changes_rent` exist and are linked via foreign key. These are not used in v1 but should be retained — they become useful later for features like "listing reduced twice" or "neighbourhood reduction frequency." Do not delete.

### Regression model inputs
The hedonic pricing model should include: `area`, `tipologia`, `neighbourhood`, `novo`, `jardim`, `garagem`, `terraco`, `varanda`, `andar`, `energia`. The calculator UI exposes a subset of these as user-facing inputs (area, tipologia, neighbourhood, jardim, garagem, terraco, varanda, novo). `andar` and `energia` are included in the model for accuracy but not exposed as calculator inputs in v1.

**Deduplication**
The same property is very likely listed on both platforms simultaneously — this is the norm in Portuguese real estate. Treat two listings as probable duplicates if they share: `city`, `neighbourhood`, `tipologia`, `area` (within 5 m² tolerance), and `price` (within 5%). `anunciante` being different across platforms is expected and should not disqualify a match. Flag likely duplicates in a `duplicate_flag` column rather than deleting immediately. Build all summary tables and model training data from a deduplicated view. Inspect flagged duplicates periodically before committing to any deletion logic.

**Source quality monitoring**
The two platforms may differ systematically in listing completeness, neighbourhood tagging accuracy, or regional coverage. Periodically check whether median price per m² or avg time on market differs significantly between platforms for the same city and property type. A large systematic difference signals a scraping or parsing issue on one platform.

**Time on market comparability**
A property may be posted on Imovirtual on day 1 and cross-posted to Casa Sapo on day 30. Time on market should be calculated from the earliest `first_seen` across both platform records for a deduplicated listing, not independently per source.

**Price change tracking**
Once deduplication is in place, track price changes on the canonical deduplicated record only. Tracking price changes independently per source will produce phantom price change events when one platform updates before the other.

---

## Data Coverage

Cities currently scraped:
- **Lisboa region:** Lisboa, Cascais, Sintra
- **Porto region:** Porto, Vila Nova de Gaia, Matosinhos, Maia
- **Setúbal region:** Almada, Costa da Caparica, Caparica e Trafaria
- **Algarve:** Albufeira, Faro, Lagoa, Lagos, Loulé, Portimão

**Rental data coverage:** Lisboa, Porto, and Setúbal regions. The Algarve rental market is too thin on imovirtual and casa sapo (verified: ~4–55 active listings per city) to produce meaningful statistics or models. All rent-facing features — the rent toggle, rent regression models, and the rental yield calculator — are restricted to these regions. Algarve pages show buy data only.

Do not expand beyond this for now. Each new city is ongoing scraper maintenance.

---

### Page 4 — Contact

A minimal page. Short intro line at the top: "We read every message and aim to respond within 48 hours."

Form fields:
- Name (text input)
- Email (text input)
- Subject (dropdown: Feedback / Data issue / Subscription question / Feature request / Partnership / Other)
- Message (text area)
- Submit button

Implementation: use Formspree (free tier) — sign up, get an endpoint URL, post form data to it and it forwards to the project Gmail. No backend needed. Alternatively use Streamlit's `st.form` with Python `smtplib` and a Gmail app password, but Formspree is simpler to maintain.

The Contact page is accessible to all users (free and paid) and appears in the sidebar navigation alongside the Legal pages.

---

## Implementation Plan for Claude Code

Work through these phases sequentially. Complete and verify each phase before starting the next. Use `workflow_dispatch` to test any GitHub Actions changes manually rather than waiting for scheduled runs.

---

### Phase 1 — Scraper updates ✓ COMPLETE

*Start here. Everything downstream depends on clean data coming in.*

- [x] Add Cascais and Sintra to the Lisbon scraper
- [x] Add Matosinhos to the Porto scraper
- [x] Add Maia to the Porto scraper (for expanded Porto metro coverage)
- [x] Add Almada to the Lisbon scraper (expanding south of Tagus market)
- [x] Fix the price calculation bug already identified
- [x] Add ingestion-level sanity checks — reject listings where:
  - `price` is 0, negative, or above €50,000,000
  - `area` is 0, negative, or above 2,000 m²
  - Price per m² (`price` / `area`) is below €200 or above €50,000 where both fields are present
  - Required fields (`city`, `tipologia`, `price`) are missing
- [x] Log all rejected listings with rejection reason — writes to `log/rejected_listings.csv`
- [x] Fix concatenated price ranges: `parse_price_raw()` detects digit-hyphen-digit in raw HTML text and takes the lower bound before non-digit stripping. Logs corrected listing IDs to console.
- [x] Add deduplication flag: `mark_duplicates()` runs after each full scrape. Uses lat/lon proximity (< 0.0001°, ~11m) + same `tipologia` + price within 5% across different platforms. Coordinate-based matching is more reliable than neighbourhood string matching. `duplicate_flag BOOLEAN DEFAULT false` column must exist on `ads_buy` and `ads_rent` (add via Neon console).
- [x] Fix imovirtual `read_ads` bug: was selecting only `id, price` so `is_active` filter on `inactive_ids` silently returned zero IDs — no listings were ever marked inactive. Fixed to select `is_active` and filter by `platform = 'imovirtual'`.
- [x] Add rent price ceiling to ingestion sanity checks: rent listings with `price > €10,000` rejected as `rent_price_above_10000_likely_buy`. Plugs a historical bug where ~8,200 buy listings accumulated in `ads_rent` (April 2026). One-time cleanup scripts (`clean_price_artefacts`, `clean_rent_contamination`) removed the historical contamination.

---

### Phase 2 — Monthly aggregation script (R) ✓ COMPLETE

*Populate `city_monthly_summary` and `neighbourhood_monthly_summary`. Run manually once after building to get initial data.*

- [x] Connect to Neon from R using `RPostgres` / `DBI`
- [x] Query `ads_buy` and `ads_rent` excluding rows where `duplicate_flag = true` or `is_active = false`
- [x] Compute city-level aggregations per listing type: `listing_count`, `median_price`, `median_price_per_m2`, `avg_time_on_market_days`, `p25_price`, `p75_price`
- [x] Compute neighbourhood-level aggregations: same metrics plus `monthly_price_change_pct` (vs prior month) and `most_common_property_type`
- [x] Sanity checks log warnings for implausible median price/m² and month-on-month price change >20%; thresholds are split by listing type (buy: €500–€20,000/m²; rent: €3–€100/m²); script does not abort
- [x] Neighbourhood threshold: rows with fewer than 10 listings per neighbourhood are dropped; cities where all neighbourhoods fall below the threshold get a single city-level fallback row (`neighbourhood = city`) so every city retains at least one data point
- [x] DELETE + INSERT pattern per snapshot_month — safely re-runnable without duplicating
- [x] GitHub Actions workflow (`monthly_aggregation.yml`) scheduled for 1st of month at 06:00 UTC with `workflow_dispatch` for manual runs
- [x] First snapshot triggered manually to populate June 2026 baseline

---

### Phase 3 — Regression model script (R) ✓ COMPLETE

*Populate `model_coefficients`, `model_metadata`, and `model_feature_stats`. Run manually once after building.*

- [x] For each city and each listing type (buy / rent), train a hedonic OLS regression:
  - Dependent variable: `log(price)`
  - Independent variables: `area`, `tipologia` (dummies), `neighbourhood` (dummies), `novo`, `jardim`, `garagem`, `terraco`, `varanda`, `energia` (dummies), `andar` (only if ≥ 50% coverage)
  - Exclude rows where `duplicate_flag = true` or `is_active = false`
  - Outlier filter: remove listings > 3 SD from city-level mean log(price/m²), applied only to listings with known area
  - Missing values: binary features imputed as 0 (absent), numerics with column median; sparse factor levels (< 10 listings) collapsed to `"other_X"`
  - Rent models restricted to Lisboa and Porto — Algarve skipped explicitly
  - Cities with fewer than 50 active listings skipped and logged
- [x] Store each coefficient and its std error as a row in `model_coefficients`
- [x] Store model-level stats (n, R², residual std error) in `model_metadata`
- [x] Store feature means/prevalences in `model_feature_stats` — required for marginalizing unspecified calculator inputs
- [x] DELETE + INSERT pattern per `snapshot_month` — safely re-runnable
- [x] R² and RSE printed per model in workflow log; R² < 0.3 flagged with warning
- [x] Workflow triggered and verified — R² values good across all cities; structural coefficients (area, tipologia dummies, amenity flags) have correct signs and magnitudes

---

### Phase 4 — GitHub Actions workflow ✓ COMPLETE

*Combine Phases 2 and 3 into a single monthly workflow.*

- [x] Extend `monthly_aggregation.yml` to also run the regression script after aggregation — added as a second job (`regress`) with `needs: aggregate`
- [x] `regression_models.yml` retained for standalone manual runs
- [x] Combined workflow triggers on schedule (1st of month, 06:00 UTC) and `workflow_dispatch`

---

### Phase 5 — Verify and sanity check ✓ COMPLETE

*Do not proceed to the dashboard until this phase is complete.*

- [x] Compare `city_monthly_summary` output against direct queries on `ads_buy`/`ads_rent` — medians match for all well-populated cities
- [x] All cities present; city names lowercase in DB (display capitalisation handled in dashboard)
- [x] No NULL values in any critical columns
- [x] Buy price per m² plausible across all cities (Sintra €3,718 → Lisboa €7,065); rent price per m² plausible for Lisboa, Porto, Vila Nova de Gaia
- [x] Regression coefficients verified — correct signs and magnitudes
- [x] R² values acceptable across all models
- [x] Matosinhos remapped to Porto in aggregation and regression scripts — raw DB tables keep `city = 'matosinhos'` (required for scraper inactive detection); summary tables and models combine under `'porto'`. Re-run `monthly_aggregation` workflow manually to apply.

---

### Phase 6 — Dashboard migration ✓ COMPLETE

*Migrate existing dashboard queries to use summary tables. Fix known data bugs.*

- [x] Add `get_city_summary()` and `get_neighbourhood_summary()` to `utils/db.py`; add `CITY_LABELS` dict with correct Portuguese diacritics
- [x] Migrate all four pages to read from `city_latest_summary` / `neighbourhood_latest_summary` (daily snapshots)
- [x] Rental yield bug fixed — was computing on raw unfiltered prices; now uses pre-aggregated medians
- [x] Price per m² outlier bug fixed — summary tables exclude outliers at aggregation time
- [x] Broken price-change history chart removed (10^15 values in `price_changes` tables; time series will return in Phase 8 once multiple monthly snapshots exist)
- [x] City names correctly capitalised throughout, including Loulé, Portimão, Vila Nova de Gaia
- [x] Matosinhos removed from city selectors (folded into Porto in summary tables)
- [x] Load time verified — substantially faster in production (summary query returns ~12 rows vs full raw listings table)

### Phase 7a — Daily aggregation ✓ COMPLETE

*Split aggregation into daily (dashboard) and monthly (archive) cadences.*

- [x] Create `city_latest_summary` and `neighbourhood_latest_summary` tables — daily snapshots with `snapshot_date`
- [x] Create daily aggregation script (`code/aggregate/daily_aggregation.R`) — mirrors monthly logic but without price change computation
- [x] Create GitHub Actions workflow (`daily_aggregation.yml`) — runs at 20:00 UTC every night
- [x] Update monthly workflow to copy from `_latest_` → `_monthly_` and compute `monthly_price_change_pct`
- [x] Dashboard now reads from `_latest_` tables for always-current data
- [x] Monthly archive tables retain historical snapshots for trend analysis and regression training

---

### Phase 7 — Visual sprint

*Apply the new design language. See Visual Design Language section for colours, typography and chart style.*

#### 7a — Theme and global styles ✓ COMPLETE
- [x] Add `.streamlit/config.toml` with base background (`#FAF7F2`), sidebar background (`#F0EBE3`), and primary accent (`#C4603A`)
- [x] Add global custom CSS in `app.py`: removed Expat Tools references, updated landing page copy
- [x] Define shared Plotly chart template in `utils/charts.py`: terracotta primary colour, minimal grey gridlines, no chart borders

#### 7b — Structural changes ✓ COMPLETE
- [x] Remove `5_Expat_Tools.py` — currency toggle now in sidebar
- [x] Add currency toggle to sidebar via `utils/sidebar.py` — reads ECB rates, returns `(rate, symbol, fmt_price)` used by all pages to convert every price amount displayed
- [x] All four pages updated: import charts template, call `render_currency_selector()`, multiply price columns by rate before charting, use symbol in axis labels and table headers

#### 7c — Explorer page (Page 1, free) ✓ COMPLETE

- [x] Hero metrics row: active listings, median price/m², cities covered, data freshness ("Updated [Month Year]")
  - [x] Fix date format truncation: use abbreviated month ("Jun 2026" not "June 2026")
- [x] Portugal overview map: `scatter_mapbox` with city centroids coloured by median price/m² (cream → terracotta), sized by √listing_count, hover shows city stats. `carto-positron` base map, centred on Portugal.
- [x] City comparison table: all cities with climate columns (sunshine hrs/yr, avg summer °C, avg winter °C) sourced from static IPMA/Wikipedia dict in the page. Caption explains source.
  - [x] Remove city-level listing breakdown (headline number shown at top, no need to repeat per-city)
  - [x] Add Maia and Almada with proper coordinates and climate data
- [x] Price per m² and time-on-market bar charts (absorbed from City Comparison page)
- [x] `1_Market_Overview.py` and `2_City_Comparison.py` removed; replaced by `1_Explorer.py`
- [ ] **What's inside** (paid features preview): Deferred to Phase 10 (after price calculators & neighbourhood pages built). Build with real working screenshots/demo video so preview accurately reflects final product.
- [ ] **Market pulse** (deferred): Optional auto-generated summary text. Low priority — skip for now.

#### 7d — Neighbourhood Deep-Dive page (Page 2, paid) — IN PROGRESS

**Page structure (reordered for product importance):**

1. **Region & type selector** — Choose region (Porto/Lisboa/Setúbal/Algarve) and buy/rent toggle
2. **Interactive choropleth map** — Neighbourhood-level price visualization (cream → terracotta by €/m²)
3. **Price calculator** ✓ NOW PRIMARY FEATURE
   - Optional inputs: neighbourhood, property type, area, new build, garden, parking, terrace, balcony
   - Output: estimated price with 95% confidence interval shown as visual band (━━━●━━━)
   - Band width dynamically widens/narrows based on features specified
   - Helper text: "Add more details to narrow the estimate"
4. **Neighbourhood browse** (secondary) — Bar chart showing all neighbourhoods for reference/filtering

**Technical details:**
- [x] GeoJSON files sourced from GADM 4.1; saved to `dashboard/static/`
- [x] Choropleth built with `choropleth_mapbox`, cream → terracotta colour scale by median price per m²
- [x] Hover display: fixed neighbourhood name spacing, removed listing counts
- [x] Multiple DB neighbourhoods aggregated by listing-count-weighted average
- [x] Algarve: city-level choropleth + neighbourhood breakdown (no calculator — buy-only, no rental model)
- [x] Price calculator uses precomputed monthly model coefficients, marginalizes unspecified features
- [x] Confidence intervals computed from model residual_std_error + feature uncertainty
- [x] Added Maia to Porto region, Almada/Costa da Caparica/Caparica to Setúbal region

#### 7e — Investment View page (Page 3, paid) ✓ COMPLETE
- [x] Horizontal bar chart for gross yield: discrete colour bands (< 3% red / 3–5% amber / > 5% green), dashed 5% benchmark line, caption explaining calculation and data scope
- [x] Price-to-rent ratio table: clean formatted table with buy price, monthly rent, yield, PTR ratio
- [x] Algarve excluded from yield display — too sparse; messaging updated to "long-term rentals are too rare in this region to reliably analyze"
- [x] Maia and Almada added to yield analysis (YIELD_CITIES set)
- [x] Renamed from `4_Rental_Yield.py` to `4_Investment_View.py` (final renumber to `3_` deferred to Phase 7c when City Comparison is absorbed into Explorer)

---

### Phase 10 — Neighbourhood detail pages

*Make neighbourhoods clickable to create a dedicated view with more granular analysis.*

**DESIGN SESSION REQUIRED:** Determine the visual layout and chart arrangement for neighbourhood detail pages before implementation. Decide on: hero section layout (photo positioning, text length), which metrics to lead with, chart order, mobile responsiveness.

#### Implementation
- [ ] Neighbourhood metadata storage:
  - [ ] JSON config file: `dashboard/static/neighbourhoods.json`
    ```json
    {
      "neighbourhoods": {
        "baixa_lisboa": {
          "display_name": "Baixa",
          "city": "lisboa",
          "intro_text": "Lisbon's historic heart...",
          "image_path": "neighbourhood_images/baixa.jpg"
        }
      }
    }
    ```
  - [ ] Images stored locally in `dashboard/static/neighbourhood_images/` — version controlled, no external dependencies
  - [ ] Load config once at app startup with `@st.cache_resource`
  - [ ] Fallback: show data-only view if metadata missing (graceful degradation)
- [ ] Neighbourhood detail page logic:
  - [ ] URL routing via `st.query_params` (e.g. `?neighbourhood=Baixa&city=lisboa`)
  - [ ] Single template renders all neighbourhoods — no per-neighbourhood page files needed
  - [ ] Render metadata (intro text + photo) from JSON config
  - [ ] Neighbourhood-specific metrics: median price, price per m², time on market, active listings, most common property type
  - [ ] Listing breakdown: property type distribution, bedroom mix, feature prevalence (parking, garden, etc.)
  - [ ] Historical trend: median price over available monthly history (allows price_change_pct comparison to city)
  - [ ] Filter all graphs by `neighbourhood = selected` only
- [ ] Make neighbourhoods clickable in existing choropleth/bar charts:
  - [ ] Plotly `clickData` event triggers `st.query_params` update
  - [ ] Alternative: custom buttons below chart ("View details")
- [ ] DB query: `get_neighbourhood_detail()` returns full historical time series for a single neighbourhood from `neighbourhood_monthly_summary` with buy/rent toggle
- [ ] Styling: inherit page design language (terracotta, same chart template)
- [ ] Demo video for Explorer landing page:
  - [ ] Screen record: choropleth map interaction, clicking into a neighbourhood, calculator inputs/outputs (20–30 seconds total)
  - [ ] Edit for pace and clarity (silent, no audio)
  - [ ] Upload to Streamlit (embedded via video URL or local asset)
  - [ ] Integrate into "What's inside" section on Explorer page

---

### Phase 8 — Price calculators

- [ ] Build buy price calculator (Page 2):
  - [ ] Inputs: neighbourhood, tipologia, area slider, jardim/garagem/terraco/varanda toggles, novo toggle — **all optional**
  - [ ] Unspecified features are marginalized out using `model_feature_stats` prevalences: expected price contribution = `β × mean`; variance contribution = `β² × p × (1−p)` per unspecified binary feature. More inputs specified → narrower interval.
  - [ ] Output: predicted price (exponentiate — model trained on log price), confidence interval; show interval widening visually as features are left blank
  - [ ] Short label: "Add more details to narrow the estimate"
  - [ ] Monthly trend: time series of predicted price for the same inputs across stored monthly coefficients
  - [ ] Show listing count that informed the estimate
  - [ ] Show % change vs prior month and vs 6 months ago
✓ **Rental yield calculator (Investment View, Page 3):**
  - [x] Restricted to rental-available cities (Lisboa, Porto, Setúbal, Cascais, Sintra, Maia, Gaia, Almada, Costa da Caparica, Caparica)
  - [x] Same optional inputs as buy calculator
  - [x] Output: estimated buy price | estimated monthly rent | gross yield %
  - [x] Caveat: "Gross yield only. Net yield will be lower after taxes, vacancy, maintenance."
  - [ ] **TODO**: Historical yield trend chart

---

### Phase 9 — Paywall, contact and launch prep

- [ ] **Add "What's inside" preview section** to Explorer page with demo video and Subscribe button
  - 20–30 second silent demo video showing choropleth map interaction, neighbourhood detail, and calculator
  - Brief descriptive text below video ("Explore neighbourhood-level analysis, historical trends, and interactive calculators...")
  - Prominent "Subscribe" button linking to LemonSqueezy
  - Informative tone, not salesy
  - **Note:** Video production (screen recording + editing) deferred to Phase 10 — implement page layout and CTA now, add video after design is finalized
- [ ] Add feedback nudge line at bottom of Explorer page ("Have a specific market or feature in mind? [Let us know](link)")
- [ ] Wire LemonSqueezy paywall gate on Pages 2 and 3 (soft gate — blur/placeholder with subscribe prompt)
- [ ] Build Contact page with Formspree form (name, email, subject dropdown, message)
- [ ] Work through pre-launch checks below
- [ ] Set up custom domain

---

## Recent Changes & Current Status (June 12–13, 2026)

### Price Estimator & Calculator Improvements
- **Confidence intervals narrowed to 50%** (was 80%, which was impractical)
  - Buy: €340k–€420k range (vs €295k–€837k before) — much more actionable
  - Rent: €1,200–€1,800 range (vs €1,000–€2,200 before) — now useful for decisions
  - Thresholds adjusted: High (±<25%), Medium (±25–40%), Low (±>40%)
- **Fixed "novo" (new build) feature** — was producing all zeros due to Portuguese text handling bug in impute_binary()
- **T5+ properties collapsed into single category** — solves sparse data overfitting (T5, T6, T7, T8, T9+ now grouped)
  - Prevents non-monotonic coefficients: was T4 > T5 < T6 (confusing for users)
  - Ridge regression now has enough observations per category to estimate properly
- **Removed neighbourhood browse bar chart** — map and price estimator are superior features

### Neighbourhood Deep-Dive Map Improvements
- **Lisbon map now shows all cities by default**
  - Before: Zoomed to Lisbon city only; Cascais/Sintra/Almada not visible without panning
  - After: Center shifted to lat=38.6956, lon=-9.2404, zoom=9 to fit all cities in viewport
- **Rent choropleth now uses raw price instead of EUR/m²**
  - More intuitive for non-experts ("what will I pay?" vs "quality ratio")
  - Avoids outlier scale compression (was 0–500 EUR/m², data clustered 17–50)
  - Buy choropleth still uses EUR/m² (better for comparing property value)
- **Added transparency message for rent data sparsity**
  - Explains why fewer neighbourhoods appear: "Rental listings are sparser across these cities"
  - Prevents users from thinking it's a bug or data quality issue

### Regional Organization
- **Folded Setúbal into Lisbon region** — was standalone region selector but had no data in database
  - Almada, Costa da Caparica, Caparica e Trafaria now appear under "Lisboa" selector
  - When scrapers populate these cities, they'll automatically appear without code changes

### Bug Fixes
- **Fixed duplicate intercept coefficients in regression models** — was causing €1 price estimates
  - Added deduplication in coefficient extraction from glmnet
  - Cleaned database: removed 15 duplicate zero-coefficient intercepts from 2026-06-12 models
- **Reduced model cache TTL from 1h to 5min** — ensures dashboard picks up model changes faster
- **Fixed rent message formatting** — clearer explanation of data sparsity vs market reality

### Daily/Monthly Aggregation Split
- **Dashboard now shows current data daily** — implemented dual-timeline aggregation strategy
- **Daily aggregation** (20:00 UTC): refreshes `city_latest_summary` and `neighbourhood_latest_summary` every night
- **Monthly archive** (1st of month): copies latest snapshot into `_monthly_` tables and computes month-over-month changes
- Dashboard queries updated to read from `_latest_` tables
- Monthly `_monthly_` archive tables retained for historical trend analysis
- Schema created, workflows configured; awaiting table creation in Neon console

### Scraper Expansion
- **Added Maia** to Porto scraper (both imovirtual and casa_sapo) — estimated ~500–2,000 listings
- **Added Almada** to Lisboa scraper (both imovirtual and casa_sapo) — estimated ~800–3,000 listings, growing market south of Tagus
- Both cities now appear on Explorer map with proper coordinates and climate data
- Integration complete: Maia shows in Porto region of Neighbourhood Deepdive, Almada as standalone region

### Data Quality & Neighbourhood Matching ✓ COMPLETE
- **Polygon matching fixed** — spatial join backfill completed with 98.9% match rate (35,508/35,947 listings)
- **Almada mapped correctly** — fixed imovirtual URL from "lisboa/almada" to "setubal/almada"
- Neighbourhood matching: 167 → 186 entries in `neighbourhood_lookup.json`; manual mapping completed
- Cascais: 1,311 imovirtual listings in database (scraped June 5)
- Sintra: 6 listings (sparse, below 10-listing aggregation threshold)

### Scraper Expansion — Setúbal Region Added
- **Extended south of Lisbon** — Added Almada, Costa da Caparica, and Caparica e Trafaria to both scrapers
- **imovirtual URLs fixed** — Corrected Almada to `setubal/almada`; added two new cities via proper URL structure
- **casasapo URLs verified** — All three cities working with direct city-level URLs
- **GeoJSON boundary file created** — Fetched `setubal.geojson` with 11 parishes from GADM
- **Dashboard fully integrated**:
  - New "Setúbal" region in Neighbourhood Deep-Dive (replacing standalone Almada)
  - Cities added to Explorer map with coordinates and climate data
  - Added to Investment View yield analysis

### UI/UX Improvements
- **Date format**: Fixed truncation ("Jun 2026" instead of "June 2026")
- **Table layout**: Removed unnecessary city-level listing breakdown in Explorer
- **City labels**: "Vila Nova de Gaia" → "Gaia"; added proper labels for new Setúbal cities
- **Hover tooltips**: 
  - Added price rounding (:.0f format) to all numeric displays
  - Removed listing count (was confusing)
  - Fixed neighbourhood name spacing ("LordeloDoOuro" → "Lordelo Do Ouro")
  - Improved overall styling for better readability
- **Algarve messaging**: Updated across Neighbourhood Deepdive and Investment View
  - Old: "...other platforms"
  - New: "...long-term rentals are too rare in this region to reliably analyze"
- **Algarve neighbourhood breakdown**: Added avg days on market and most common property type fields

### Implementation Roadmap (Remaining)

**Complete in this order** (Phase 8 → 10 → 7 finish → 9):

1. **Phase 8 (Price calculators) — Build first:**
   - [ ] Buy price calculator (Page 2: Neighbourhood Deep-Dive)
   - [ ] Rental yield calculator (Page 3: Investment View)
   
2. **Phase 10 (Neighbourhood detail pages) — Build:**
   - [ ] Dynamic neighbourhood detail pages with price history, property breakdown, metrics
   - [ ] Make neighbourhoods clickable in existing choro/bar charts
   - [ ] Screen record demo video

3. **Phase 7 finish (Visual sprint) — Polish after features complete:**
   - [ ] Market pulse: optional auto-generated summary (low priority — skip if time-constrained)
   - [ ] What's inside preview: build with real working screenshots + demo video of actual features

4. **Phase 9 (Paywall & launch) — Last:**
   - [ ] Wire paywall soft gates on Pages 2 & 3
   - [ ] Build Contact page
   - [ ] Set up LemonSqueezy flow

### Next Steps (Immediate)
1. Run monthly aggregation to capture Setúbal baseline and re-aggregate with corrected Almada
2. Monitor first Setúbal scraper runs to verify listing volumes and neighbourhood quality
3. **Then: Start Phase 7 finish work** (market pulse + what's inside)

---

## Pre-Launch Checklist

- [x] All data bugs fixed and verified
  - [x] Polygon matching: spatial join backfill completed at 98.9% match rate
  - [x] Almada URL fixed (was "lisboa/almada", now "setubal/almada")
  - [x] Neighbourhood matching: manual mapping completed; 99%+ match rate achieved
  - [x] Cascais/Sintra: 1,311 & 6 listings now scraped
- [ ] Summary tables populated and sanity checked
  - [ ] **PENDING**: Re-run monthly aggregation (workflow_dispatch) to capture Setúbal baseline and re-aggregate with corrected Almada
- [x] Regression models producing sensible outputs
  - [x] R² values good across all models
  - [x] Coefficients have correct signs and magnitudes
- [x] Currency toggle working on all price displays
- [x] Dashboard UI updated with new design language
  - [x] Date format fixed, table layout optimized, hovers improved
  - [x] Maia, Almada, and Setúbal cities fully integrated
- [x] Scraper expansion complete
  - [x] Setúbal region (Almada, Costa da Caparica, Caparica e Trafaria) added to both scrapers
  - [x] GeoJSON boundaries fetched for Setúbal region
  - [x] Dashboard updated with new region, coordinates, climate data
  - [x] All cities in CITY_LABELS with proper names
  - [x] No city names showing in lowercase
- [ ] Paywall gate working on Pages 2 and 3
- [ ] LemonSqueezy subscription flow tested end to end
- [ ] Contact form tested — messages arriving in Gmail
- [ ] "What's inside" preview section on Explorer page (ready to implement)
- [ ] Load time acceptable (target under 3 seconds for Explorer page)
- [ ] Custom domain set up (can do after first paying subscriber if preferred)
