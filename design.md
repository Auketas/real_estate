# Portugal Real Estate Dashboard — Design & Architecture Brief

## Overview

This document defines the agreed design direction, page structure, visual language, and database architecture for the Portugal real estate dashboard. It is intended as a brief for Claude Code to implement.

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
A simple two-column section with a heading like "Go deeper with a subscription." Two cards side by side:

- **Neighbourhood Deep-Dive** — short description (e.g. "Explore price per m², time on market, and monthly trends at neighbourhood level across Lisbon, Porto and the Algarve. Includes an interactive map.") with a static screenshot of the neighbourhood choropleth map.
- **Investment View** — short description (e.g. "Gross yield estimates and a rental yield calculator by property type and neighbourhood. Built for buyers evaluating rental potential.") with a static screenshot of the yield chart.

Below the two cards: a prominent Subscribe button linking to LemonSqueezy.

This section should feel like a natural part of the page, not an intrusive upsell. Keep the tone informative rather than salesy.

**Section 6: Feedback nudge**
A single subtle line near the bottom of the page, below the preview section. Suggested copy: "Have a specific market or feature in mind? We're actively expanding — [let us know](link to Contact page)." Style as secondary text, small font, not a banner or alert box. The goal is to invite input without signalling incompleteness.

---

### Page 2 — Neighbourhood Deep-Dive (paid)

City-level analysis with neighbourhood granularity.

**Controls (sidebar or top of page):**
- City selector (Lisboa / Porto / Algarve)
- Buy / Rent toggle

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
- Size range in m² (slider)
- Key features (multiselect: parking, garden, terrace, pool, new build)

Outputs:
- Predicted price with confidence interval, displayed as a prominent metric with range shown below (e.g. "€385,000 — estimated range €340,000–€430,000")
- "Up X% from last month" / "Down X% over 6 months" as delta indicators
- Small bar or line chart showing predicted price for these specifications over available monthly history
- Number of listings that informed the estimate, shown as a small caption for transparency

Implementation note: predictions are served from a precomputed monthly coefficients table in Neon (see Database section). Apply coefficients to user inputs at query time. Do not run regression live.

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
Same input structure as price calculator on Page 2, but outputs:
- Estimated buy price for specifications
- Estimated monthly rent for same specifications
- Gross yield percentage
- "Purchase prices for this type are up/down X% over 6 months"

Same coefficient table architecture as buy price calculator — run a parallel regression on rental listings monthly and store coefficients separately.

Honest caveat displayed prominently: "These are gross yield estimates based on asking prices and asking rents. Net yield will be lower after taxes, vacancy, maintenance and management costs."

---

## Database Architecture (Neon / PostgreSQL)

### Principle

Pre-aggregate everything. The dashboard should never run heavy calculations at query time. All expensive computations happen in scheduled jobs and results are stored in summary tables.

### New tables to create

**`city_monthly_summary`**
Pre-aggregated city-level stats, refreshed monthly.
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
Pre-aggregated neighbourhood-level stats, refreshed monthly.
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

**Monthly aggregation job:** Schedule a job to run on the 1st of each month that:
1. Populates `city_monthly_summary` and `neighbourhood_monthly_summary` from active listings
2. Runs the hedonic regression models (buy and rent, per city) and writes coefficients to `model_coefficients` and `model_metadata`
3. Runs sanity checks (flag any median price per m² outside a plausible range, any yield above 20%, any listing count changes >50% month-on-month)

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
- **Lisboa region:** Lisboa (add Cascais and Sintra to scraper)
- **Porto region:** Porto, Vila Nova de Gaia (consider adding Matosinhos)
- **Algarve:** Six largest cities already covered

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

---

### Phase 2 — Monthly aggregation script (R)

*Populate `city_monthly_summary` and `neighbourhood_monthly_summary`. Run manually once after building to get initial data.*

- [ ] Connect to Neon from R using `RPostgres` or `DBI`
- [ ] Query `ads_buy` and `ads_rent` excluding rows where `duplicate_flag = true` or `is_active = false`
- [ ] Compute city-level aggregations per listing type:
  - `listing_count`, `median_price`, `median_price_per_m2`, `avg_time_on_market_days` (from `last_seen - first_seen`), `p25_price`, `p75_price`
- [ ] Compute neighbourhood-level aggregations per city per listing type:
  - Same metrics plus `monthly_price_change_pct` (compare to prior month's row in same table) and `most_common_tipologia`
- [ ] Run sanity checks before inserting — abort and log if any check fails:
  - Median price per m² outside €500–€20,000
  - Month-on-month listing count change above 50%
  - Price change above 20% or below -20%
- [ ] Insert into summary tables using `snapshot_month = first day of current month`
- [ ] Use `ON CONFLICT DO UPDATE` to safely rerun without duplicating
- [ ] Run manually once to populate tables with current data

---

### Phase 3 — Regression model script (R)

*Populate `model_coefficients` and `model_metadata`. Run manually once after building.*

- [ ] For each city and each listing type (buy / rent), train a hedonic OLS regression:
  - Dependent variable: `log(price)` (log transform improves fit for property prices)
  - Independent variables: `area`, `tipologia` (dummies), `neighbourhood` (dummies), `novo`, `jardim`, `garagem`, `terraco`, `varanda`, `energia` (dummies where not null), `andar` (where not null)
  - Exclude rows where `duplicate_flag = true` or `is_active = false`
  - Exclude outliers beyond 3 standard deviations from city-level mean price per m²
- [ ] Store each coefficient and its std error as a row in `model_coefficients`
- [ ] Store model-level stats (n, R², residual std error) in `model_metadata`
- [ ] Use `ON CONFLICT DO UPDATE` to safely rerun
- [ ] Run manually once to populate with current data
- [ ] Verify outputs look sensible — coefficients should have intuitive signs (larger area = higher price, garagem positive, etc.)

---

### Phase 4 — GitHub Actions workflow

*Automate Phases 2 and 3 to run monthly.*

- [ ] Create new workflow YAML in `.github/workflows/`
- [ ] Schedule: `cron: '0 6 1 * *'` (6am on 1st of each month)
- [ ] Add `workflow_dispatch` trigger for manual runs
- [ ] Workflow steps: checkout repo → set up R → install packages → run aggregation script → run regression script
- [ ] Add Neon connection string as GitHub Actions secret if not already present
- [ ] Test by triggering manually via `workflow_dispatch`
- [ ] Verify tables are populated correctly after test run

---

### Phase 5 — Verify and sanity check

*Do not proceed to the dashboard until this phase is complete.*

- [ ] Compare `city_monthly_summary` output against direct queries on `ads_buy`/`ads_rent` — do medians match?
- [ ] Check all cities present and correctly capitalised
- [ ] Check no NULL values in critical columns
- [ ] Check price per m² values are in plausible range for all cities
- [ ] Check regression coefficients have sensible signs and magnitudes
- [ ] Check `model_metadata` R² values — below 0.3 suggests a data problem worth investigating before building the calculator

---

### Phase 6 — Dashboard migration

*Migrate existing dashboard queries to use summary tables. Fix known data bugs.*

- [ ] Migrate all dashboard queries from `ads_buy`/`ads_rent` to `city_monthly_summary` and `neighbourhood_monthly_summary`
- [ ] Verify load time improvement
- [ ] Fix rental yield calculation bug (Porto showing 1000%+)
- [ ] Fix price per m² calculation bug (Lagos showing astronomical values)
- [ ] Fix price change over time chart (values in 10^15 range)
- [ ] Properly capitalise all city names throughout

---

### Phase 7 — Visual sprint

*Apply the new design language. See Visual Design Language section for colours, typography and chart style.*

- [ ] Apply warm colour theme throughout (see palette in Visual Design Language section)
- [ ] Remove standalone Expat Tools page
- [ ] Add currency toggle to sidebar (EUR / GBP / USD / NOK / SEK / DKK) — apply globally to all price displays
- [ ] Rebuild Explorer page (Page 1):
  - [ ] Hero metrics row (4 stat cards)
  - [ ] Country choropleth map coloured by median price per m² — cream → terracotta scale, hover shows city stats
  - [ ] City comparison summary table including climate columns
  - [ ] Market pulse auto-generated text (template-driven)
- [ ] Rebuild Neighbourhood Deep-Dive page (Page 2):
  - [ ] Replace dot map with neighbourhood choropleth — hover shows neighbourhood stats
- [ ] Rebuild Investment View page (Page 3):
  - [ ] Fix and clean gross yield by city chart
  - [ ] Add price-to-rent ratio table

---

### Phase 8 — Price calculators

- [ ] Build buy price calculator (Page 2):
  - [ ] Inputs: neighbourhood, tipologia, area slider, jardim/garagem/terraco/varanda checkboxes, novo toggle
  - [ ] Output: predicted price (remember to exponentiate — model is trained on log price), confidence interval from residual std error
  - [ ] Monthly trend: time series of predicted price for same inputs across stored monthly coefficients
  - [ ] Show listing count that informed the estimate
  - [ ] Show % change vs prior month and vs 6 months ago
- [ ] Build rental yield calculator (Page 3):
  - [ ] Same inputs as buy calculator
  - [ ] Output: estimated buy price, estimated monthly rent, gross yield %
  - [ ] Show trend in yield over available monthly history
  - [ ] Display caveat: gross yield only, net yield will be lower

---

### Phase 9 — Paywall, contact and launch prep

- [ ] Add "What's inside" preview section to Explorer page with static screenshots and Subscribe button
- [ ] Add feedback nudge line at bottom of Explorer page
- [ ] Wire LemonSqueezy paywall gate on Pages 2 and 3 (soft gate — blur/placeholder with subscribe prompt)
- [ ] Build Contact page with Formspree form (name, email, subject dropdown, message)
- [ ] Work through pre-launch checks below
- [ ] Set up custom domain

---

## Pre-Launch Checklist

- [ ] All data bugs fixed and verified
- [ ] Summary tables populated and sanity checked
- [ ] Regression models producing sensible outputs
- [ ] Currency toggle working on all price displays
- [ ] Paywall gate working on Pages 2 and 3
- [ ] LemonSqueezy subscription flow tested end to end
- [ ] Contact form tested — messages arriving in Gmail
- [ ] Cascais and Sintra scrapers live and producing data
- [ ] No city names showing in lowercase
- [ ] Load time acceptable (target under 3 seconds for Explorer page)
- [ ] Custom domain set up (can do after first paying subscriber if preferred)
