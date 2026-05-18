# Product Decisions & Discussion Summary

## Product vision
A paid data dashboard for people considering buying real estate in or moving to Portugal.
Target: predominantly Northern European and American expats, with significant overlap between
lifestyle movers and buy-to-let investors. Both audiences are served by the same product —
the investor framing (yield data, price trends) provides the clearest value proposition and
willingness to pay, while the lifestyle content (cost of living, climate, expat tools) comes
for free alongside it.

## Monetization
- **Format**: live web dashboard (lower ongoing maintenance than monthly PDF reports)
- **Payments**: LemonSqueezy (handles EU VAT automatically, simpler than Stripe for solo products)
- **Access control**: `streamlit-authenticator` library with per-user bcrypt credentials
- **MVP flow**: customer pays on LemonSqueezy → manually add credentials to `config.yaml` → push
- **Future**: automate user provisioning via LemonSqueezy webhook once volume justifies it
- **Pricing**: not yet decided

## Tech stack decisions

| Layer | Choice | Why |
|---|---|---|
| Scraping | R + GitHub Actions | Already working, no reason to change |
| Database | Neon Postgres | Already in use, serverless, free tier sufficient |
| Dashboard | Python + Streamlit | Low code, easy deploy, native auth, Python is flexible |
| Hosting | Streamlit Community Cloud | Free tier, auto-deploys from GitHub, zero server management |
| Charts | Plotly | Interactive, good defaults, works well in Streamlit |

Streamlit was chosen over Metabase (not polished enough for a paid product) and a custom
React app (too much build time for an MVP).

## Data sources

### Live (already scraping)
- **Imovirtual** — listing prices, buy + rent, 8 cities. Live since before this session.
- **Casa Sapo** — listing prices, buy + rent, 8 cities. Scraper fixed and workflow added this session.

### Planned additions (all free, infrequently updated)
- **INE** (Statistics Portugal) — transaction prices, housing price index. Official, has API.
- **ECB** — EUR exchange rates (USD, GBP, NOK, SEK, DKK). Free API, already wired into dashboard.
- **DGPJ** — crime statistics by region. Annual release, downloadable.
- **IPMA** — climate data by region (sunshine hours, temperatures). Relevant for Northern European audience.
- **Banco de Portugal** — mortgage rates. Monthly.
- **Numbeo** — cost of living by city. Crowdsourced, very popular with expats. Freemium API.
- **OpenStreetMap / Overpass API** — amenities density (schools, hospitals, supermarkets). Free.

### Difficult / blocked
- **Idealista** — scraper written but Cloudflare blocks it. No workflow. Parked for now.
- **Confidencial Imobiliário** — most detailed transaction database in Portugal, but commercial.
- **Portal das Finanças** — actual IMT transaction records. Public access very limited.

## Key analytical angles
These are derived from existing data without new sources:

- **Gross rental yield** — (annual rent / buy price) × 100, by city and apartment type.
  Already computed in the Rental Yield dashboard page.
- **Days on market** — first_seen to last_seen for inactive listings. Indicator of market heat.
- **Price reduction rate** — % of listings that get reduced, and by how much.
- **Listing vs transaction price gap** — once INE transaction data is added.
- **Currency-adjusted prices** — EUR prices shown in USD/GBP/NOK automatically.

## Geography
8 cities currently scraped: porto, lisboa, albufeira, loule, portimao, lagos, lagoa, faro.
These cover the three main regions: Porto, Lisbon, Algarve.
Dashboard supports three levels: national overview → city comparison → neighbourhood deep-dive.

## Dashboard pages (current)
1. **Market Overview** — headline metrics and price trends across all cities
2. **City Comparison** — side-by-side price distributions and per-m² comparisons
3. **Neighbourhood Deep-dive** — single city breakdown with map
4. **Rental Yield** — gross yield by city and apartment type, investor-focused
5. **Expat Tools** — live exchange rates, cost of living reference, climate snapshot

## What was done this session
- Fixed all bugs in `casasapo_scraper.R` (syntax error, wrong variable names, wrong function calls, missing libraries, platform filter in DB queries)
- Created `run_casasapo_scraper.R` runner script
- Created `.github/workflows/casasapo.yml` scheduled 12 hours after imovirtual
- Built full Streamlit dashboard scaffold (5 pages, auth, DB layer, ECB exchange rates)
- All changes pushed to GitHub

## Immediate next steps
1. Install 64-bit Python 3.11+ locally (current install is 32-bit 3.8, incompatible with pyarrow)
2. Test dashboard locally against real Neon data
3. Deploy to Streamlit Community Cloud
4. Decide on pricing and set up LemonSqueezy product page
5. Add INE transaction data as first supplementary source
6. Consider a Claude scheduled routine to monitor scraper health across both scrapers
