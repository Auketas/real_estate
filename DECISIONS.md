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
  - Checkout: https://realestatept.lemonsqueezy.com/checkout
  - Tax category: SaaS — personal use
- **Pricing**: €19/month. Optional 6-month plan at a discount (second product in LemonSqueezy).
- **Access control**: `streamlit-authenticator==0.3.3` with per-user bcrypt credentials
- **MVP flow**: customer pays on LemonSqueezy → manually add credentials block to Streamlit secrets UI
- **Future**: automate user provisioning via LemonSqueezy webhook once volume justifies it

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

Streamlit Community Cloud free tier sleeps after inactivity — mitigated with a GitHub Actions
keep-alive workflow (`.github/workflows/keep_alive.yml`) that pings the app every 10 minutes.

## Data sources

### Live (already scraping)
- **Imovirtual** — listing prices, buy + rent, 8 cities. Live since before this session.
- **Casa Sapo** — listing prices, buy + rent, 8 cities. Scraper fixed and workflow added.

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

The pre-login screen doubles as a public landing page — product description, feature list,
pricing, and a Subscribe button linking to LemonSqueezy. No separate marketing site needed at MVP.

## Price calculator & regression models

Users can predict property prices based on features (size, neighbourhood, amenities) via hedonic regression models trained on current listing data.

**Two-stream approach:**
- **Live models** (snapshot_date): trained daily at 19:00 UTC, used for current-day predictions in the calculator
- **Monthly snapshots** (snapshot_month): archived on 1st of each month, immutable for historical comparison
- **Safety mechanism**: if monthly snapshot fails to create on scheduled date, the daily job retroactively creates it

**Model scope:**
- Train on all currently-active listings in a city/listing_type combination
- Minimum 50 listings required to train
- Outliers: remove listings > 3 SD from city mean log(price/m²), keep area-missing listings
- Features: area, new build (novo), garden, parking, terrace, balcony, floor (andar), property type (tipologia), energy efficiency (energia), neighbourhood
- Outputs: coefficient estimates, standard errors, R², residual std error, feature means (for marginalization)
- Algarve: skip rent models (data too sparse)

**Dashboard integration:**
- Neighbourhood Deep-dive: price calculator with monthly trend chart
- Investment View: rental yield calculator (buy + rent predictions)
- Both show optional "Price trend" expansion revealing how predicted prices evolved across months

## Immediate next steps
1. Test all dashboard pages end-to-end on the live URL
2. Add INE transaction data as first supplementary source
3. Automate LemonSqueezy webhook → user provisioning once paying subscribers arrive
