# Real Estate Data Product — Claude Code Context

## What this project is
A Portugal real estate data pipeline and paid dashboard targeting expats and buy-to-let investors (primarily Northern European and American). Scrapers collect listing data daily, store it in a shared Postgres database, and a Streamlit dashboard presents analysis to paying subscribers.

## Repo structure
```
.github/workflows/
  r.yml               — imovirtual scraper, runs daily 06:00 UTC
  casasapo.yml        — casa sapo scraper, runs daily 18:00 UTC (12h gap to avoid DB conflicts)
code/scrape/
  imovirtual_scraper.R     — live and working
  run_imovirtual_scraper.R — entry point called by workflow
  casasapo_scraper.R       — fixed and ready, workflow created
  run_casasapo_scraper.R   — entry point called by workflow
  idealista_scraper.R      — scraper exists but no DB integration, no workflow (Cloudflare blocks it)
dashboard/
  app.py                   — Streamlit entry point + login screen
  config.yaml              — bcrypt user credentials (gitignored, never commit)
  requirements.txt
  .streamlit/secrets.toml  — DB credentials for local dev (gitignored, never commit)
  utils/db.py              — all DB queries with st.cache_data caching
  utils/auth.py            — streamlit-authenticator login, shared across pages
  pages/
    1_Market_Overview.py
    2_City_Comparison.py
    3_Neighbourhood_Deepdive.py
    4_Rental_Yield.py       — gross yield = (annual rent / buy price) × 100
    5_Expat_Tools.py        — ECB exchange rates, cost of living, climate
log/
  scraper_log.csv          — appended after every scraper run (committed by Actions bot)
```

## Database (Neon Postgres)
Credentials via env vars: `NEON_DBNAME`, `NEON_HOST`, `NEON_USER`, `NEON_PASSWORD`

Tables: `ads_buy`, `ads_rent`, `price_changes_buy`, `price_changes_rent`

Key columns: `id, price, area, tipologia, andar, anunciante, tipo, novo, jardim, energia, elevador, garagem, terraco, varanda, lat, lon, neighbourhood, city, platform, is_active, first_seen, last_seen`

Both scrapers write to the same tables, separated by `platform` (`'imovirtual'` or `'casa_sapo'`). **Always filter by platform** in queries to avoid cross-contamination.

Cities scraped: porto, lisboa, albufeira, loule, portimao, lagos, lagoa, faro

## GitHub Actions secrets needed
`NEON_DBNAME`, `NEON_HOST`, `NEON_USER`, `NEON_PASSWORD` — already configured for imovirtual, casasapo.yml uses the same ones.

## Dashboard status
- Code is written and pushed, not yet deployed or tested locally
- Blocked on local test: user's Python is 32-bit 3.8; needs 64-bit Python 3.11+ installed
- Deployment target: Streamlit Community Cloud (share.streamlit.io)
- Auth: `streamlit-authenticator` with `config.yaml`
- Payments: plan to use LemonSqueezy; manually add users to `config.yaml` at MVP stage

## What's next
1. User installs 64-bit Python 3.11+, runs `pip install -r dashboard/requirements.txt`
2. Fill in `dashboard/.streamlit/secrets.toml` with Neon credentials
3. Generate a bcrypt hash and add a user to `dashboard/config.yaml` (see SETUP.md)
4. Run `streamlit run dashboard/app.py` and verify all pages render
5. Deploy to Streamlit Community Cloud
6. Add supplementary data sources (see DECISIONS.md for the list)
7. Set up LemonSqueezy product and payment flow

## Gotchas
- `read_ads()` in both scrapers filters by `platform` — do not remove this or scrapers will mark each other's listings inactive
- `inactive_ids` is derived from `setdiff(db_ads$id, current_ads$id)` — `read_ads` only returns `id` and `price`, no `is_active` column
- Scraper log uses `platform = "casa_sapo"` (underscore) — keep consistent
- Dashboard `config.yaml` and `secrets.toml` are gitignored — do not commit them
- Streamlit Community Cloud: set secrets via the web UI, not in the repo
