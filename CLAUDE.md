# Real Estate Data Product — Claude Code Context

## What this project is
A Portugal real estate data pipeline and paid dashboard targeting expats and buy-to-let investors (primarily Northern European and American). Scrapers collect listing data daily, store it in a shared Postgres database, and a Streamlit dashboard presents analysis to paying subscribers.

## Repo structure
```
.github/workflows/
  r.yml               — imovirtual scraper, runs daily 06:00 UTC
  casasapo.yml        — casa sapo scraper, runs daily 18:00 UTC (12h gap to avoid DB conflicts)
  keep_alive.yml      — pings Streamlit app every 10 min to prevent sleep
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

## Dashboard
- **Live at:** https://real-estate-pt.streamlit.app/
- **Landing page:** `app.py` pre-login screen is public-facing — shows product description, features, €19/month price, and a Subscribe button. No separate marketing site.
- **Auth:** `streamlit-authenticator==0.3.3` — credentials stored in Streamlit Community Cloud secrets UI (not in repo). `auth.py` falls back to secrets when `config.yaml` is absent (cloud), reads file when present (local dev).
- **Adding a subscriber:** generate a bcrypt hash locally, add a `[credentials.usernames.name]` block in the Streamlit secrets UI. No code change needed.
- **Payments:** LemonSqueezy at https://realestatept.lemonsqueezy.com/checkout — €19/month. Manually add users via secrets UI at MVP stage.
- **Local dev:** use 64-bit Python 3.10 venv (`dashboard/.venv`), activate with `.\.venv\Scripts\Activate.ps1`, run `streamlit run app.py` from `dashboard/`.

## What's next
1. Test all dashboard pages end-to-end on the live URL
2. Add supplementary data sources (see DECISIONS.md for the list)
3. Set up LemonSqueezy product and payment flow

## Gotchas
- `read_ads()` in both scrapers filters by `platform` — do not remove this or scrapers will mark each other's listings inactive
- `inactive_ids` is derived from `setdiff(db_ads$id, current_ads$id)` — `read_ads` only returns `id` and `price`, no `is_active` column
- Scraper log uses `platform = "casa_sapo"` (underscore) — keep consistent
- Dashboard `config.yaml` and `secrets.toml` are gitignored — do not commit them
- Streamlit Community Cloud: set secrets via the web UI, not in the repo
- `streamlit-authenticator` is pinned to `==0.3.3` — newer versions have a breaking Hasher API change
