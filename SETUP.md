# Setup & Deployment Instructions

## Prerequisites
- Git and GitHub account (already set up)
- Neon Postgres database (already set up)
- **64-bit Python 3.11 or 3.12** — required for the dashboard
  - Download from https://www.python.org/downloads/
  - Choose "Windows installer (64-bit)"
  - Tick "Add Python to PATH" during install
  - Your existing 32-bit Python 3.8 (used by nothing critical) is unaffected

---

## Running the dashboard locally

**1. Install dependencies**
```
pip install -r dashboard/requirements.txt
```

**2. Fill in database credentials**

Edit `dashboard/.streamlit/secrets.toml` (this file is gitignored — never commit it):
```toml
NEON_DBNAME  = "your_dbname"
NEON_HOST    = "your_host.neon.tech"
NEON_USER    = "your_user"
NEON_PASSWORD = "your_password"
```

**3. Create a login user**

Generate a bcrypt hash for your password:
```python
python -c "import streamlit_authenticator as sa; print(sa.Hasher(['your_password']).generate())"
```

Edit `dashboard/config.yaml` (also gitignored):
```yaml
credentials:
  usernames:
    your_username:
      email: you@example.com
      name: Your Name
      password: "$2b$12$<paste hash here>"
cookie:
  expiry_days: 30
  key: "any-long-random-string"
  name: "portugal_re_auth"
```

**4. Run**
```
streamlit run dashboard/app.py
```
Opens automatically at http://localhost:8501

---

## Deploying to Streamlit Community Cloud

1. Go to https://share.streamlit.io and sign in with GitHub
2. Click **New app**
3. Select repository `Auketas/real_estate`, branch `main`, main file `dashboard/app.py`
4. Under **Advanced settings → Secrets**, paste your Neon credentials in TOML format:
   ```toml
   NEON_DBNAME   = "..."
   NEON_HOST     = "..."
   NEON_USER     = "..."
   NEON_PASSWORD = "..."
   ```
5. Click **Deploy** — takes ~2 minutes

Every push to `main` triggers an automatic redeploy.

**Custom domain** (optional): upgrade to a paid Streamlit Cloud plan, or host on Railway/Render (~€5/month) for a custom domain from day one.

---

## Adding a paying user

1. Generate a bcrypt hash for their password (see step 3 above)
2. Add an entry to `dashboard/config.yaml`:
   ```yaml
   new_username:
     email: customer@example.com
     name: Customer Name
     password: "$2b$12$<hash>"
   ```
3. `git push` — Streamlit Cloud redeploys automatically, user can log in immediately

---

## Payment setup (LemonSqueezy)

1. Create an account at https://lemonsqueezy.com
2. Create a product (e.g. monthly subscription at €X/month)
3. When a customer pays, you receive an email notification
4. Follow the "Adding a paying user" steps above to grant access
5. To automate this later: wire up a LemonSqueezy webhook to a small script that adds the user to `config.yaml` and pushes — but manual is fine at MVP scale

---

## GitHub Actions scraper schedules

| Scraper     | Schedule       | File                          |
|-------------|---------------|-------------------------------|
| Imovirtual  | 06:00 UTC daily | `.github/workflows/r.yml`    |
| Casa Sapo   | 18:00 UTC daily | `.github/workflows/casasapo.yml` |

Both can also be triggered manually via the **Actions** tab → **Run workflow**.

Scraper runs append a row to `log/scraper_log.csv` and auto-commit it.
