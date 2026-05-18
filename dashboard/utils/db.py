import streamlit as st
import pandas as pd
from sqlalchemy import create_engine, text


@st.cache_resource
def get_engine():
    """
    Creates one database connection pool for the whole app lifetime.
    cache_resource means Streamlit only runs this once, not on every page load.
    pool_pre_ping silently reconnects if the connection has gone stale.
    """
    url = (
        f"postgresql+psycopg2://{st.secrets['NEON_USER']}:{st.secrets['NEON_PASSWORD']}"
        f"@{st.secrets['NEON_HOST']}/{st.secrets['NEON_DBNAME']}?sslmode=require"
    )
    return create_engine(url, pool_pre_ping=True)


@st.cache_data(ttl=3600)
def get_listings(type: str = "buy", city: str = None, platform: str = None) -> pd.DataFrame:
    """
    cache_data with ttl=3600 means results are cached for 1 hour.
    The dashboard won't hammer the DB on every user interaction.
    """
    table = "ads_buy" if type == "buy" else "ads_rent"
    conditions = ["is_active = 1"]
    params = {}
    if city:
        conditions.append("city = :city")
        params["city"] = city
    if platform:
        conditions.append("platform = :platform")
        params["platform"] = platform
    where = " AND ".join(conditions)
    sql = text(f"SELECT * FROM {table} WHERE {where}")
    with get_engine().connect() as conn:
        return pd.read_sql(sql, conn, params=params)


@st.cache_data(ttl=3600)
def get_price_history(type: str = "buy", city: str = None) -> pd.DataFrame:
    table = "price_changes_buy" if type == "buy" else "price_changes_rent"
    conditions = []
    params = {}
    if city:
        conditions.append("city = :city")
        params["city"] = city
    where = ("WHERE " + " AND ".join(conditions)) if conditions else ""
    sql = text(f"""
        SELECT date, city, AVG(new_price) AS avg_price, COUNT(*) AS n_changes
        FROM {table}
        {where}
        GROUP BY date, city
        ORDER BY date
    """)
    with get_engine().connect() as conn:
        return pd.read_sql(sql, conn, params=params)


@st.cache_data(ttl=86400)
def get_exchange_rates() -> dict:
    """Fetches EUR rates from the ECB free API. Cached for 24 hours."""
    import requests
    resp = requests.get(
        "https://data-api.ecb.europa.eu/service/data/EXR/D.USD+GBP+NOK+SEK+DKK.EUR.SP00.A"
        "?format=jsondata&lastNObservations=1",
        timeout=10
    )
    resp.raise_for_status()
    data = resp.json()
    series = data["dataSets"][0]["series"]
    keys   = data["structure"]["dimensions"]["series"]
    currencies = [c["values"] for c in keys if c["id"] == "CURRENCY"][0]
    rates = {}
    for i, series_data in enumerate(series.values()):
        obs = series_data["observations"]
        if obs:
            rate = list(obs.values())[0][0]
            rates[currencies[i]["id"]] = rate
    return rates
