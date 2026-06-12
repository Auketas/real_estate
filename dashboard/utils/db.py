import streamlit as st
import pandas as pd
from sqlalchemy import create_engine, text

CITY_LABELS = {
    "lisboa":            "Lisboa",
    "porto":             "Porto",
    "cascais":           "Cascais",
    "sintra":            "Sintra",
    "almada":            "Almada",
    "costa-da-caparica": "Costa da Caparica",
    "caparica-e-trafaria": "Caparica",
    "maia":              "Maia",
    "albufeira":         "Albufeira",
    "faro":              "Faro",
    "lagoa":             "Lagoa",
    "lagos":             "Lagos",
    "loule":             "Loulé",
    "portimao":          "Portimão",
    "vila-nova-de-gaia": "Gaia",
}


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


@st.cache_data(ttl=3600)
def get_city_summary(listing_type: str = None) -> pd.DataFrame:
    conditions = []
    params = {}
    if listing_type:
        conditions.append("listing_type = :listing_type")
        params["listing_type"] = listing_type
    where = ("WHERE " + " AND ".join(conditions)) if conditions else ""
    sql = text(f"SELECT * FROM city_latest_summary {where}")
    with get_engine().connect() as conn:
        df = pd.read_sql(sql, conn, params=params)
    df["city_label"] = df["city"].map(CITY_LABELS).fillna(df["city"].str.title())
    return df


@st.cache_data(ttl=3600)
def get_region_neighbourhood_summary(cities: tuple, listing_type: str = None) -> pd.DataFrame:
    """Load neighbourhood summary for multiple cities. Pass cities as a tuple."""
    city_params = {f"c{i}": city for i, city in enumerate(cities)}
    placeholders = ", ".join(f":c{i}" for i in range(len(cities)))
    conditions = [f"city IN ({placeholders})"]
    params = dict(city_params)
    if listing_type:
        conditions.append("listing_type = :listing_type")
        params["listing_type"] = listing_type
    sql = text("SELECT * FROM neighbourhood_latest_summary WHERE " + " AND ".join(conditions))
    with get_engine().connect() as conn:
        return pd.read_sql(sql, conn, params=params)


@st.cache_data(ttl=3600)
def get_neighbourhood_summary(city: str, listing_type: str = None) -> pd.DataFrame:
    conditions = ["city = :city"]
    params = {"city": city}
    if listing_type:
        conditions.append("listing_type = :listing_type")
        params["listing_type"] = listing_type
    sql = text("SELECT * FROM neighbourhood_latest_summary WHERE " + " AND ".join(conditions))
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


@st.cache_data(ttl=3600)
def get_model_coefficients(city: str, listing_type: str, snapshot_month: str) -> pd.DataFrame:
    """
    Fetch regression coefficients for a city, listing type, and month.
    Returns: id, snapshot_month, listing_type, city, variable_name, coefficient, std_error
    """
    sql = text("""
        SELECT * FROM model_coefficients
        WHERE city = :city AND listing_type = :listing_type AND snapshot_month = :snapshot_month
    """)
    params = {"city": city, "listing_type": listing_type, "snapshot_month": snapshot_month}
    with get_engine().connect() as conn:
        return pd.read_sql(sql, conn, params=params)


@st.cache_data(ttl=3600)
def get_model_metadata(city: str, listing_type: str, snapshot_month: str) -> pd.DataFrame:
    """
    Fetch model-level metadata (n_observations, r_squared, residual_std_error).
    Returns: id, snapshot_month, listing_type, city, n_observations, r_squared, residual_std_error
    """
    sql = text("""
        SELECT * FROM model_metadata
        WHERE city = :city AND listing_type = :listing_type AND snapshot_month = :snapshot_month
    """)
    params = {"city": city, "listing_type": listing_type, "snapshot_month": snapshot_month}
    with get_engine().connect() as conn:
        return pd.read_sql(sql, conn, params=params)


@st.cache_data(ttl=3600)
def get_model_feature_stats(city: str, listing_type: str, snapshot_month: str) -> pd.DataFrame:
    """
    Fetch feature means/prevalences for marginalization of unspecified inputs.
    Returns: id, snapshot_month, listing_type, city, variable_name, feature_mean
    """
    sql = text("""
        SELECT * FROM model_feature_stats
        WHERE city = :city AND listing_type = :listing_type AND snapshot_month = :snapshot_month
    """)
    params = {"city": city, "listing_type": listing_type, "snapshot_month": snapshot_month}
    with get_engine().connect() as conn:
        return pd.read_sql(sql, conn, params=params)


@st.cache_data(ttl=60)
def get_latest_live_model_date(listing_type: str) -> str:
    """
    Fetch the most recent snapshot_date (live model) for a listing type.
    Returns date string in ISO format (YYYY-MM-DD), or None if no models exist.
    """
    sql = text("""
        SELECT MAX(snapshot_date) AS latest_date FROM model_metadata
        WHERE listing_type = :listing_type AND snapshot_date IS NOT NULL
    """)
    params = {"listing_type": listing_type}
    with get_engine().connect() as conn:
        df = pd.read_sql(sql, conn, params=params)
    if df.empty or df["latest_date"].iloc[0] is None:
        return None
    return str(df["latest_date"].iloc[0])


@st.cache_data(ttl=3600)
def get_available_snapshot_months(listing_type: str) -> list:
    """
    Fetch distinct snapshot months from model_metadata, sorted descending (most recent first).
    Returns list of date strings in ISO format (YYYY-MM-DD).
    """
    sql = text("""
        SELECT DISTINCT snapshot_month FROM model_metadata
        WHERE listing_type = :listing_type AND snapshot_month IS NOT NULL
        ORDER BY snapshot_month DESC
    """)
    params = {"listing_type": listing_type}
    with get_engine().connect() as conn:
        df = pd.read_sql(sql, conn, params=params)
    return df["snapshot_month"].astype(str).tolist() if not df.empty else []
