import streamlit as st
import plotly.express as px
import utils.charts  # registers the Portugal Plotly template
from utils.auth import require_auth
from utils.db import get_city_summary
from utils.sidebar import render_currency_selector

require_auth()

rate, symbol, fmt_price = render_currency_selector()

st.title("Market Overview")
st.caption("Pre-aggregated city-level stats from the latest monthly snapshot.")

type_filter = st.radio("", ["Buy", "Rent"], horizontal=True)
type_key = type_filter.lower()

df = get_city_summary(listing_type=type_key)
df["price_display"]   = df["median_price"]      * rate
df["ppm2_display"]    = df["median_price_per_m2"] * rate

# ---- Headline metrics
col1, col2, col3, col4 = st.columns(4)
col1.metric("Total active listings",  f"{df['listing_count'].sum():,}")
col2.metric(f"Median price ({symbol})", fmt_price(df["median_price"].median()))
col3.metric("Cities covered",          df["city"].nunique())
col4.metric("Avg. time on market",     f"{df['avg_time_on_market_days'].mean():.0f} days")

st.divider()

# ---- Median price by city
st.subheader("Median asking price by city")
fig = px.bar(
    df.sort_values("price_display", ascending=False),
    x="city_label", y="price_display",
    labels={"price_display": f"Median price ({symbol})", "city_label": ""},
)
st.plotly_chart(fig, use_container_width=True)

# ---- Median price per m²
st.subheader("Median price per m²")
fig2 = px.bar(
    df.sort_values("ppm2_display", ascending=False),
    x="city_label", y="ppm2_display",
    labels={"ppm2_display": f"Median {symbol}/m²", "city_label": ""},
)
st.plotly_chart(fig2, use_container_width=True)
