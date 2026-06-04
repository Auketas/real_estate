import streamlit as st
import plotly.express as px
from utils.auth import require_auth
from utils.db import get_city_summary

require_auth()

st.title("Market Overview")
st.caption("Pre-aggregated city-level stats from the latest monthly snapshot.")

type_filter = st.radio("", ["Buy", "Rent"], horizontal=True)
type_key = type_filter.lower()

df = get_city_summary(listing_type=type_key)

# ---- Headline metrics
col1, col2, col3, col4 = st.columns(4)
col1.metric("Total active listings",   f"{df['listing_count'].sum():,}")
col2.metric("Median price (€)",        f"{df['median_price'].median():,.0f}")
col3.metric("Cities covered",          df["city"].nunique())
col4.metric("Avg. time on market",     f"{df['avg_time_on_market_days'].mean():.0f} days")

st.divider()

# ---- Median price by city
st.subheader("Median asking price by city")
fig = px.bar(
    df.sort_values("median_price", ascending=False),
    x="city_label", y="median_price",
    labels={"median_price": "Median price (€)", "city_label": ""},
)
st.plotly_chart(fig, use_container_width=True)

# ---- Median price per m²
st.subheader("Median price per m²")
fig2 = px.bar(
    df.sort_values("median_price_per_m2", ascending=False),
    x="city_label", y="median_price_per_m2",
    labels={"median_price_per_m2": "Median €/m²", "city_label": ""},
)
st.plotly_chart(fig2, use_container_width=True)
