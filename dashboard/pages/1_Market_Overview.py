import streamlit as st
import pandas as pd
import plotly.express as px
from utils.auth import require_auth
from utils.db import get_listings, get_price_history

require_auth()

st.title("Market Overview")
st.caption("Active listings across all cities — updated every hour.")

type_filter = st.radio("", ["Buy", "Rent"], horizontal=True)
type_key = type_filter.lower()

df = get_listings(type=type_key)

# ---- Headline metrics -------------------------------------------------------
cities = sorted(df["city"].unique())
col1, col2, col3, col4 = st.columns(4)
col1.metric("Total active listings", f"{len(df):,}")
col2.metric("Median price (€)",      f"{df['price'].median():,.0f}")
col3.metric("Cities covered",        len(cities))
col4.metric("Avg. area (m²)",        f"{pd.to_numeric(df['area'], errors='coerce').median():.0f}")

st.divider()

# ---- Median price per city --------------------------------------------------
st.subheader("Median asking price by city")
city_summary = (
    df.groupby("city")["price"]
    .median()
    .reset_index()
    .sort_values("price", ascending=False)
)
fig = px.bar(city_summary, x="city", y="price", labels={"price": "Median price (€)", "city": ""})
st.plotly_chart(fig, use_container_width=True)

# ---- Price trend over time --------------------------------------------------
st.subheader("Price change activity over time")
history = get_price_history(type=type_key)
if not history.empty:
    fig2 = px.line(history, x="date", y="avg_price", color="city",
                   labels={"avg_price": "Avg. revised price (€)", "date": ""})
    st.plotly_chart(fig2, use_container_width=True)
else:
    st.info("No price change history yet.")
