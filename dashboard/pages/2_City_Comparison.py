import streamlit as st
import plotly.express as px
import pandas as pd
from utils.auth import require_auth
from utils.db import get_listings

require_auth()

st.title("City Comparison")

type_filter = st.radio("", ["Buy", "Rent"], horizontal=True)
type_key = type_filter.lower()

df = get_listings(type=type_key)
df["area_m2"] = pd.to_numeric(df["area"], errors="coerce")
df["price_per_m2"] = df["price"] / df["area_m2"]

cities = sorted(df["city"].unique())
selected = st.multiselect("Select cities to compare", cities, default=cities)
df = df[df["city"].isin(selected)]

# ---- Price distribution -----------------------------------------------------
st.subheader("Price distribution")
fig = px.box(df, x="city", y="price", points=False,
             labels={"price": "Asking price (€)", "city": ""})
fig.update_yaxes(range=[0, df["price"].quantile(0.95)])  # clip extreme outliers
st.plotly_chart(fig, use_container_width=True)

# ---- Price per m² -----------------------------------------------------------
st.subheader("Price per m²")
fig2 = px.box(df, x="city", y="price_per_m2", points=False,
              labels={"price_per_m2": "€ per m²", "city": ""})
fig2.update_yaxes(range=[0, df["price_per_m2"].quantile(0.95)])
st.plotly_chart(fig2, use_container_width=True)

# ---- Summary table ----------------------------------------------------------
st.subheader("Summary")
summary = df.groupby("city").agg(
    listings    = ("price", "count"),
    median_price= ("price", "median"),
    median_m2   = ("price_per_m2", "median"),
    median_area = ("area_m2", "median"),
).reset_index().sort_values("median_price", ascending=False)
summary.columns = ["City", "Listings", "Median price (€)", "Median €/m²", "Median area (m²)"]
st.dataframe(summary, use_container_width=True, hide_index=True)
