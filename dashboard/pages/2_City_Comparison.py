import streamlit as st
import plotly.express as px
from utils.auth import require_auth
from utils.db import get_city_summary

require_auth()

st.title("City Comparison")

type_filter = st.radio("", ["Buy", "Rent"], horizontal=True)
type_key = type_filter.lower()

df = get_city_summary(listing_type=type_key)

all_labels = sorted(df["city_label"].unique())
selected   = st.multiselect("Select cities to compare", all_labels, default=all_labels)
df = df[df["city_label"].isin(selected)]

# ---- Summary table
st.subheader("Summary")
summary = df[["city_label", "listing_count", "median_price", "median_price_per_m2",
              "p25_price", "p75_price", "avg_time_on_market_days"]].copy()
summary.columns = ["City", "Listings", "Median price (€)", "Median €/m²",
                   "P25 price (€)", "P75 price (€)", "Avg. days on market"]
st.dataframe(summary.sort_values("Median price (€)", ascending=False),
             use_container_width=True, hide_index=True)

# ---- Price per m²
st.subheader("Median price per m²")
fig = px.bar(
    df.sort_values("median_price_per_m2", ascending=False),
    x="city_label", y="median_price_per_m2",
    labels={"median_price_per_m2": "Median €/m²", "city_label": ""},
)
st.plotly_chart(fig, use_container_width=True)

# ---- Time on market
st.subheader("Average time on market (days)")
fig2 = px.bar(
    df.sort_values("avg_time_on_market_days", ascending=False),
    x="city_label", y="avg_time_on_market_days",
    labels={"avg_time_on_market_days": "Days", "city_label": ""},
)
st.plotly_chart(fig2, use_container_width=True)
