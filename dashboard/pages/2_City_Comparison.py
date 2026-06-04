import streamlit as st
import plotly.express as px
import utils.charts
from utils.auth import require_auth
from utils.db import get_city_summary
from utils.sidebar import render_currency_selector

require_auth()

rate, symbol, fmt_price = render_currency_selector()

st.title("City Comparison")

type_filter = st.radio("", ["Buy", "Rent"], horizontal=True)
type_key = type_filter.lower()

df = get_city_summary(listing_type=type_key)

all_labels = sorted(df["city_label"].unique())
selected   = st.multiselect("Select cities to compare", all_labels, default=all_labels)
df = df[df["city_label"].isin(selected)]

df["price_display"] = df["median_price"]        * rate
df["ppm2_display"]  = df["median_price_per_m2"] * rate
df["p25_display"]   = df["p25_price"]            * rate
df["p75_display"]   = df["p75_price"]            * rate

# ---- Summary table
st.subheader("Summary")
summary = df[["city_label", "listing_count", "price_display", "ppm2_display",
              "p25_display", "p75_display", "avg_time_on_market_days"]].copy()
summary.columns = ["City", "Listings", f"Median price ({symbol})", f"Median {symbol}/m²",
                   f"P25 ({symbol})", f"P75 ({symbol})", "Avg. days on market"]
for col in [f"Median price ({symbol})", f"Median {symbol}/m²", f"P25 ({symbol})", f"P75 ({symbol})"]:
    summary[col] = summary[col].map("{:,.0f}".format)
summary["Avg. days on market"] = summary["Avg. days on market"].map("{:.0f}".format)
st.dataframe(summary.sort_values(f"Median price ({symbol})", ascending=False),
             use_container_width=True, hide_index=True)

# ---- Price per m²
st.subheader(f"Median price per m² ({symbol})")
fig = px.bar(
    df.sort_values("ppm2_display", ascending=False),
    x="city_label", y="ppm2_display",
    labels={"ppm2_display": f"Median {symbol}/m²", "city_label": ""},
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
