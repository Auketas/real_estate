import streamlit as st
import plotly.express as px
import utils.charts
from utils.auth import require_auth
from utils.db import get_neighbourhood_summary, CITY_LABELS
from utils.sidebar import render_currency_selector

require_auth()

rate, symbol, fmt_price = render_currency_selector()

st.title("Neighbourhood Deep-dive")

CITIES = list(CITY_LABELS.keys())

col1, col2 = st.columns(2)
city     = col1.selectbox("City", CITIES, format_func=lambda c: CITY_LABELS[c])
type_key = col2.radio("", ["buy", "rent"], horizontal=True)

df = get_neighbourhood_summary(city=city, listing_type=type_key)

if df.empty:
    st.warning("No neighbourhood data available for this city and listing type.")
    st.stop()

city_label = CITY_LABELS[city]

df["price_display"] = df["median_price"]        * rate
df["ppm2_display"]  = df["median_price_per_m2"] * rate
df_sorted = df.sort_values("price_display", ascending=False)

# ---- Median price per neighbourhood
st.subheader(f"Median price per neighbourhood — {city_label}")
fig = px.bar(
    df_sorted, x="neighbourhood", y="price_display",
    hover_data={"listing_count": True, "ppm2_display": True, "avg_time_on_market_days": True,
                "price_display": False},
    labels={
        "price_display":          f"Median price ({symbol})",
        "ppm2_display":           f"Median {symbol}/m²",
        "avg_time_on_market_days": "Avg. days on market",
        "listing_count":          "Listings",
        "neighbourhood":          "",
    },
)
fig.update_xaxes(tickangle=45)
st.plotly_chart(fig, use_container_width=True)

# ---- Price per m²
st.subheader(f"Median price per m² ({symbol})")
fig2 = px.bar(
    df_sorted, x="neighbourhood", y="ppm2_display",
    labels={"ppm2_display": f"Median {symbol}/m²", "neighbourhood": ""},
)
fig2.update_xaxes(tickangle=45)
st.plotly_chart(fig2, use_container_width=True)
