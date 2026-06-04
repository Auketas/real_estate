import streamlit as st
import plotly.express as px
from utils.auth import require_auth
from utils.db import get_neighbourhood_summary, CITY_LABELS

require_auth()

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

# ---- Median price per neighbourhood
st.subheader(f"Median price per neighbourhood — {city_label}")
df_sorted = df.sort_values("median_price", ascending=False)
fig = px.bar(
    df_sorted, x="neighbourhood", y="median_price",
    hover_data=["listing_count", "median_price_per_m2", "avg_time_on_market_days"],
    labels={"median_price": "Median price (€)", "neighbourhood": ""},
)
fig.update_xaxes(tickangle=45)
st.plotly_chart(fig, use_container_width=True)

# ---- Price per m²
st.subheader("Median price per m²")
fig2 = px.bar(
    df_sorted, x="neighbourhood", y="median_price_per_m2",
    labels={"median_price_per_m2": "Median €/m²", "neighbourhood": ""},
)
fig2.update_xaxes(tickangle=45)
st.plotly_chart(fig2, use_container_width=True)
