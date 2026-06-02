import streamlit as st
import plotly.express as px
import pandas as pd
from utils.auth import require_auth
from utils.db import get_listings

require_auth()

st.title("Neighbourhood Deep-dive")

col1, col2 = st.columns(2)
city     = col1.selectbox("City", ["porto", "vila-nova-de-gaia", "matosinhos",
                                    "lisboa", "cascais", "sintra",
                                    "albufeira", "loule", "portimao", "lagos", "lagoa", "faro"])
type_key = col2.radio("", ["buy", "rent"], horizontal=True)

df = get_listings(type=type_key, city=city)
df["area_m2"]      = pd.to_numeric(df["area"], errors="coerce")
df["price_per_m2"] = df["price"] / df["area_m2"]

df_nb = df.dropna(subset=["neighbourhood"])

if df_nb.empty:
    st.warning("No neighbourhood data available yet for this city.")
    st.stop()

# ---- Median price per neighbourhood ----------------------------------------
st.subheader(f"Median price per neighbourhood — {city.title()}")
nb_summary = (
    df_nb.groupby("neighbourhood")
    .agg(listings=("price", "count"), median_price=("price", "median"),
         median_m2=("price_per_m2", "median"))
    .reset_index()
    .query("listings >= 5")          # hide neighbourhoods with too few data points
    .sort_values("median_price", ascending=False)
)

fig = px.bar(nb_summary, x="neighbourhood", y="median_price",
             hover_data=["listings", "median_m2"],
             labels={"median_price": "Median price (€)", "neighbourhood": ""})
fig.update_xaxes(tickangle=45)
st.plotly_chart(fig, use_container_width=True)

# ---- Map --------------------------------------------------------------------
st.subheader("Listings map")
df_map = df.dropna(subset=["lat", "lon"])
df_map["lat"] = pd.to_numeric(df_map["lat"], errors="coerce")
df_map["lon"] = pd.to_numeric(df_map["lon"], errors="coerce")
df_map = df_map.dropna(subset=["lat", "lon"])

if not df_map.empty:
    fig2 = px.scatter_mapbox(
        df_map, lat="lat", lon="lon", color="price",
        hover_data=["neighbourhood", "area", "tipologia"],
        color_continuous_scale="Viridis",
        zoom=11, mapbox_style="carto-positron",
        labels={"price": "Price (€)"},
    )
    st.plotly_chart(fig2, use_container_width=True)
else:
    st.info("No coordinate data available for this city yet.")
