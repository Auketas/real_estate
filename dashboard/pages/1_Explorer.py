import numpy as np
import pandas as pd
import streamlit as st
import plotly.express as px
import utils.charts
from utils.auth import require_auth
from utils.db import get_city_summary
from utils.sidebar import render_currency_selector

require_auth()

rate, symbol, fmt_price = render_currency_selector()

# ── Static city data ──────────────────────────────────────────────────────────

CITY_COORDS = {
    "lisboa":            (38.717, -9.142),
    "porto":             (41.157, -8.629),
    "vila-nova-de-gaia": (41.133, -8.616),
    "cascais":           (38.697, -9.421),
    "sintra":            (38.801, -9.390),
    "almada":            (38.675, -9.137),
    "maia":              (41.176, -8.617),
    "albufeira":         (37.089, -8.250),
    "faro":              (37.020, -7.935),
    "lagoa":             (37.137, -8.454),
    "lagos":             (37.103, -8.673),
    "loule":             (37.144, -8.024),
    "portimao":          (37.136, -8.538),
}

# Approximate annual climate data (source: Wikipedia / IPMA averages)
CLIMATE = {
    "lisboa":            dict(sunshine=2799, summer_temp=26, winter_temp=13),
    "porto":             dict(sunshine=2494, summer_temp=23, winter_temp=11),
    "vila-nova-de-gaia": dict(sunshine=2494, summer_temp=23, winter_temp=11),
    "cascais":           dict(sunshine=2800, summer_temp=25, winter_temp=13),
    "sintra":            dict(sunshine=2600, summer_temp=24, winter_temp=11),
    "almada":            dict(sunshine=2750, summer_temp=25, winter_temp=13),
    "maia":              dict(sunshine=2500, summer_temp=23, winter_temp=11),
    "albufeira":         dict(sunshine=3300, summer_temp=28, winter_temp=15),
    "faro":              dict(sunshine=3106, summer_temp=28, winter_temp=15),
    "lagoa":             dict(sunshine=3000, summer_temp=27, winter_temp=15),
    "lagos":             dict(sunshine=3100, summer_temp=27, winter_temp=15),
    "loule":             dict(sunshine=3100, summer_temp=28, winter_temp=15),
    "portimao":          dict(sunshine=3000, summer_temp=28, winter_temp=15),
}

# ── Data ─────────────────────────────────────────────────────────────────────

st.title("Explorer")

type_filter = st.radio("", ["Buy", "Rent"], horizontal=True)
type_key    = type_filter.lower()

df = get_city_summary(listing_type=type_key)
df["price_display"] = df["median_price"]        * rate
df["ppm2_display"]  = df["median_price_per_m2"] * rate

snapshot_label = pd.to_datetime(df["snapshot_month"].iloc[0]).strftime("%b %Y") \
    if not df.empty else "—"

# ── Hero metrics ──────────────────────────────────────────────────────────────

col1, col2, col3, col4 = st.columns(4)
col1.metric("Active listings",     f"{df['listing_count'].sum():,}")
col2.metric(f"Median {symbol}/m²", fmt_price(df["median_price_per_m2"].median()))
col3.metric("Cities covered",      df["city"].nunique())
col4.metric("Data freshness",      f"Updated {snapshot_label}")

st.divider()

# ── Portugal overview map ─────────────────────────────────────────────────────

df_map = df[df["city"].isin(CITY_COORDS)].copy()
df_map["lat"]        = df_map["city"].map(lambda c: CITY_COORDS[c][0])
df_map["lon"]        = df_map["city"].map(lambda c: CITY_COORDS[c][1])
df_map["size_plot"]  = np.sqrt(df_map["listing_count"].clip(lower=1))

fig_map = px.scatter_mapbox(
    df_map,
    lat="lat", lon="lon",
    color="ppm2_display",
    size="size_plot",
    size_max=40,
    color_continuous_scale=["#F5E6C8", "#C4603A"],
    hover_name="city_label",
    hover_data={
        "price_display":          True,
        "ppm2_display":           True,
        "listing_count":          True,
        "avg_time_on_market_days":":.0f",
        "lat": False, "lon": False, "size_plot": False,
    },
    labels={
        "price_display":           f"Median price ({symbol})",
        "ppm2_display":            f"Median {symbol}/m²",
        "listing_count":           "Listings",
        "avg_time_on_market_days": "Avg. days on market",
    },
    mapbox_style="carto-positron",
    zoom=5.8,
    center={"lat": 39.4, "lon": -8.4},
)
fig_map.update_layout(
    coloraxis_colorbar=dict(title=f"{symbol}/m²"),
    margin=dict(l=0, r=0, t=0, b=0),
    height=480,
)
st.plotly_chart(fig_map, use_container_width=True)

st.divider()

# ── City comparison table ─────────────────────────────────────────────────────

st.subheader("City comparison")

df_table = df.copy()
df_table["sunshine"]     = df_table["city"].map(lambda c: CLIMATE.get(c, {}).get("sunshine"))
df_table["summer_temp"]  = df_table["city"].map(lambda c: CLIMATE.get(c, {}).get("summer_temp"))
df_table["winter_temp"]  = df_table["city"].map(lambda c: CLIMATE.get(c, {}).get("winter_temp"))

tbl = df_table[[
    "city_label", "price_display", "ppm2_display",
    "avg_time_on_market_days", "sunshine", "summer_temp", "winter_temp",
]].copy()
tbl.columns = [
    "City", f"Median price ({symbol})", f"Median {symbol}/m²",
    "Avg. days on market", "Sunshine hrs/yr", "Avg summer °C", "Avg winter °C",
]
tbl[f"Median price ({symbol})"] = tbl[f"Median price ({symbol})"].map("{:,.0f}".format)
tbl[f"Median {symbol}/m²"]      = tbl[f"Median {symbol}/m²"].map("{:,.0f}".format)
tbl["Avg. days on market"]      = tbl["Avg. days on market"].map("{:.0f}".format)

st.dataframe(
    tbl.sort_values(f"Median {symbol}/m²", ascending=False),
    use_container_width=True, hide_index=True,
)
st.caption(
    "Climate figures are long-run annual averages (IPMA / Wikipedia). "
    "Summer = Jun–Aug average daily high. Winter = Dec–Feb average daily high."
)

st.divider()

# ── Price per m² chart ────────────────────────────────────────────────────────

st.subheader(f"Median price per m² ({symbol})")
fig_ppm2 = px.bar(
    df.sort_values("ppm2_display", ascending=False),
    x="city_label", y="ppm2_display",
    labels={"ppm2_display": f"Median {symbol}/m²", "city_label": ""},
)
st.plotly_chart(fig_ppm2, use_container_width=True)

# ── Time on market chart ──────────────────────────────────────────────────────

st.subheader("Average time on market (days)")
fig_tom = px.bar(
    df.sort_values("avg_time_on_market_days", ascending=False),
    x="city_label", y="avg_time_on_market_days",
    labels={"avg_time_on_market_days": "Days", "city_label": ""},
)
st.plotly_chart(fig_tom, use_container_width=True)
