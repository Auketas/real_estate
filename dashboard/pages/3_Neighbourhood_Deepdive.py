import os
import json
import numpy as np
import pandas as pd
import streamlit as st
import plotly.express as px
import utils.charts
from utils.auth import require_auth
from utils.db import (get_region_neighbourhood_summary, get_neighbourhood_summary,
                      get_city_summary, CITY_LABELS)
from utils.sidebar import render_currency_selector

require_auth()

rate, symbol, fmt_price = render_currency_selector()

STATIC = os.path.join(os.path.dirname(os.path.dirname(__file__)), "static")

@st.cache_data
def _load_json(filename):
    with open(os.path.join(STATIC, filename), encoding="utf-8") as f:
        return json.load(f)

LOOKUP = _load_json("neighbourhood_lookup.json")

ALGARVE_CITIES = ("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")
ALMADA_CITIES = ("almada",)

REGIONS = {
    "Porto":   dict(cities=("porto", "vila-nova-de-gaia", "maia"),
                    geojson="porto_region.geojson",  featureidkey="properties.NAME_3",
                    lat=41.16, lon=-8.62, zoom=11),
    "Lisboa":  dict(cities=("lisboa", "cascais", "sintra"),
                    geojson="lisboa_region.geojson", featureidkey="properties.NAME_3",
                    lat=38.72, lon=-9.14, zoom=10),
    "Almada":  dict(cities=ALMADA_CITIES,
                    geojson="almada.geojson",        featureidkey="properties.NAME_2",
                    lat=38.675, lon=-9.137, zoom=12),
    "Algarve": dict(cities=ALGARVE_CITIES,
                    geojson="algarve.geojson",       featureidkey="properties.NAME_2",
                    lat=37.13, lon=-8.25, zoom=8),
}

COLOR_SCALE = ["#F5E6C8", "#C4603A"]


def weighted_avg(group, val_col):
    w = group["listing_count"].fillna(0)
    v = group[val_col]
    mask = (w > 0) & v.notna()
    if mask.sum() == 0:
        return np.nan
    return np.average(v[mask], weights=w[mask])


st.title("Neighbourhood Deep-dive")

col1, col2 = st.columns(2)
region = col1.selectbox("Region", list(REGIONS.keys()))
cfg    = REGIONS[region]

is_algarve = (region == "Algarve")
if is_algarve:
    col2.caption(
        "Rental data is not available for the Algarve — long-term rentals "
        "are too rare in this region to reliably analyze."
    )
    type_key = "buy"
else:
    type_key = col2.radio("", ["buy", "rent"], horizontal=True)

geojson = _load_json(cfg["geojson"])

# ── Algarve: city-level choropleth + neighbourhood bar chart ─────────────────
if is_algarve:
    df_city = get_city_summary(listing_type="buy")
    df_city = df_city[df_city["city"].isin(ALGARVE_CITIES)].copy()
    df_city["ppm2_display"]  = df_city["median_price_per_m2"] * rate
    df_city["price_display"] = df_city["median_price"]        * rate

    fig = px.choropleth_mapbox(
        df_city, geojson=geojson,
        locations="city_label", featureidkey="properties.NAME_2",
        color="ppm2_display",
        color_continuous_scale=COLOR_SCALE,
        mapbox_style="carto-positron",
        zoom=cfg["zoom"], center={"lat": cfg["lat"], "lon": cfg["lon"]},
        opacity=0.75,
        hover_data={
            "city_label":    True,
            "ppm2_display":  ":.0f",
            "price_display": ":.0f",
        },
        labels={
            "city_label":    "City",
            "ppm2_display":  f"Median {symbol}/m²",
            "price_display": f"Median price ({symbol})",
        },
    )
    fig.update_layout(
        coloraxis_colorbar=dict(title=f"{symbol}/m²"),
        margin=dict(l=0, r=0, t=0, b=0),
    )
    st.plotly_chart(fig, use_container_width=True)

    st.subheader("Neighbourhood breakdown")
    algarve_city = st.selectbox(
        "Select city", list(ALGARVE_CITIES), format_func=lambda c: CITY_LABELS[c]
    )
    df_nbhd = get_neighbourhood_summary(city=algarve_city, listing_type="buy")
    if df_nbhd.empty:
        st.info("No neighbourhood data available for this city.")
    else:
        df_nbhd["ppm2_display"] = df_nbhd["median_price_per_m2"] * rate
        fig_bar = px.bar(
            df_nbhd.sort_values("ppm2_display", ascending=False),
            x="neighbourhood", y="ppm2_display",
            hover_data={
                "avg_time_on_market_days": ":.0f",
                "most_common_property_type": True,
            },
            labels={"ppm2_display": f"Median {symbol}/m²", "neighbourhood": "",
                    "avg_time_on_market_days": "Avg. days on market",
                    "most_common_property_type": "Most common type"},
        )
        fig_bar.update_xaxes(tickangle=45)
        st.plotly_chart(fig_bar, use_container_width=True)

# ── Porto / Lisboa: neighbourhood choropleth ─────────────────────────────────
else:
    df_nbhd = get_region_neighbourhood_summary(cfg["cities"], listing_type=type_key)

    if df_nbhd.empty:
        st.warning("No neighbourhood data available.")
        st.stop()

    # Map DB neighbourhood names to GeoJSON feature names
    df_nbhd["feature_name"] = df_nbhd["neighbourhood"].map(LOOKUP)
    df_matched = df_nbhd.dropna(subset=["feature_name"]).copy()

    # Aggregate: multiple DB neighbourhoods may map to the same parish polygon
    df_choro = (
        df_matched.groupby("feature_name")
        .apply(lambda x: pd.Series({
            "median_price_per_m2": weighted_avg(x, "median_price_per_m2"),
            "median_price":        weighted_avg(x, "median_price"),
            "avg_time_on_market":  weighted_avg(x, "avg_time_on_market_days"),
            "listing_count":       int(x["listing_count"].sum()),
        }))
        .reset_index()
    )
    df_choro["ppm2_display"]  = df_choro["median_price_per_m2"] * rate
    df_choro["price_display"] = df_choro["median_price"]        * rate

    matched_pct = len(df_matched) / max(len(df_nbhd), 1) * 100

    def format_neighbourhood_name(name):
        """Add spaces before capital letters: LordeloDoOuro -> Lordelo Do Ouro"""
        return ''.join(c if i == 0 else (' ' + c if c.isupper() else c)
                      for i, c in enumerate(name))

    df_choro["feature_name_display"] = df_choro["feature_name"].apply(format_neighbourhood_name)
    df_choro["ppm2_rounded"] = df_choro["ppm2_display"].round(0)
    df_choro["price_rounded"] = df_choro["price_display"].round(0)

    fig = px.choropleth_mapbox(
        df_choro, geojson=geojson,
        locations="feature_name", featureidkey=cfg["featureidkey"],
        color="ppm2_display",
        color_continuous_scale=COLOR_SCALE,
        mapbox_style="carto-positron",
        zoom=cfg["zoom"], center={"lat": cfg["lat"], "lon": cfg["lon"]},
        opacity=0.75,
        hover_data={
            "feature_name_display":   True,
            "ppm2_rounded":           True,
            "price_rounded":          True,
            "avg_time_on_market": ":.0f",
        },
        labels={
            "feature_name_display":   "Neighbourhood",
            "ppm2_rounded":           f"Median {symbol}/m²",
            "price_rounded":          f"Median price ({symbol})",
            "avg_time_on_market":     "Avg. days on market",
        },
    )
    fig.update_layout(
        coloraxis_colorbar=dict(title=f"{symbol}/m²"),
        margin=dict(l=0, r=0, t=0, b=0),
    )
    st.plotly_chart(fig, use_container_width=True)
    st.caption(
        f"{matched_pct:.0f}% of neighbourhoods matched to map polygons. "
        "Unmatched neighbourhoods appear in the bar chart below only."
    )

    # Secondary bar chart — all neighbourhoods including unmatched
    st.subheader(f"Price per m² by neighbourhood — {region}")
    df_nbhd["ppm2_display"] = df_nbhd["median_price_per_m2"] * rate
    fig_bar = px.bar(
        df_nbhd.sort_values("ppm2_display", ascending=False),
        x="neighbourhood", y="ppm2_display",
        hover_data={"avg_time_on_market_days": ":.0f"},
        labels={"ppm2_display": f"Median {symbol}/m²", "neighbourhood": "",
                "avg_time_on_market_days": "Avg. days on market"},
    )
    fig_bar.update_xaxes(tickangle=45)
    st.plotly_chart(fig_bar, use_container_width=True)
