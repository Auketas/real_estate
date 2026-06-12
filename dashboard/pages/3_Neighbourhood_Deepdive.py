import os
import json
import numpy as np
import pandas as pd
import streamlit as st
import plotly.express as px
import utils.charts
from utils.auth import require_auth
from utils.db import (get_region_neighbourhood_summary, get_neighbourhood_summary,
                      get_city_summary, CITY_LABELS, get_model_coefficients,
                      get_model_metadata, get_model_feature_stats, get_available_snapshot_months,
                      get_latest_live_model_date)
from utils.sidebar import render_currency_selector
from utils.calculator import predict_price, get_available_neighbourhoods, get_available_tipologias

require_auth()

rate, symbol, fmt_price = render_currency_selector()

STATIC = os.path.join(os.path.dirname(os.path.dirname(__file__)), "static")

@st.cache_data
def _load_json(filename):
    with open(os.path.join(STATIC, filename), encoding="utf-8") as f:
        return json.load(f)

LOOKUP = _load_json("neighbourhood_lookup.json")

ALGARVE_CITIES = ("albufeira", "faro", "lagoa", "lagos", "loule", "portimao")
SETUBAL_CITIES = ("almada", "costa-da-caparica", "caparica-e-trafaria")

REGIONS = {
    "Porto":   dict(cities=("porto", "vila-nova-de-gaia", "maia"),
                    geojson="porto_region.geojson",  featureidkey="properties.NAME_3",
                    lat=41.16, lon=-8.62, zoom=11),
    "Lisboa":  dict(cities=("lisboa", "cascais", "sintra"),
                    geojson="lisboa_region.geojson", featureidkey="properties.NAME_3",
                    lat=38.72, lon=-9.14, zoom=10),
    "Setúbal": dict(cities=SETUBAL_CITIES,
                    geojson="setubal.geojson",       featureidkey="properties.NAME_3",
                    lat=38.64, lon=-9.16, zoom=11),
    "Algarve": dict(cities=ALGARVE_CITIES,
                    geojson="algarve.geojson",       featureidkey="properties.NAME_2",
                    lat=37.13, lon=-8.25, zoom=8),
}

COLOR_SCALE = ["#ffffcc", "#800000"]  # Pale yellow → dark maroon for maximum contrast


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

    # Neighbourhood field already contains GeoJSON feature names (from spatial join backfill via lat/lon)
    # No mapping needed; use directly as feature_name for choropleth
    df_nbhd["feature_name"] = df_nbhd["neighbourhood"]

    # Drop nulls and aggregate: multiple cities in region may have same neighbourhood
    df_matched = df_nbhd.dropna(subset=["feature_name"]).copy()
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

    # ── Price Estimator ───────────────────────────────────────────────────────
    st.divider()
    st.subheader("Price Estimator")

    # Get latest live model for current predictions
    first_city = cfg["cities"][0]
    latest_date = get_latest_live_model_date(type_key)
    available_months = get_available_snapshot_months(type_key)

    if latest_date is None:
        st.warning("Live model not yet available. Models train daily after new listings are processed.")
    else:
        # Fetch live model data
        coef = get_model_coefficients(first_city, type_key, latest_date)
        metadata = get_model_metadata(first_city, type_key, latest_date)
        feature_stats = get_model_feature_stats(first_city, type_key, latest_date)

        if coef.empty or metadata.empty:
            st.warning(f"No live model available for {region} ({type_key})")
        else:
            # Get available options
            available_neighbourhoods = get_available_neighbourhoods(coef)
            available_tipologias = get_available_tipologias(coef)

            # Input section
            st.write("Specify property details (all fields optional):")

            col1, col2, col3 = st.columns(3)

            with col1:
                neighbourhood = st.selectbox(
                    "Neighbourhood",
                    [None] + available_neighbourhoods,
                    format_func=lambda x: "Select neighbourhood" if x is None else x,
                    key="calc_neighbourhood"
                )

            with col2:
                tipologia = st.selectbox(
                    "Property type",
                    [None] + available_tipologias,
                    format_func=lambda x: "Select type" if x is None else x,
                    key="calc_tipologia"
                )

            with col3:
                area = st.number_input(
                    "Area (m²)",
                    min_value=0,
                    max_value=500,
                    value=None,
                    step=10,
                    key="calc_area"
                )

            # Feature toggles
            col1, col2, col3, col4, col5 = st.columns(5)
            with col1:
                novo = st.checkbox("New build", key="calc_novo")
            with col2:
                jardim = st.checkbox("Garden", key="calc_jardim")
            with col3:
                garagem = st.checkbox("Parking", key="calc_garagem")
            with col4:
                terraco = st.checkbox("Terrace", key="calc_terraco")
            with col5:
                varanda = st.checkbox("Balcony", key="calc_varanda")

            # Build inputs dict (None for unspecified)
            inputs = {
                "neighbourhood": neighbourhood,
                "tipologia": tipologia,
                "area": area if area and area > 0 else None,
                "novo": 1 if novo else None,
                "jardim": 1 if jardim else None,
                "garagem": 1 if garagem else None,
                "terraco": 1 if terraco else None,
                "varanda": 1 if varanda else None,
            }

            # Calculate
            result = predict_price(inputs, coef, feature_stats, metadata)

            if "error" in result:
                st.error(result["error"])
            else:
                # Display result with visual band
                predicted_price = result["predicted_price"]
                ci_lower = result["ci_lower"]
                ci_upper = result["ci_upper"]

                # Main price display
                st.write("")
                price_col, range_col = st.columns([1, 2])

                with price_col:
                    st.metric(
                        "Estimated price",
                        fmt_price(predicted_price)
                    )

                with range_col:
                    # Estimated range
                    st.write(f"Estimated range: {fmt_price(ci_lower)} – {fmt_price(ci_upper)}")

                    # Confidence indicator: narrow → green, medium → yellow, wide → red
                    # Based on range as % of central estimate
                    range_width = ci_upper - ci_lower
                    range_pct = (range_width / predicted_price * 100) if predicted_price > 0 else 100

                    if range_pct < 30:
                        confidence_color = "🟢 High confidence"
                    elif range_pct < 60:
                        confidence_color = "🟡 Medium confidence"
                    else:
                        confidence_color = "🔴 Low confidence (add details)"

                    st.caption(f"Confidence: {confidence_color} (80% range: ±{range_pct:.0f}%)")

                # Helper text
                if any(v is None for k, v in inputs.items() if k not in ["neighbourhood", "tipologia"]):
                    st.caption("💡 Add more details to narrow the estimate")

                st.write("")

                # Historical price trend (if monthly snapshots available)
                if available_months:
                    with st.expander("📊 Price trend (monthly snapshots)"):
                        trend_prices = []
                        for month in available_months[:12]:  # Show last 12 months
                            coef_hist = get_model_coefficients(first_city, type_key, month)
                            feat_hist = get_model_feature_stats(first_city, type_key, month)
                            meta_hist = get_model_metadata(first_city, type_key, month)
                            if not coef_hist.empty and not feat_hist.empty:
                                result_hist = predict_price(inputs, coef_hist, feat_hist, meta_hist)
                                if "predicted_price" in result_hist:
                                    trend_prices.append({
                                        "Month": month,
                                        "Price": result_hist["predicted_price"]
                                    })
                        if trend_prices:
                            df_trend = pd.DataFrame(trend_prices)
                            st.line_chart(df_trend.set_index("Month")["Price"])
                            st.caption("How this property's predicted price has changed across monthly snapshots")

    # ── Neighbourhood Browse (Secondary) ────────────────────────────────────────
    st.divider()
    st.subheader(f"Browse neighbourhoods — {region}")
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
