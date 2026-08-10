import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
import utils.charts
from utils.auth import require_auth
from utils.db import (get_city_summary, get_model_coefficients, get_model_metadata,
                      get_model_feature_stats, CITY_LABELS, get_available_snapshot_months,
                      get_latest_live_model_date)
from utils.sidebar import render_currency_selector
from utils.calculator import predict_price, get_available_neighbourhoods, get_available_tipologias

require_auth()

rate, symbol, fmt_price = render_currency_selector()

# Algarve rental data is too sparse on these platforms for reliable yield estimates
YIELD_CITIES = {"porto", "vila-nova-de-gaia", "maia", "lisboa", "cascais", "sintra", "almada", "costa-da-caparica", "caparica-e-trafaria"}

st.title("Investment View")
st.caption(
    "Gross yield = annualised median asking rent / median asking buy price. "
    "Asking prices only — not transaction data. "
    "Lisboa and Porto regions only — long-term rentals are too rare in the Algarve to reliably analyze."
)

buy  = get_city_summary(listing_type="buy") [["city", "city_label", "median_price"]]
rent = get_city_summary(listing_type="rent")[["city", "median_price"]].rename(
    columns={"median_price": "median_rent"}
)

merged = buy.merge(rent, on="city")
merged = merged[merged["city"].isin(YIELD_CITIES)]
merged["gross_yield"]    = (merged["median_rent"] * 12) / merged["median_price"] * 100
merged["price_to_rent"]  = merged["median_price"] / (merged["median_rent"] * 12)
merged = merged[merged["gross_yield"].between(0, 20)]

merged["buy_display"]  = merged["median_price"] * rate
merged["rent_display"] = merged["median_rent"]  * rate


def yield_band(y):
    if y < 3:   return "< 3%"
    elif y < 5: return "3–5%"
    else:       return "> 5%"


merged["yield_band"] = merged["gross_yield"].apply(yield_band)

BAND_COLOURS = {"< 3%": "#d73027", "3–5%": "#fee08b", "> 5%": "#1a9850"}
BAND_ORDER   = ["< 3%", "3–5%", "> 5%"]

# ---- Gross yield by city
st.subheader("Gross yield by city")
fig = px.bar(
    merged.sort_values("gross_yield"),
    x="gross_yield", y="city_label", orientation="h",
    color="yield_band",
    color_discrete_map=BAND_COLOURS,
    category_orders={"yield_band": BAND_ORDER},
    labels={"gross_yield": "Gross yield (%)", "city_label": "", "yield_band": "Yield band"},
    hover_data={"yield_band": False, "gross_yield": ":.1f"},
)
fig.add_vline(x=5, line_dash="dash", line_color="#6B6B6B",
              annotation_text="5% benchmark", annotation_position="top right")
st.plotly_chart(fig, use_container_width=True)

# ---- City comparison table
st.subheader("Market metrics by city")
city_table = merged[["city_label", "buy_display", "rent_display", "gross_yield"]].copy()
city_table.columns = ["City", f"Median buy ({symbol})", f"Median monthly rent ({symbol})", "Gross yield (%)"]
city_table[f"Median buy ({symbol})"]          = city_table[f"Median buy ({symbol})"].map("{:,.0f}".format)
city_table[f"Median monthly rent ({symbol})"] = city_table[f"Median monthly rent ({symbol})"].map("{:,.0f}".format)
city_table["Gross yield (%)"]                 = city_table["Gross yield (%)"].map("{:.1f}%".format)
st.dataframe(
    city_table.sort_values("Gross yield (%)", ascending=False),
    use_container_width=True, hide_index=True,
)



# ---- Rental Yield Calculator ────────────────────────────────────────────────

st.divider()
st.subheader("Rental Yield Estimator")

st.caption("Estimate buy price, monthly rent, and gross yield for a property specification. Lisboa, Porto, and Setúbal only.")

# Get latest live model for current predictions
latest_date = get_latest_live_model_date("buy")
available_months = get_available_snapshot_months("buy")

if latest_date is None:
    st.warning("Live model not yet available. Models train daily after new listings are processed.")
else:
    # City selector (rent-available regions only)
    RENT_AVAILABLE_CITIES = {
        "lisboa": "Lisboa",
        "porto": "Porto",
        "cascais": "Cascais",
        "sintra": "Sintra",
        "almada": "Almada",
        "costa-da-caparica": "Costa da Caparica",
        "caparica-e-trafaria": "Caparica",
        "maia": "Maia",
        "vila-nova-de-gaia": "Gaia",
    }

    selected_city = st.selectbox(
        "City",
        list(RENT_AVAILABLE_CITIES.keys()),
        format_func=lambda x: RENT_AVAILABLE_CITIES.get(x, x),
        key="yield_city"
    )

    # Fetch live model data for both buy and rent
    coef_buy = get_model_coefficients(selected_city, "buy", latest_date)
    metadata_buy = get_model_metadata(selected_city, "buy", latest_date)
    feature_stats_buy = get_model_feature_stats(selected_city, "buy", latest_date)

    coef_rent = get_model_coefficients(selected_city, "rent", latest_date)
    metadata_rent = get_model_metadata(selected_city, "rent", latest_date)
    feature_stats_rent = get_model_feature_stats(selected_city, "rent", latest_date)

    if coef_buy.empty or coef_rent.empty:
        st.warning(f"Model data not available for {RENT_AVAILABLE_CITIES[selected_city]}")
    else:
        # Get available options
        available_neighbourhoods = get_available_neighbourhoods(coef_buy)
        available_tipologias = get_available_tipologias(coef_buy)

        # Input section
        st.write("Specify property details (all fields optional):")

        col1, col2, col3 = st.columns(3)

        with col1:
            neighbourhood = st.selectbox(
                "Neighbourhood",
                [None] + available_neighbourhoods,
                format_func=lambda x: "Select neighbourhood" if x is None else x,
                key="yield_neighbourhood"
            )

        with col2:
            tipologia = st.selectbox(
                "Property type",
                [None] + available_tipologias,
                format_func=lambda x: "Select type" if x is None else x,
                key="yield_tipologia"
            )

        with col3:
            area = st.number_input(
                "Area (m²)",
                min_value=0,
                max_value=500,
                value=None,
                step=10,
                key="yield_area"
            )

        # Feature toggles
        col1, col2, col3, col4, col5 = st.columns(5)
        with col1:
            novo = st.checkbox("New build", key="yield_novo")
        with col2:
            jardim = st.checkbox("Garden", key="yield_jardim")
        with col3:
            garagem = st.checkbox("Parking", key="yield_garagem")
        with col4:
            terraco = st.checkbox("Terrace", key="yield_terraco")
        with col5:
            varanda = st.checkbox("Balcony", key="yield_varanda")

        # Build inputs dict
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
        result_buy = predict_price(inputs, coef_buy, feature_stats_buy, metadata_buy)
        result_rent = predict_price(inputs, coef_rent, feature_stats_rent, metadata_rent)

        if "error" in result_buy or "error" in result_rent:
            st.error("Could not calculate yield")
        else:
            buy_price = result_buy["predicted_price"]
            rent_price = result_rent["predicted_price"]
            gross_yield = (rent_price * 12 / buy_price * 100) if buy_price > 0 else 0

            # Display results
            st.write("")
            col1, col2, col3 = st.columns(3)

            with col1:
                st.metric("Buy price", fmt_price(buy_price))

            with col2:
                st.metric("Monthly rent", fmt_price(rent_price))

            with col3:
                st.metric("Gross yield", f"{gross_yield:.1f}%")

            # Caveat
            st.info(
                "⚠️ **Gross yield only.** Net yield will be lower after taxes, "
                "vacancy, maintenance, and management costs."
            )

            st.write("")

            # Historical yield trend (if monthly snapshots available)
            if available_months and len(available_months) > 1:
                with st.expander("📊 Yield trend (monthly snapshots)"):
                    trend_data = []
                    for month in available_months[-12:]:  # Show last 12 months
                        coef_buy_hist = get_model_coefficients(selected_city, "buy", month)
                        feat_buy_hist = get_model_feature_stats(selected_city, "buy", month)
                        meta_buy_hist = get_model_metadata(selected_city, "buy", month)

                        coef_rent_hist = get_model_coefficients(selected_city, "rent", month)
                        feat_rent_hist = get_model_feature_stats(selected_city, "rent", month)
                        meta_rent_hist = get_model_metadata(selected_city, "rent", month)

                        if not coef_buy_hist.empty and not coef_rent_hist.empty and not feat_buy_hist.empty and not feat_rent_hist.empty:
                            result_buy_hist = predict_price(inputs, coef_buy_hist, feat_buy_hist, meta_buy_hist)
                            result_rent_hist = predict_price(inputs, coef_rent_hist, feat_rent_hist, meta_rent_hist)

                            if "predicted_price" in result_buy_hist and "predicted_price" in result_rent_hist:
                                buy_hist = result_buy_hist["predicted_price"]
                                rent_hist = result_rent_hist["predicted_price"]
                                yield_hist = (rent_hist * 12 / buy_hist * 100) if buy_hist > 0 else 0

                                trend_data.append({
                                    "index": len(trend_data),
                                    "month_str": str(month),
                                    "buy_native": buy_hist,
                                    "rent_native": rent_hist,
                                    "yield": yield_hist,
                                    "is_current": False
                                })

                    # Add current estimates at the end
                    trend_data.append({
                        "index": len(trend_data),
                        "month_str": "Current",
                        "buy_native": buy_price,
                        "rent_native": rent_price,
                        "yield": gross_yield,
                        "is_current": True
                    })

                    if trend_data:
                        df_hist = pd.DataFrame(trend_data)
                        df_hist["buy_display"] = df_hist["buy_native"] * rate
                        df_hist["rent_display"] = df_hist["rent_native"] * rate

                        # Sort chronologically: separate current from historical, sort historical by date, then append current
                        df_current = df_hist[df_hist["is_current"] == True]
                        df_historical = df_hist[df_hist["is_current"] == False].copy()
                        df_historical["month_dt"] = pd.to_datetime(df_historical["month_str"])
                        df_historical = df_historical.sort_values("month_dt")
                        df_hist = pd.concat([df_historical, df_current], ignore_index=True)
                        month_order = df_hist["month_str"].tolist()

                        # Create subplots for buy price and yield
                        col1, col2 = st.columns(2)

                        with col1:
                            fig_buy = px.line(
                                df_hist,
                                x="month_str", y="buy_display",
                                markers=True,
                                labels={"month_str": "Month", "buy_display": f"Buy price ({symbol})"},
                                color="is_current",
                                color_discrete_map={False: "#C4603A", True: "#7A8C6E"},
                                category_orders={"month_str": month_order},
                            )
                            fig_buy.update_traces(
                                hovertemplate="<b>%{x}</b><br>" + symbol + " %{y:,.0f}<extra></extra>",
                                marker=dict(size=8)
                            )
                            fig_buy.update_xaxes(type="category")
                            fig_buy.update_layout(showlegend=False, hovermode="x unified")
                            st.plotly_chart(fig_buy, use_container_width=True)
                            st.caption("Estimated buy price over time (green = current estimate)")

                        with col2:
                            fig_yield = px.line(
                                df_hist,
                                x="month_str", y="yield",
                                markers=True,
                                labels={"month_str": "Month", "yield": "Gross yield (%)"},
                                color="is_current",
                                color_discrete_map={False: "#C4603A", True: "#7A8C6E"},
                                category_orders={"month_str": month_order},
                            )
                            fig_yield.update_traces(
                                hovertemplate="<b>%{x}</b><br>%{y:.1f}%<extra></extra>",
                                marker=dict(size=8)
                            )
                            fig_yield.update_xaxes(type="category")
                            fig_yield.update_layout(showlegend=False, hovermode="x unified")
                            st.plotly_chart(fig_yield, use_container_width=True)
                            st.caption("Estimated gross yield over time (green = current estimate)")
