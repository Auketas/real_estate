import streamlit as st
import plotly.express as px
import utils.charts
from utils.auth import require_auth
from utils.db import (get_city_summary, get_model_coefficients, get_model_metadata,
                      get_model_feature_stats, CITY_LABELS)
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

# ---- Price-to-rent ratio table
st.subheader("Price-to-rent ratio by city")
ptr = merged[["city_label", "price_to_rent", "buy_display",
              "rent_display", "gross_yield"]].copy()
ptr.columns = ["City", "Price-to-rent ratio", f"Median buy ({symbol})",
               f"Median monthly rent ({symbol})", "Gross yield (%)"]
ptr[f"Median buy ({symbol})"]          = ptr[f"Median buy ({symbol})"].map("{:,.0f}".format)
ptr[f"Median monthly rent ({symbol})"] = ptr[f"Median monthly rent ({symbol})"].map("{:,.0f}".format)
ptr["Gross yield (%)"]                 = ptr["Gross yield (%)"].map("{:.1f}%".format)
ptr["Price-to-rent ratio"]             = ptr["Price-to-rent ratio"].map("{:.1f}".format)
st.dataframe(
    ptr.sort_values("Gross yield (%)", ascending=False),
    use_container_width=True, hide_index=True,
)


# ---- Rental Yield Calculator ────────────────────────────────────────────────

st.divider()
st.subheader("Rental Yield Calculator")

st.caption("Estimate buy price, monthly rent, and gross yield for a property specification. Lisboa, Porto, and Setúbal only.")

# Get latest snapshot month
df_latest = get_city_summary(listing_type="buy")
latest_month = df_latest["snapshot_month"].max() if not df_latest.empty else None

if latest_month is None:
    st.warning("Model data not yet available. Run monthly aggregation first.")
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

    # Fetch model data for both buy and rent
    coef_buy = get_model_coefficients(selected_city, "buy", latest_month)
    metadata_buy = get_model_metadata(selected_city, "buy", latest_month)
    feature_stats_buy = get_model_feature_stats(selected_city, "buy", latest_month)

    coef_rent = get_model_coefficients(selected_city, "rent", latest_month)
    metadata_rent = get_model_metadata(selected_city, "rent", latest_month)
    feature_stats_rent = get_model_feature_stats(selected_city, "rent", latest_month)

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
