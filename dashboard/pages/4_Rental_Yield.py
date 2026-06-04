import streamlit as st
import plotly.express as px
import utils.charts
from utils.auth import require_auth
from utils.db import get_city_summary
from utils.sidebar import render_currency_selector

require_auth()

rate, symbol, fmt_price = render_currency_selector()

st.title("Rental Yield")
st.caption("Gross yield = annualised median asking rent / median asking buy price. Asking prices only — not transaction data.")

buy  = get_city_summary(listing_type="buy") [["city", "city_label", "median_price"]]
rent = get_city_summary(listing_type="rent")[["city", "median_price"]].rename(columns={"median_price": "median_rent"})

merged = buy.merge(rent, on="city")
merged["gross_yield"]   = (merged["median_rent"] * 12) / merged["median_price"] * 100
merged["price_to_rent"] = merged["median_price"] / (merged["median_rent"] * 12)
merged = merged[merged["gross_yield"].between(0, 20)]

merged["buy_display"]  = merged["median_price"] * rate
merged["rent_display"] = merged["median_rent"]  * rate

# ---- Gross yield by city
st.subheader("Gross yield by city")
fig = px.bar(
    merged.sort_values("gross_yield"),
    x="gross_yield", y="city_label", orientation="h",
    color="gross_yield",
    color_continuous_scale=[(0, "#d73027"), (0.3, "#fee08b"), (0.5, "#1a9850"), (1, "#1a9850")],
    range_color=[0, 10],
    labels={"gross_yield": "Gross yield (%)", "city_label": ""},
)
fig.add_vline(x=5, line_dash="dash", line_color="#6B6B6B",
              annotation_text="5% benchmark", annotation_position="top right")
fig.update_coloraxes(showscale=False)
st.plotly_chart(fig, use_container_width=True)

# ---- Price-to-rent ratio table
st.subheader("Price-to-rent ratio by city")
ptr = merged[["city_label", "price_to_rent", "buy_display", "rent_display", "gross_yield"]].copy()
ptr.columns = ["City", "Price-to-rent ratio", f"Median buy ({symbol})",
               f"Median monthly rent ({symbol})", "Gross yield (%)"]
ptr[f"Median buy ({symbol})"]          = ptr[f"Median buy ({symbol})"].map("{:,.0f}".format)
ptr[f"Median monthly rent ({symbol})"] = ptr[f"Median monthly rent ({symbol})"].map("{:,.0f}".format)
ptr["Gross yield (%)"]                 = ptr["Gross yield (%)"].map("{:.1f}".format)
ptr["Price-to-rent ratio"]             = ptr["Price-to-rent ratio"].map("{:.1f}".format)
st.dataframe(ptr.sort_values("Gross yield (%)", ascending=False),
             use_container_width=True, hide_index=True)
