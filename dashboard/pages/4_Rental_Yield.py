import streamlit as st
import plotly.express as px
from utils.auth import require_auth
from utils.db import get_city_summary

require_auth()

st.title("Rental Yield")
st.caption("Gross yield = annualised median asking rent / median asking buy price. Asking prices only — not transaction data.")

buy  = get_city_summary(listing_type="buy") [["city", "city_label", "median_price", "listing_count"]]
rent = get_city_summary(listing_type="rent")[["city", "median_price"]].rename(columns={"median_price": "median_rent"})

merged = buy.merge(rent, on="city")
merged["gross_yield"]  = (merged["median_rent"] * 12) / merged["median_price"] * 100
merged["price_to_rent"] = merged["median_price"] / (merged["median_rent"] * 12)
merged = merged[merged["gross_yield"].between(0, 20)]

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
fig.add_vline(x=5, line_dash="dash", line_color="grey",
              annotation_text="5% benchmark", annotation_position="top right")
fig.update_coloraxes(showscale=False)
st.plotly_chart(fig, use_container_width=True)

# ---- Price-to-rent ratio
st.subheader("Price-to-rent ratio by city")
ptr = merged[["city_label", "price_to_rent", "median_price", "median_rent", "gross_yield"]].copy()
ptr.columns = ["City", "Price-to-rent ratio", "Median buy price (€)", "Median monthly rent (€)", "Gross yield (%)"]
ptr["Median buy price (€)"]    = ptr["Median buy price (€)"].map("{:,.0f}".format)
ptr["Median monthly rent (€)"] = ptr["Median monthly rent (€)"].map("{:,.0f}".format)
ptr["Gross yield (%)"]         = ptr["Gross yield (%)"].map("{:.1f}".format)
ptr["Price-to-rent ratio"]     = ptr["Price-to-rent ratio"].map("{:.1f}".format)
st.dataframe(ptr.sort_values("Gross yield (%)"), use_container_width=True, hide_index=True)
