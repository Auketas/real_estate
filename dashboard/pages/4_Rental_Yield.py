import streamlit as st
import plotly.express as px
import pandas as pd
from utils.auth import require_auth
from utils.db import get_listings

require_auth()

st.title("Rental Yield")
st.caption("Gross yield = (annual rent / buy price) × 100. Data from live listings.")

cities = ["porto", "vila-nova-de-gaia", "matosinhos",
          "lisboa", "cascais", "sintra",
          "albufeira", "loule", "portimao", "lagos", "lagoa", "faro"]

# ---- Load both buy and rent listings ----------------------------------------
buy  = get_listings(type="buy")[["city", "neighbourhood", "price", "area", "tipologia"]]
rent = get_listings(type="rent")[["city", "neighbourhood", "price", "area", "tipologia"]]

buy.rename(columns={"price": "buy_price"}, inplace=True)
rent.rename(columns={"price": "rent_price"}, inplace=True)

# Median buy and rent price per city × tipologia
buy_agg  = buy.groupby(["city", "tipologia"])["buy_price"].median().reset_index()
rent_agg = rent.groupby(["city", "tipologia"])["rent_price"].median().reset_index()

merged = buy_agg.merge(rent_agg, on=["city", "tipologia"])
merged["gross_yield"] = (merged["rent_price"] * 12) / merged["buy_price"] * 100
merged = merged.dropna(subset=["gross_yield"])
merged = merged[merged["tipologia"].notna()]

# ---- City-level yield -------------------------------------------------------
st.subheader("Gross yield by city")
city_yield = (
    merged.groupby("city")["gross_yield"]
    .mean()
    .reset_index()
    .sort_values("gross_yield", ascending=False)
)
fig = px.bar(city_yield, x="city", y="gross_yield",
             labels={"gross_yield": "Avg. gross yield (%)", "city": ""},
             color="gross_yield", color_continuous_scale="RdYlGn")
fig.add_hline(y=5, line_dash="dash", line_color="grey",
              annotation_text="5% benchmark", annotation_position="top right")
st.plotly_chart(fig, use_container_width=True)

# ---- Yield by city × bedroom type ------------------------------------------
st.subheader("Yield by city and apartment type")
city_filter = st.selectbox("Filter to one city (or see all)", ["All"] + cities)
df_plot = merged if city_filter == "All" else merged[merged["city"] == city_filter]

fig2 = px.bar(df_plot.sort_values("gross_yield", ascending=False),
              x="tipologia", y="gross_yield", color="city", barmode="group",
              labels={"gross_yield": "Gross yield (%)", "tipologia": "Type", "city": "City"})
st.plotly_chart(fig2, use_container_width=True)

# ---- Raw table --------------------------------------------------------------
with st.expander("Show data table"):
    st.dataframe(
        merged.sort_values("gross_yield", ascending=False)
              .rename(columns={"buy_price": "Median buy (€)", "rent_price": "Median rent/mo (€)",
                                "gross_yield": "Gross yield (%)", "tipologia": "Type",
                                "city": "City"}),
        use_container_width=True, hide_index=True
    )
