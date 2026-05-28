import streamlit as st
from utils.auth import load_authenticator

st.set_page_config(
    page_title="Portugal Real Estate",
    layout="wide",
)

authenticator = load_authenticator()

if not st.session_state.get("authentication_status"):
    st.title("Portugal Real Estate Intelligence")
    st.markdown(
        "Daily data from Imovirtual and Casa Sapo — covering Porto, Lisbon, and the Algarve. "
        "Built for expats and buy-to-let investors who want numbers, not listings."
    )

    st.divider()

    col1, col2, col3 = st.columns(3)
    with col1:
        st.markdown("**Market Overview**")
        st.caption(
            "Headline metrics and median asking prices across all cities. "
            "Track how the market is moving week by week."
        )
        st.markdown("**City Comparison**")
        st.caption(
            "Side-by-side price distributions and price-per-m² for any combination "
            "of Porto, Lisbon, and the Algarve cities."
        )
    with col2:
        st.markdown("**Neighbourhood Deep-dive**")
        st.caption(
            "Drill into a single city by neighbourhood. Includes an interactive map "
            "of active listings with price and size overlays."
        )
        st.markdown("**Rental Yield**")
        st.caption(
            "Gross yield by city and apartment type, calculated from live buy and rent listings. "
            "Benchmarked against the 5% rule."
        )
    with col3:
        st.markdown("**Expat Tools**")
        st.caption(
            "Live ECB exchange rates for USD, GBP, NOK, SEK, and DKK. "
            "Property price converter, cost-of-living reference, and climate snapshot."
        )

    st.divider()

    st.markdown("### €19 / month")
    st.link_button(
        "Subscribe",
        "https://realestatept.lemonsqueezy.com/checkout",
        type="primary",
    )

    st.divider()
    st.markdown("**Already a subscriber? Log in below.**")

st.divider()
col_l, col_m, col_r = st.columns(3)
with col_l:
    st.page_link("pages/Legal_Terms.py", label="Terms of Service")
with col_m:
    st.page_link("pages/Legal_Privacy.py", label="Privacy Policy")
with col_r:
    st.page_link("pages/Legal_Refunds.py", label="Refund Policy")

authenticator.login()

if st.session_state.get("authentication_status"):
    authenticator.logout(location="sidebar")
    st.title("Portugal Real Estate Dashboard")
    st.write(f"Welcome, **{st.session_state['name']}**. Use the sidebar to navigate.")
    st.markdown("""
    ### What's inside
    - **Market Overview** — headline numbers across all cities
    - **City Comparison** — Lisbon vs Porto vs Algarve side-by-side
    - **Neighbourhood Deep-dive** — drill into a single city by neighbourhood
    - **Rental Yield** — gross yield by area for buy-to-let investors
    - **Expat Tools** — live exchange rates, cost of living reference
    """)

elif st.session_state.get("authentication_status") is False:
    st.error("Username or password is incorrect.")
