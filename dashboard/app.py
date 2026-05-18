import streamlit as st
from utils.auth import load_authenticator

st.set_page_config(
    page_title="Portugal Real Estate",
    layout="wide",
)

authenticator = load_authenticator()
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

else:
    st.info("Please enter your credentials to access the dashboard.")
