import streamlit as st
from utils.auth import require_auth
from utils.db import get_exchange_rates

require_auth()

st.title("Expat Tools")

# ---- Exchange rates ---------------------------------------------------------
st.subheader("Live exchange rates (EUR base)")
st.caption("Source: European Central Bank. Updated daily.")

try:
    rates = get_exchange_rates()
    currencies = {"USD": "🇺🇸 US Dollar", "GBP": "🇬🇧 British Pound",
                  "NOK": "🇳🇴 Norwegian Krone", "SEK": "🇸🇪 Swedish Krona",
                  "DKK": "🇩🇰 Danish Krone"}
    cols = st.columns(len(currencies))
    for col, (code, label) in zip(cols, currencies.items()):
        if code in rates:
            col.metric(label, f"{rates[code]:.4f}", help=f"1 EUR = {rates[code]:.4f} {code}")
except Exception:
    st.warning("Could not load exchange rates right now.")

st.divider()

# ---- Price converter --------------------------------------------------------
st.subheader("Property price converter")
eur_price = st.number_input("Price in EUR (€)", min_value=0, value=300_000, step=10_000)
try:
    rates = get_exchange_rates()
    c1, c2, c3 = st.columns(3)
    c1.metric("USD $",  f"{eur_price * rates.get('USD', 1):,.0f}")
    c2.metric("GBP £",  f"{eur_price * rates.get('GBP', 1):,.0f}")
    c3.metric("NOK kr", f"{eur_price * rates.get('NOK', 1):,.0f}")
except Exception:
    st.info("Exchange rates unavailable — converter offline.")

st.divider()

# ---- Cost of living reference -----------------------------------------------
st.subheader("Cost of living reference")
st.caption("Rough monthly estimates for a couple. Update these figures as needed.")

cost_data = {
    "Item":           ["Rent 2-bed (Porto)",  "Rent 2-bed (Lisbon)", "Groceries",
                       "Utilities",            "Dining out (2x/week)","Health insurance",
                       "Public transport"],
    "Monthly (EUR)":  [1_200,                  1_800,                  400,
                       100,                    200,                    150,
                       80],
}
import pandas as pd
st.dataframe(pd.DataFrame(cost_data), use_container_width=True, hide_index=True)

st.divider()

# ---- Climate snapshot -------------------------------------------------------
st.subheader("Climate snapshot")
st.caption("Average annual sunshine hours by region.")
climate = pd.DataFrame({
    "Region":           ["Algarve", "Lisbon", "Porto"],
    "Sunshine hrs/yr":  [3_300,      2_800,    2_400],
    "Avg summer (°C)":  [27,         26,       22],
    "Avg winter (°C)":  [16,         13,       10],
})
st.dataframe(climate, use_container_width=True, hide_index=True)
