import streamlit as st
from utils.db import get_exchange_rates

_SYMBOLS = {
    "EUR": "€",
    "GBP": "£",
    "USD": "$",
    "NOK": "kr",
    "SEK": "kr",
    "DKK": "kr",
}

_CURRENCIES = list(_SYMBOLS.keys())


def render_currency_selector():
    """
    Render the currency selector in the sidebar.
    Returns (rate, symbol, fmt_price) where:
      - rate      : float — multiply any EUR amount by this to convert
      - symbol    : str   — currency symbol for labels
      - fmt_price : callable(eur_value, decimals=0) -> str
    """
    ecb_rates = get_exchange_rates()
    rates = {"EUR": 1.0, **ecb_rates}

    currency = st.sidebar.selectbox(
        "Currency",
        _CURRENCIES,
        index=0,
        key="currency_select",
    )

    rate   = rates.get(currency, 1.0)
    symbol = _SYMBOLS[currency]

    def fmt_price(eur_value, decimals=0):
        try:
            converted = float(eur_value) * rate
            return f"{symbol}{converted:,.{decimals}f}"
        except (TypeError, ValueError):
            return "—"

    return rate, symbol, fmt_price
