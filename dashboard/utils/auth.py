from pathlib import Path
import streamlit as st
import streamlit_authenticator as stauth
import yaml
from yaml.loader import SafeLoader


def load_authenticator():
    config_path = Path(__file__).parent.parent / "config.yaml"
    with open(config_path) as f:
        config = yaml.load(f, Loader=SafeLoader)
    return stauth.Authenticate(
        config["credentials"],
        config["cookie"]["name"],
        config["cookie"]["key"],
        config["cookie"]["expiry_days"],
    )


def require_auth():
    """
    Call this at the top of every page.
    If the user is not logged in, stop rendering and show a message.
    """
    if not st.session_state.get("authentication_status"):
        st.warning("Please log in from the Home page.")
        st.stop()
