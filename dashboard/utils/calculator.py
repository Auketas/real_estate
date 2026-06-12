"""
Price calculator: predict listing prices using hedonic regression coefficients.

User specifies optional features. Unspecified features are marginalized out using
feature prevalences from the training data, yielding wider confidence intervals.
"""

import numpy as np
import pandas as pd


def predict_price(
    inputs: dict,
    coefficients: pd.DataFrame,
    feature_stats: pd.DataFrame,
    metadata: pd.DataFrame,
) -> dict:
    """
    Predict price given user inputs and model coefficients.

    Args:
        inputs: dict like {
            "neighbourhood": "Baixa",
            "tipologia": "T2",
            "area": 80,
            "novo": True,
            "jardim": None,  # None = not specified
            "garagem": None,
            "terraco": None,
            "varanda": None,
        }
        coefficients: DataFrame from get_model_coefficients()
        feature_stats: DataFrame from get_model_feature_stats()
        metadata: DataFrame from get_model_metadata()

    Returns:
        dict with:
            - "predicted_log_price": log(price) from linear regression
            - "predicted_price": exp(log_price)
            - "variance": total variance (used for CI)
            - "ci_lower": lower bound of 95% CI
            - "ci_upper": upper bound of 95% CI
            - "n_observations": number of observations that trained the model
    """

    # Get metadata
    if metadata.empty:
        return {"error": "No model available for this city/type"}

    residual_std_error = metadata.iloc[0]["residual_std_error"]
    n_obs = metadata.iloc[0]["n_observations"]

    # Start with intercept
    coef_dict = {row["variable_name"]: row["coefficient"]
                 for _, row in coefficients.iterrows()}

    # R's lm() stores intercept as "(Intercept)" with parentheses
    intercept = coef_dict.get("(Intercept)", coef_dict.get("intercept", 0))
    predicted_log_price = intercept

    # Track total variance contribution
    variance = residual_std_error ** 2  # residual variance

    # Process each input
    for variable_name, value in inputs.items():
        if value is None:
            # Feature not specified: marginalize it out
            stats_row = feature_stats[feature_stats["variable_name"] == variable_name]
            if not stats_row.empty:
                feature_mean = stats_row.iloc[0]["feature_mean"]
                coef = coef_dict.get(variable_name, 0)

                # Add expected contribution: β × mean
                predicted_log_price += coef * feature_mean

                # Add variance: β² × p(1-p) for binary, β² × var for continuous
                # Assume binary for simplicity; if continuous, feature_mean is the actual mean
                p = feature_mean
                if 0 < p < 1:
                    # Binary feature: variance = β² × p(1-p)
                    variance += (coef ** 2) * p * (1 - p)
                # else: already baked into residual_std_error for continuous features
        else:
            # Feature specified: use its coefficient
            # Need to construct the variable name as it appears in the model
            # e.g., tipologia="T2" becomes "tipologia_T2", binary flags stay as-is

            if variable_name in ["novo", "jardim", "garagem", "terraco", "varanda"]:
                # Boolean/binary: model stores as "X_imp", value is 0 or 1
                coef_name = f"{variable_name}_imp"
                coef_value = value
            elif variable_name == "area":
                # Continuous: model stores as "area_imp"
                coef_name = "area_imp"
                coef_value = value
            else:
                # Categorical: construct encoded name (e.g., tipologia_fT2)
                # R's factor encoding adds 'f' to variable names
                coef_name = f"{variable_name}_f{value}"
                coef_value = 1

            coef = coef_dict.get(coef_name, 0)
            predicted_log_price += coef * coef_value

    # Convert from log scale
    predicted_price = np.exp(predicted_log_price)

    # Compute 50% confidence interval (practical range for user decisions)
    # CI = exp(log_price ± 0.67 × sqrt(variance))  [50% ≈ 0.67 SD]
    std_error = np.sqrt(variance)
    ci_lower_log = predicted_log_price - 0.67 * std_error
    ci_upper_log = predicted_log_price + 0.67 * std_error
    ci_lower = np.exp(ci_lower_log)
    ci_upper = np.exp(ci_upper_log)

    return {
        "predicted_log_price": predicted_log_price,
        "predicted_price": predicted_price,
        "variance": variance,
        "std_error": std_error,
        "ci_lower": ci_lower,
        "ci_upper": ci_upper,
        "n_observations": n_obs,
    }


def get_available_neighbourhoods(
    coefficients: pd.DataFrame,
) -> list:
    """Extract list of neighbourhoods that appear in the model coefficients."""
    neighbourhood_vars = coefficients[
        coefficients["variable_name"].str.startswith("neighbourhood_")
    ]["variable_name"].str.replace("neighbourhood_f", "").tolist()

    return sorted(neighbourhood_vars)


def get_available_tipologias(
    coefficients: pd.DataFrame,
) -> list:
    """Extract list of property types that appear in the model coefficients."""
    tipologia_vars = coefficients[
        coefficients["variable_name"].str.startswith("tipologia_")
    ]["variable_name"].str.replace("tipologia_f", "").tolist()

    return sorted(tipologia_vars)
