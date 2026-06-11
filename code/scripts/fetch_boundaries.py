"""
One-off script to download Portugal administrative boundaries from GADM
and save GeoJSON files used by the neighbourhood deep-dive map.

Run from the repo root:
    python code/scripts/fetch_boundaries.py

Output files:
    dashboard/static/algarve.geojson        — 6 Algarve municipalities (level 2)
    dashboard/static/porto_region.geojson   — Porto + VNG + Matosinhos parishes (level 3)
    dashboard/static/lisboa_region.geojson  — Lisboa + Cascais + Sintra parishes (level 3)
    dashboard/static/setubal.geojson        — Almada + Costa da Caparica + Caparica e Trafaria parishes (level 3)
"""

import io
import json
import os
import unicodedata
import zipfile

import requests

GADM_BASE = "https://geodata.ucdavis.edu/gadm/gadm4.1/json"
OUT_DIR   = os.path.join(os.path.dirname(__file__), "..", "..", "dashboard", "static")


def normalise(s):
    """Lowercase + strip accents + strip spaces for fuzzy name matching."""
    s = unicodedata.normalize("NFD", s.lower()).encode("ascii", "ignore").decode()
    return s.replace(" ", "")


def download_gadm(level):
    url = f"{GADM_BASE}/gadm41_PRT_{level}.json.zip"
    print(f"Downloading GADM level {level} from {url} ...")
    r = requests.get(url, timeout=180)
    r.raise_for_status()
    with zipfile.ZipFile(io.BytesIO(r.content)) as z:
        name = next(n for n in z.namelist() if n.endswith(".json"))
        with z.open(name) as f:
            return json.load(f)


def filter_by_municipality(geojson, municipalities):
    """Keep features whose NAME_2 (municipality) matches the given list."""
    targets = {normalise(m) for m in municipalities}
    features = [
        f for f in geojson["features"]
        if normalise(f["properties"].get("NAME_2", "")) in targets
    ]
    return {"type": "FeatureCollection", "features": features}


def filter_by_name2(geojson, municipalities):
    """Keep features whose NAME_2 matches — for level 2 (municipality polygons)."""
    targets = {normalise(m) for m in municipalities}
    features = [
        f for f in geojson["features"]
        if normalise(f["properties"].get("NAME_2", "")) in targets
    ]
    return {"type": "FeatureCollection", "features": features}


def save(geojson, filename):
    os.makedirs(OUT_DIR, exist_ok=True)
    path = os.path.join(OUT_DIR, filename)
    with open(path, "w", encoding="utf-8") as f:
        json.dump(geojson, f, ensure_ascii=False)
    print(f"  Saved {len(geojson['features'])} features -> {path}")


def print_names(geojson, name_key, label):
    names = sorted({f["properties"].get(name_key, "") for f in geojson["features"]})
    print(f"\n  {label} feature names ({name_key}):")
    for n in names:
        print(f"    {n}")


if __name__ == "__main__":
    # ---- Algarve: municipality polygons (level 2) ---------------------------
    level2 = download_gadm(2)
    algarve = filter_by_name2(level2, [
        "Albufeira", "Faro", "Lagoa", "Lagos", "Loulé", "Portimão"
    ])
    save(algarve, "algarve.geojson")
    print_names(algarve, "NAME_2", "Algarve")

    # ---- Porto + VNG + Matosinhos: parish polygons (level 3) ----------------
    level3 = download_gadm(3)
    porto = filter_by_municipality(level3, [
        "Porto", "Vila Nova de Gaia", "Matosinhos"
    ])
    save(porto, "porto_region.geojson")
    print_names(porto, "NAME_3", "Porto region parishes")

    # ---- Lisboa + Cascais + Sintra: parish polygons (level 3) ---------------
    lisboa = filter_by_municipality(level3, [
        "Lisboa", "Cascais", "Sintra"
    ])
    save(lisboa, "lisboa_region.geojson")
    print_names(lisboa, "NAME_3", "Lisboa region parishes")

    # ---- Almada + Costa da Caparica + Caparica e Trafaria: parish polygons (level 3) ----
    setubal = filter_by_municipality(level3, [
        "Almada", "Costa da Caparica", "Caparica e Trafaria"
    ])
    save(setubal, "setubal.geojson")
    print_names(setubal, "NAME_3", "Setúbal region parishes")

    print("\nDone. Check the NAME_3 lists above against the DB neighbourhood")
    print("names to build the lookup table in dashboard/static/neighbourhood_lookup.json")
