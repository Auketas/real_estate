#!/usr/bin/env python3
"""
Analyse neighbourhood coverage in the real estate database.

Checks:
1. What percentage of listings have NULL/empty neighbourhood
2. What percentage of listings have a neighbourhood that can't be matched to GeoJSON polygons

The script loads all GeoJSON files (used by the dashboard for maps), extracts the features
available in each region, and checks if each database neighbourhood is present.

Regions are mapped as follows:
- Porto, Matosinhos, Vila Nova de Gaia, Maia: porto_region.geojson
- Lisboa, Cascais, Sintra: lisboa_region.geojson
- Almada: almada.geojson
- Albufeira, Faro, Lagoa, Lagos, Loulé, Portimão: algarve.geojson (uses NAME_2)
"""

import json
import psycopg2
import os
from collections import defaultdict

# Database connection from environment variables
conn = psycopg2.connect(
    dbname=os.getenv("NEON_DBNAME"),
    host=os.getenv("NEON_HOST"),
    user=os.getenv("NEON_USER"),
    password=os.getenv("NEON_PASSWORD"),
    port=5432,
    sslmode="require"
)

# Map regions to their GeoJSON files and cities
REGION_CONFIG = {
    'porto': {
        'geojson': 'dashboard/static/porto_region.geojson',
        'cities': ['porto', 'matosinhos', 'vila_nova_de_gaia', 'maia'],
        'feature_key': 'NAME_3'
    },
    'lisboa': {
        'geojson': 'dashboard/static/lisboa_region.geojson',
        'cities': ['lisboa', 'cascais', 'sintra'],
        'feature_key': 'NAME_3'
    },
    'almada': {
        'geojson': 'dashboard/static/almada.geojson',
        'cities': ['almada'],
        'feature_key': 'NAME_3'
    },
    'algarve': {
        'geojson': 'dashboard/static/algarve.geojson',
        'cities': ['albufeira', 'faro', 'lagoa', 'lagos', 'loule', 'portimao'],
        'feature_key': 'NAME_2'
    }
}

# Load neighbourhood lookup mapping scraper names to GeoJSON features
with open('dashboard/static/neighbourhood_lookup.json') as f:
    LOOKUP = json.load(f)

# Extract available features from all GeoJSON files
GEOJSON_FEATURES = {}
for region, config in REGION_CONFIG.items():
    with open(config['geojson']) as f:
        geojson = json.load(f)
    features = set()
    for feature in geojson['features']:
        name = feature['properties'].get(config['feature_key'])
        if name:
            features.add(name)
    GEOJSON_FEATURES[region] = features
    print(f"\n{region.upper()}: {len(features)} GeoJSON features loaded")


def get_city_region(city):
    """Determine which region a city belongs to."""
    for region, config in REGION_CONFIG.items():
        if city in config['cities']:
            return region
    return None


# ============================================================================
# CHECK 1: NULL/EMPTY NEIGHBOURHOODS
# ============================================================================
print("\n" + "="*80)
print("CHECK 1: LISTINGS WITH NULL/EMPTY NEIGHBOURHOOD")
print("="*80)

cur = conn.cursor()

# Buy listings
cur.execute("""
SELECT
    COUNT(*) as total,
    COUNT(CASE WHEN neighbourhood IS NULL OR neighbourhood = '' THEN 1 END) as null_count
FROM ads_buy
WHERE is_active = 1
""")
buy_total, buy_null = cur.fetchone()
buy_pct = 100.0 * buy_null / buy_total if buy_total > 0 else 0

print(f"\nBUY LISTINGS:")
print(f"  Total: {buy_total:,}")
print(f"  NULL/empty: {buy_null:,} ({buy_pct:.2f}%)")

# Rent listings
cur.execute("""
SELECT
    COUNT(*) as total,
    COUNT(CASE WHEN neighbourhood IS NULL OR neighbourhood = '' THEN 1 END) as null_count
FROM ads_rent
WHERE is_active = 1
""")
rent_total, rent_null = cur.fetchone()
rent_pct = 100.0 * rent_null / rent_total if rent_total > 0 else 0

print(f"\nRENT LISTINGS:")
print(f"  Total: {rent_total:,}")
print(f"  NULL/empty: {rent_null:,} ({rent_pct:.2f}%)")

print(f"\nCOMBINED:")
combined_total = buy_total + rent_total
combined_null = buy_null + rent_null
combined_pct = 100.0 * combined_null / combined_total if combined_total > 0 else 0
print(f"  Total: {combined_total:,}")
print(f"  NULL/empty: {combined_null:,} ({combined_pct:.2f}%)")


# ============================================================================
# CHECK 2: NEIGHBOURHOODS NOT MATCHING GEOJSON FEATURES
# ============================================================================
print("\n" + "="*80)
print("CHECK 2: LISTINGS WITH UNMATCHED NEIGHBOURHOODS (not in GeoJSON)")
print("="*80)

# Get all unique neighbourhoods from database
cur.execute("""
SELECT DISTINCT neighbourhood, city
FROM (
    SELECT neighbourhood, city FROM ads_buy WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
    UNION
    SELECT neighbourhood, city FROM ads_rent WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
) t
ORDER BY city, neighbourhood
""")

neighbourhoods_by_city = defaultdict(list)
for neighbourhood, city in cur.fetchall():
    neighbourhoods_by_city[city].append(neighbourhood)

# Check which ones can't be matched to GeoJSON
unmatched_by_city = {}
matched_by_city = {}

for city, neighbourhoods in neighbourhoods_by_city.items():
    region = get_city_region(city)
    if not region:
        print(f"\n⚠️  UNKNOWN REGION for city: {city}")
        continue

    matched = []
    unmatched = []

    for neighbourhood in neighbourhoods:
        # Check if neighbourhood is in lookup
        if neighbourhood in LOOKUP:
            geojson_name = LOOKUP[neighbourhood]
            # Check if the mapped GeoJSON feature exists
            if geojson_name in GEOJSON_FEATURES[region]:
                matched.append(neighbourhood)
            else:
                unmatched.append((neighbourhood, f"lookup maps to '{geojson_name}' which doesn't exist in GeoJSON"))
        else:
            unmatched.append((neighbourhood, "not in lookup.json"))

    matched_by_city[city] = matched
    unmatched_by_city[city] = unmatched

# Count listings per neighbourhood
cur.execute("""
SELECT neighbourhood, city,
       (SELECT COUNT(*) FROM ads_buy WHERE is_active = 1 AND city = t.city AND neighbourhood = t.neighbourhood) +
       (SELECT COUNT(*) FROM ads_rent WHERE is_active = 1 AND city = t.city AND neighbourhood = t.neighbourhood) as count
FROM (
    SELECT DISTINCT neighbourhood, city FROM (
        SELECT neighbourhood, city FROM ads_buy WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
        UNION
        SELECT neighbourhood, city FROM ads_rent WHERE is_active = 1 AND neighbourhood IS NOT NULL AND neighbourhood != ''
    ) t
) t
ORDER BY city, neighbourhood
""")

listing_counts = {}
for neighbourhood, city, count in cur.fetchall():
    listing_counts[(neighbourhood, city)] = count

# Report by city
print("\nSUMMARY BY CITY:")
print("-" * 80)

total_matched = 0
total_unmatched = 0
total_matched_listings = 0
total_unmatched_listings = 0

for city in sorted(neighbourhoods_by_city.keys()):
    region = get_city_region(city)
    if not region:
        continue

    matched = matched_by_city[city]
    unmatched = unmatched_by_city[city]

    matched_count = sum(listing_counts.get((n, city), 0) for n in matched)
    unmatched_count = sum(listing_counts.get((n, city), 0) for n, _ in unmatched)

    matched_pct = 100.0 * matched_count / (matched_count + unmatched_count) if (matched_count + unmatched_count) > 0 else 0

    print(f"\n{city.upper()} (region: {region})")
    print(f"  Matched neighbourhoods: {len(matched)}")
    print(f"  Unmatched neighbourhoods: {len(unmatched)}")
    if matched_count + unmatched_count > 0:
        print(f"  Matched listings: {matched_count:,} ({matched_pct:.1f}%)")
        print(f"  Unmatched listings: {unmatched_count:,} ({100-matched_pct:.1f}%)")

    total_matched += len(matched)
    total_unmatched += len(unmatched)
    total_matched_listings += matched_count
    total_unmatched_listings += unmatched_count

    # Show unmatched neighbourhoods if any
    if unmatched:
        print(f"  Unmatched: {', '.join([n for n, _ in unmatched[:5]])}" +
              ("..." if len(unmatched) > 5 else ""))

print("\n" + "="*80)
print("OVERALL SUMMARY")
print("="*80)

print(f"\nNeighbourhoods:")
print(f"  Total unique: {total_matched + total_unmatched}")
print(f"  Matched to GeoJSON: {total_matched}")
print(f"  Unmatched: {total_unmatched}")
if total_matched + total_unmatched > 0:
    print(f"  Match rate: {100.0 * total_matched / (total_matched + total_unmatched):.1f}%")

print(f"\nListings:")
print(f"  Total (non-NULL): {total_matched_listings + total_unmatched_listings:,}")
print(f"  Can be mapped to GeoJSON polygons: {total_matched_listings:,} ({100.0 * total_matched_listings / (total_matched_listings + total_unmatched_listings) if (total_matched_listings + total_unmatched_listings) > 0 else 0:.1f}%)")
print(f"  Cannot be mapped: {total_unmatched_listings:,} ({100.0 * total_unmatched_listings / (total_matched_listings + total_unmatched_listings) if (total_matched_listings + total_unmatched_listings) > 0 else 0:.1f}%)")

# Show most problematic unmatched neighbourhoods
print("\n" + "="*80)
print("MOST IMPACTFUL UNMATCHED NEIGHBOURHOODS (by listing count)")
print("="*80)

impact_list = []
for city, unmatched_list in unmatched_by_city.items():
    for neighbourhood, reason in unmatched_list:
        count = listing_counts.get((neighbourhood, city), 0)
        impact_list.append((count, city, neighbourhood, reason))

impact_list.sort(reverse=True)
for count, city, neighbourhood, reason in impact_list[:20]:
    print(f"{count:5d} listings  |  {city:15s}  |  {neighbourhood:30s}  ({reason})")

conn.close()
