#!/usr/bin/env python3
"""
Analyse neighbourhood coverage by checking if neighbourhoods match actual GeoJSON polygons.

Shows per region:
- % of listings with NULL neighbourhood
- % of listings whose neighbourhood is an actual polygon name from the GeoJSON
- % of listings whose neighbourhood doesn't exist in the GeoJSON (data issue)
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
        'cities': ['porto', 'matosinhos', 'vila_nova_de_gaia', 'vila-nova-de-gaia', 'maia'],
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
        'feature_key': 'NAME_2'
    },
    'algarve': {
        'geojson': 'dashboard/static/algarve.geojson',
        'cities': ['albufeira', 'faro', 'lagoa', 'lagos', 'loule', 'loulé', 'portimao', 'portimão'],
        'feature_key': 'NAME_2'
    }
}

# ============================================================================
# Extract polygon names from GeoJSON files
# ============================================================================
print("\n" + "=" * 80)
print("LOADING GEOJSON POLYGON NAMES")
print("=" * 80)

POLYGON_NAMES = {}  # region -> set of polygon names

for region, config in REGION_CONFIG.items():
    filepath = config['geojson']
    if os.path.exists(filepath):
        with open(filepath, encoding='utf-8') as f:
            geojson = json.load(f)

        names = set()
        for feature in geojson['features']:
            name = feature['properties'].get(config['feature_key'])
            if name:
                names.add(name)

        POLYGON_NAMES[region] = names
        print(f"✓ {region}: {len(names)} polygons")
    else:
        print(f"✗ {region}: GeoJSON file not found")


def get_city_region(city):
    """Determine which region a city belongs to."""
    for region, config in REGION_CONFIG.items():
        if city in config['cities']:
            return region
    return None


# ============================================================================
# CHECK 1: NULL/EMPTY NEIGHBOURHOODS
# ============================================================================
print("\n" + "=" * 80)
print("CHECK 1: LISTINGS WITH NULL/EMPTY NEIGHBOURHOOD")
print("=" * 80)

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
# CHECK 2: NEIGHBOURHOODS MATCHING ACTUAL GEOJSON POLYGONS
# ============================================================================
print("\n" + "=" * 80)
print("CHECK 2: NEIGHBOURHOODS MATCHING GEOJSON POLYGONS (per region)")
print("=" * 80)

# Get all unique (neighbourhood, city) pairs
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

# Count listings per neighbourhood+city
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

# Analyze per region
print("\nSUMMARY BY REGION:")
print("-" * 80)

region_stats = {}

for region, config in REGION_CONFIG.items():
    polygon_names = POLYGON_NAMES.get(region, set())
    cities = config['cities']

    matched_neighbourhoods = 0
    unmatched_neighbourhoods = 0
    matched_listings = 0
    unmatched_listings = 0

    region_city_stats = {}

    for city in cities:
        if city not in neighbourhoods_by_city:
            continue

        city_matched = 0
        city_unmatched = 0
        city_matched_list = 0
        city_unmatched_list = 0

        for neighbourhood in neighbourhoods_by_city[city]:
            count = listing_counts.get((neighbourhood, city), 0)

            if neighbourhood in polygon_names:
                city_matched += 1
                city_matched_list += count
            else:
                city_unmatched += 1
                city_unmatched_list += count

        matched_neighbourhoods += city_matched
        unmatched_neighbourhoods += city_unmatched
        matched_listings += city_matched_list
        unmatched_listings += city_unmatched_list

        region_city_stats[city] = {
            'matched_neighbourhoods': city_matched,
            'unmatched_neighbourhoods': city_unmatched,
            'matched_listings': city_matched_list,
            'unmatched_listings': city_unmatched_list
        }

    region_stats[region] = {
        'matched_neighbourhoods': matched_neighbourhoods,
        'unmatched_neighbourhoods': unmatched_neighbourhoods,
        'matched_listings': matched_listings,
        'unmatched_listings': unmatched_listings,
        'city_stats': region_city_stats,
        'polygon_count': len(polygon_names)
    }

    total_neighbourhoods = matched_neighbourhoods + unmatched_neighbourhoods
    total_listings = matched_listings + unmatched_listings

    print(f"\n{region.upper()}")
    print(f"  GeoJSON polygons available: {len(polygon_names)}")
    print(f"  Neighbourhoods in data: {total_neighbourhoods}")
    print(f"    → Matching GeoJSON: {matched_neighbourhoods} ({100.0 * matched_neighbourhoods / total_neighbourhoods if total_neighbourhoods > 0 else 0:.1f}%)")
    print(f"    → Not in GeoJSON: {unmatched_neighbourhoods} ({100.0 * unmatched_neighbourhoods / total_neighbourhoods if total_neighbourhoods > 0 else 0:.1f}%)")

    if total_listings > 0:
        print(f"  Listings: {total_listings:,}")
        print(f"    → In matched neighbourhoods: {matched_listings:,} ({100.0 * matched_listings / total_listings:.1f}%)")
        print(f"    → In unmatched neighbourhoods: {unmatched_listings:,} ({100.0 * unmatched_listings / total_listings:.1f}%)")

    # Show city breakdown
    if region_city_stats:
        print(f"  By city:")
        for city in sorted(region_city_stats.keys()):
            stats = region_city_stats[city]
            city_total = stats['matched_listings'] + stats['unmatched_listings']
            if city_total > 0:
                city_matched_pct = 100.0 * stats['matched_listings'] / city_total
                print(f"    {city:20s}: {stats['matched_listings']:6,} matched ({city_matched_pct:5.1f}%) / {stats['unmatched_listings']:6,} unmatched")


# ============================================================================
# OVERALL SUMMARY
# ============================================================================
print("\n" + "=" * 80)
print("OVERALL SUMMARY")
print("=" * 80)

total_with_neighbourhood = sum(r['matched_listings'] + r['unmatched_listings'] for r in region_stats.values())
total_matched = sum(r['matched_listings'] for r in region_stats.values())
total_unmatched = sum(r['unmatched_listings'] for r in region_stats.values())

print(f"\nListings with neighbourhood assigned: {total_with_neighbourhood:,}")
if total_with_neighbourhood > 0:
    print(f"  → In GeoJSON polygons: {total_matched:,} ({100.0 * total_matched / total_with_neighbourhood:.1f}%)")
    print(f"  → Not in GeoJSON: {total_unmatched:,} ({100.0 * total_unmatched / total_with_neighbourhood:.1f}%)")

print(f"\nListings with NULL neighbourhood: {combined_null:,} ({combined_pct:.2f}%)")

grand_total = combined_null + total_with_neighbourhood
if grand_total > 0:
    print(f"\nTOTAL LISTINGS MISSING FROM POLYGON MAPS:")
    missing = combined_null + total_unmatched
    print(f"  {missing:,} ({100.0 * missing / grand_total:.1f}%)")

conn.close()
