#!/usr/bin/env python3
"""
Backfill neighbourhoods for all listings using spatial join (point-in-polygon).

This script goes through ALL listings in ads_buy and ads_rent and assigns them to
neighbourhoods by finding which GeoJSON polygon their coordinates fall inside.

Approach:
1. Load all GeoJSON files (porto, lisboa, algarve, almada)
2. For each listing with lon/lat, find the polygon it's inside
3. Use the polygon's NAME_3 (parishes) or NAME_2 (municipalities) as the neighbourhood
4. Update the database with the corrected neighbourhood

This is the same approach used by the scrapers for new listings.
"""

import json
import psycopg2
import os
from shapely.geometry import Point, shape
from shapely.strtree import STRtree

# Database connection
conn = psycopg2.connect(
    dbname=os.getenv("NEON_DBNAME"),
    host=os.getenv("NEON_HOST"),
    user=os.getenv("NEON_USER"),
    password=os.getenv("NEON_PASSWORD"),
    port=5432,
    sslmode="require"
)

# ============================================================================
# Load GeoJSON files
# ============================================================================
print("\n" + "=" * 80)
print("LOADING GEOJSON FILES")
print("=" * 80)

geojson_dir = "dashboard/static"
geojsons = {}

regions = {
    "porto": {
        "filename": "porto_region.geojson",
        "feature_key": "NAME_3"
    },
    "lisboa": {
        "filename": "lisboa_region.geojson",
        "feature_key": "NAME_3"
    },
    "algarve": {
        "filename": "algarve.geojson",
        "feature_key": "NAME_2"
    },
    "almada": {
        "filename": "almada.geojson",
        "feature_key": "NAME_2"
    }
}

for region_name, config in regions.items():
    filepath = os.path.join(geojson_dir, config["filename"])
    if os.path.exists(filepath):
        with open(filepath, encoding='utf-8') as f:
            geojson_data = json.load(f)

        # Convert GeoJSON features to shapely geometries with properties
        features = []
        for feature in geojson_data['features']:
            geom = shape(feature['geometry'])
            props = feature['properties']
            features.append({
                'geometry': geom,
                'name_3': props.get('NAME_3'),
                'name_2': props.get('NAME_2')
            })

        geojsons[region_name] = features
        print(f"✓ {region_name}: {len(features)} features")

# ============================================================================
# Function to assign neighbourhood via spatial join
# ============================================================================
def assign_neighbourhood_spatial(lon, lat):
    """Find which polygon a point is inside and return its NAME_3 or NAME_2."""
    if lon is None or lat is None:
        return None

    try:
        point = Point(lon, lat)

        for region_name, features in geojsons.items():
            for feature in features:
                if feature['geometry'].contains(point):
                    # Prefer NAME_3 (parishes), fall back to NAME_2 (municipalities)
                    name = feature['name_3'] or feature['name_2']
                    if name:
                        return name

        return None
    except Exception as e:
        return None

# ============================================================================
# Get listings with valid coordinates
# ============================================================================
print("\n" + "=" * 80)
print("PROCESSING LISTINGS")
print("=" * 80)

cur = conn.cursor()

# Get buy listings with coordinates
print("Fetching buy listings with coordinates...")
cur.execute("""
    SELECT id, lat, lon FROM ads_buy
    WHERE lat IS NOT NULL AND lon IS NOT NULL AND lat != 0 AND lon != 0
""")
buy_listings = cur.fetchall()
print(f"  Found {len(buy_listings)} buy listings with coordinates")

# Get rent listings with coordinates
print("Fetching rent listings with coordinates...")
cur.execute("""
    SELECT id, lat, lon FROM ads_rent
    WHERE lat IS NOT NULL AND lon IS NOT NULL AND lat != 0 AND lon != 0
""")
rent_listings = cur.fetchall()
print(f"  Found {len(rent_listings)} rent listings with coordinates")

# ============================================================================
# Backfill BUY listings
# ============================================================================
print("\n" + "=" * 80)
print("BACKFILLING BUY LISTINGS")
print("=" * 80)

buy_matched = 0
buy_unmatched = 0

for i, (listing_id, lat, lon) in enumerate(buy_listings):
    if (i + 1) % 5000 == 0:
        print(f"  Progress: {i + 1} / {len(buy_listings)}")

    neighbourhood = assign_neighbourhood_spatial(lon, lat)

    if neighbourhood:
        cur.execute(
            "UPDATE ads_buy SET neighbourhood = %s WHERE id = %s",
            (neighbourhood, listing_id)
        )
        buy_matched += 1
    else:
        buy_unmatched += 1

conn.commit()

print(f"\nBuy listings neighbourhood assignment results:")
print(f"  Matched: {buy_matched} ({100.0 * buy_matched / len(buy_listings):.1f}%)")
print(f"  Unmatched: {buy_unmatched} ({100.0 * buy_unmatched / len(buy_listings):.1f}%)")
print("✓ Buy listings updated")

# ============================================================================
# Backfill RENT listings
# ============================================================================
print("\n" + "=" * 80)
print("BACKFILLING RENT LISTINGS")
print("=" * 80)

rent_matched = 0
rent_unmatched = 0

for i, (listing_id, lat, lon) in enumerate(rent_listings):
    if (i + 1) % 5000 == 0:
        print(f"  Progress: {i + 1} / {len(rent_listings)}")

    neighbourhood = assign_neighbourhood_spatial(lon, lat)

    if neighbourhood:
        cur.execute(
            "UPDATE ads_rent SET neighbourhood = %s WHERE id = %s",
            (neighbourhood, listing_id)
        )
        rent_matched += 1
    else:
        rent_unmatched += 1

conn.commit()

print(f"\nRent listings neighbourhood assignment results:")
print(f"  Matched: {rent_matched} ({100.0 * rent_matched / len(rent_listings):.1f}%)")
print(f"  Unmatched: {rent_unmatched} ({100.0 * rent_unmatched / len(rent_listings):.1f}%)")
print("✓ Rent listings updated")

# ============================================================================
# Final summary
# ============================================================================
print("\n" + "=" * 80)
print("SUMMARY")
print("=" * 80)

total_matched = buy_matched + rent_matched
total_processed = len(buy_listings) + len(rent_listings)

print(f"\nTotal listings processed: {total_processed:,}")
print(f"Total matched to polygons: {total_matched:,} ({100.0 * total_matched / total_processed:.1f}%)")
print(f"Total still unmatched: {total_processed - total_matched:,} ({100.0 * (total_processed - total_matched) / total_processed:.1f}%)")

print("\nBackfill complete! Re-run the neighbourhood coverage analysis to verify results.")

cur.close()
conn.close()
