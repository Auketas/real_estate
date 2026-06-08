#!/usr/bin/env python3
"""
Check neighbourhood mapping coverage - run locally with database credentials.
"""
import json
import os
import sys

try:
    import psycopg2
except ImportError:
    print("Installing psycopg2...")
    import subprocess
    subprocess.check_call([sys.executable, "-m", "pip", "install", "psycopg2-binary", "-q"])
    import psycopg2

# Load the lookup
with open('dashboard/static/neighbourhood_lookup.json', 'r', encoding='utf-8') as f:
    lookup = json.load(f)

lookup_names = set(lookup.keys())
print(f"Loaded {len(lookup_names)} neighbourhood mappings\n")

# Check environment variables
dbname = os.getenv('NEON_DBNAME')
host = os.getenv('NEON_HOST')
user = os.getenv('NEON_USER')
password = os.getenv('NEON_PASSWORD')

if not all([dbname, host, user, password]):
    print("Error: Database credentials not set in environment.")
    print("\nSet these environment variables:")
    print("  NEON_DBNAME")
    print("  NEON_HOST")
    print("  NEON_USER")
    print("  NEON_PASSWORD")
    sys.exit(1)

try:
    conn = psycopg2.connect(
        dbname=dbname,
        host=host,
        user=user,
        password=password,
        sslmode='allow'
    )
    cur = conn.cursor()

    # Query all active, non-duplicate listings
    cur.execute("""
        SELECT neighbourhood, COUNT(*) as count
        FROM ads_buy
        WHERE is_active = 1 AND duplicate_flag = 0
        GROUP BY neighbourhood
        UNION ALL
        SELECT neighbourhood, COUNT(*) as count
        FROM ads_rent
        WHERE is_active = 1 AND duplicate_flag = 0
        GROUP BY neighbourhood
    """)

    listings_by_neighbourhood = {}
    for neighbourhood, count in cur.fetchall():
        if neighbourhood in listings_by_neighbourhood:
            listings_by_neighbourhood[neighbourhood] += count
        else:
            listings_by_neighbourhood[neighbourhood] = count

    conn.close()

    # Calculate stats
    total_listings = sum(listings_by_neighbourhood.values())
    matched_listings = sum(
        count for nb, count in listings_by_neighbourhood.items()
        if nb in lookup_names
    )
    unmatched_listings = total_listings - matched_listings
    match_percentage = 100 * matched_listings / total_listings if total_listings > 0 else 0

    # Find top unmatched neighbourhoods
    unmatched_nbs = {
        nb: count for nb, count in listings_by_neighbourhood.items()
        if nb not in lookup_names
    }
    unmatched_nbs = dict(sorted(unmatched_nbs.items(), key=lambda x: x[1], reverse=True))

    # Print results
    print("=" * 60)
    print("NEIGHBOURHOOD MAPPING COVERAGE")
    print("=" * 60 + "\n")
    print(f"Total active listings: {total_listings:,}")
    print(f"Matched to parishes:   {matched_listings:,} ({match_percentage:.1f}%)")
    print(f"Unmatched:             {unmatched_listings:,} ({100-match_percentage:.1f}%)\n")

    if unmatched_nbs:
        print(f"Top unmatched neighbourhoods:\n")
        for i, (nb, count) in enumerate(list(unmatched_nbs.items())[:20], 1):
            pct = 100 * count / total_listings
            print(f"{i:2d}. {nb:35s} {count:>6,} listings ({pct:>5.2f}%)")
    else:
        print("All neighbourhoods are mapped!")

except psycopg2.OperationalError as e:
    print(f"Database connection error: {e}")
    sys.exit(1)
except Exception as e:
    print(f"Error: {e}")
    import traceback
    traceback.print_exc()
    sys.exit(1)
