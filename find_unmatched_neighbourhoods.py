#!/usr/bin/env python3
"""
Find all currently unmatched neighbourhoods in the database.
Run locally: python find_unmatched_neighbourhoods.py
"""
import json
import os
import sys

try:
    import psycopg2
except ImportError:
    import subprocess
    subprocess.check_call([sys.executable, "-m", "pip", "install", "psycopg2-binary", "-q"])
    import psycopg2

# Load lookup
with open('dashboard/static/neighbourhood_lookup.json', 'r', encoding='utf-8') as f:
    lookup = json.load(f)
lookup_names = set(lookup.keys())

# Get credentials
dbname = os.getenv('NEON_DBNAME')
host = os.getenv('NEON_HOST')
user = os.getenv('NEON_USER')
password = os.getenv('NEON_PASSWORD')

if not all([dbname, host, user, password]):
    print("Error: Database credentials not set")
    sys.exit(1)

try:
    conn = psycopg2.connect(
        dbname=dbname, host=host, user=user, password=password, sslmode='allow'
    )
    cur = conn.cursor()

    # Get all unmatched neighbourhoods
    cur.execute("""
        SELECT neighbourhood, COUNT(*) as count
        FROM ads_buy
        WHERE is_active::integer = 1 AND duplicate_flag::integer = 0
        GROUP BY neighbourhood
        UNION ALL
        SELECT neighbourhood, COUNT(*) as count
        FROM ads_rent
        WHERE is_active::integer = 1 AND duplicate_flag::integer = 0
        GROUP BY neighbourhood
    """)

    unmatched = {}
    for nb, count in cur.fetchall():
        if nb not in lookup_names:
            if nb in unmatched:
                unmatched[nb] += count
            else:
                unmatched[nb] = count

    conn.close()

    # Sort by count
    unmatched = dict(sorted(unmatched.items(), key=lambda x: x[1], reverse=True))

    print("=== ALL UNMATCHED NEIGHBOURHOODS ===\n")
    print(f"Total unmatched: {len(unmatched)}\n")

    total_count = sum(unmatched.values())
    print(f"Listings in unmatched neighbourhoods: {total_count:,}\n")

    # Show all (or first 100 if too many)
    limit = min(100, len(unmatched))
    print(f"Showing first {limit}:\n")

    for i, (nb, count) in enumerate(list(unmatched.items())[:limit], 1):
        pct = 100 * count / total_count
        nb_display = nb if nb else "(NULL)"
        print(f"{i:3d}. {nb_display:40s} {count:>6,} listings ({pct:>5.2f}%)")

except Exception as e:
    print(f"Error: {e}")
    sys.exit(1)
