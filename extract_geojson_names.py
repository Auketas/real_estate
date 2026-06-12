import json

regions = {
    'porto': 'dashboard/static/porto_region.geojson',
    'lisboa': 'dashboard/static/lisboa_region.geojson',
    'algarve': 'dashboard/static/algarve.geojson',
    'almada': 'dashboard/static/almada.geojson'
}

for region_name, filepath in regions.items():
    with open(filepath) as f:
        geojson = json.load(f)

    print(f"\n=== {region_name.upper()} GEOJSON FEATURES ({len(geojson['features'])} total) ===")
    for feature in sorted(geojson['features'], key=lambda x: x['properties'].get('NAME_3', x['properties'].get('NAME_2', ''))):
        name = feature['properties'].get('NAME_3') or feature['properties'].get('NAME_2')
        if name:
            print(name)
