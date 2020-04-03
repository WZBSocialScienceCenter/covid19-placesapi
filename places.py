import pickle
import math
import json
import os
import sys
from datetime import datetime

import pandas as pd
import googlemaps
import populartimes

from apikeys import API_KEY

#%%

PLACE_SEARCHES = [
    ('restaurant', 'restaurant'),
    ('bar', 'bar'),
    ('club', 'night_club'),
    ('train station', 'train_station'),
    ('tourist information', None),
    ('sights', 'tourist_attraction'),
    ('park', 'park'),
    ('mall', 'shopping_mall'),
    ('shopping', 'shopping_mall'),
    ('supermarket', 'supermarket'),
    ('street market', None),
    ('hardware store', 'hardware_store')
]

PLACE_SEARCH_RADIUS = 20000  # in meters
LIMIT_NUM_PLACES = 20

RESULT_FILE = 'data/pois/%s.csv'
RESULT_POP_FILE = 'data/pois/%s_pop.json'
QUERIED_FILE = 'data/places_of_interest_queried_cities.pickle'

#%%

def haversine(a_lat, a_lng, b_lat, b_lng):
    """
    haversine: calculate great circle distance between two points on earth in km
    """

    R = 6371     # earth radius in km

    a_lat = math.radians(a_lat)
    a_lng = math.radians(a_lng)
    b_lat = math.radians(b_lat)
    b_lng = math.radians(b_lng)

    d_lat = b_lat - a_lat
    d_lng = b_lng - a_lng

    a = math.pow(math.sin(d_lat / 2), 2) + math.cos(a_lat) * math.cos(b_lat) * math.pow(math.sin(d_lng / 2), 2)
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))

    return R * c

#%%
if len(sys.argv) >= 2:
    t_start_ymdh = sys.argv[1]
    assert len(t_start_ymdh) == 14

    skip_already_queried_cities = len(sys.argv) == 3 and sys.argv[2] == 'skip_queried_cities'

    if skip_already_queried_cities:
        print('will skip already queried cities')
else:
    t_start_ymdh = datetime.now().strftime('%Y-%m-%d_h%H')
    skip_already_queried_cities = False

result_file = RESULT_FILE % t_start_ymdh
result_pop_file = RESULT_POP_FILE % t_start_ymdh

gmaps = googlemaps.Client(key=API_KEY)

cities = pd.read_csv('data/cities_edited.csv')

if os.path.exists(result_file):
    print('loading existing POI CSV file', result_file)
    existing_pois = pd.read_csv(result_file)
    existing_place_ids = set(existing_pois.place_id)
    print('> %d existing places' % len(existing_place_ids))
    existing_queried_cities = set(existing_pois.city)
    print('> %d existing cities' % len(existing_queried_cities))
else:
    existing_pois = None
    existing_place_ids = set()
    existing_queried_cities = set()

if os.path.exists(result_pop_file):
    print('loading existing POI initial popularity score JSON file', result_file)
    with open(result_pop_file) as f:
        resultrows_pop = json.load(f)
    print('> %d existing place popularity score entries' % len(resultrows_pop))
else:
    resultrows_pop = []

try:
    with open(QUERIED_FILE, mode='rb') as f:
        queried_cities = pickle.load(f)
except FileNotFoundError:
    queried_cities = []

#%%

print('querying places in cities ...')

resultrows = []
for city_i, cityrow in cities.iterrows():
    print('> city %d/%d: %s' % (city_i+1, len(cities), cityrow.city))

    if skip_already_queried_cities and cityrow.city in existing_queried_cities:
        print('> skipping (already queried this city)')
        continue

    for place_query, place_type in PLACE_SEARCHES:
        utcnow = datetime.utcnow()

        query_id = t_start_ymdh + cityrow.city + cityrow.country + place_query
        if query_id in queried_cities:
            print('>> skipping (already queried this city for this kind of places)')
            continue

        kwargs = {}
        if place_type is not None:
            kwargs['type'] = place_type

        full_query = place_query + ' in ' + cityrow.city + ', ' + cityrow.country
        print('>> query: "%s" in lat=%.4f, lng=%.4f' % (full_query, cityrow.lat, cityrow.lng))

        places = gmaps.places(query=full_query, location=(cityrow.lat, cityrow.lng), radius=PLACE_SEARCH_RADIUS,
                              open_now=True, **kwargs)

        if places['status'] != 'OK':
            print('>> skipping (bad status: %s)' % places['status'])
            continue

        print('>> got %d results' % len(places['results']))
        queried_cities.append(query_id)

        n_pois = 0
        for i_place, place in enumerate(places['results']):
            if i_place >= LIMIT_NUM_PLACES:
                break

            print('>>> place: %s' % place['name'])

            place_lat, place_lng = place['geometry']['location']['lat'], place['geometry']['location']['lng']
            dist = haversine(cityrow.lat, cityrow.lng, place_lat, place_lng)
            if dist > (PLACE_SEARCH_RADIUS / 1000) * 2:    # accept larger radius here
                print('>> found place is out of search radius (distance is %.2f)' % dist)
                continue

            if place['place_id'] in existing_place_ids:
                print('>> skipping (already queried place with ID %s)' % place['place_id'])
                continue

            poptimes = populartimes.get_id(api_key=API_KEY, place_id=place['place_id'])

            if 'current_popularity' in poptimes and 'populartimes' in poptimes:
                print('>>>> adding this place as place of interest')

                resultrows.append(cityrow.to_list() + [
                    place_query,
                    place['place_id'],
                    place['name'],
                    place.get('formatted_address', ''),
                    place['geometry']['location']['lat'],
                    place['geometry']['location']['lng']
                ])

                resultrows_pop.append([
                    place['place_id'],
                    utcnow.strftime('%Y-%m-%d'),
                    utcnow.hour,
                    poptimes['current_popularity'],
                    poptimes['populartimes']
                ])

                existing_place_ids.add(place['place_id'])
                n_pois += 1

        print('>> got %d places of interest for this city and query' % n_pois)

    print('preparing and storing dataset')

    places_of_interest = pd.DataFrame(resultrows, columns=cities.columns.to_list() + [
        'query', 'place_id', 'name', 'addr', 'place_lat', 'place_lng'
    ])

    if existing_pois is not None:
        places_of_interest = pd.concat((existing_pois, places_of_interest), ignore_index=True)

    places_of_interest = places_of_interest \
        .drop_duplicates(['city', 'country', 'iso2', 'query', 'place_id'])\
        .sort_values(by=['country', 'city', 'query', 'name'])\
        .reset_index(drop=True)

    print('got %d places of interest so far' % len(places_of_interest))

    places_of_interest.to_csv(result_file, index=False)

    with open(result_pop_file, 'w') as f:
        json.dump(resultrows_pop, f, indent=2)

    with open(QUERIED_FILE, mode='wb') as f:
        pickle.dump(queried_cities, f)

    print('\n')

print('done.')
