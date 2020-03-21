import pickle
import math
from datetime import datetime

import pandas as pd
import googlemaps
import populartimes

from apikeys import API_KEY

#%%

PLACE_SEARCHES = [
    'restaurant',
    'bar',
    'club',
    'train station',
    'tourist information',
    'sights',
    'park',
    'mall',
    'supermarket',
    'hardware store'
]

PLACE_SEARCH_RADIUS = 3000  # in meters
LIMIT_NUM_PLACES = 10

RESULT_FILE = 'data/places_of_interest.csv'
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

gmaps = googlemaps.Client(key=API_KEY)

cities = pd.read_csv('data/cities_edited.csv')
existing_pois = pd.read_csv(RESULT_FILE)
existing_place_ids = set(existing_pois.place_id)

try:
    with open(QUERIED_FILE, mode='rb') as f:
        queried_cities = pickle.load(f)
except FileNotFoundError:
    queried_cities = []

#%%

resultrows = []
for city_i, cityrow in cities.iterrows():
    print('> city %d/%d: %s' % (city_i+1, len(cities), cityrow.city))
    for place_query in PLACE_SEARCHES:
        full_query = place_query + ' in ' + cityrow.city + ', ' + cityrow.country
        print('>> query: "%s" in lat=%.4f, lng=%.4f' % (full_query, cityrow.lat, cityrow.lng))

        query_id = cityrow.city + cityrow.country + place_query
        if query_id in queried_cities:
            print('>> skipping (already queried this city for this kind of places)')

            # don't forget to re-add to results
            existing_rows = existing_pois.loc[(existing_pois.city == cityrow.city) &
                                              (existing_pois.country == cityrow.country) &
                                              (existing_pois['query'] == place_query), :]

            if len(existing_rows) > 0:
                resultrows.extend([row.to_list() for _, row in existing_rows.iterrows()])

            continue

        places = gmaps.places(query=full_query, location=(cityrow.lat, cityrow.lng), radius=PLACE_SEARCH_RADIUS)

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
                # don't forget to re-add to results
                resultrows.append(existing_pois.loc[existing_pois.place_id == place['place_id'], :].iloc[0].to_list())
                continue

            poptimes = populartimes.get_id(api_key=API_KEY, place_id=place['place_id'])

            if 'current_popularity' in poptimes and 'populartimes' in poptimes:
                print('>>>> adding this place as place of interest')
                now = datetime.now()

                resultrows.append(cityrow.to_list() + [
                    place_query,
                    place['place_id'],
                    place['name'],
                    place['formatted_address'],
                    place['geometry']['location']['lat'],
                    place['geometry']['location']['lng'],
                    # poptimes['current_popularity'],
                    # poptimes['populartimes'][now.weekday()]['data'][now.hour]       # corresponding "usual" popularity
                ])

                existing_place_ids.add(place['place_id'])
                n_pois += 1

        print('>> got %d places of interest for this city and query' % n_pois)

    print('got %d places of interest so far' % len(resultrows))

    print('preparing and storing dataset')

    places_of_interest = pd.DataFrame(resultrows, columns=cities.columns.to_list() + [
        'query', 'place_id', 'name', 'addr', 'place_lat', 'place_lng'
    ])

    places_of_interest.to_csv(RESULT_FILE, index=False)

    with open(QUERIED_FILE, mode='wb') as f:
        pickle.dump(queried_cities, f)
