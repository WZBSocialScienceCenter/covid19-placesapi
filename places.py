from datetime import datetime

import pandas as pd
import googlemaps
import populartimes

from apikeys import API_KEY


PLACE_SEARCHES = [
    'restaurant',
    'bar',
    'club',
    'main station',
    'tourist information',
    'sights',
    'park',
    'mall',
    'supermarket',
    'grocery store',
    'hardware store'
]

PLACE_SEARCH_RADIUS = 3000  # in meters

#%%

gmaps = googlemaps.Client(key=API_KEY)

#%%

cities = pd.read_csv('data/cities.csv')
resultrows = []

cityrow = cities.loc[cities.city == 'Berlin', :].iloc[0]
cityrow


place_query = PLACE_SEARCHES[5]
place_query

places = gmaps.places(query=place_query, location=(cityrow.lat, cityrow.lng), radius=PLACE_SEARCH_RADIUS)

places['status'] == 'OK'

place = places['results'][0]

for place in places['results']:
    poptimes = populartimes.get_id(api_key=API_KEY, place_id=place['place_id'])

    now = datetime.now()

    if 'current_popularity' in poptimes and 'populartimes' in poptimes:
        resultrows.append(cityrow.to_list() + [
            place_query,
            place['place_id'],
            place['name'],
            place['formatted_address'],
            place['geometry']['location']['lat'],
            place['geometry']['location']['lng'],
            poptimes['current_popularity'],
            poptimes['populartimes'][now.weekday()]['data'][now.hour]       # corresponding "usual" popularity
        ])
