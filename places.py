import logging
from datetime import datetime

import pandas as pd
import googlemaps
import populartimes

from apikeys import API_KEY

logger = logging.getLogger('places')
logger.addHandler(logging.NullHandler())
logger.setLevel(logging.INFO)

#%%

PLACE_SEARCHES = [
    'restaurant',
    'bar',
    'gastro pub',
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

RESULT_FILE = 'data/places_of_interest.csv'

#%%

gmaps = googlemaps.Client(key=API_KEY)

#%%

cities = pd.read_csv('data/cities.csv')
existing_pois = pd.read_csv('data/places_of_interest.csv')
existing_place_ids = set(existing_pois.place_id)
resultrows = []

cityrow = cities.loc[cities.city == 'Berlin', :].iloc[0]
cityrow

logger.info('> city: %s' % cityrow.city)
for place_query in PLACE_SEARCHES:
    logger.info('>> query: %s' % place_query)

    # existing_places_mask = ((existing_pois.city == cityrow.city) &
    #                         (existing_pois.country == cityrow.country) &
    #                         (existing_pois['query'] == place_query))
    #
    # if sum(existing_places_mask) > 0:
    #     logger.info('>> skipping (already fetched %d places for this city and query)' % sum(existing_places_mask))
    #     # TODO
    #     continue

    places = gmaps.places(query=place_query, location=(cityrow.lat, cityrow.lng), radius=PLACE_SEARCH_RADIUS)

    if places['status'] != 'OK':
        logger.warning('>> skipping (bad status: %s)' % places['status'])
        continue

    logger.info('>> got %d results' % len(places['results']))
    n_pois = 0
    for place in places['results']:
        logger.info('>>> place: %s' % place['name'])

        if place['place_id'] in existing_place_ids:
            logger.info('>> skipping (already queried place with ID %s)' % place['place_id'])
            # don't forget to re-add to results
            resultrows.append(existing_pois.loc[existing_pois.place_id == place['place_id'], :].iloc[0].to_list())
            continue

        poptimes = populartimes.get_id(api_key=API_KEY, place_id=place['place_id'])

        if 'current_popularity' in poptimes and 'populartimes' in poptimes:
            logger.info('>>>> adding this place as place of interest')
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
            n_pois += 1

    logger.info('>> got %d places of interest for this city and query' % n_pois)

#%%

logger.info('got %d places of interest in total' % len(resultrows))

logger.info('preparing and storing dataset')

places_of_interest = pd.DataFrame(resultrows, columns=cities.columns.to_list() + [
    'query', 'place_id', 'name', 'addr', 'place_lat', 'place_lng'
])

places_of_interest.to_csv(RESULT_FILE, index=False)
