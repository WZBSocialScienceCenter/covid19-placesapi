import glob
import os
import pickle

import pandas as pd
import googlemaps

from apikeys import API_KEY


RESULT_FILE = 'data/places_of_interest_tz.csv'
TIMEZONES_CACHE_FILE = 'data/city_timezones_cache.pickle'

#%%

datasets = []
for csvfile in glob.glob('data/pois/*.csv'):
    print('loading', csvfile)
    datasets.append(pd.read_csv(csvfile))

print('loaded %d datasets with %d rows in total' % (len(datasets), sum(map(len, datasets))))

#%%

print('concatenating datasets')
full = pd.concat(datasets)\
    .drop_duplicates('place_id')\
    .sort_values(['country', 'city', 'query', 'name'])\
    .reset_index(drop=True)

print('full dataset has %d places' % len(full))

#%%

gmaps = googlemaps.Client(key=API_KEY)

if os.path.exists(TIMEZONES_CACHE_FILE):
    print('loading timezones cache from', TIMEZONES_CACHE_FILE)
    with open(TIMEZONES_CACHE_FILE, 'rb') as f:
        city_tzs = pickle.load(f)
else:
    city_tzs = {}

#%%

resultrows = []
for poi_i, poirow in full.iterrows():
    print('place of interest %d/%d: %s in %s, %s' % (poi_i+1, len(full), poirow['name'], poirow.city, poirow.country))

    if (poirow.iso2, poirow.city) in city_tzs:
        print('> fetch from cache')
        poi_tz = city_tzs[(poirow.iso2, poirow.city)]
    else:
        print('> fetch from API')
        poi_tz = gmaps.timezone((poirow.place_lat, poirow.place_lng))

        if poi_tz['status'] == 'OK':
            city_tzs[(poirow.iso2, poirow.city)] = poi_tz
        else:
            poi_tz = None

    if poi_tz is None:
        print('> failed to fetch TZ information')
    else:
        print('> OK')
        resultrows.append([poirow.place_id, poi_tz['timeZoneId'], poi_tz['rawOffset'], poi_tz['dstOffset']])

print('\n')

#%%

print('will store timezones cache to', TIMEZONES_CACHE_FILE)

with open(TIMEZONES_CACHE_FILE, 'wb') as f:
    pickle.dump(city_tzs, f)

pois_tz = pd.DataFrame(resultrows, columns=['place_id', 'tz_id', 'tz_rawoffset', 'tz_dstoffset'])

full = pd.merge(full, pois_tz, how='left', on='place_id')

print('got TZ information for %d of %d places' % (sum(~full.tz_id.isna()), len(full)))

print('saving data to file', RESULT_FILE)
full.to_csv(RESULT_FILE, index=False)

