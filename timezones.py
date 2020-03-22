import pandas as pd
import googlemaps

from apikeys import API_KEY


RESULT_FILE = 'data/places_of_interest_tz.csv'

#%%

pois = pd.read_csv('data/places_of_interest.csv')

gmaps = googlemaps.Client(key=API_KEY)

city_tzs = {}
resultrows = []

#%%

for poi_i, poirow in pois.iterrows():
    print('place of interest %d/%d: %s in %s, %s' % (poi_i+1, len(pois), poirow['name'], poirow.city, poirow.country))

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

pois_tz = pd.DataFrame(resultrows, columns=['place_id', 'tz_id', 'tz_rawoffset', 'tz_dstoffset'])

pois = pd.merge(pois, pois_tz, how='left', on='place_id')

print('got TZ information for %d of %d places' % (sum(~pois.tz_id.isna()), len(pois)))

print('saving data to file', RESULT_FILE)
pois.to_csv(RESULT_FILE, index=False)
