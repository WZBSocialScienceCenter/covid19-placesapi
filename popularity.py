import os
from datetime import datetime, timedelta

import pandas as pd
import populartimes

from apikeys import API_KEY

HOURS_OF_INTEREST = {
    'restaurant': range(9, 23),
    'bar': list(range(12, 24)) + list(range(0, 4)),
    'club': list(range(20, 24)) + list(range(0, 6)),
    'train station': range(5, 21),
    'tourist information': range(8, 21),
    'sights': range(6, 21),
    'park': range(6, 21),
    'mall': range(6, 21),
    'supermarket': range(6, 21),
    'street market': range(6, 21),
    'hardware store': range(6, 21),
}

DATADIR = 'data/popularity'
EVERY_NTH_HOUR = 2  # None

#%%

pois = pd.read_csv('data/places_of_interest_tz.csv')   #.sample(20)
utcnow = datetime.utcnow()
utcdate_ymd = utcnow.strftime('%Y-%m-%d')
utcweekday = utcnow.weekday()
utchour = utcnow.hour

if EVERY_NTH_HOUR and utchour % EVERY_NTH_HOUR != 0:
    print('skipping (current UTC hour is %d and will only run every %d hour)' % (utchour, EVERY_NTH_HOUR))
    exit(0)

datadir_today = os.path.join(DATADIR, utcdate_ymd)

if not os.path.exists(datadir_today):
    print('creating directory', datadir_today, '\n')
    os.mkdir(datadir_today, mode=0o755)

#%%

resultrows = []

for poi_i, poirow in pois.iterrows():
    print('place of interest %d/%d: %s, %s' % (poi_i+1, len(pois), poirow.city, poirow.country))

    poi_tzoffset = timedelta(seconds=poirow.tz_rawoffset + poirow.tz_dstoffset)
    poi_localtime = utcnow + poi_tzoffset
    poi_localwd = poi_localtime.weekday()
    poi_localhour = poi_localtime.hour

    poi_hinterest = HOURS_OF_INTEREST.get(poirow['query'], list(range(6, 21)))
    if not isinstance(poi_hinterest, list):
        poi_hinterest = list(poi_hinterest)

    if poi_localhour not in poi_hinterest:
        print('> skipping (local hour %d not in hours of interest %s)'
              % (poi_localhour, ', '.join(map(str, poi_hinterest))))
        continue

    try:
        poptimes = populartimes.get_id(API_KEY, poirow.place_id)
    except Exception:  # catch any exception
        poptimes = {}

    if 'current_popularity' in poptimes and 'populartimes' in poptimes:
        print('> got popularity data')

        resultrows.append([
            poirow.place_id,
            utcdate_ymd,
            utcweekday,
            utchour,
            poi_localtime.strftime('%Y-%m-%d'),
            poi_localwd,
            poi_localhour,
            poptimes['current_popularity'],
            poptimes['populartimes'][poi_localwd]['data'][poi_localhour]
        ])
    else:
        print('> failed to fetch popularity data')

print('\n')

#%%

popdata = pd.DataFrame(resultrows, columns=[
    'place_id',
    'utc_date', 'utc_weekday', 'utc_hour',
    'local_date', 'local_weekday', 'local_hour',
    'current_pop', 'usual_pop'
])

outfile = os.path.join(datadir_today, '%s_h%s.csv' % (utcdate_ymd, str(utchour).zfill(2)))
print('saving data to file', outfile)
popdata.to_csv(outfile, index=False)

print('done.')
