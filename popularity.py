import os
from datetime import datetime

import pandas as pd
import populartimes

from apikeys import API_KEY

DATADIR = 'data/popularity'

#%%

pois = pd.read_csv('data/places_of_interest.csv')
now = datetime.now()
date_ymd = now.strftime('%Y-%m-%d')
weekday = now.weekday()
hour = now.hour

datadir_today = os.path.join(DATADIR, date_ymd)

if not os.path.exists(datadir_today):
    print('creating directory', datadir_today)
    os.mkdir(datadir_today, mode=0o755)

#%%

resultrows = []

for poi_i, poirow in pois.iterrows():
    print('place of interest %d/%d: %s in %s, %s' % (poi_i+1, len(pois), poirow.name, poirow.city, poirow.country))
    poptimes = populartimes.get_id(API_KEY, poirow.place_id)

    if 'current_popularity' in poptimes and 'populartimes' in poptimes:
        print('> got popularity data')

        resultrows.append([
            poirow.place_id,
            date_ymd,
            weekday,
            hour,
            poptimes['current_popularity'],
            poptimes['populartimes'][weekday]['data'][hour]
        ])
    else:
        print('> failed to fetch popularity data')

print('\n\n')

#%%

popdata = pd.DataFrame(resultrows, columns=['place_id', 'date', 'weekday', 'hour', 'current_pop', 'usual_pop'])
outfile = os.path.join(datadir_today, '%s_h%d.csv' % (date_ymd, hour))
print('saving data to file', outfile)
popdata.to_csv(outfile, index=False)

print('done.')
