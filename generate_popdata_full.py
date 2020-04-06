import glob
import json
from datetime import datetime, timedelta

import pandas as pd


RESULT_FILE = 'data/popularity.csv'

#%%

pois = pd.read_csv('data/places_of_interest_tz.csv')

#%%

print('loading CSVs of regular popularity requests')

datasets = []
for csvfile in glob.glob('data/popularity/**/*.csv'):
    print('> loading', csvfile)
    datasets.append(pd.read_csv(csvfile))

print()

#%%

print('loading CSVs of initial popularity requests upon POI identification')

for jsonfile in glob.glob('data/pois/*_pop.json'):
    print('> loading', jsonfile)

    with open(jsonfile) as f:
        jsondata = json.load(f)

    resultrows = []
    for row in jsondata:
        place_id, utc_date, utc_hour, current_pop, usual_pop = row
        t = datetime.strptime(utc_date + ' ' + str(utc_hour), '%Y-%m-%d %H')
        place_match = pois.place_id == place_id

        if sum(place_match) == 0:
            print('> no existing POI data found for place', place_id)
            continue

        poirow = pois.loc[pois.place_id == place_id, :].iloc[0]
        poi_tzoffset = timedelta(seconds=int(poirow.tz_rawoffset) + int(poirow.tz_dstoffset))
        poi_localtime = t + poi_tzoffset
        poi_localwd = poi_localtime.weekday()
        poi_localhour = poi_localtime.hour

        resultrows.append([
            place_id,
            utc_date,
            t.weekday(),
            utc_hour,
            poi_localtime.strftime('%Y-%m-%d'),
            poi_localwd,
            poi_localhour,
            current_pop,
            usual_pop[poi_localwd]['data'][poi_localhour]
        ])

    if resultrows:
        datasets.append(pd.DataFrame(resultrows, columns=[
            'place_id',
            'utc_date', 'utc_weekday', 'utc_hour',
            'local_date', 'local_weekday', 'local_hour',
            'current_pop', 'usual_pop'
        ]))

print()

#%%

print('concatenating datasets')
full = pd.concat(datasets)\
    .sort_values(['utc_date', 'utc_hour'])\
    .drop_duplicates(['place_id', 'local_date', 'local_hour'])

print('saving full dataset with %d rows to %s' % (len(full), RESULT_FILE))
full.to_csv(RESULT_FILE, index=False)

print('done.')
