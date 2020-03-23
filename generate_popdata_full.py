import glob

import pandas as pd


RESULT_FILE = 'data/popularity.csv'

#%%

datasets = []
for csvfile in glob.glob('data/popularity/**/*.csv'):
    print('loading', csvfile)
    datasets.append(pd.read_csv(csvfile))

#%%

print('concatenating datasets')
full = pd.concat(datasets)

print('saving full dataset with %d rows to %s' % (len(full), RESULT_FILE))
full.to_csv(RESULT_FILE, index=False)

print('done.')
