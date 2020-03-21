import pandas as pd

cities = pd.read_csv('data/worldcities.csv')
cities = cities.loc[(cities.capital == 'primary') & (~cities.population.isna()) & (cities.population >= 100000),
                    ['city', 'country', 'iso2', 'lat', 'lng']]

cities.to_csv('data/cities.csv', index=False)
