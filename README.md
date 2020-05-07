# Using Google Places data to analyze changes in mobility during the COVID-19 pandemic

April / May 2020

Author: Markus Konrad

This repository contains code to obtain and analyse "popular times" data from Google Places. It also contains data fetched between March 22nd and April 15th 2020 for different places world-wide. See [this blog post](https://datascience.blog.wzb.eu/?p=1467) for background information.


## Code

All Python code in the root directory is used to fetch and manage popularity data.

The main scripts are:

1. `places.py`: search for *places of interest* (POI) using the Google Maps places search API; try to fetch popularity data from each found place to assess whether it is a POI (i.e. for this place we can potentially get popularity data); store results in `data/pois`; you may run this script several times and different times of the day to get good results
2. `generate_pois_full.py`: generate a complete POI dataset from all previously found POI; identify timezone for each place (according to its city's geo-coordinates); store result to `data/places_of_interest_tz.csv`
3. `popularity.py`: 

## Datasets


