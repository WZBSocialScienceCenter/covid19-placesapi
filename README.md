# Using Google Places data to analyze changes in mobility during the COVID-19 pandemic

April / May 2020

Author: Markus Konrad

This repository contains code to obtain and analyse "popular times" data from Google Places. It also contains data fetched between March 22nd and April 15th 2020 for different places world-wide. See [this blog post](https://datascience.blog.wzb.eu/2020/05/11/using-google-places-data-to-analyze-changes-in-mobility-during-the-covid-19-pandemic/) for background information.


## Code

All Python code in the root directory is used to fetch and manage popularity data.

The main scripts are:

1. `places.py`: search for *places of interest* (POI) using the Google Maps places search API; try to fetch popularity data from each found place to assess whether it is a POI (i.e. for this place we can potentially get popularity data); store results in `data/pois/`; you may run this script several times and different times of the day to get good results
2. `generate_pois_full.py`: generate a complete POI dataset from all previously found POI; identify timezone for each place (according to its city's geo-coordinates); store result to `data/places_of_interest_tz.csv`
3. `popularity.py`: for the POI listed in `data/places_of_interest_tz.csv`, fetch popularity data and store to `data/popularity/`; can (and should) be used for periodic data collection (e.g. with a cronjob) for *each* hour; a schedule at which *local time* at the given POI data should be collected can be set
4. `generate_popularity_full.py`: generate a complete popularity dataset from previously collected popularity data in `data/popularity/` and `data/pois/`; store result to `data/popularity.csv`

R code to reproduce the plots etc. is available in `analysis/`.

## Datasets

All datasets are located in `data/`. The main datasets are:

- `places_of_interest_tz.csv`: all POI with their meta data
    - `city`: queried city
    - `country`: country name
    - `iso2`: 2-letter ISO code for the country
    - `lat`: **city** geo-coordinates latitude 
    - `lng`: **city** geo-coordinates longitude
    - `query`: query used to find the place
    - `place_id`: Google Place ID
    - `name`: name of the place
    - `addr`: address of the place
    - `place_lat`: **place** geo-coordinates latitude 
    - `place_lng`: **place** geo-coordinates longitude
    - `tz_id`: timezone ID
    - `tz_rawoffset`: time zone offset
    - `tz_dstoffset`: time zone DST offset

- `popularity.csv`: popularity values for POI; you should only use the `local_` date/time values for temporal analysis
    - `place_id`: Google Place ID
    - `utc_date`: UTC date when popularity was fetched
    - `utc_weekday`: UTC weekday when popularity was fetched
    - `utc_hour`: UTC hour when popularity was fetched
    - `local_date`: local date (according to place timezone) when popularity was fetched
    - `local_weekday`: local weekday (according to place timezone) when popularity was fetched
    - `local_hour`: local hour (according to place timezone) when popularity was fetched
    - `current_pop`: current popularity at this local date and time
    - `usual_pop`: usual popularity at local weekday and hour

## License

Apache License 2.0. See *LICENSE* file.

