source('dataprep.R')
source('plotting.R')

popsm <- load_pop_data()

collection_time <- range_collection_time(popsm)
collection_time

popdata <- filter(popsm, region == 'Europe') %>% select(-region, -utc_date, -utc_hour)
popdata

#

country_means <- group_by(popdata, country) %>% group_means_ci(pop_diff)
country_means

plot_means_errorbars(country_means, country, mean_pop_diff, lwr_pop_diff, upr_pop_diff,
                     'TEST Means in Europe', collection_time)


# 

ts_country_means <- group_by(popdata, local_time, country) %>% group_means_ci(pop_diff)
ts_country_means

ts_means <- group_by(ts_country_means %>% ungroup(), local_time) %>% group_means_ci(mean_pop_diff) 

low_obs_to_NA <- function(df, nvar, thresh, ...) {
    nvar <- enquo(nvar)
    yvars <- enquos(...)
    fill_NAs <- pull(df, !!nvar) < 5

    df_copy <- df
    df_copy[fill_NAs, sapply(yvars, quo_name)] <- NA
    df_copy
}

ts_means <- low_obs_to_NA(ts_means, n, 5, mean_mean_pop_diff, lwr_mean_pop_diff, upr_mean_pop_diff)

plot_ts_means_ribbon(ts_means, local_time, mean_mean_pop_diff, lwr_mean_pop_diff, upr_mean_pop_diff,
                     'TEST Means over time in Europe', collection_time)