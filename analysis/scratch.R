source('dataprep.R')
source('plotting.R')

popsm <- load_pop_data()

collection_time <- range_collection_time(popsm)
collection_time

popsm <- select(popsm, -utc_date, -utc_hour)

popdata <- filter(popsm, region == 'Europe')
popdata <- group_by(popdata, local_day, place_id) %>% do(interpolate_per_place(.)) %>% ungroup() %>% select(-region)
popdata

# Europe Means per country

country_means <- group_by(popdata, country) %>% group_means_ci(pop_diff)
country_means

plot_means_errorbars(country_means, country, mean_pop_diff, lwr_pop_diff, upr_pop_diff,
                     'TEST Means in Europe', collection_time)


# Europe Means over time

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


# Means per country and category

country_cat_means <- group_by(popdata, country, category) %>% group_means_ci(pop_diff)
country_cat_means


plot_country_categories(country_cat_means, country, mean_pop_diff, lwr_pop_diff, upr_pop_diff,
                        'TEST Means per country and category in Europe', collection_time)


# Means per category

cat_means <- group_by(country_cat_means, category) %>% group_means_ci(mean_pop_diff)
cat_means

plot_means_errorbars(cat_means, category, mean_mean_pop_diff, lwr_mean_pop_diff, upr_mean_pop_diff,
                     'TEST Means in Europe', collection_time) + coord_flip()


# Daily patterns

popdaily <- filter(popdata, local_hour %in% 6:22)
popdaily

daily_country_means <- group_by(popdaily, country, local_weekend, local_hour) %>%
    group_means_ci(pop_diff)
daily_country_means
daily_means <- group_by(daily_country_means, local_weekend, local_hour) %>% group_means_ci(mean_pop_diff)
daily_means

plot_daily_means_ribbon(daily_means, local_hour, mean_mean_pop_diff, lwr_mean_pop_diff, upr_mean_pop_diff,
                        local_weekend, 'TEST', collection_time)

daily_country_cat_means <- group_by(popdaily, country, category, local_weekend, local_hour) %>%
    group_means_ci(pop_diff)
daily_country_cat_means

daily_cat_means <- group_by(daily_country_cat_means, category, local_weekend, local_hour) %>%
    group_means_ci(mean_pop_diff)
daily_cat_means

plot_daily_cat_means_ribbon(daily_cat_means, local_hour, mean_mean_pop_diff, lwr_mean_pop_diff, upr_mean_pop_diff,
                            category, 'TEST', collection_time)
