source('dataprep.R')
source('plotting.R')

popsm <- load_pop_data()

collection_time <- range_collection_time(popsm)
collection_time

popdata <- filter(popsm, region == 'Europe') %>% select(-region, -utc_date, -utc_hour)
country_means <- group_by(popdata, country) %>% group_means_ci(pop_diff)
country_means

plot_means_errorbars(country_means, country, mean_pop_diff, lwr_pop_diff, upr_pop_diff,
                     'TEST Means in Europe', collection_time)
