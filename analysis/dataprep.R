library(dplyr)
library(lubridate)

load_pop_data <- function() {
    poi <- read.csv('../data/places_of_interest_tz.csv', stringsAsFactors = FALSE)
    regions <- read.csv('../data/regions.csv', stringsAsFactors = FALSE)
    poi <- left_join(poi, regions, by = 'country')
    stopifnot(sum(is.na(poi$region)) == 0)
    
    query_categ <- read.csv('../data/query_categories.csv', stringsAsFactors = FALSE)
    poi <- left_join(poi, query_categ, by = 'query')
    stopifnot(sum(is.na(poi$category)) == 0)
    
    poptime <- read.csv('../data/popularity.csv', stringsAsFactors = FALSE)
    
    pop <- left_join(poptime, poi, by = 'place_id')
    stopifnot(sum(is.na(pop$name)) == 0)
    
    pop$local_time <- ymd_h(paste(pop$local_date, pop$local_hour))
    # select(pop, local_date, local_hour, local_time) %>% sample_n(20)
    
    pop$pop_diff <- pop$current_pop - pop$usual_pop
    
    # reduced dataset:
    select(pop, local_time, region, country, city, category, query, name, pop_diff, utc_date, utc_hour) %>%
        arrange(local_time, region, country, city, category)
}

range_collection_time <- function(popdata) {
    strftime(range(ymd_h(paste(popdata$utc_date, popdata$utc_hour))), "%Y-%m-%d %I%p", tz = 'UTC')
}


# set values with very few observations to NA to produce gaps in plot
low_obs_to_NA <- function(df, col, thresh) {
    df[[col]] <- ifelse(df$n < thresh, NA, df[[col]])
    df
}


group_means_ci <- function(grp, var) {
    var <- enquo(var)
    mean_name <- paste0("mean_", quo_name(var))
    sd_name <- paste0("sd_", quo_name(var))
    bounds_name <- paste0(c("lwr_", "upr_"), quo_name(var))
    summarise(grp,
              !!mean_name := mean(!!var),
              !!sd_name := sd(!!var),
              n = n(),
              !!bounds_name[1] := mean(!!var) - 1.96 * sd(!!var) / sqrt(n),
              !!bounds_name[2] := mean(!!var) + 1.96 * sd(!!var) / sqrt(n))
}

