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
    
    pop <- mutate(pop,
                  pop_diff = current_pop - usual_pop,
                  local_day = ymd(local_date),
                  local_time = ymd_h(paste(local_date, local_hour)),
                  local_weekday = wday(local_time, week_start = 1),
                  local_weekend = !(local_weekday %in% 1:5))
    
    # reduced dataset:
    select(pop, local_time, local_day, local_hour, local_weekday, local_weekend,
           region, country, city, category, query, place_id, name, pop_diff, utc_date, utc_hour) %>%
        arrange(local_time, region, country, city, category)
}


range_collection_time <- function(popdata) {
    strftime(range(ymd_h(paste(popdata$utc_date, popdata$utc_hour))), "%Y-%m-%d %I%p", tz = 'UTC')
}


# interpolate a place's pop_diff values for a single day
# does linear interpolation with a maximum gap size of 2 NAs
# use with group_by(local_day, place_id) and do()
interpolate_per_place <- function(grp) {
    hrange <- range(grp$local_hour)
    fill <- data.frame(local_hour = seq(hrange[1], hrange[2]))
    
    fill <- select(grp, local_hour, pop_diff) %>% right_join(fill, by = 'local_hour')
    fill$is_approx <- is.na(fill$pop_diff)
    
    if (sum(fill$is_approx) == 0) {
        return(grp)
    }
    
    fill$pop_diff <- zoo::na.approx(fill$pop_diff, maxgap = 3, na.rm = FALSE)
    fill
    
    distinctrow <- select(grp, -local_time, -local_hour, -pop_diff) %>% distinct()
    stopifnot(nrow(distinctrow) == 1)
    
    newrows <- bind_cols(distinctrow[rep(1, sum(fill$is_approx)), ], filter(fill, is_approx)) %>%
        select(-is_approx) %>% mutate(local_time = ymd_h(paste(local_day, local_hour)))
    
    result <- bind_rows(grp, newrows) %>% arrange(local_time, region, country, city, category)
    stopifnot(nrow(result) >= nrow(grp))
    stopifnot(sort(result$local_hour) == seq(hrange[1], hrange[2]))
    
    result
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
              !!mean_name := mean(!!var, na.rm = TRUE),
              !!sd_name := sd(!!var, na.rm = TRUE),
              n = sum(!is.na(!!var)),
              !!bounds_name[1] := mean(!!var, na.rm = TRUE) - 1.96 * sd(!!var, na.rm = TRUE) / sqrt(n),
              !!bounds_name[2] := mean(!!var, na.rm = TRUE) + 1.96 * sd(!!var, na.rm = TRUE) / sqrt(n))
}
