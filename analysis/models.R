library(lme4)
library(broom)
library(stringr)

source('dataprep.R')
source('plotting.R')

pop <- load_pop_data()
collection_time <- range_collection_time(pop)
cities <- distinct(pop, iso2, city, lat, lng)
pop <- select(pop, -utc_date, -utc_hour, -lat, -lng)


###############################
### estimations for Germany ###
###############################

# data

popde <- filter(pop, iso2 == 'DE')
popde_approx <- select(popde, -current_pop, -usual_pop) %>% group_by(local_day, place_id) %>%
    do(interpolate_per_place(.)) %>% ungroup()


hist(popde$pop_diff)
hist(log(popde$pop_diff))
hist(popde$pop_frac)
hist(log(popde$pop_frac))   # normal distrib.

### estimates per category ###

# linear mixed model 1: intercept only – pop_frac per FE category with nested RE place_id in city, crossed RE weekday

m1_ratio_cat <- lmer(log(pop_frac) ~ category + (1|city/place_id) + (1|local_weekday), data = popde)
plot(m1_ratio_cat)
qqnorm(resid(m1_ratio_cat)); qqline(resid(m1_ratio_cat))

# linear mixed model 2: intercept only – pop_frac per FE category with nested RE place_id in city

m2_ratio_cat <- lmer(log(pop_frac) ~ category + (1|city/place_id), data = popde)
plot(m2_ratio_cat)
qqnorm(resid(m2_ratio_cat)); qqline(resid(m2_ratio_cat))

# compare
anova(m1_ratio_cat, m2_ratio_cat)  # -> choose m1


summary(m1_ratio_cat)

# fixed effects
popde_cat_fe <- tidy(m1_ratio_cat, effects = 'fixed', conf.int = TRUE)
popde_cat_fe

# intercept (= first category level)
popde_cat_constant <- filter(popde_cat_fe, term == '(Intercept)') %>% pull(estimate)

# estimated ratios per category (converted back from log scale)
popde_cat_estim <- filter(popde_cat_fe, term != '(Intercept)') %>%
    mutate(mean = exp(popde_cat_constant + estimate),
           mean_lwr = exp(popde_cat_constant + conf.low),
           mean_upr = exp(popde_cat_constant + conf.high)) %>%
    bind_rows(filter(popde_cat_fe, term == '(Intercept)') %>%   # add first categ. level
                  mutate(term = paste0('category', levels(popde$category)[1]),
                         mean = exp(estimate),
                         mean_lwr = exp(conf.low),
                         mean_upr = exp(conf.high)))
popde_cat_estim

# get proper category names
popde_cat_estim <- mutate(popde_cat_estim, categ = str_sub(term, nchar('category') + 1)) %>%
    select(categ, mean, mean_lwr, mean_upr) %>% arrange(categ)
popde_cat_estim

plot_categ_means_ci(popde_cat_estim, categ, mean, mean_lwr, mean_upr,
                    'Change in mobility by type of place in Germany', SUBTITLE_RATIOS, collection_time)


### daily trends per category and working day / weekend ###


#popde_approx$local_hour_ord <- as.ordered(popde_approx$local_hour)
popde_approx_daytime <- filter(popde_approx, 8 <= local_hour, local_hour <= 21)
popde_approx_daytime$local_hour_fact <- as.factor(popde_approx_daytime$local_hour)

m1_daytrends_cat <- lmer(log(pop_frac) ~ local_hour_fact + local_weekend + category + (1|city/place_id), data = popde_approx_daytime)
plot(m1_daytrends_cat)

summary(m1_daytrends_cat)

popde_daytrends_fe <- tidy(m1_daytrends_cat, effects = 'fixed', conf.int = TRUE)
popde_daytrends_fe

popde_daytrends_constant <- filter(popde_daytrends_fe, term == '(Intercept)') %>% pull(estimate)
