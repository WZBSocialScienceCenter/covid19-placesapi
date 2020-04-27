library(lme4)
library(broom)
library(stringr)

source('dataprep.R')
source('plotting.R')

pop <- load_pop_data()
collection_time <- range_collection_time(pop)
cities <- distinct(pop, iso2, city, lat, lng)
pop <- select(pop, -utc_date, -utc_hour, -lat, -lng)

popde <- filter(pop, iso2 == 'DE')
popde_approx <- select(popde, -current_pop, -usual_pop) %>% group_by(local_day, place_id) %>%
    do(interpolate_per_place(.)) %>% ungroup()


hist(popde$pop_diff)
hist(popde$pop_frac)

# linear mixed model 1: intercept only – pop_diff per FE category with nested RE place_id in city, crossed RE weekday
m1_diff_cat <- lmer(pop_diff ~ category + (1|city/place_id) + (1|local_weekday), data = popde)
plot(m1_diff_cat)
qqnorm(resid(m1_diff_cat))
qqline(resid(m1_diff_cat))

m2_diff_cat <- lmer(pop_diff ~ category + (1|city/place_id), data = popde)
plot(m2_diff_cat)
qqnorm(resid(m2_diff_cat))
qqline(resid(m2_diff_cat))

m1_ratio_cat <- lmer(log(pop_frac) ~ category + (1|city/place_id) + (1|local_weekday), data = popde)
plot(m1_ratio_cat)
qqnorm(resid(m1_ratio_cat)); qqline(resid(m1_ratio_cat))


m2_ratio_cat <- lmer(log(pop_frac) ~ category + (1|city/place_id), data = popde)
plot(m2_ratio_cat)
qqnorm(resid(m2_ratio_cat)); qqline(resid(m2_ratio_cat))

anova(m1_ratio_cat, m2_ratio_cat)

summary(m1_ratio_cat)

popde_cat_fe <- tidy(m1_ratio_cat, effects = 'fixed', conf.int = TRUE)
popde_cat_fe

popde_cat_constant <- filter(popde_cat_fe, term == '(Intercept)') %>% pull(estimate)

popde_cat_fe

popde_cat_fe <- filter(popde_cat_fe, term != '(Intercept)') %>%
    mutate(mean = exp(popde_cat_constant + estimate),
           mean_lwr = exp(popde_cat_constant + conf.low),
           mean_upr = exp(popde_cat_constant + conf.high)) %>%
    bind_rows(filter(popde_cat_fe, term == '(Intercept)') %>%
                  mutate(term = paste0('category', levels(popde$category)[1]),
                         mean = exp(estimate),
                         mean_lwr = exp(estimate + conf.low),
                         mean_upr = exp(estimate + conf.high)))
popde_cat_fe

popde_cat_fe <- mutate(popde_cat_fe, categ = str_sub(term, nchar('category') + 1)) %>%
    select(categ, mean, mean_lwr, mean_upr) %>% arrange(categ)
popde_cat_fe



#popde_approx$local_hour_ord <- as.ordered(popde_approx$local_hour)
popde_approx_daytime <- filter(popde_approx, 8 <= local_hour, local_hour <= 21)
popde_approx_daytime$local_hour_fact <- as.factor(popde_approx_daytime$local_hour)

m1_daytrends_cat <- lmer(log(pop_frac) ~ local_hour_fact + local_weekend + category + (1|city/place_id), data = popde_approx_daytime)
plot(m1_daytrends_cat)

summary(m1_daytrends_cat)

popde_daytrends_fe <- tidy(m1_daytrends_cat, effects = 'fixed', conf.int = TRUE)
popde_daytrends_fe

popde_daytrends_constant <- filter(popde_daytrends_fe, term == '(Intercept)') %>% pull(estimate)
