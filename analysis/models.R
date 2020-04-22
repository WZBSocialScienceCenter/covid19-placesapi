library(lme4)
library(broom)

source('dataprep.R')
source('plotting.R')

pop <- load_pop_data()
collection_time <- range_collection_time(pop)
cities <- distinct(pop, iso2, city, lat, lng)
pop <- select(pop, -utc_date, -utc_hour, -lat, -lng)

popde <- filter(pop, iso2 == 'DE')


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

popde_fe <- tidy(m1_ratio_cat, effects = 'fixed', conf.int = TRUE)
popde_fe

popde_constant <- filter(popde_fe, term == '(Intercept)') %>% pull(estimate)

popde_fe <- filter(popde_fe, term != '(Intercept)') %>%
    mutate(mean = exp(popde_constant + estimate),
           mean_lwr = exp(popde_constant + conf.low),
           mean_upr = exp(popde_constant + conf.high))
popde_fe

 