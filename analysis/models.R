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

# estimate means and 95%CI per category
catnames <- sort(levels(popde$category))
popde_cat_preddata <- data.frame(category = catnames)
popde_cat_preddata

popde_cat_pred_func <- function(fit) {
    predict(fit, popde_cat_preddata, re.form = NA)
}

popde_cat_boot <- bootMer(m1_ratio_cat, nsim = 500, FUN = popde_cat_pred_func,
                          use.u = TRUE, ncpus = 3, parallel = 'multicore')


popde_cat_estim <- data.frame(t(apply(popde_cat_boot$t, 2, function(x, q) { exp(quantile(x, q)) }, c(0.025,0.50,0.975))))
colnames(popde_cat_estim) <- c('mean_lwr', 'mean', 'mean_upr')
popde_cat_estim$category <- catnames
popde_cat_estim

(p <- plot_categ_means_ci(popde_cat_estim, category, mean, mean_lwr, mean_upr,
                         'Change in popularity by type of place in Germany', SUBTITLE_RATIOS, collection_time))
ggsave('plots/de_mobchange_categ.png', p)


### daily trends per category and working day / weekend ###

# only look at values between 8am and 8pm
popde_approx_daytime <- filter(popde_approx, 8 <= local_hour, local_hour <= 20)
popde_approx_daytime$local_hour_fact <- as.factor(popde_approx_daytime$local_hour)

m1_de_daytrends_cat <- lmer(log(pop_frac) ~ local_hour_fact + local_weekend + category + (1|city/place_id), data = popde_approx_daytime)
plot(m1_de_daytrends_cat)
summary(m1_de_daytrends_cat)

m2_de_daytrends_cat <- lmer(log(pop_frac) ~ local_weekend/local_hour_fact + category + (1|city/place_id), data = popde_approx_daytime)
plot(m2_de_daytrends_cat)
summary(m2_de_daytrends_cat)

m3_de_daytrends_cat <- lmer(log(pop_frac) ~ category/local_weekend/local_hour_fact + (1|city/place_id), data = popde_approx_daytime)
plot(m3_de_daytrends_cat)
summary(m3_de_daytrends_cat)


# confint(m1_daytrends_cat, c('local_hour_fact9', 'categoryparks'))
# exp(predict(m1_daytrends_cat, data.frame(local_hour_fact = "13", local_weekend = TRUE, category = 'parks'), re.form = NA))

de_daytrends_preddata <- expand.grid(local_hour_fact = levels(popde_approx_daytime$local_hour_fact),
                                     local_weekend = c(TRUE, FALSE),
                                     category = catnames)
de_daytrends_preddata


de_daytrends_pred_func <- function(fit) {
    predict(fit, de_daytrends_preddata, re.form = NA)
}

de_daytrends_boot <- bootMer(m3_de_daytrends_cat, nsim = 500, FUN = de_daytrends_pred_func,
                             use.u = TRUE, ncpus = 3, parallel = 'multicore')

de_daytrends_boot

de_daytrends_estim <- data.frame(t(apply(de_daytrends_boot$t, 2, function(x, q) { exp(quantile(x, q)) }, c(0.025,0.50,0.975))))
colnames(de_daytrends_estim) <- c('mean_lwr', 'mean', 'mean_upr')
de_daytrends_estim <- bind_cols(de_daytrends_preddata, de_daytrends_estim)
de_daytrends_estim$local_hour <- as.integer(levels(de_daytrends_estim$local_hour_fact))
de_daytrends_estim

plot_daily_cat_means_ribbon(de_daytrends_estim, local_hour, mean, mean_lwr, mean_upr, category, 'foo', 'bar')

popde_daytrends_fe <- tidy(m1_daytrends_cat, effects = 'fixed', conf.int = TRUE)
popde_daytrends_fe

popde_daytrends_constant <- filter(popde_daytrends_fe, term == '(Intercept)') %>% pull(estimate)
