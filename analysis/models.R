library(lme4)
library(broom)
library(stringr)

source('dataprep.R')
source('plotting.R')


QUANTILES_FOR_CI <- c(0.05, 0.50, 0.95)

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


popde_cat_boot <- readRDS('tmp/popde_cat_boot.RDS')
# popde_cat_boot <- bootMer(m1_ratio_cat, nsim = 500, FUN = popde_cat_pred_func,
#                           use.u = FALSE, ncpus = 3, parallel = 'multicore')
# 
# saveRDS(popde_cat_boot, 'tmp/popde_cat_boot.RDS')

popde_cat_estim <- data.frame(t(apply(popde_cat_boot$t, 2, function(x, q) { exp(quantile(x, q)) }, QUANTILES_FOR_CI)))
colnames(popde_cat_estim) <- c('mean_lwr', 'mean', 'mean_upr')
popde_cat_estim$category <- catnames
popde_cat_estim

(p <- plot_categ_means_ci(popde_cat_estim, category, mean, mean_lwr, mean_upr,
                         'Change in popularity by type of place in Germany',
                         SUBTITLE_RATIOS, collection_time))
ggsave('plots/de_mobchange_categ.png', p)


### daily trends per category and working day / weekend ###

# only look at values between 8am and 8pm
popde_approx_daytime <- filter(popde_approx, 8 <= local_hour, local_hour <= 20)
popde_approx_daytime$local_hour_fact <- as.factor(popde_approx_daytime$local_hour)

m1_de_daytrends_cat <- lmer(log(pop_frac) ~ local_hour_fact + local_weekend + category + (1|city/place_id), data = popde_approx_daytime)
plot(m1_de_daytrends_cat)
qqnorm(resid(m1_de_daytrends_cat)); qqline(resid(m1_de_daytrends_cat))
summary(m1_de_daytrends_cat)

m2_de_daytrends_cat <- lmer(log(pop_frac) ~ local_weekend/local_hour_fact + category + (1|city/place_id), data = popde_approx_daytime)
plot(m2_de_daytrends_cat)
qqnorm(resid(m2_de_daytrends_cat)); qqline(resid(m2_de_daytrends_cat))
summary(m2_de_daytrends_cat)

m3_de_daytrends_cat <- lmer(log(pop_frac) ~ category/local_weekend/local_hour_fact + (1|city/place_id), data = popde_approx_daytime)
plot(m3_de_daytrends_cat)
qqnorm(resid(m3_de_daytrends_cat)); qqline(resid(m3_de_daytrends_cat))
summary(m3_de_daytrends_cat)

anova(m1_de_daytrends_cat, m2_de_daytrends_cat, m3_de_daytrends_cat)


de_daytrends_preddata <- expand.grid(local_hour_fact = levels(popde_approx_daytime$local_hour_fact),
                                     local_weekend = c(TRUE, FALSE),
                                     category = catnames)
de_daytrends_preddata


de_daytrends_pred_func <- function(fit) {
    predict(fit, de_daytrends_preddata, re.form = NA)
}

de_daytrends_boot <- readRDS('tmp/de_daytrends_boot.RDS')
# de_daytrends_boot <- bootMer(m3_de_daytrends_cat, nsim = 500, FUN = de_daytrends_pred_func,
#                              use.u = FALSE, ncpus = 3, parallel = 'multicore')
# saveRDS(de_daytrends_boot, 'tmp/de_daytrends_boot.RDS')

de_daytrends_boot

de_daytrends_estim <- data.frame(t(apply(de_daytrends_boot$t, 2, function(x, q) { exp(quantile(x, q)) }, QUANTILES_FOR_CI)))
colnames(de_daytrends_estim) <- c('mean_lwr', 'mean', 'mean_upr')
de_daytrends_estim <- bind_cols(de_daytrends_preddata, de_daytrends_estim)
de_daytrends_estim$local_hour <- as.integer(levels(de_daytrends_estim$local_hour_fact))
de_daytrends_estim

(p <- plot_daily_cat_means_ribbon(de_daytrends_estim, local_hour, mean, mean_lwr, mean_upr,
                                  local_weekend, category, 'Daily patterns in popularity change in Germany',
                                  SUBTITLE_RATIOS, collection_time))

ggsave('plots/de_daily_mobchange_categ.png', p)

###############################
### estimations for Germany ###
###############################


eu_low_obs <- filter(pop, region == 'Europe') %>% count(country) %>% filter(n < 100)
eu_low_obs

popeu <- filter(pop, region == 'Europe', !(country %in% eu_low_obs$country))

### estimates per country ###

# TODO: filter countries w/ low num obs. per category / not all categories

eu_m1_ratio_cntry <- lmer(log(pop_frac) ~ country + (1|city/category/place_id) + (1|country:local_weekday),
                          data = popeu)
plot(eu_m1_ratio_cntry)
qqnorm(resid(eu_m1_ratio_cntry)); qqline(resid(eu_m1_ratio_cntry))
summary(eu_m1_ratio_cntry)

# simpler

eu_m2_ratio_cntry <- lmer(log(pop_frac) ~ country + (1|city/category/place_id) + (1|country:local_weekend),
                          data = popeu)
plot(eu_m2_ratio_cntry)
qqnorm(resid(eu_m2_ratio_cntry)); qqline(resid(eu_m2_ratio_cntry))
summary(eu_m2_ratio_cntry)

# even simpler

eu_m3_ratio_cntry <- lmer(log(pop_frac) ~ country + (1|city/place_id) + (1|country:local_weekend),
                          data = popeu)
plot(eu_m3_ratio_cntry)
qqnorm(resid(eu_m3_ratio_cntry)); qqline(resid(eu_m3_ratio_cntry))
summary(eu_m3_ratio_cntry)

anova(eu_m1_ratio_cntry, eu_m2_ratio_cntry, eu_m3_ratio_cntry)

eu_ratio_cntry_preddata <- data.frame(country = sort(unique(popeu$country)))
eu_ratio_cntry_preddata

eu_ratio_cntry_pred_func <- function(fit) {
    predict(fit, eu_ratio_cntry_preddata, re.form = NA)
}

eu_ratio_cntry_boot <- bootMer(?, nsim = 10, FUN = eu_ratio_cat_pred_func,   # TODO
                             use.u = FALSE, ncpus = 4, parallel = 'multicore')    # this is quite slow
saveRDS(eu_ratio_cntry_boot, 'tmp/eu_ratio_cntry_boot.RDS')

eu_ratio_cntry_estim <- data.frame(t(apply(eu_ratio_cntry_boot$t, 2, function(x, q) { exp(quantile(x, q)) }, QUANTILES_FOR_CI)))
colnames(eu_ratio_cntry_estim) <- c('mean_lwr', 'mean', 'mean_upr')
eu_ratio_cntry_estim <- bind_cols(eu_ratio_cat_preddata, eu_ratio_cntry_estim)
eu_ratio_cntry_estim


### estimates per country and category ###

# linear mixed model 1

eu_m1_ratio_cat <- lmer(log(pop_frac) ~ country:category + (1|city/place_id) + (1|country:local_weekday),
                        control = lmerControl('Nelder_Mead'),
                        data = popeu)
plot(eu_m1_ratio_cat)
qqnorm(resid(eu_m1_ratio_cat)); qqline(resid(eu_m1_ratio_cat))
summary(eu_m1_ratio_cat)

# linear mixed model 2: simpler

eu_m2_ratio_cat <- lmer(log(pop_frac) ~ country:category + (1|city/place_id), data = popeu)
plot(eu_m2_ratio_cat)
qqnorm(resid(eu_m2_ratio_cat)); qqline(resid(eu_m2_ratio_cat))
summary(eu_m2_ratio_cat)

# linear mixed model 3: in between

eu_m3_ratio_cat <- lmer(log(pop_frac) ~ country:category + (1|city/place_id) + (1|country:local_weekend),
                        data = popeu)
plot(eu_m3_ratio_cat)
qqnorm(resid(eu_m3_ratio_cat)); qqline(resid(eu_m3_ratio_cat))
summary(eu_m3_ratio_cat)

anova(eu_m1_ratio_cat, eu_m2_ratio_cat, eu_m3_ratio_cat)

eu_ratio_cat_preddata <- distinct(popeu, country, category) %>% arrange(country, category)
eu_ratio_cat_preddata

eu_ratio_cat_pred_func <- function(fit) {
    predict(fit, eu_ratio_cat_preddata, re.form = NA)
}

eu_ratio_cat_boot <- readRDS('tmp/eu_ratio_cat_boot.RDS')
# eu_ratio_cat_boot <- bootMer(eu_m3_ratio_cat, nsim = 200, FUN = eu_ratio_cat_pred_func,
#                              use.u = FALSE, ncpus = 4, parallel = 'multicore')    # this is quite slow
# saveRDS(eu_ratio_cat_boot, 'tmp/eu_ratio_cat_boot.RDS')

eu_ratio_cat_estim <- data.frame(t(apply(eu_ratio_cat_boot$t, 2, function(x, q) { exp(quantile(x, q)) }, QUANTILES_FOR_CI)))
colnames(eu_ratio_cat_estim) <- c('mean_lwr', 'mean', 'mean_upr')
eu_ratio_cat_estim <- bind_cols(eu_ratio_cat_preddata, eu_ratio_cat_estim)
eu_ratio_cat_estim

eu_ratio_cat_estim$ci_range <- eu_ratio_cat_estim$mean_upr - eu_ratio_cat_estim$mean_lwr
eu_ratio_cat_estim <- filter(eu_ratio_cat_estim, ci_range <= 1)    # too large CIs for some country/category combinations

(p <- plot_categ_means_country_ci(eu_ratio_cat_estim, country, mean, mean_lwr, mean_upr, category,
                                  'Change in popularity by type of place in Europe',
                                  SUBTITLE_RATIOS, collection_time))
ggsave('plots/eu_mobchange_categ.png', p, width = 8, height = 12)
