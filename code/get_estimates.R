
source("code/functions/estimator.R")

gss_trends <- map_df(gss_trust_vars, function(v) estimate_trend(v, df = gss_72_16_trust_trend))
saveRDS(gss_trends, "output/gss_trends_full.RDS")
cross_vars <- gss_72_16_trust_trend %>% select(racethn, partysum, relig4, citizen, educ3, agecat3, parent_birth) %>% names(.)

combos = cross(list(gss_trust_vars, cross_vars)) %>%
  transpose()

gss_trends_cross = map2_df(combos[[1]], combos[[2]], estimate_trend_cross)
saveRDS(gss_trends_cross, "output/gss_trends_cross.RDS")
