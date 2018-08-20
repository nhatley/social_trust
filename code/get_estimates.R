library(tidyverse)

gss_to_estimate = read_rds("data/gss_72_16_clean.RDS")

source("code/functions.R")

test = estimate_trend(var_in = "fair", df = gss_to_estimate)

rm(var_in)
rm(sg_var)

test_cross = estimate_trend_cross(var_in = "fair",
                                  sg_var = "racethn",
                                  df = gss_to_estimate)

# gss_trends <- map_df(gss_trust_vars, 
#                      function(v) estimate_trend(
#saveRDS(gss_trends, "output/gss_trends_full.RDS")
#cross_vars <- gss_72_16_trust_trend %>% select(racethn, partysum, relig4, citizen, educ3, agecat3, parent_birth) %>% names(.)

#combos = cross(list(gss_trust_vars, cross_vars)) %>%
#  transpose()

#gss_trends_cross = map2_df(combos[[1]], combos[[2]], estimate_trend_cross)
#saveRDS(gss_trends_cross, "output/gss_trends_cross.RDS")
