library(pewmethods)

gss_trends_cross = read_rds("output/gss_trends_cross.RDS")
gss_trends = read_rds("output/gss_trends_full.RDS")


race_trust = gss_trends_cross %>% filter(subgroup == "racethn" & var == "trust")
educ_trust = gss_trends_cross %>% filter(subgroup == "educ3" & var == "trust")

plot = race_trust %>% 
  filter(cat!="Other race") %>% 
  ggplot(aes(year, est, group = punch)) +
  geom_point(aes(color = cat)) +
  geom_line(aes(color = cat))
  #facet_wrap(~punch)
plot

plot = educ_trust %>% 
  ggplot(aes(year, est)) +
  geom_point(aes(color = cat)) +
  geom_line(aes(color = cat)) +
  facet_wrap(~punch)
plot

gss_trends %>% 
  filter(var=="trust") %>% 
  ggplot(aes(year, est, group = punch)) +
  geom_point(aes(color = punch)) +
  geom_line(aes(color = punch))
