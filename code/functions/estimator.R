estimate_trend <- function(var_in, df) {
  var1 <- as.name(var_in)
  var2 <- enquo(var1)
  df_out = df %>% 
    filter(!is.na(!!var2)) %>% 
    get_totals(var_in, ., "weight_trend", by = "year") %>% 
    gather(year, est, -var_in) %>% 
    gather(var, punch, -one_of("est", "year")) %>% 
    select(everything(), est, year)
  
  return(df_out)
}



estimate_trend_cross <- function(var_in, sg_var) {
  var1 <- as.name(var_in)
  var2 <- enquo(var1)
  sg_var1 <- as.name(sg_var)
  sg_var2 <- enquo(sg_var1)
  print(var2)
  print(sg_var2)

df = gss_72_16_trust_trend %>% 
  filter(!is.na(!!var1)) %>% 
  filter(!is.na(!!sg_var1)) %>% 
  group_by(!!sg_var1, !!var1, year) %>% 
  summarise(weight = sum(weight_trend, na.rm = T)) %>%
  group_by(!!sg_var1, year) %>% 
  mutate(total = sum(weight),
         est = weight/total) %>% 
  select(!!sg_var1, !!var1, year, est)  %>% 
  gather(subgroup, cat, -year, -est, -!!var1) %>%
  gather(var, punch, -subgroup, -cat, -year, -est) %>%
  select(subgroup, cat, var, punch, year, est)
return(df)
}

