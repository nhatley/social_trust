make_unique_id <- function(df, year_var = "year", id_var = "id") {

  
  if(year_var != "year" & id_var != "id") {
    year = rlang::sym(year_var)
    id = rlang::sym(id)
    
    out = df %>% 
      mutate(year_id = paste0(!!id, "_", !!year))   
  }
  
  if(year_var != "year") {
    year = rlang::sym(year_var)
    
    out = df %>% 
      mutate(year_id = paste0(id, "_", !!year))   
  }
  
  if(id_var != "id") {
    id = rlang::sym(id)
    
    out = df %>% 
      mutate(year_id = paste0(!!id, "_", year))   
  }
  else{
    out = df %>% 
      mutate(year_id = paste0(id, "_", year))   
    
  }
    
    
  return(out)
}


estimate_trend <- function(var_in, df) {#var_in = "trust"
  # var1 <- as.name(var_in)
  # var2 <- enquo(var1)
  sym_var = rlang::sym(var_in)
  
  df_out = df %>% 
    filter(!is.na(!!sym_var)) %>% 
    group_by(!!sym_var, year) %>% 
    summarise(wt = sum(weight_trend)) %>% 
    group_by(year) %>% 
    mutate(wt_total = sum(wt),
           est = wt/wt_total
    ) %>% 
    arrange(year) %>% 
    select(-wt_total, -wt) %>% 
    gather(var, punch, -one_of("est", "year")) 
  
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
