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


estimate_trend <- function(var_in, df) {#var_in = "fair" df = gss_to_estimate
  sym_var = rlang::sym(var_in)
  
  df_out = df %>% 
    filter(!is.na(!!sym_var)) %>% 
    group_by(!!sym_var, year) %>% 
    summarise(wt = sum(weight_trend),
              obs = n()) %>% 
    group_by(year) %>% 
    mutate(wt_total = sum(wt),
           wt_est = wt/wt_total,
           unwt_total_obs_year = sum(obs)
    ) %>% 
    arrange(year) %>% 
    select(-wt_total, -wt) %>% 
    gather(var, punch, var_in) %>% 
    select(year, var, punch, wt_est, starts_with("unwt_"))
  
  return(df_out)
}

estimate_trend_cross <- function(var_in, sg_var, df) {#sg_var = "racethn"
  sym_var = rlang::sym(var_in)
  sym_sg_var = rlang::sym(sg_var)
  print(var_in)
  print(sg_var)
  
  df_out = df %>% 
    filter(!is.na(!!sym_var)) %>% 
    filter(!is.na(!!sym_sg_var)) %>% 
    group_by(!!sym_sg_var, !!sym_var, year) %>% 
    summarise(wt = sum(weight_trend, na.rm = T),
              obs = n()) %>%
    group_by(!!sym_sg_var, year) %>% 
    mutate(wt_total = sum(wt),
           wt_est = wt/wt_total,
           unwt_total_obs_subgroup = sum(obs)
    ) %>% 
    #select(-obs) %>% 
    gather(subgroup_var, subgroup, sg_var) %>%
    gather(var, punch, var_in) %>%
    group_by(year) %>% 
    mutate(unwt_total_obs_year = sum(obs)) %>% 
    select(year, var, punch, subgroup, subgroup_var, wt_est, starts_with("unwt_"))
  return(df_out)
}
