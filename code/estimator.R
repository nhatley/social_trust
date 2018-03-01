estimate_trend <- function(var_in, df) {
  var1 <- as.name(var_in)
  var2 <- enquo(var1)
  df_out = df %>% 
    filter(!is.na(!!var2)) %>% 
    get_totals(var_in, ., "trend_weight", by = "year") %>% 
    gather(var, punch, -starts_with("Freq"))
  
  
  df_out$source <- paste0(df$source[1])
  return(df_out)
}


