library(pewmethods)

cps_08_13_read = read.csv("benchmark_data/CPS_CIVENG_08_13/cps_00015.csv", 
                          header =  T)

# cps_08_13_old = read.csv("benchmark_data/CPS_CIVENG_08_13/old/cps_00014.csv", 
#                           header =  T)

# CEORGCIV (Participated in service or civic organization)
# CEORGCOM (Participated in school, neighborhood, or community association)
# CEORGOTHER (Participated in other type of organization)
# CEORGRELIG (Participated in religious organization)
# CEORGSPORT (Participated in sports/recreation organization)
# CEFAMDINNR (Frequency eating dinner with household members)
# CEFAVORS (Frequency doing favors for neighbors and vice-versa)
# CESOCIALIZE (Frequency talk with neighbors)
# CENEIGHCONF (Trust in people in neighborhood)
# CEPOLCONV (Frequency discussing politics with friends or family)
# CEPUBOFF (Contacted a public official)
# CEVOTEFREQ (Frequency voting in local elections)

cps_civeng_08_13  = cps_08_13_read %>% 
  filter(MONTH==11) %>%   #filters to civeng 
  filter(AGE>17) %>%
  mutate(VOTE_LOCAL_BM = as.factor(ifelse(CEVOTEFREQ < 98, CEVOTEFREQ, NA)),
         CONT_OFFICIAL_BM = as.factor(ifelse(CEPUBOFF < 98, CEPUBOFF, NA)),
         TALK_NGHBRS_BM = as.factor(ifelse(CESOCIALIZE < 98, CESOCIALIZE, NA)), #only had less than once a month >2010
         TRUST_NGHBRS_BM = as.factor(ifelse(CENEIGHCONF < 98, CENEIGHCONF, NA)),
         COMM_ASSOC_BM = as.factor(ifelse(CEORGCOM < 98, CEORGCOM, NA)),
         CIVIC_ASSOC_BM = as.factor(ifelse(CEORGCIV < 98, CEORGCIV, NA)), 
         REC_ASSOC_BM = as.factor(ifelse(CEORGSPORT < 98, CEORGSPORT, NA)),
         year = YEAR,
         hh_id = SERIAL,
         per_id = PERNUM,
         cps_id = CPSIDP,
         hh_final_wt = HWTFINL, 
         #hh_sup_wt = HWTSUPP,
         final_wt = WTFINL,
         sup_wt = WTSUPP
  ) %>% 
  mutate(na_weight = ifelse(is.na(sup_wt), 1, 0))

make_est <- function(var){
  quote_var <- enquo(var) #this makes var== ~var (for dplyr progamming)
  named_var <- quo_name(quote_var) #this makes var a string ("var")
  
  cps_civeng_08_13 %>% 
    filter(!is.na(!!quote_var)) %>%  
    get_totals(named_var, ., "final_wt", by = "YEAR") %>% 
    gather(year, est, -!!quote_var) %>% 
    gather(var, punch, -year, -est) %>% 
    select(var, punch, year, est)
}

#test = make_est(CONT_OFFICIAL_BM)

VOTE_LOCAL_BM = make_est(VOTE_LOCAL_BM) %>% 
  mutate(label = case_when(
    punch==1 ~ "Never",
    punch==2 ~ "Rarely",
    punch==3 ~ "Sometimes",
    punch==4 ~ "Always"
  ))
CONT_OFFICIAL_BM = make_est(CONT_OFFICIAL_BM) %>% 
  mutate(label = case_when(
    punch==1 ~ "No",
    punch==2 ~ "Yes"
  ))
TRUST_NGHBRS_BM = make_est(TRUST_NGHBRS_BM) %>% 
  mutate(label = case_when(
    punch==1 ~ "None",
    punch==2 ~ "Some",
    punch==3 ~ "Most",
    punch==4 ~ "All"
  ))
COMM_ASSOC_BM = make_est(COMM_ASSOC_BM) %>% 
  mutate(label = case_when(
    punch==1 ~ "No",
    punch==2 ~ "Yes"
  ))
CIVIC_ASSOC_BM = make_est(CIVIC_ASSOC_BM) %>% 
  mutate(label = case_when(
    punch==1 ~ "No",
    punch==2 ~ "Yes"
  ))
REC_ASSOC_BM = make_est(REC_ASSOC_BM) %>% 
  mutate(label = case_when(
    punch==1 ~ "No",
    punch==2 ~ "Yes"
  ))

civeng_trends = bind_rows(lapply(ls(pattern="_BM"), FUN = get)) %>% as.data.frame()

label_data = civeng_trends %>% filter(year==2011)

civeng_trends_plot = civeng_trends %>% 
  filter(punch < 96) %>% 
  ggplot(aes(year, est, group = factor(punch))) +
  geom_point(aes(color = factor(punch)), alpha = .6, size = .85) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), 
                     labels = c(0,10,20,30,40,50,60,70,80,90,100)) +
  geom_line(aes(color = factor(punch))) + facet_wrap(~var, ncol = 2) +
  geom_text_repel(data = label_data, aes(label = label), size = 2.75) +
  theme_bw()

ggsave("output/cps_civeng_trends.pdf", width = 8.5, height =11)



# 
# cps13 = cps_civeng_08_13 %>% filter(year==2013)
# weights = cps_civeng_08_13 %>% select(cps_id, final_wt, sup_wt, year)
# 
# isexact = weights %>% 
#   group_by(year, cps_id) %>% 
#   mutate(samewts = ifelse(final_wt==sup_wt, 1, 0))
#   tablena(isexact$samewts, isexact$year)
##NOTE: sup_wts and the final weights are the same for civgeng supplements 

