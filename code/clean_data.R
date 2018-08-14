
#library(pewmethods)

##rewriting without pewmethods
library(tidyverse)

gss_trust_vars <- c("trust", "fair", "helpful", "CONFINAN", "CONBUS", "CONCLERG", "CONEDUC", "CONFED", "CONLABOR", "CONPRESS", "CONMEDIC", "CONTV", "CONJUDGE", "CONSCI", "CONLEGIS", "CONARMY") %>% 
  str_to_lower

source("code/functions/functions.R")
# gss_72_16_fctr = haven::as_factor(
#   haven::read_spss(file = "data/cold_storage/GSS7216_R1.sav", user_na = T))
# #^^takes forever to load

# gss_trust_factor_vars = gss_72_16_fctr %>% 
#   set_names(str_to_lower) %>% 
#   make_unique_id %>% ##see code/functions/functions.R
#   select(year_id,
#          one_of(gss_trust_vars))
# 
# saveRDS(gss_trust_factor_vars, "data/cold_storage/gss_72_16_trust_vars.RDS")

##saving the labels to merge on to numeric version which is much smaller

trust_labels = map_df(gss_trust_vars, function(var){
  tibble(
    var = var, 
    question_text = pluck(gss_trust_factor_vars, var, attr_getter("label")),
    levels = pluck(gss_trust_factor_vars, var, attr_getter("levels"))
  )
}) %>% 
  mutate(question_text = str_to_lower(question_text))
  
saveRDS(trust_labels, "data/gss_trust_var_labels.RDS")

gss_72_16 = read_rds("data/gss_72_16.RDS") %>% 
  set_names(str_to_lower)

#names(gss_72_16) <- str_to_lower(names(gss_72_16))

gss_rec = gss_72_16 %>% 
  rename(attend_orig = attend) %>% 
  mutate(
    ideo = as.factor(case_when(
      polviews == 1 ~ "Very liberal",
      polviews %in% 2:3 ~ "Liberal",
      polviews == 4 ~ "Moderate",
      polviews %in% 5:6 ~ "Conservative",
      polviews == 7 ~ "Very conservative",
      polviews %in% 8:9 ~  NA_character_
    )
    ),
    party4 = as.factor(case_when(
      partyid %in% 0:1 ~ "Democrat",
      partyid %in% 5:6 ~ "Republican",
      partyid %in% 2:4 ~ "Independent",
      TRUE ~ "Other"
    )
    ),
    partysum = as.factor(case_when(
      partyid %in% 0:2 ~ "Dem/Dem Ln",
      partyid %in% 4:6 ~ "Rep/Rep Ln",
      TRUE ~ "No Lean"
    )
    ),
    attend = as.factor(case_when(
      attend_orig %in% 5:8 ~ "More/Almost/every week", 
      attend_orig==4 ~ "Once or twice a month", 
      attend_orig %in% 2:3 ~ "A few times a year", 
      attend_orig %in% 0:1 ~ "Never",
      TRUE ~ NA_character_
    )
    ),
    relig4 = as.factor(case_when(
      relig==1 | relig %in% 10:11 ~ "Protestant",
      relig==2 ~ "Catholic",
      relig == 4 ~ "Unaffiliated",
      relig %in% 98:99 ~ NA_character_,
      TRUE ~ "Other"
    )
    ),
    hispanic = 
      factor(ifelse(hispanic != 0, 1, 0),
             levels = c(1, 0),
             labels = c("Hispanic", "Non-Hispanic")),
    temp_race = ifelse(hispanic=="Hispanic", 100, as.numeric(race)),
    racethn = as.factor(case_when(
      temp_race==1 ~ "White, non-Hispanic",
      temp_race==2 ~ "Black, non-Hispanic",
      temp_race==100 ~ "Hispanic",
        TRUE ~ "Other race"
    )
    ),
    parent_birth = as.factor(case_when(
      parborn==0 ~ "Both parents born in US",
      parborn %in% 1:2 | parborn==4 | parborn==6 | parborn==8 ~ "One/both parent born OUTSIDE US",
      TRUE ~ NA_character_
    )
    ),
    grandparent_birth = as.factor(case_when(
      granborn %in% -1:0 ~ "All grandparents born in US",
      granborn %in% 1:4 ~ "At least one grandparent born OUTSIDE US",
      TRUE ~ NA_character_
    )
    ),
    citizen = as.factor(case_when(
      uscitzn %in% 0:1 | uscitzn %in% 3:4 ~ "Yes",
      uscitzn==2 ~ "No",
      TRUE ~ NA_character_
    )
    ),
    agecat3 = cut(age, breaks = c(17, 29, 64, 99), labels = c("18-29", "30-64", "65+")),
    educ3 = as.factor(case_when(
      degree == 0 ~ "HS or less",
      degree == 1 & educ <= 12 ~ "HS or less",
      (degree == 1 & educ >= 13 & educ < 98) | degree == 2 ~ "Some college",
      degree == 3 ~ "College grad+",
      degree == 4 ~ "College grad+"
    )
      ),
    sex = as.factor(case_when(
      sex==1 ~ "Male",
      sex==2 ~ "Female"
    )
    )
  ) %>% select(id, 
               year,
               ideo,
               party4,
               partysum,
               attend,
               relig4,
               racethn,
               temp_race,
               hispanic,
               parent_birth,
               grandparent_birth,
               citizen,
               agecat3,
               educ3, 
               sex,
               year,
               weight_trend = wtssall,
               one_of(gss_trust_vars)) %>% 
  make_unique_id

gss_trust_factor_recodes <- map_dfc(gss_trust_vars, 
                                   function(var_in){#var_in = "trust"
  sym_var = rlang::sym(var_in)
  labels = trust_labels %>% 
    filter(var == !!var_in)
  out = gss_rec %>% 
    #group_by(year_id) %>% 
    transmute(!!sym_var := factor(!!sym_var, labels = labels$levels),
              year_id = year_id)
  # out = out %>%
  #   select(year_id,
  #     one_of(gss_trust_vars))
  return(out)
}) %>% 
  select(year_id,
         one_of(gss_trust_vars))
  
gss_to_estimate = gss_rec %>% 
  select(-one_of(gss_trust_vars)) %>% 
  left_join(gss_trust_factor_recodes) %>% 
  na_if("IAP") %>% 
  na_if("DK") %>% 
  na_if("NA") %>% 
  mutate_if(is.factor, funs(fct_drop(.)))


