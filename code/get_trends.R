
library(pewmethods)


gss_72_16_fctr = haven::as_factor(
  haven::read_spss(file = "data/cold_storage/GSS7216_R1.sav", user_na = T))
##^^takes forever to load

gss_72_16 = read_rds("data/gss_72_16.RDS")

names(gss_72_16) <- str_to_lower(names(gss_72_16))

gss_rec = gss_72_16 %>% 
  rename(attend_orig = attend) %>% 
  mutate(id = 1:n(),
    ideo = fct_case_when(
      polviews == 1 ~ "Very liberal",
      polviews %in% 2:3 ~ "Liberal",
      polviews == 4 ~ "Moderate",
      polviews %in% 5:6 ~ "Conservative",
      polviews == 7 ~ "Very conservative",
      polviews %in% 8:9 ~  NA_character_
    ),
    party4 = fct_case_when(
      partyid %in% 0:1 ~ "Democrat",
      partyid %in% 5:6 ~ "Republican",
      partyid %in% 2:4 ~ "Independent",
      TRUE ~ "Other"
    ),
    partysum = fct_case_when(
      partyid %in% 0:2 ~ "Dem/Dem Ln",
      partyid %in% 4:6 ~ "Rep/Rep Ln",
      TRUE ~ "No Lean"
    ),
    attend = fct_case_when(
      attend_orig %in% 5:8 ~ "More/Almost/every week", 
      attend_orig==4 ~ "Once or twice a month", 
      attend_orig %in% 2:3 ~ "A few times a year", 
      attend_orig %in% 0:1 ~ "Never",
      TRUE ~ NA_character_
    ),
    relig4 = fct_case_when(
      relig==1 | relig %in% 10:11 ~ "Protestant",
      relig==2 ~ "Catholic",
      relig == 4 ~ "Unaffiliated",
      relig %in% 98:99 ~ NA_character_,
      TRUE ~ "Other"
    ),
    hispanic = factor(ifelse(hispanic != 0, 1, 0), levels = c(1, 0), labels = c("Hispanic", "Non-Hispanic")),
    #multi_race = ifelse(racecen2 > 0 | racecen3 > 0, 4, 0), #mixed race; racecen2 +3 asks other races R idents with
    # race = case_when(racecen1 == 1 & multi_race == 0 ~ 1,
    #                  racecen1 == 2 & multi_race == 0 ~ 2,
    #                  TRUE ~ 4),
    # racethn = ifelse(hispanic == "Hispanic", 3, race),
    # racethn = fct_case_when(
    #   racethn==1 ~ "White, non-Hispanic",
    #   racethn==2 ~ "Black, non-Hispanic",
    #   racethn==3 ~ "Hispanic",
    #   racethn==4 ~ "Other race"), ##older versions dont have multi-race option
    temp_race = ifelse(hispanic=="Hispanic", 100, as.numeric(race)),
    racethn = fct_case_when(
      temp_race==1 ~ "White, non-Hispanic",
      temp_race==2 ~ "Black, non-Hispanic",
      temp_race==100 ~ "Hispanic",
        TRUE ~ "Other race"),
    parent_birth = fct_case_when(
      parborn==0 ~ "Both parents born in US",
      parborn %in% 1:2 | parborn==4 | parborn==6 | parborn==8 ~ "One/both parent born OUTSIDE US",
      TRUE ~ NA_character_
    ),
    grandparent_birth = fct_case_when(
      granborn %in% -1:0 ~ "All grandparents born in US",
      granborn %in% 1:4 ~ "At least one grandparent born OUTSIDE US",
      TRUE ~ NA_character_
    ),
    citizen = fct_case_when(
      uscitzn %in% 0:1 | uscitzn %in% 3:4 ~ "Yes",
      uscitzn==2 ~ "No",
      TRUE ~ NA_character_
    ),
    agecat3 = cut(age, breaks = c(17, 29, 64, 99), labels = c("18-29", "30-64", "65+")),
    educ3 = fct_case_when(degree == 0 ~ "HS or less",
                          degree == 1 & educ <= 12 ~ "HS or less",
                          (degree == 1 & educ >= 13 & educ < 98) | degree == 2 ~ "Some college",
                          degree == 3 ~ "College grad+",
                          degree == 4 ~ "College grad+"),
    sex = fct_case_when(
      sex==1 ~ "Male",
      sex==2 ~ "Female"
    )
  ) %>% select(id, 
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
               weight_trend = wtssall)

gss_trust_vars <- str_to_lower(c("trust", "fair", "helpful", "CONFINAN", "CONBUS", "CONCLERG", "CONEDUC", "CONFED", 
                                 "CONLABOR", "CONPRESS", "CONMEDIC", "CONTV", "CONJUDGE", "CONSCI", "CONLEGIS", "CONARMY"))

gss_72_16_trust <- gss_72_16_fctr %>% 
  mutate(id = 1:n()) %>% 
  select(id, year, gss_trust_vars) %>% 
  na_if("IAP") %>% 
  na_if("DK") %>% 
  na_if("NA") %>% 
  mutate_if(is.factor, funs(fct_drop(.)))

gss_72_16_trust_trend <- gss_72_16_trust %>% left_join(gss_rec)

source("code/functions/estimator.R")

gss_trends <- map_df(gss_trust_vars, function(v) estimate_trend(v, df = gss_72_16_trust_trend))
saveRDS(gss_trends, "output/gss_trends_full.RDS")
cross_vars <- gss_72_16_trust_trend %>% select(racethn, partysum, relig4, citizen, educ3, agecat3, parent_birth) %>% names(.)

combos = cross(list(gss_trust_vars, cross_vars)) %>%
  transpose()

gss_trends_cross = map2_df(combos[[1]], combos[[2]], estimate_trend_cross)
saveRDS(gss_trends_cross, "output/gss_trends_cross.RDS")
