rm(list = ls())
options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors

library(tidyverse)
library(readxl)
library(lavaan)
library(knitr)
library(flextable)
library(officer)
library(data.table)
library(fastDummies)

if (Sys.getenv('USERNAME') == 'ATHEISIN'){
  setwd("C:/Users/atheisin/Box/VoW Analysis")
} else {
  setwd(paste0('/Users/',Sys.getenv('USER'),'/Library/CloudStorage/Box-Box/VoW Analysis'))
}

survey_2021 <- readxl::read_xlsx("VoW data March 2024/2021.xlsx") %>% 
  dplyr::select(QB, Q1B, Q1C, Q3, Q4, Q5, Q6, Q8, Q14, Q15, Q16A, Q16B, Q16C, Q16E, Q16F, Q17, Q18, Q19, Q20,Q21A, Q21B, Q21C, Q21D, Q22, Q24, Q28, TSMART_STATE) %>% 
  rename(age = QB,
         trust_gov_state = Q1B,
         trust_gov_local = Q1C,
         trust_drink_water_home = Q3,
         trust_water_safety = Q4,
         drink_water_quality_loc = Q6,
         trust_pipes_safety = Q5,
         water_infras_quality_loc = Q8,
         water_svc_satisf = Q14,
         water_svc_afford = Q15, 
         water_svc_qual_main_break = Q16A,
         water_svc_qual_swg_ovfl = Q16B,
         water_svc_qual_low_press = Q16C,
         water_svc_qual_dirty = Q16E,
         water_svc_qual_taste = Q16F,
         wtp_rate_increase_qual_reduce_pollution = Q17,
         wtp_rate_increase_qual_drinking_trust_water_safety = Q18,
         wtp_rate_increase_qual_taste_smell = Q19,
         wtp_rate_increase_qual_prevent_shutoff = Q20,
         wtp_50 = Q21A,
         wtp_40 = Q21B,
         wtp_30 = Q21C,
         wtp_20 = Q21D,
         urbanicity = Q22,
         political_views = Q24,
         income_before_tax = Q28,
         state = TSMART_STATE) %>% 
  mutate(year=2021)
  
survey_2022 <- readxl::read_xlsx("VoW data March 2024/2022.xlsx") %>% 
  dplyr::select(QB, Q1B, Q1C, Q3, Q4, Q5, Q6, Q8, Q17, Q18, Q20A, Q20B, Q20C, Q20E, Q20F, Q21, Q22, Q23, Q24, Q25A, Q25B, Q25C, Q25D, Q26, Q27, Q29, Q33, RECORDID) %>% 
  rename(age = QB,
         trust_gov_state = Q1B,
         trust_gov_local = Q1C,
         trust_drink_water_home = Q3,
         trust_water_safety = Q4,
         drink_water_quality_loc = Q6,
         trust_pipes_safety = Q5,
         water_infras_quality_loc = Q8,
         water_svc_satisf = Q17,
         water_svc_afford = Q18, 
         water_svc_qual_main_break = Q20A,
         water_svc_qual_swg_ovfl = Q20B,
         water_svc_qual_low_press = Q20C,
         water_svc_qual_dirty = Q20E,
         water_svc_qual_taste = Q20F,
         wtp_rate_increase_qual_reduce_pollution = Q21,
         wtp_rate_increase_qual_drinking_trust_water_safety = Q22,
         wtp_rate_increase_qual_taste_smell = Q23,
         wtp_rate_increase_qual_prevent_shutoff = Q24,
         wtp_50 = Q25A,
         wtp_40 = Q25B,
         wtp_30 = Q25C,
         wtp_20 = Q25D,
         water_source = Q26,
         urbanicity = Q27,
         political_views = Q29,
         income_before_tax = Q33,
         state = RECORDID) %>% 
  mutate(state = substr(state,1,2)) %>%
  mutate(year=2022)
  
  
  
survey_2023 <- readxl::read_xlsx("VoW data March 2024/2023.xlsx") %>% 
  dplyr::select(QB, Q1b, Q1c, Q3, Q4, Q5, Q6, Q8, Q17, Q18, Q20a, Q20b, Q20c, Q20e, Q20f, Q21, Q22, Q23, Q24, Q25a, Q25b, Q25c, Q25d, Q26, Q27, Q29, Q33, recordid) %>% 
  rename(age = QB,
         trust_gov_state = Q1b,
         trust_gov_local = Q1c,
         trust_drink_water_home = Q3, #
         trust_water_safety = Q4, #
         drink_water_quality_loc = Q6,
         trust_pipes_safety = Q5, #
         water_infras_quality_loc = Q8,
         water_svc_satisf = Q17,
         water_svc_afford = Q18, 
         water_svc_qual_main_break = Q20a,
         water_svc_qual_swg_ovfl = Q20b,
         water_svc_qual_low_press = Q20c,
         water_svc_qual_dirty = Q20e,
         water_svc_qual_taste = Q20f,
         wtp_rate_increase_qual_reduce_pollution = Q21,
         wtp_rate_increase_qual_drinking_trust_water_safety = Q22,
         wtp_rate_increase_qual_taste_smell = Q23,
         wtp_rate_increase_qual_prevent_shutoff = Q24,
         wtp_50 = Q25a,
         wtp_40 = Q25b,
         wtp_30 = Q25c,
         wtp_20 = Q25d,
         water_source = Q26,
         urbanicity = Q27,
         political_views = Q29,
         income_before_tax = Q33,
         state = recordid
         ) %>% 
  mutate(year=2023) %>%
  mutate(state = substr(state,1,2))
   

vow_surveys <- survey_2021 %>% 
  plyr::rbind.fill(survey_2022) %>% 
  plyr::rbind.fill(survey_2023) %>% 
  mutate(age = case_when(age==1 ~ "18-29",
                         age==2 ~ "18-29",
                         age==3 ~ "30-39",
                         age==4 ~ "30-39",
                         age==5 ~ "40-49",
                         age==6 ~ "40-49",
                         age==7 ~ "50-59",
                         age==8 ~ "50-59",
                         age==9 ~ "60-74",
                         age==10 ~ "60-74",
                         age==11 ~ "75+",
                         age==12 ~ "refused"),
         #across(starts_with("trust_gov"), ~ ifelse(.x == 5, NA, .x)),
         across(c(trust_gov_local, trust_gov_state),
                list(~ factor(case_when(.==1 ~ "always",
                                        .==2 ~ "most of the time",
                                        .==3 ~ "some of the time",
                                        .==4 ~ "never",
                                        TRUE ~ "DK/NA"),
                              levels=c("never", "some of the time", "DK/NA",  "most of the time", "always"), ordered = T)),
                .names="{.col}"),
         drink_home_type = case_when(trust_drink_water_home==1 ~ "unfiltered tap",
                                     trust_drink_water_home==2 ~ "filtered tap",
                                     trust_drink_water_home==3 ~ "bottled water",
                                     trust_drink_water_home==4 ~ "bottled water",
                                     trust_drink_water_home==5 ~ "Other",
                                     trust_drink_water_home==6 ~ "DK/NA",
                                   ),
         #trust_drink_water_home = ifelse(trust_drink_water_home==6,NA, trust_drink_water_home),
         across(c(trust_drink_water_home),
              list(~ factor(case_when(.==1 ~ "unfiltered tap",
                                      .==2 ~ "filtered tap",
                                      .==3 ~ "bottled water",
                                      .==4 ~ "bottled water",
                                      .==5 ~ "Other",
                                      .==6 ~ "DK/NA",),
                       levels=c("Other","bottled water","DK/NA", "filtered tap", "unfiltered tap"), ordered = T)),
         .names="{.col}"),
         #trust_water_safety = ifelse(trust_water_safety==5,NA, trust_water_safety),
         #trust_pipes_safety = ifelse(trust_pipes_safety==5,NA, trust_pipes_safety),
         across(c(trust_water_safety, trust_pipes_safety),
                list(~factor(case_when(.==1 ~ "favS",
                                .==2 ~ "favSW",
                                .==3 ~ "ufavS",
                                .==4 ~ "ufavSW",
                                .==5 ~ "DK/NA"),
                     levels=c("ufavS", "ufavSW", "DK/NA", "favSW","favS"), ordered = T)),
                .names="{.col}"),
         #water_svc_satisf = ifelse(water_svc_satisf==5,NA, water_svc_satisf),
         across(c(water_svc_satisf),
                list(~ factor(case_when(.==1 ~ "very satisfied",
                                        .==2 ~ "sw satisfied",
                                        .==3 ~ "sw unsatisfied",
                                        .==4 ~ "very unsatisfied",
                                        .==5 ~ "DK/NA"), 
                              levels=c("very unsatisfied","sw unsatisfied","DK/NA","sw satisfied","very satisfied"), ordered = T)),
                .names="{.col}"),
         #water_svc_afford = ifelse(water_svc_afford==5,NA, water_svc_afford),
         across(c(water_svc_afford),
                list(~ factor(case_when(.==1 ~ "very affordable",
                                        .==2 ~ "sw affordable",
                                        .==3 ~ "sw unaffordable",
                                        .==4 ~ "very unaffordable",
                                        .==5 ~ "DK/NA"), 
                              levels=c("very unaffordable","sw unaffordable","DK/NA","sw affordable","very affordable"), ordered = T)),
                .names="{.col}"),
         #drink_water_quality_loc = ifelse(drink_water_quality_loc==5,NA, drink_water_quality_loc),
         across(c(drink_water_quality_loc),
                list(~ factor(case_when(.==1 ~ "extremely concerned",
                                        .==2 ~ "very concerned",
                                        .==3 ~ "sw concerned",
                                        .==4 ~ "not concerned",
                                        .==5 ~ "DK/NA"), 
                              levels=c("extremely concerned","very concerned","sw concerned","DK/NA","not concerned"), ordered = T)),
                .names="{.col}"),
         #water_infras_quality_loc = ifelse(water_infras_quality_loc==5,NA, water_infras_quality_loc),
         across(c(water_infras_quality_loc),
                list(~ factor(case_when(.==1 ~ "very good",
                                        .==2 ~ "sw good",
                                        .==3 ~ "sw bad",
                                        .==4 ~ "very bad",
                                        .==5 ~ "DK/NA"), 
                              levels=c("very bad","sw bad","DK/NA","sw good","very good"), ordered = T)),
                .names="{.col}"),
         #across(starts_with("water_svc_qual"), ~ ifelse(.x == 5, NA, .x)),
         across(c(starts_with("water_svc_qual")),
                list(~ factor(case_when(.==1 ~ "freq",
                                       .==2 ~ "occasionally",
                                       .==3 ~ "rarely",
                                       .==4 ~ "never",
                                       .==5 ~ "DK/NA"),
                              levels = c("freq", "occasionally", "rarely", "DK/NA", "never"), ordered = T)),
                .names="{.col}"),
         #across(matches("^wtp_\\d+$"), ~ ifelse(.x == 5, NA, .x)),
         across(
           matches("^wtp_\\d+$"),
           ~ case_when(
             .x == 1 ~ "very willing",
             .x == 2 ~ "sw willing",
             .x == 3 ~ "sw unwilling",
             .x == 4 ~ "very unwilling",
             .x == 5 ~ "DK/NA"
           ),
           .names = "{.col}"
         ),
         wtp = case_when(!is.na(wtp_rate_increase_qual_reduce_pollution) ~ "infrasture/pollution",
                         !is.na(wtp_rate_increase_qual_drinking_trust_water_safety) ~ "drinking water safety",
                         !is.na(wtp_rate_increase_qual_taste_smell) ~ "drinking water taste and smell",
                         !is.na(wtp_rate_increase_qual_prevent_shutoff) ~ "help ensure water affordability for all"),
         wtp_value_relaxed = case_when(wtp_50 %in% c('very willing','sw willing') ~ '$50',
                                       wtp_40 %in% c('very willing','sw willing') ~ '$40',
                                       wtp_30 %in% c('very willing','sw willing') ~ '$30',
                                       wtp_20 %in% c('very willing','sw willing') ~ '$20',
                                       TRUE ~ '<$20'),
         wtp_value_relaxed = factor(wtp_value_relaxed, levels = c('<$20','$20','$30','$40','$50')),
         wtp_value_strict = case_when(wtp_50 %in% c('very willing') ~ '$50',
                                       wtp_40 %in% c('very willing') ~ '$40',
                                       wtp_30 %in% c('very willing') ~ '$30',
                                       wtp_20 %in% c('very willing') ~ '$20',
                                       TRUE ~ '<$20'),
         wtp_value_strict = factor(wtp_value_strict, levels = c('<$20','$20','$30','$40','$50')),
         urban_type = case_when(urbanicity==4 ~ "Rural",
                                urbanicity==3 ~ "Small Town",
                                urbanicity==2 ~ "Suburban",
                                urbanicity==1 ~ "City",
                                T ~ "DK/Refused"),
         urban_type = factor(urban_type, levels = c("DK/Refused","Rural","Small Town","Suburban","City"), ordered = T), #ranked rural to big city 
         urban_type_chr = as.character(urban_type),
         urbanicity = factor(urbanicity, levels = c(4,3,2,1), ordered = T), #ranked rural to big city 
         political_views_type = case_when(political_views==1 ~ "Very liberal",
                                     political_views==2 ~ "Somewhat liberal",
                                     political_views==3 ~ "Moderate",
                                     political_views==4 ~ "Somewhat conservative",
                                     political_views==5 ~ "Conservative",
                                     T ~ "DK/Refused"),
         political_views_type = factor(political_views_type, levels = c("Very liberal","Somewhat liberal","Moderate","DK/Refused","Somewhat conservative","Conservative"), ordered = T),  #ranked liberal to conservative
         #political_views = factor(political_views, levels = c(1,2,3,4,5), ordered = T),  #ranked liberal to conservative
         income_before_tax_type = case_when(income_before_tax==1 ~ "<$30k",
                                            income_before_tax==2 ~ "$30k-$60k",
                                            income_before_tax==3 ~ "$60k-$75k",
                                            income_before_tax==4 ~ "$75k-$100k",
                                            income_before_tax==5 ~ ">$100k",
                                            T ~ "DK/Refused"),
         income_before_tax_type =  factor(income_before_tax_type, levels = c("DK/Refused","<$30k","$30k-$60k","$60k-$75k","$75k-$100k",">$100k"), ordered = T), #ranked low income (under 30k) to high income (over 100k)
         water_source_type = case_when(water_source==1 ~ "Local water system",
                                       water_source==2 ~ "Private well",
                                       T ~ "DK/Refused"),
         water_source_type = factor(water_source_type, levels = c("Local water system","Private well","DK/Refused"), ordered = T),  #ranked liberal to conservative
         #wtp_value_strict2 = gsub("\\$", "", wtp_value_strict),
         #wtp_value_strict2 = ifelse(wtp_value_strict2=="<20", "19",wtp_value_strict2),
         #wtp_value_strict2 = as.numeric(wtp_value_strict2),
         #wtp_value_strict3 = scale(wtp_value_strict2),
         #wtp_value_relaxed = as.factor(wtp_value_relaxed),
         #wtp_value_relaxed2 = gsub("\\$", "", wtp_value_relaxed),
         #wtp_value_relaxed2 = ifelse(wtp_value_relaxed2=="<20", "19",wtp_value_relaxed2),
         #wtp_value_relaxed2 = as.numeric(wtp_value_relaxed2),
         #wtp_value_relaxed3 = scale(wtp_value_relaxed2),
         y2022 = ifelse(year==2022, 1, 0),
         y2023 = ifelse(year==2023, 1, 0),
         area_rural = ifelse(urbanicity==4,1,0),
         area_small_town = ifelse(urbanicity==3,1,0),
         area_suburban = ifelse(urbanicity==2,1,0),
         income_before_tax_low = ifelse(income_before_tax==1,1,0),
         income_before_tax_high = ifelse(income_before_tax==5,1,0),
         wtp_dw_safety = ifelse(wtp=="drinking water safety",1, 0),
         wtp_olig = ifelse(wtp=="drinking water taste and smell",1,0),
         wtp_affordability = ifelse(wtp=="help ensure water affordability for all",1,0)
  ) %>% 
  mutate(region_broad = case_when(
    state %in% c('ME','NH','VT','MA','RI','CT','NY','PA','NJ') ~ "NE",
    state %in% c('OH','IN','MI','WI','IL','MO','IA','MN','ND','SD','NE','KS') ~ "MW",
    state %in% c('DE','MD','DC','WV','VA','NC','KY','TN','SC','GA','AL','MS','AR','OK','TX','LA','FL') ~ "S",
    TRUE ~ "W"
  )) %>% 
  #dummy_cols(select_columns = "REGION", remove_first_dummy = FALSE) %>% 
  # dummy_cols(select_columns = "CENSUS_DIV", remove_first_dummy = FALSE) %>% 
  dummy_cols(select_columns = "region_broad", remove_first_dummy = FALSE) %>%
  select(-wtp_rate_increase_qual_drinking_trust_water_safety,
         -wtp_rate_increase_qual_prevent_shutoff,
         -wtp_rate_increase_qual_taste_smell,
         -wtp_rate_increase_qual_reduce_pollution,
         -political_views,
         # -trust_drink_water_home,
         -urbanicity,
         -income_before_tax,
         -water_source)

rm(survey_2021,survey_2022,survey_2023)

 
