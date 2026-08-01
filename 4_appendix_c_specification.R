#===============================================================================
#Alternative latent variable construction
#Appendix C
#===============================================================================

ordered_vars <- c("wtp_value_strict",
                  "trust_water_safety", "drink_water_quality_loc", "trust_pipes_safety", 
                  "trust_gov_local","trust_gov_state", "trust_drink_water_home",
                  "water_infras_quality_loc", "water_svc_satisf", "water_svc_qual_taste",
                  "water_svc_qual_dirty",
                  "water_svc_afford", "water_svc_qual_main_break",
                  "water_svc_qual_swg_ovfl", "water_svc_qual_low_press",
                  "income_before_tax", "urbanicity", "political_views", "age"
)

#main specification 
fits_main <- list(
  safety  = vow_surveys %>% filter(wtp == "drinking water safety"),
  taste   = vow_surveys %>% filter(wtp == "drinking water taste and smell"),
  poll    = vow_surveys %>% filter(wtp == "infrasture/pollution"),
  afford  = vow_surveys %>% filter(wtp == "help ensure water affordability for all")
)

#############################################################
## Alternate latent variable construction 1 (Tables C1, C2)

model_con_trust_strict_AVE_testing1 <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~ trust_water_safety + trust_pipes_safety + drink_water_quality_loc + trust_gov_local
  satisfaction =~  water_svc_satisf + water_svc_afford + water_infras_quality_loc
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*trust + c2*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023
  
  

  ###########################
  # Indirect effects
  ###########################
  ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
'

latent_loadings_con_trust_testing1 <- get_latent_loadings(fits_main, model_con_trust_strict_AVE_testing1, ordered_vars)

latent_variables_con_trust_testing1 <-  reduce(
  latent_loadings_con_trust_testing1,
  ~ full_join(.x, .y, by = c("rhs"))
) %>% 
  rename(`Drinking Water Safety` = safety,
         `Drinking water Taste & Smell` = taste,
         `Infrastructure and Water Quality` = poll,
         `Prevent Shutoffs` = afford
  ) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C1 Latent Variable Construction Committment Trust Theory")

struct_list_main1strict_testing1 <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict_AVE_testing1,
  ordered_vars = ordered_vars
)

struct_table_main_robustness_AVE1 <- make_struct_table(struct_list_main1strict_testing1) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C2 Results for Commitment Trust Theory using Strict Measure of WTP")


#############################################################
## Alternate latent variable construction 2 (Tables C3, C4)

model_con_trust_strict_AVE_testing2 <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~ trust_water_safety + trust_pipes_safety + drink_water_quality_loc + trust_gov_state + trust_gov_local
  satisfaction =~  water_svc_satisf + water_svc_afford + water_infras_quality_loc
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*trust + c2*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023
  

  ###########################
  # Indirect effects
  ###########################
  ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
'


latent_loadings_con_trust_testing2 <- get_latent_loadings(fits_main, model_con_trust_strict_AVE_testing2, ordered_vars)


latent_variables_con_trust_testing2 <-  reduce(
  latent_loadings_con_trust_testing2,
  ~ full_join(.x, .y, by = c("rhs"))
) %>% 
  rename(`Drinking Water Safety` = safety,
         `Drinking water Taste & Smell` = taste,
         `Infrastructure and Water Quality` = poll,
         `Prevent Shutoffs` = afford
  ) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C3 Latent Variable Construction Committment Trust Theory")


struct_list_main1strict_testing2 <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict_AVE_testing2,
  ordered_vars = ordered_vars
)

struct_table_main_robustness_AVE2 <- make_struct_table(struct_list_main1strict_testing2) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C4 Results for Commitment Trust Theory using Strict Measure of WTP")




#############################################################
## Alternate latent variable construction 3 (Tables C5, C6)

model_con_trust_strict_AVE_testing3 <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~ trust_water_safety + trust_pipes_safety + drink_water_quality_loc  
  satisfaction =~  water_svc_satisf + water_infras_quality_loc
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*trust + c2*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023
  

  ###########################
  # Indirect effects
  ###########################
  ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
'

latent_loadings_con_trust_testing3 <- get_latent_loadings(fits_main, model_con_trust_strict_AVE_testing3, ordered_vars)


latent_variables_con_trust_testing3 <-  reduce(
  latent_loadings_con_trust_testing3,
  ~ full_join(.x, .y, by = c("rhs"))
) %>% 
  rename(`Drinking Water Safety` = safety,
         `Drinking water Taste & Smell` = taste,
         `Infrastructure and Water Quality` = poll,
         `Prevent Shutoffs` = afford
  ) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C5 Latent Variable Construction Committment Trust Theory")


struct_list_main1strict_testing3 <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict_AVE_testing3,
  ordered_vars = ordered_vars
)

struct_table_main_robustness_AVE3 <- make_struct_table(struct_list_main1strict_testing3) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C6 Results for Commitment Trust Theory using Strict Measure of WTP")



#############################################################
## Alternate latent variable construction 1 (Tables C7, C8)

model_con_trust_strict_AVE_testing4 <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~  trust_gov_state + trust_gov_local + drink_water_quality_loc + trust_drink_water_home
  satisfaction =~  water_svc_satisf + water_svc_afford + water_infras_quality_loc
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*trust + c2*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023
  
  

  ###########################
  # Indirect effects
  ###########################
  ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
'

latent_loadings_con_trust_testing4 <- get_latent_loadings(fits_main, model_con_trust_strict_AVE_testing4, ordered_vars)

latent_variables_con_trust_testing4 <-  reduce(
  latent_loadings_con_trust_testing4,
  ~ full_join(.x, .y, by = c("rhs"))
) %>% 
  rename(`Drinking Water Safety` = safety,
         `Drinking water Taste & Smell` = taste,
         `Infrastructure and Water Quality` = poll,
         `Prevent Shutoffs` = afford
  ) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C7 Latent Variable Construction Committment Trust Theory")

struct_list_main1strict_testing4 <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict_AVE_testing4,
  ordered_vars = ordered_vars
)

struct_table_main_robustness_AVE4 <- make_struct_table(struct_list_main1strict_testing4) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C8 Results for Commitment Trust Theory using Strict Measure of WTP")

 
#############################################################
## Alternate latent variable construction 1 (Tables C1, C2)

model_con_trust_strict_AVE_testing5 <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~ trust_water_safety + trust_pipes_safety + drink_water_quality_loc + trust_drink_water_home
  satisfaction =~  water_svc_satisf + water_svc_afford + water_infras_quality_loc
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*trust + c2*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023
  
  

  ###########################
  # Indirect effects
  ###########################
  ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
'

latent_loadings_con_trust_testing5 <- get_latent_loadings(fits_main, model_con_trust_strict_AVE_testing5, ordered_vars)

latent_variables_con_trust_testing5 <-  reduce(
  latent_loadings_con_trust_testing5,
  ~ full_join(.x, .y, by = c("rhs"))
) %>% 
  rename(`Drinking Water Safety` = safety,
         `Drinking water Taste & Smell` = taste,
         `Infrastructure and Water Quality` = poll,
         `Prevent Shutoffs` = afford
  ) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C9 Latent Variable Construction Committment Trust Theory")

struct_list_main1strict_testing5 <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict_AVE_testing5,
  ordered_vars = ordered_vars
)

struct_table_main_robustness_AVE5 <- make_struct_table(struct_list_main1strict_testing5) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table C10 Results for Commitment Trust Theory using Strict Measure of WTP")

 
 

doc_main_model_spec <- read_docx() %>%
  body_add_flextable(latent_variables_con_trust_testing1) %>%
  body_add_par("") %>%
  body_add_flextable(struct_table_main_robustness_AVE1) %>%
  body_add_par("") %>%
  body_add_flextable(latent_variables_con_trust_testing2) %>%
  body_add_par("") %>%
  body_add_flextable(struct_table_main_robustness_AVE2) %>%
  body_add_par("") %>%
  body_add_flextable(latent_variables_con_trust_testing3) %>%
  body_add_par("") %>%
  body_add_flextable(struct_table_main_robustness_AVE3) %>%
  body_add_par("") %>%
  body_add_flextable(latent_variables_con_trust_testing4) %>%
  body_add_par("") %>%
  body_add_flextable(struct_table_main_robustness_AVE4) %>%   
  body_add_par("") %>%
  body_add_flextable(latent_variables_con_trust_testing5) %>%
  body_add_par("") %>%
  body_add_flextable(struct_table_main_robustness_AVE5)  

print(doc_main_model_spec, target = "article submission/tables/tables3_appendix_c.docx")
