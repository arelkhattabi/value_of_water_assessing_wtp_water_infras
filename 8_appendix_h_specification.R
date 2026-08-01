
#-------------------------------------------------------------------------------
# APPENDIX G Heterogeneity by census region
#-------------------------------------------------------------------------------

model_con_trust_strict_time <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~ trust_water_safety  + trust_pipes_safety + drink_water_quality_loc
  satisfaction =~   water_svc_satisf + water_svc_afford + water_infras_quality_loc
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

ordered_vars <- c(
  "wtp_value_strict",
  "trust_water_safety", "drink_water_quality_loc", "trust_pipes_safety",
  "water_infras_quality_loc", "water_svc_satisf", "water_svc_qual_taste",
  "water_svc_qual_dirty",
  "water_svc_afford", "water_svc_qual_main_break",
  "water_svc_qual_swg_ovfl", "water_svc_qual_low_press",
  "income_before_tax_type", "urban_type", "political_views_type", "age"
)

for(wtp_q in c('drinking water safety',
               'drinking water taste and smell',
               'infrasture/pollution',
               'help ensure water affordability for all')){

  fits_region <- list(
    northeast  = vow_surveys %>% filter(region_broad == 'NE') %>% filter(wtp == wtp_q),
    midwest   = vow_surveys %>% filter(region_broad == 'MW') %>% filter(wtp == wtp_q),
    south    = vow_surveys %>% filter(region_broad == 'S') %>% filter(wtp == wtp_q),
    west  = vow_surveys %>% filter(region_broad == 'W') %>% filter(wtp == wtp_q)
  )

  struct_list_temp <- make_struct_list(
    fits = fits_region,
    model = model_con_trust_strict_time,
    ordered_vars = ordered_vars
  )
  
  temp <- make_struct_table_region(struct_list_temp) %>% 
    flextable() %>% 
    autofit() %>%
    width(width = 1.5) %>% 
    set_caption(paste("Question", wtp_q))
  
  assign(paste0("table_",wtp_q), temp)
}

 


# --- export to Word ---
#doc_main_model_spec <- read_docx() %>%
#  body_add_flextable(latent_variables_con_trust) %>% 
#  body_add_par("") %>%
#  body_add_flextable(struct_table_main1strict_main) %>% 
#  body_add_par("") %>%
#  body_add_flextable(latent_variables_con_trust2021) %>% 
#  body_add_par("") %>%
#  body_add_flextable(ft_struct_sensitivity1) %>% 
#  body_add_par("") %>%
#  body_add_flextable(latent_variables_con_trust_all_resp) %>% 
#  body_add_par("") %>%
#  body_add_flextable(ft_struct_sensitivity2)  %>% 
#  body_add_par("") %>%
#  body_add_flextable(struct_table_main1strict_main_time_region)


#print(doc_main_model_spec, target = paste0("/Users/",Sys.getenv("USER"),"/Library/CloudStorage/Box-Box/VoW Analysis/article submission/R script/SEM_main_model_results.docx"))

