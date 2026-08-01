#-------------------------------------------------------------------------------
#Results using only sample of households that self-report being served by a local water system
#Appendix F
#-------------------------------------------------------------------------------

model_con_trust_strict <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~ trust_water_safety   + trust_pipes_safety + drink_water_quality_loc 
  satisfaction =~ water_svc_satisf + water_svc_afford  + water_infras_quality_loc
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*trust + c2*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2023
  

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

fits_sensitivity2 <- list(
  safety  = vow_surveys %>% filter(water_source_type=='Local water system') %>% filter(wtp == "drinking water safety"),
  taste   = vow_surveys %>% filter(water_source_type=='Local water system') %>% filter(wtp == "drinking water taste and smell"),
  poll    = vow_surveys %>% filter(water_source_type=='Local water system') %>% filter(wtp == "infrasture/pollution"),
  afford  = vow_surveys %>% filter(water_source_type=='Local water system') %>% filter(wtp == "help ensure water affordability for all")
)


latent_loadings_con_trust_all_resp <- get_latent_loadings(fits_sensitivity2, model_con_trust_strict, ordered_vars)


latent_variables_con_trust_all_resp <-  reduce(
  latent_loadings_con_trust_all_resp,
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
  set_caption("Appendix Table F1. Latent Variable Construction Commitment Trust Theory--Non-Well Respondents")



struct_list_sensitivity2_strict <- make_struct_list(
  fits = fits_sensitivity2,
  model = model_con_trust_strict,
  ordered_vars = ordered_vars
)

ft_struct_sensitivity2 <- make_struct_table(struct_list_sensitivity2_strict) %>% 
  flextable() %>%  
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Appendix Table F2. Results for Commitment Trust Theory using Strict Measure of WTP--Non-Well Respondents")

# --- export to Word ---
doc_main_model_spec <- read_docx() %>%
 body_add_flextable(latent_variables_con_trust_all_resp) %>%
 body_add_par("") %>%
 body_add_flextable(ft_struct_sensitivity2)  

print(doc_main_model_spec, target = "article submission/tables/tables6_appendix_f.docx")



rm(fits_sensitivity2,ft_struct_sensitivity2,latent_loadings_con_trust_all_resp,latent_variables_con_trust_all_resp,
   struct_list_sensitivity2_strict,model_con_trust_strict, doc_main_model_spec)
