#===============================================================================
#Appendix A: Expectation-confirmation theory
#===============================================================================


model_con_trust_strict_no_satisfaction <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~ trust_water_safety + drink_water_quality_loc + trust_pipes_safety + water_infras_quality_loc
  satisfaction =~ water_svc_satisf + water_svc_afford  
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  #trust ~ a*service_quality
  #satisfaction ~ b1*trust + b2*service_quality
  satisfaction ~ b2*service_quality
  wtp_value_strict ~ c1*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type
  

  ###########################
  # Indirect effects
  ###########################
  # ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  # ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  # ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  # total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  # total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  # total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
'

ordered_vars <- c(
  "wtp_value_strict",
  "trust_water_safety", "drink_water_quality_loc", "trust_pipes_safety",
  "water_infras_quality_loc", "water_svc_satisf", "water_svc_qual_taste",
  "water_svc_qual_dirty", "trust_drink_water_home",
  "water_svc_afford", "water_svc_qual_main_break",
  "water_svc_qual_swg_ovfl", "water_svc_qual_low_press",
  "income_before_tax_type", "urban_type", "political_views_type", "age"
)

# MAIN ESTIMATES ARE BASED ON ENTIRE SAMPLE
fits_main <- list(
  safety  = vow_surveys %>% filter(wtp == "drinking water safety"),
  taste   = vow_surveys %>% filter(wtp == "drinking water taste and smell"),
  poll    = vow_surveys %>% filter(wtp == "infrasture/pollution"),
  afford  = vow_surveys %>% filter(wtp == "help ensure water affordability for all")
)

latent_loadings_con_trust_no_satis <- get_latent_loadings(fits_main, model_con_trust_strict_no_satisfaction, ordered_vars)


latent_variables_con_trust_no_satis <-  reduce(
  latent_loadings_con_trust_no_satis,
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
  set_caption("latent no satisfaction")



struct_list_main1strict_no_satis <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict_no_satisfaction,
  ordered_vars = ordered_vars
)

struct_table_main1strict_main_no_satis <- make_struct_table(struct_list_main1strict_no_satis) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Result Not Satisfied")


#rm(struct_table_main1strict_main_no_satis,struct_list_main1strict_no_satis,
   # latent_loadings_con_trust_no_satis,latent_variables_con_trust_no_satis,
   # model_con_trust_strict_no_satisfaction)
   # 

