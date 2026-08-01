#===============================================================================
#Construction Expectation Confirmation Theory -- STRICT
#Appendix A
#===============================================================================

model_exp_conf_strict <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~ trust_water_safety + trust_pipes_safety + drink_water_quality_loc
  satisfaction =~ water_svc_satisf + water_svc_afford  + water_infras_quality_loc
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #or does satisfaction build towards trust? i.e., repeated positive experiences (high satisfaction) can build trust over time.
  satisfaction ~ a*service_quality
  trust ~ b*satisfaction
  wtp_value_strict ~ c1*trust + c2*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023

  
  ###########################
  # Indirect effects
  ###########################
  # ind_sat_trust_wtp := b * c1              # satisfaction -> trust -> WTP
  # ind_service_sat_wtp := a * c2            # service_quality -> satisfaction -> WTP
  # total_sat_wtp := c2 + (b*c1)           # total effect of trust on WTP
  # total_service_wtp := c3 + (a*c2) + (a*b*c1)  # total effect of service quality including all indirect paths
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


#main specification is the sample in 2022 & 2023 for which we know were not on wells
fits_main <- list(
  safety  = vow_surveys %>% filter(wtp == "drinking water safety"),
  taste   = vow_surveys %>% filter(wtp == "drinking water taste and smell"),
  poll    = vow_surveys %>% filter(wtp == "infrasture/pollution"),
  afford  = vow_surveys %>% filter(wtp == "help ensure water affordability for all")
)


latent_loadings_exp_conf <- get_latent_loadings(fits_main, model_exp_conf_strict, ordered_vars)

latent_variables_exp_conf <-  reduce(
  latent_loadings_exp_conf,
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
  set_caption("Appendix Table A1. Latent Variable Construction Expectation Confirmation Theory")


struct_list_alt2strict <- make_struct_list(
  fits = fits_main,
  model = model_exp_conf_strict,
  ordered_vars = ordered_vars
)

struct_table_alt2strict_main <- make_struct_table(struct_list_alt2strict) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Table A2. Results for Expectation Confirmation Theory using Strict Measure of WTP")


# --- export to Word ---
doc_alt_model_spec <- read_docx() %>%
 body_add_flextable(latent_variables_exp_conf) %>%
 body_add_par("") %>%
 body_add_flextable(struct_table_alt2strict_main)


print(doc_alt_model_spec, target = "article submission/tables/tables2_appendix_a.docx")

rm(latent_loadings_exp_conf,latent_variables_exp_conf,struct_list_alt2strict,struct_table_alt2strict_main,
   model_exp_conf_strict, doc_alt_model_spec)
