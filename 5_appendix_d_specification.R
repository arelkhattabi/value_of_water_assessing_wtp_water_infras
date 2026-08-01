library(semTools)
library(lavaan)
library(lavinteract)

## HTMT table
measurement_model <- '
  trust =~ trust_water_safety + trust_pipes_safety + drink_water_quality_loc

  satisfaction =~ water_svc_satisf + water_svc_afford + water_infras_quality_loc

  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + 
                     water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + 
                     water_svc_qual_low_press
'

htmt_results_all <- lapply(
  names(fits_main),
  function(x) {
    semTools::htmt(
      measurement_model,
      data = fits_main[[x]]
    )
  }
)


names(htmt_results_all) <- names(fits_main)

htmt_results_all

## VIF table
model_con_trust_strict <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~  trust_water_safety + trust_pipes_safety + drink_water_quality_loc
  satisfaction =~ water_svc_satisf + water_svc_afford + water_infras_quality_loc 
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*trust + c2*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023
  #wtp_value_strict ~ c1*trust + c2*service_quality + y2022 + y2023 + income_before_tax_type +  urban_type + political_views_type


  ###########################
  # Indirect effects
  ###########################
  #ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  #ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  #ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  #total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  #total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  #total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
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

fits_main <- list(
  safety  = vow_surveys %>% filter(wtp == "drinking water safety"),
  taste   = vow_surveys %>% filter(wtp == "drinking water taste and smell"),
  poll    = vow_surveys %>% filter(wtp == "infrasture/pollution"),
  afford  = vow_surveys %>% filter(wtp == "help ensure water affordability for all")
)

struct_list_main1strict <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict,
  ordered_vars = ordered_vars
)

struct_table_main1strict_vif <- make_vif_table(struct_list_main1strict) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Table D2. Variance Inflation Factors")

## Table, WTP no satisfaction
model_con_trust_strict <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~  trust_water_safety + trust_pipes_safety + drink_water_quality_loc
  satisfaction =~ water_svc_satisf + water_svc_afford + water_infras_quality_loc 
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*trust + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023
  #wtp_value_strict ~ c1*trust + c2*service_quality + y2022 + y2023 + income_before_tax_type +  urban_type + political_views_type


  ###########################
  # Indirect effects
  ###########################
  #ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  #ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  #ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  #total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  #total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  #total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
'

struct_list_nosatis <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict,
  ordered_vars = ordered_vars
)

struct_table_nosatis <- make_struct_table(struct_list_nosatis) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Table D3. Main results, excluding Satisfaction from WTP function")

## Table, WTP no trust
model_con_trust_strict <- '
 ###########################
  # Measurement models
  ###########################  
  trust =~  trust_water_safety + trust_pipes_safety + drink_water_quality_loc
  satisfaction =~ water_svc_satisf + water_svc_afford + water_infras_quality_loc 
  service_quality =~ water_svc_qual_taste + water_svc_qual_dirty + water_svc_qual_swg_ovfl +
                     water_svc_qual_main_break + water_svc_qual_low_press

   
  ###########################
  # Structural model
  ###########################
  #does trust underpin satisfaction? Trust acts as a lens: a trusted provider is more likely to satisfy even when service quality fluctuates, i.e., people with high trust are more forgiving of minor service failures
  trust ~ a*service_quality
  satisfaction ~ b1*trust + b2*service_quality
  wtp_value_strict ~ c1*satisfaction + c3*service_quality + income_before_tax_type +  urban_type + political_views_type + y2022 + y2023
  #wtp_value_strict ~ c1*trust + c2*service_quality + y2022 + y2023 + income_before_tax_type +  urban_type + political_views_type


  ###########################
  # Indirect effects
  ###########################
  #ind_trust_sat_wtp := b1 * c2              # trust -> satisfaction -> WTP
  #ind_service_trust_wtp := a * c1           # service_quality -> trust -> WTP
  #ind_service_sat_wtp := b2 * c2            # service_quality -> satisfaction -> WTP
  #total_trust_wtp := c1 + (b1*c2)           # total effect of trust on WTP
  #total_service_wtp := c3 + (a*c1) + (b2*c2) + (a*b1*c2)  # total effect of service quality including all indirect paths
  #total_service_thru_satisfaction := (b2*c2) + (a*b1*c2)
'

struct_list_notrust <- make_struct_list(
  fits = fits_main,
  model = model_con_trust_strict,
  ordered_vars = ordered_vars
)

struct_table_notrust <- make_struct_table(struct_list_notrust) %>% 
  flextable() %>% 
  autofit() %>%
  width(width = 1.5) %>% 
  set_caption("Table D4. Main results, excluding Trust from WTP function")


# --- export to Word ---
doc_diagnostic_spec <- read_docx() %>%
  body_add_flextable(struct_table_main1strict_vif) %>%
  body_add_par("") %>%
  body_add_flextable(struct_table_nosatis) %>%
  body_add_par("") %>%
  body_add_flextable(struct_table_notrust)



print(doc_diagnostic_spec, target = "article submission/tables/tables1_diagnostic.docx")



