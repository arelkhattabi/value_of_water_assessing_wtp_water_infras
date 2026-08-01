library(semTools)
library(lavaan)

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
