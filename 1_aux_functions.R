## Auxiliary functions for SEM output:

var_levels <- list(
  age = c('refused','18-29','30-39','40-49','50-59','60-74','75+'),
  urban_type_chr = c("DK/Refused","Rural","Small Town","Suburban","City"),
  political_views_type = c("Very liberal","Somewhat liberal","Moderate","DK/Refused","Somewhat conservative","Very conservative"),
  income_before_tax_type = c('DK/Refused','<$30k','$30k-$60k','$60k-$75k','$75k-$100k','>$100k'),
  drink_home_type = c('DK/NA','Other','bottled water','filtered tap','unfiltered tap'),
  water_source_type = c("Local water system", "Private well", "DK/Refused")
)

create_block_table <- function(varname, data, var_levels) {
  desired_levels <- var_levels[[varname]]         # may be NULL if not provided
  
  tbl <- data %>%
    # if factor -> preserve labels, then convert to character (so pivot uses labels)
    mutate(
      tmp_var = as.character(.data[[varname]]),
      tmp_var = ifelse(is.na(tmp_var), "No response", tmp_var)
    ) %>%
    group_by(year, tmp_var) %>%
    tally() %>%
    group_by(year) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    select(-n) %>%
    pivot_wider(
      names_from = tmp_var,
      values_from = pct,
      values_fill = 0
    ) %>%
    ungroup() %>%
    mutate(Variable = varname) %>%
    select(Variable, year, everything())
  
  # Re-order category columns if desired_levels provided
  if (!is.null(desired_levels)) {
    # intersection keeps only levels present in this table, in the desired order
    present_levels <- intersect(desired_levels, colnames(tbl))
    # prepare final column order: Variable, year, desired levels present, then any other columns
    other_cols <- setdiff(colnames(tbl), c("Variable", "year", present_levels))
    final_order <- c("Variable", "year", present_levels, other_cols)
    # select in that order (use tidyselect via all_of)
    tbl <- tbl %>% select(all_of(final_order))
  }
  
  return(tbl)
}

make_struct_list <- function(fits, model, ordered_vars) {
  # fits <- fits_main 
  # nm <- "safety"
  # model <- model_con_trust_strict
  # ordered_vars
  lapply(
    names(fits),
    function(nm) {
      message("Running model for: ", nm)
      
      fit_tmp <- sem(
        model,
        data = fits[[nm]],
        ordered = ordered_vars,
        estimator = "WLSMV"
      )
      
      print(inspect(fit_tmp, "cor.lv"))
      
      latent_scores <- lavPredict(fit_tmp)
      df <- as.data.frame(latent_scores)[, c("trust", "satisfaction", "service_quality")]  
      
      struct_clean <- parameterEstimates(fit_tmp, standardized = TRUE) %>%
        filter(op == "~") %>% 
        transmute(
          Path = paste0(lhs, " ~ ", rhs),
          cell = paste0(
            round(std.all, 3),
            case_when(
              pvalue <= 0.01 ~ "***",
              pvalue <= 0.05 ~ "**",
              pvalue <= 0.10 ~ "*",
              pvalue <= 0.15 ~ ".",
              TRUE ~ ""
            ),
            " (", round(se, 3), ")"
          )
        ) %>% 
        mutate(
          Path = str_replace(Path, "trust", "Trust"),
          Path = str_replace(Path, "wtp_value_strict3", "WTP (strict)"),
          Path = str_replace(Path, "wtp_value_relaxed3", "WTP (relaxed)"),
          Path = str_replace(Path, "wtp_value_strict", "WTP (strict)"),
          Path = str_replace(Path, "wtp_value_relaxed", "WTP (relaxed)"),
          Path = str_replace(Path, "satisfaction", "Satisfaction"),
          Path = str_replace(Path, "wtp_affordability", "WTP to prevent shutoffs & nonpayment"),
          Path = str_replace(Path, "wtp_dw_safety", "WTP to make drinking water safer and healthier"),
          Path = str_replace(Path, "wtp_olig", "WTP to make drinking water taste & smell better"),
          Path = str_replace(Path, "income_before_tax_type", "Income (before tax)"),
          Path = str_replace(Path, "urban_type", "Urbanicity"),
          Path = str_replace(Path, "service_quality", "Service quality"),
          Path = str_replace(Path, "political_views_type", "Political views"),
          Path = str_replace(Path, "y2022", "Year: 2022"),
          Path = str_replace(Path, "y2023", "Year: 2023")
        ) %>% 
        separate(Path, into = c("lhs", "rhs"), sep = "~") %>%
        mutate(
          lhs = trimws(lhs),
          rhs = trimws(rhs)
        )
      
      colnames(struct_clean)[3] <- nm
      
      # ---- Compute VIFs for structural predictors ----
      vif_clean <- lav_vif(fit_tmp)$vif_table %>%
        transmute(
          Path = paste0(outcome, " ~ ", predictor),
          cell = round(vif, 3)
        ) %>%
        mutate(
          Path = str_replace(Path, "trust", "Trust"),
          Path = str_replace(Path, "wtp_value_strict3", "WTP (strict)"),
          Path = str_replace(Path, "wtp_value_relaxed3", "WTP (relaxed)"),
          Path = str_replace(Path, "wtp_value_strict", "WTP (strict)"),
          Path = str_replace(Path, "wtp_value_relaxed", "WTP (relaxed)"),
          Path = str_replace(Path, "satisfaction", "Satisfaction"),
          Path = str_replace(Path, "wtp_affordability", "WTP to prevent shutoffs & nonpayment"),
          Path = str_replace(Path, "wtp_dw_safety", "WTP to make drinking water safer and healthier"),
          Path = str_replace(Path, "wtp_olig", "WTP to make drinking water taste & smell better"),
          Path = str_replace(Path, "income_before_tax_type", "Income (before tax)"),
          Path = str_replace(Path, "urban_type", "Urbanicity"),
          Path = str_replace(Path, "service_quality", "Service quality"),
          Path = str_replace(Path, "political_views_type", "Political views"),
          Path = str_replace(Path, "y2022", "Year: 2022"),
          Path = str_replace(Path, "y2023", "Year: 2023")
        ) %>%
        separate(Path, into = c("lhs", "rhs"), sep = "~") %>%
        mutate(
          lhs = trimws(lhs),
          rhs = trimws(rhs)
        ) %>%
        as.data.frame()
      
      colnames(vif_clean)[3] <- nm
      
      # ---- Extract essential fit stats ----
      fit_stats <- fitMeasures(fit_tmp, c("npar", "nobs", "chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))
      fit_row <- tibble::tibble(
        lhs = rep(NA, 6),
        rhs = c("N", "χ²",
                "p", "CFI", "RMSEA", "SRMR"),
        !!nm := c(
          paste0(lavInspect(fit_tmp, "nobs")),
          paste0(round(fit_stats["chisq"], 2), " df=", fit_stats["df"]),
          paste0(signif(fit_stats["pvalue"], 2)),
          paste0(round(fit_stats["cfi"], 3)),
          paste0(round(fit_stats["rmsea"], 3)),
          paste0(round(fit_stats["srmr"], 3))
        )
        
        
      )
      
      list(
        table = struct_clean,
        footer = fit_row,
        vif = vif_clean
      )
    }
  )
}


make_struct_table <- function(struct_list){
  
  # struct_list <- struct_list_main1strict
  struct_tables <- lapply(struct_list, `[[`, "table")
  
  struct_table <- Reduce(function(x, y) full_join(x, y, by = c("lhs","rhs")), struct_tables) %>% 
    group_by(lhs) %>%
    do({
      header <- tibble(
        lhs = unique(.$lhs),
        rhs = NA_character_,
        safety = NA_character_,
        taste = NA_character_,
        poll = NA_character_,
        afford = NA_character_
      )
      bind_rows(header, .)
    }) %>%
    ungroup() %>% 
    mutate(rhs = ifelse(is.na(rhs), lhs, rhs)) %>% 
    dplyr::select(-lhs) %>% 
    rename(`Drinking Water Safety` = safety,
           `Drinking water Taste & Smell` = taste,
           `Infrastructure and Water Quality` = poll,
           `Prevent Shutoffs` = afford
    )
  
  # Append footers
  struct_footers <- lapply(struct_list, `[[`, "footer")
  footer_block <- reduce(
    struct_footers,
    ~ full_join(.x, .y, by = c("lhs","rhs"))
  ) %>% 
    rename(`Drinking Water Safety` = safety,
           `Drinking water Taste & Smell` = taste,
           `Infrastructure and Water Quality` = poll,
           `Prevent Shutoffs` = afford
    ) %>% 
    dplyr::select(-lhs)
  
  struct_table_footer <- bind_rows(struct_table, footer_block)
  
  return(struct_table_footer)
}

make_vif_table <- function(struct_list){
  
  # struct_list <- struct_list_main1strict
  vif_tables <- lapply(struct_list, `[[`, "vif")
  
  vif_table <- Reduce(function(x, y) full_join(x, y, by = c("lhs","rhs")), vif_tables) %>% 
    group_by(lhs) %>%
    do({
      header <- tibble(
        lhs = unique(.$lhs),
        rhs = NA_character_,
        safety = NA_real_,
        taste = NA_real_,
        poll = NA_real_,
        afford = NA_real_
      )
      bind_rows(header, .)
    }) %>%
    ungroup() %>% 
    mutate(rhs = ifelse(is.na(rhs), lhs, rhs)) %>% 
    dplyr::select(-lhs) %>% 
    rename(`Drinking Water Safety` = safety,
           `Drinking water Taste & Smell` = taste,
           `Infrastructure and Water Quality` = poll,
           `Prevent Shutoffs` = afford
    )
  
  return(vif_table)
}

make_struct_table_region <- function(struct_list){
  
  # struct_list <- struct_list_main1strict
  struct_tables <- lapply(struct_list, `[[`, "table")
  
  struct_table <- Reduce(function(x, y) full_join(x, y, by = c("lhs","rhs")), struct_tables) %>% 
    group_by(lhs) %>%
    do({
      header <- tibble(
        lhs = unique(.$lhs),
        rhs = NA_character_,
        northeast = NA_character_,
        midwest = NA_character_,
        south = NA_character_,
        west = NA_character_
      )
      bind_rows(header, .)
    }) %>%
    ungroup() %>% 
    mutate(rhs = ifelse(is.na(rhs), lhs, rhs)) %>% 
    dplyr::select(-lhs) %>% 
    rename(`Northeast` = northeast,
           `Midwest` = midwest,
           `South` = south,
           `West` = west
    )
  
  # Append footers
  struct_footers <- lapply(struct_list, `[[`, "footer")
  footer_block <- reduce(
    struct_footers,
    ~ full_join(.x, .y, by = c("lhs","rhs"))
  ) %>% 
    rename(`Northeast` = northeast,
           `Midwest` = midwest,
           `South` = south,
           `West` = west
    ) %>% 
    dplyr::select(-lhs)
  
  struct_table_footer <- bind_rows(struct_table, footer_block)
  
  return(struct_table_footer)
}

get_latent_loadings <- function(fits, model, ordered_vars = NULL) {
  lapply(
    names(fits),
    function(nm) {
      message("Running model for: ", nm)
      
      fit_latent <- sem(
        model,
        data = fits[[nm]],      
        ordered = ordered_vars,
        estimator = "WLSMV"
      )
      
      pe <- parameterEstimates(fit_latent, standardized = TRUE)
      
      latent_loadings <- pe %>%
        dplyr::filter(op == "=~") %>%
        dplyr::select(lhs, rhs, est, se, pvalue, std.all) %>%
        dplyr::arrange(lhs, rhs)
      
      formatted <- latent_loadings %>%
        group_by(lhs) %>%
        group_modify(~{
          subheader <- tibble(
            lhs = unique(.x$lhs),
            rhs = NA_character_,
            est = NA_real_,
            se = NA_real_,
            pvalue = NA_real_,
            std.all = NA_real_
          )
          bind_rows(subheader, .x)
        }) %>%
        ungroup() %>% 
        mutate(rhs = ifelse(is.na(rhs), lhs, rhs)) %>% 
        dplyr::select(rhs, std.all) %>% 
        mutate(std.all = as.character(round(std.all,2))) %>% 
        rename(!!nm := std.all)   
      
      
      list_to_df <- function(latent_list) {
        df <- do.call(rbind, latent_list) %>%
          as.data.frame() %>%
          tibble::rownames_to_column("latent_variable")
        
        return(df)
      }
      # Extract standardized solution
      std_sol <- inspect(fit_latent, what = "std")
      
      # Get loadings (lambda) and error variances (theta)
      lambda <- std_sol$lambda
      theta  <- std_sol$theta
      
      # Function to compute CR and AVE
      compute_CR_AVE <- function(lambda, theta) {
        CR  <- (sum(lambda))^2 / ((sum(lambda))^2 + sum(theta))
        AVE <- sum(lambda^2) / (sum(lambda^2) + sum(theta))
        return(c(CR = CR, AVE = AVE))
      }
      
      
      
      # Apply to each latent variable
      out <- lapply(1:ncol(lambda), function(j) {
        inds <- which(lambda[, j] != 0)
        compute_CR_AVE(lambda[inds, j], diag(theta)[inds])
      })
      names(out) <- colnames(lambda)
      
      df_latent <- list_to_df(out) %>% 
        filter(!str_detect(latent_variable,"wtp")) %>% 
        rename(rhs = latent_variable) %>% 
        mutate(CR = round(CR, 2),
               AVE = round(AVE, 2),
               latent_var := paste0("CR=", CR, " AVE=", AVE)) %>% 
        dplyr::select(rhs, latent_var)
      
      
      
      latent_table_formatted <- formatted %>% 
        left_join(df_latent) %>% 
        mutate(!!nm := coalesce(!!sym(nm), latent_var)) %>%       
        select(-latent_var) %>% 
        mutate(rhs = case_when(
          rhs == "satisfaction" ~ "Satisfaction",
          rhs == "trust" ~ "Trust",
          rhs == "service_quality" ~ "Service quality",
          rhs == "water_infras_quality_loc" ~ "Rating of local water infrastructure quality",
          rhs == "water_svc_satisf" ~ "Water service satisfaction",
          rhs == "water_svc_afford" ~ "Water service affordability",
          rhs == "water_svc_qual_taste" ~ "Taste & odor",
          rhs == "water_svc_qual_dirty" ~ "Dirty or cloudy Water",
          rhs == "water_svc_qual_swg_ovfl" ~ "Sewer overflow",
          rhs == "water_svc_qual_main_break" ~ "Main breaks",
          rhs == "water_svc_qual_low_press" ~ "Low pressure",
          rhs == "drink_water_quality_loc" ~ "Concern with local drinking water quality",
          rhs == "trust_pipes_safety" ~ "Pipe safety",
          rhs == "trust_water_safety" ~ "Water safety",
          TRUE ~ rhs  # keep everything else unchanged
        ))
      
      return(latent_table_formatted)
    }
  )
}

