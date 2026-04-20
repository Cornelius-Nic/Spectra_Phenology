################################################################################
#                             Script READ ME!
#               Important functions used in this project
################################################################################

########################################################
# Evaluate Model Performance
# Obtained from White et al. (2025) with modifications 
########################################################
assess_prediction_performance = function(obs, pred) {
  
  # Remove NAs
  valid = complete.cases(obs, pred)
  obs   = obs[valid]
  pred  = pred[valid]
  
  # Fit linear model
  linear <- lm(obs ~ pred, na.action = na.exclude)
  linear_summary <- summary(linear)
  
  # Metrics
  R2 <- linear_summary$r.squared
  intercept <- linear_summary$coefficients[1,1]
  slope <- linear_summary$coefficients[2,1]
  BIAS <- mean(obs-pred, na.rm = TRUE)
  RMSE <- sqrt(sum(((obs-pred)^2))/length(obs))
  perRMSE <- (RMSE / (max(obs, na.rm =  TRUE) - min(obs, na.rm = TRUE))) * 100
  names(perRMSE) <- NULL
  
  # Metrics
  return(round(data.table::data.table(
    n = length(obs),
    R2 = R2,
    BIAS = BIAS,
    RMSE = RMSE,
    perRMSE = perRMSE,
    intercept = intercept,
    slope = slope,
    slope_ci_low = confint(linear)[2 , 1],
    slope_ci_hi = confint(linear)[2 , 2]
    ), 6))
  
}


#######################################################
# Add performance metrics to prediction dataframes
#######################################################
add_metrics_to_df = function(df, metrics) {
  df$R2 = metrics$R2
  df$RMSE = metrics$RMSE
  df$perRMSE = metrics$perRMSE
  df$BIAS = metrics$BIAS
  df$intercept = metrics$intercept
  df$slope = metrics$slope
  df
}


############################################################
# ANOVA + TukeyHSD for Trait ~ Week (within Species)
############################################################

get_dataset_sig_by_week = function(df_sub, trait, species) {
  
  # -------------------------------
  # MAP NUMERIC LEVELS → SEASONS
  # -------------------------------
  df_sub = df_sub %>%
    mutate(Week = recode(as.character(Week),
                         "1"  = "Early",
                         "11" = "Peak",
                         "23" = "Late")) %>%
    mutate(Week = factor(Week, levels = c("Early", "Peak", "Late")))
  
  out_list = list()
  
  # Loop through weeks inside this species × trait
  for (wk in levels(df_sub$Week)) {
    
    df_wk = df_sub %>% filter(Week == wk)
    
    # Must have multiple datasets
    if (length(unique(df_wk$Dataset)) < 2) next
    
    # -------------------------------
    # ANOVA: Value ~ Dataset
    # -------------------------------
    aov_model = aov(Value ~ Dataset, data = df_wk)
    aov_fit   = summary(aov_model)
    
    # Extract ANOVA table values
    aov_df = as.data.frame(aov_fit[[1]])
    aov_F  = aov_df$`F value`[1]
    aov_p  = aov_df$`Pr(>F)`[1]
    
    # -------------------------------
    # TukeyHSD
    # -------------------------------
    tukey_res = TukeyHSD(aov_model, which = "Dataset")$Dataset
    sig_df    = as.data.frame(tukey_res)
    
    sig_df$Comparison = rownames(sig_df)
    rownames(sig_df)  = NULL
    
    # -------------------------------
    # Build final output row
    # -------------------------------
    sig_df = sig_df %>%
      dplyr::select(Comparison, `p adj`) %>%
      mutate(
        Trait  = trait,
        Species = species,
        Week = wk,
        ANOVA_F = aov_F,
        ANOVA_p = aov_p,
        ANOVA_sig = case_when(
          ANOVA_p < 0.001 ~ "***",
          ANOVA_p < 0.01  ~ "**",
          ANOVA_p < 0.05  ~ "*",
          TRUE            ~ "ns"
        ),
        Significance = case_when(
          `p adj` < 0.001 ~ "***",
          `p adj` < 0.01  ~ "**",
          `p adj` < 0.05  ~ "*",
          TRUE            ~ "ns"
        )
      ) %>%
      dplyr::select(
        Trait, Species, Week,
        Comparison, `p adj`, Significance,
        ANOVA_F, ANOVA_p, ANOVA_sig
      )
    
    out_list[[wk]] = sig_df
  }
  
  return(do.call(rbind, out_list))
}
