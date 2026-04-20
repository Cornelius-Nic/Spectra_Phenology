################################################################################
#                             Script README!
# Prediction of functional traits from 4 model coefficients developed for
# FRESH SPECTRA by Nichodemus and Meireles (....) and Kothari et al. (2023) New Phytologist!
# Files saved in RDS
################################################################################
# Packages
################################################################################
library("readr")

# Import data
fresh_spec_traits = readRDS("Data/Processed/fresh_spec_traits.RDS")

################################################################################
# Import data
# 01_Model - All season
################################################################################

plsr_coef_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_all_season/coef_boot_LMA.csv",
                    "EWT" = "Data/Processed/Coefficients_and_Results/FM_all_season/coef_boot_EWT.csv",
                    "C"   = "Data/Processed/Coefficients_and_Results/FM_all_season/coef_boot_C.csv",
                    "N"   = "Data/Processed/Coefficients_and_Results/FM_all_season/coef_boot_N.csv")

trait_coefs = lapply(plsr_coef_paths, read_csv)

################################################################################
# Predict traits
################################################################################
# Remove non-spectral columns
non_spec = 1:14
meta_df  = fresh_spec_traits[, non_spec[1:8], drop = FALSE]
# Define predictors
s = as.matrix(cbind("Intercept" = 1, fresh_spec_traits[ , -c(non_spec)] ))

# Predict traits
all_season_preds = lapply(trait_coefs, function(x, y = s, z = meta_df) {
  coef_mat = t(as.matrix(x))
  preds = y %*% coef_mat
  cbind(z, preds)
})

# Prediction Summary
all_season_summary = lapply(names(all_season_preds), function(trait){
  mat = all_season_preds[[trait]][, -(1:ncol(meta_df)), drop = FALSE]
  
  Mean = apply(mat, 1, mean, na.rm = T)
  SD   = apply(mat, 1, sd, na.rm = T)
  CI_low = apply(mat, 1, function(x) quantile(x, 0.025, na.rm = T))
  CI_upp = apply(mat, 1, function(x) quantile(x, 0.975, na.rm = T))
  
  data.frame(
    SampleID = seq_len(nrow(mat)),
    meta_df,
    Trait     = trait,
    Pred_mean = Mean,
    Pred_SD   = SD,
    CI_0.025  = CI_low,
    CI_0.975  = CI_upp
  )
})

names(all_season_summary) = names(all_season_preds)

################################################################################
# Export trait data
################################################################################
saveRDS(all_season_preds, "Data/Processed/Trait_Predictions/1000_trait_predictions_fresh_spectra_all_season.rds")
saveRDS(all_season_summary, "Data/Processed/Trait_Predictions/trait_prediction_summary_fresh_spectra_all_season.rds")


################################################################################
# 02_Model - Week as covariate
################################################################################

plsr_coef_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/coef_boot_LMA.csv",
                    "EWT" = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/coef_boot_EWT.csv",
                    "C"   = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/coef_boot_C.csv",
                    "N"   = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/coef_boot_N.csv")

trait_coefs = lapply(plsr_coef_paths, read_csv)

################################################################################
# Process data
################################################################################
# Remove non-spectral columns
non_spec = 1:14
not_predictor = 2:14
meta_df  = fresh_spec_traits[, non_spec[1:8], drop = FALSE]
# Define predictors
s = as.matrix(cbind("Intercept" = 1, fresh_spec_traits[ , -c(not_predictor)] ))

# Predict traits
week_covar_preds = lapply(trait_coefs, function(x, y = s, z = meta_df) {
  coef_mat = t(as.matrix(x))
  preds = y %*% coef_mat
  cbind(z, preds)
})

# Prediction Summary
week_covar_summary = lapply(names(week_covar_preds), function(trait){
  mat = week_covar_preds[[trait]][, -(1:ncol(meta_df)), drop = FALSE]
  
  Mean = apply(mat, 1, mean, na.rm = T)
  SD   = apply(mat, 1, sd, na.rm = T)
  CI_low = apply(mat, 1, function(x) quantile(x, 0.025, na.rm = T))
  CI_upp = apply(mat, 1, function(x) quantile(x, 0.975, na.rm = T))
  
  data.frame(
    SampleID = seq_len(nrow(mat)),
    meta_df,
    Trait     = trait,
    Pred_mean = Mean,
    Pred_SD   = SD,
    CI_0.025  = CI_low,
    CI_0.975  = CI_upp
  )
})

names(week_covar_summary) = names(week_covar_preds)

################################################################################
# Export trait data
################################################################################
saveRDS(week_covar_preds, "Data/Processed/Trait_Predictions/1000_trait_predictions_fresh_spectra_week_as_covariate.rds")
saveRDS(week_covar_summary, "Data/Processed/Trait_Predictions/trait_prediction_summary_fresh_spectra_week_as_covariate.rds")


################################################################################
##  03_Model - Peak season 
################################################################################

plsr_coef_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_LMA.csv",
                    "EWT" = "Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_EWT.csv",
                    "C"   = "Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_C.csv",
                    "N"   = "Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_N.csv")

trait_coefs = lapply(plsr_coef_paths, read_csv)

################################################################################
# Process data
################################################################################
# Remove non-spectral columns
non_spec = 1:14
meta_df  = fresh_spec_traits[, non_spec[1:8], drop = FALSE]
# Define predictors
s = as.matrix(cbind("Intercept" = 1, fresh_spec_traits[ , -c(non_spec)] ))

# Predict traits
peak_season_preds = lapply(trait_coefs, function(x, y = s, z = meta_df) {
  coef_mat = t(as.matrix(x))
  preds = y %*% coef_mat
  cbind(z, preds)
})

# Prediction Summary
peak_season_summary = lapply(names(peak_season_preds), function(trait){
  mat = peak_season_preds[[trait]][, -(1:ncol(meta_df)), drop = FALSE]
  
  Mean = apply(mat, 1, mean, na.rm = T)
  SD   = apply(mat, 1, sd, na.rm = T)
  CI_low = apply(mat, 1, function(x) quantile(x, 0.025, na.rm = T))
  CI_upp = apply(mat, 1, function(x) quantile(x, 0.975, na.rm = T))
  
  data.frame(
    SampleID = seq_len(nrow(mat)),
    meta_df,
    Trait     = trait,
    Pred_mean = Mean,
    Pred_SD   = SD,
    CI_0.025  = CI_low,
    CI_0.975  = CI_upp
  )
})

names(peak_season_summary) = names(peak_season_preds)

################################################################################
# Export trait data
################################################################################
saveRDS(peak_season_preds, "Data/Processed/Trait_Predictions/1000_trait_predictions_fresh_spectra_peak_season.rds")
saveRDS(peak_season_summary, "Data/Processed/Trait_Predictions/trait_prediction_summary_fresh_spectra_peak_season.rds")


################################################################################
# 04_Model - Kothari et al. (2023) - New Phytologist!
################################################################################

plsr_coef_paths = c("C"      = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/C.csv",
                    "N"      = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/N.csv",
                    "EWT"    = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/EWTCorrectedModels/EWTFresh_ref.csv",
                    "LMA"    = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/LMA.csv",
                    "Lignin" = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/lign.csv",
                    "LDMC"   = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/LDMC.csv",
                    "ChlA"   = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/chlA.csv",
                    "ChlB"   = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/chlB.csv",
                    "Carot"  = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/car.csv",
                    "Cell"   = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/cell.csv",
                    "Hemic"  = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/hemi.csv",
                    "Sol"    = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/sol.csv",
                    "K"      = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/K.csv",
                    "Mn"     = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/Mn.csv",
                    "P"      = "Data/PLSR_Models/Kothari_etal2023_NewPhytologist/dataverse_files/RefModels/P.csv")

trait_coefs = lapply(plsr_coef_paths, read_csv)

################################################################################
# Process data
################################################################################
non_spec = 1:14
meta_df  = fresh_spec_traits[, non_spec[1:8], drop = FALSE]
# Define predictors
s = as.matrix(cbind("Intercept" = 1, fresh_spec_traits[ , -c(non_spec)] ))

# Predict traits
kothari_preds = lapply(trait_coefs, function(x, y = s, z = meta_df) {
  coef_mat = t(as.matrix(x))
  preds = y %*% coef_mat
  cbind(z, preds)
})

# Prediction Summary
kothari_summary = lapply(names(kothari_preds), function(trait){
  mat = kothari_preds[[trait]][, -(1:ncol(meta_df)), drop = FALSE]
  
  Mean = apply(mat, 1, mean, na.rm = T)
  SD   = apply(mat, 1, sd, na.rm = T)
  CI_low = apply(mat, 1, function(x) quantile(x, 0.025, na.rm = T))
  CI_upp = apply(mat, 1, function(x) quantile(x, 0.975, na.rm = T))
  
  data.frame(
    SampleID = seq_len(nrow(mat)),
    meta_df,
    Trait     = trait,
    Pred_mean = Mean,
    Pred_SD   = SD,
    CI_0.025  = CI_low,
    CI_0.975  = CI_upp
  )
})

names(kothari_summary) = names(kothari_preds)


###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
###                         IMPORTANT NOTE!!!
###
### EWT in Kothari et a. (2023) was calculated in mm.
### We scale to g/cm2 (divide by 10) for uniformity.
###
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###

kothari_preds$EWT[, -(1:ncol(meta_df))] =
  kothari_preds$EWT[, -(1:ncol(meta_df))] / 10

kothari_summary$EWT = transform(
  kothari_summary$EWT,
  Pred_mean = Pred_mean / 10,
  Pred_SD   = Pred_SD   / 10,
  CI_0.025  = CI_0.025  / 10,
  CI_0.975  = CI_0.975  / 10
)

################################################################################
# Export trait data
################################################################################
saveRDS(kothari_preds, "Data/Processed/Trait_Predictions/100_trait_predictions_fresh_spectra_Kothari_2023.rds")
saveRDS(kothari_summary, "Data/Processed/Trait_Predictions/trait_prediction_summary_fresh_spectra_Kothari_2023.rds")
