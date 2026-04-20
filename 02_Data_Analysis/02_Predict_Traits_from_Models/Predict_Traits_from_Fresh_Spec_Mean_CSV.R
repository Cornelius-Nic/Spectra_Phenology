################################################################################
#                             Script README!
# Prediction of functional traits from 4 model coefficients developed for 
# FRESH SPECTRA by Nichodemus and Meireles (....) and Kothari et al. (2023). New Phytologist!
# Files saved in CSV
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

# Average the coefs for each trait
trait_coefs_mu = lapply(trait_coefs, colMeans)

# Combine plsr coefs
coefs = sapply(trait_coefs_mu, function(x){
  x[[1]]
  x
}, simplify=TRUE)

####################
# Predict traits
####################
non_spec = 1:14
s = as.matrix(cbind("intercept" = 1, fresh_spec_traits[ , -c(non_spec)] ))
t = s %*% coefs
all_season_traits = cbind(fresh_spec_traits[non_spec[1:8]], t)

################################################################################
# Export trait data
################################################################################
write_csv(all_season_traits, "Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_all_season.csv")


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

## Average the coefs for each trait
trait_coefs_mu = lapply(trait_coefs, colMeans)


# Combine plsr coefs
coefs = sapply(trait_coefs_mu, function(x){
  x[[1]]
  x
}, simplify=TRUE)


####################
# Predict traits
####################
not_predictor = 2:14
not_spec      = 1:14
s = as.matrix(cbind("intercept" = 1, fresh_spec_traits[ , -c(not_predictor)] ))
t = s %*% coefs
week_covar_traits = data.frame(fresh_spec_traits[ , not_spec[1:8]], t)
#week_covar_traits = data.frame(fresh_spec_traits[ , not_spec], t)
  

################################################################################
# Export trait data
################################################################################
write_csv(week_covar_traits, "Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_week_as_covariate.csv")


################################################################################
##  Peak season model 
################################################################################

plsr_coef_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_LMA.csv",
                    "EWT" = "Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_EWT.csv",
                    "C"   = "Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_C.csv",
                    "N"   = "Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_N.csv")

trait_coefs = lapply(plsr_coef_paths, read_csv)

################################################################################
# Process data
################################################################################

## Average the coefs for each trait
trait_coefs_mu = lapply(trait_coefs, colMeans)

# Combine plsr coefs
coefs = sapply(trait_coefs_mu, function(x){
  x[[1]]
  x
}, simplify=TRUE)


####################
# Predict traits
####################
non_spec = 1:14
s = as.matrix(cbind("intercept" = 1, fresh_spec_traits[ , -c(non_spec)] ))
t = s %*% coefs
peak_traits = cbind(fresh_spec_traits[non_spec[1:8]], t)

################################################################################
# Export trait data
################################################################################
write_csv(peak_traits, "Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_peak_season.csv")


################################################################################
# Kothari et al. (2023) model - New Phytologist!
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

# Average the coefs for each trait
trait_coefs_mu = lapply(trait_coefs, colMeans)

# Combine plsr coefs
coefs = sapply(trait_coefs_mu, function(x){
  x[[1]]
  x
}, simplify=TRUE)

######################
# Predict traits
######################
non_spec = 1:14
s = as.matrix(cbind("intercept" = 1, fresh_spec_traits[ , -c(non_spec)] ))
t = s %*% coefs
kothari_traits = cbind(fresh_spec_traits[non_spec[1:8]], t)


###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
###                         IMPORTANT NOTE!!!
###
### EWT in Kothari et a. (2023) was calculated in mm.
### We scale to g/cm2 (divide by 10) for uniformity.
###
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###

kothari_traits$EWT = kothari_traits$EWT / 10

################################################################################
# Export trait data
################################################################################
write_csv(kothari_traits, "Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_Kothari_2023.csv")
