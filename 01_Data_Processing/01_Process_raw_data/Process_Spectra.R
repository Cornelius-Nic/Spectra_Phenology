################################################################################
# 1. Packages
################################################################################

library("spectrolab")
library("stringr")

source("R/Utils/Utils.R")

################################################################################
# 2. Import data
################################################################################

spec_base_path_fresh  = "Data/Raw/Spectral Data/Fresh_leaf"
spec_base_path_dry    = "Data/Raw/Spectral Data/Dry_Leaf_BlackAndPaperBackground_Aug24/"

spec_dirs_fresh       = dir(spec_base_path_fresh, include.dirs = TRUE, full.names = TRUE)
spec_dirs_dry       = dir(spec_base_path_dry, include.dirs = TRUE, full.names = TRUE)

spec_raw_fresh        = lapply(spec_dirs_fresh, read_spectra)
names(spec_raw_fresh) = basename(spec_dirs_fresh)

spec_raw_dry        = lapply(spec_dirs_dry, read_spectra)
names(spec_raw_dry) = basename(spec_dirs_dry)


################################################################################
# 3. Process data - Fresh
################################################################################

####################
# 3.1 Fix names
####################

spec_fresh = lapply(spec_raw_fresh, function(x){
  y = x
  names(y) = str_extract(names(y), "[^_]+")
  y
})

####################
# Trim
####################

spec_fresh = lapply(spec_fresh, function(x){
  lambda = 400:2400
  fwhm   = spectrolab::make_fwhm(x, lambda)
  resample(spec = x, new_bands = lambda, fwhm = fwhm)
})

####################
# Each branch has 5 spectra
# Average them by branch
####################

spec_mean_fresh = lapply(spec_fresh, function(x){
  aggregate(x, by = names(x), mean)
})


####################
# Export to data.frame
####################

spec_df_fresh = lapply(spec_mean_fresh, function(x){
  y = as.data.frame(x)
  names(y)[1] = "Branch_ID"
  y
})


####################
# Add individual_ID and Species data
####################

spec_df_fresh = lapply(spec_df_fresh, function(x){
  add_species(add_individual_id(x = x, branch_id_col = "Branch_ID"))
})

########################################
# Export spectral data as tibble
########################################
saveRDS(object = spec_df_fresh, file = "Data/Processed/spectra_fresh.RDS")


########################################################################
# Script to process dry spec data collected after 1 year
# Aim: Match sensor overlap and split black and paper background 
########################################################################

####################
# Fix names
####################

# Extract spec data with black background
spec_black = lapply(spec_raw_dry, function(x){
  y = x[!grepl("_P_", names(x))]
  names(y) = str_extract(names(y), "^[^_]+")
  y
})

# Extract spec data with paper background
spec_paper = lapply(spec_raw_dry, function(x){
  y = x[grepl("_P_", names(x))]
  names(y) = str_extract(names(y), "[^_]+_P")
  y
})

####################
# Match_sensor_overlap
####################

spec_match_black = lapply(spec_black, function(x){
  match_sensors(x, c(990, 1900))
})

spec_match_paper = lapply(spec_paper, function(x){
  match_sensors(x, c(990, 1900))
})

####################
# Trim
####################

spec_trim_black = lapply(spec_match_black, function(x){
  lambda = 400:2400
  fwhm   = spectrolab::make_fwhm(x, lambda)
  resample(spec = x, new_bands = lambda, fwhm = fwhm)
})

spec_trim_paper = lapply(spec_match_paper, function(x){
  lambda = 400:2400
  fwhm   = spectrolab::make_fwhm(x, lambda)
  resample(spec = x, new_bands = lambda, fwhm = fwhm)
})

####################
# Each branch has 5 spectra
# Average them by branch
####################

spec_mean_black = lapply(spec_trim_black, function(x){
  aggregate(x, by = names(x), mean)
})

spec_mean_paper = lapply(spec_trim_paper, function(x){
  aggregate(x, by = names(x), mean)
})

########################################
# Export to data.frame
########################################

# Spectra data with black background
spec_black_df = lapply(spec_mean_black, function(x){
  y = as.data.frame(x)
  names(y)[1] = "Branch_ID"
  y
})

# Spectra data with paper background
spec_paper_df = lapply(spec_mean_paper, function(x){
  y = as.data.frame(x)
  names(y)[1] = "Branch_ID"
  y
})

####################
# Add individual_ID and Species data
####################

# Spectra data with black background
spec_black_df = lapply(spec_black_df, function(x){
  add_species(add_individual_id(x = x, branch_id_col = "Branch_ID"))
})

# Spectra data with paper background
spec_paper_df = lapply(spec_paper_df, function(x){
  add_species(add_individual_id(x = x, branch_id_col = "Branch_ID"))
})

################################################################################
# Export spectral data as tibble
################################################################################

saveRDS(object = spec_black_df, file = "Data/Processed/spectra_dry_black.RDS")
saveRDS(object = spec_paper_df, file = "Data/Processed/spectra_dry_paper.RDS")
