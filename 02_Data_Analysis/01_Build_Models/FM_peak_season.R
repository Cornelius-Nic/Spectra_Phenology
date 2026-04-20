################################################################################
#                             Script README!
# Peak_season model is a subset of the whole datasets 
# We selected Weeks 7 (July 11, 2023) to 16 (September 12, 2023)
# Period covered: 10 Weeks 
################################################################################
# Packages
################################################################################
library("pls")
library("caret")
library("dplyr")
library("plsVarSel")

source("R/Utils/important_functions.R")

################################################################################
# Read data
################################################################################
fresh_spec_traits = readRDS("Data/Processed/fresh_spec_traits.RDS")

# Select peak season: Weeks 7 - 16
peak_season = fresh_spec_traits[fresh_spec_traits$Week %in% 7:16, ]

# Divide the data by Branch_ID
train_idx = createDataPartition(peak_season$Branch_ID, p = 0.75, list = FALSE)

peak_train = peak_season[train_idx, ]
peak_test  = peak_season[-train_idx, ]

# Variables for model fitting
predictor_columns = 15:ncol(fresh_spec_traits)

################################################################################
# Model tuning: to select the optimal number of components for bootstrapping.
################################################################################
############################
# Leaf Mass per Area = LMA
############################
LMA_valid = which(!is.na(peak_train$LMA))

LMA_model = plsr(peak_train$LMA[LMA_valid] ~ as.matrix(peak_train[,predictor_columns]), 
                 ncomp = 30,method = "oscorespls", validation = "CV", scale = F)

# Select optimal number of components
ncomp_LMA = selectNcomp(LMA_model, method = "onesigma", plot = T)

# Plot RMSEP to select optimal number of components
plot(RMSEP(LMA_model), legendpos = "topright")


####################################
# Equivalent Water Thickness = EWT
####################################
EWT_valid = which(!is.na(peak_train$EWT))

EWT_model = plsr(peak_train$EWT[EWT_valid] ~ as.matrix(peak_train[,predictor_columns]),
                ncomp = 30, method = "oscorespls", validation = "CV", scale = F)

# Optimal number of components
ncomp_EWT = selectNcomp(EWT_model, method = "onesigma", plot = T)

# Plot RMSEP to select optimal number of components
plot(RMSEP(EWT_model), legendpos = "topright")


###############
# Carbon = C
###############
C_valid = which(!is.na(peak_train$C))

C_model = plsr(peak_train$C[C_valid] ~ as.matrix(peak_train[C_valid, predictor_columns]),
               ncomp = 30, method = "oscorespls", validation = "CV", scale = F)

# Optimal number of components
ncomp_C = selectNcomp(C_model, method = "onesigma", plot = T)
ncomp_C = 3 # Manually select the minimum number of components

# Plot RMSEP to select optimal number of components
plot(RMSEP(C_model), legendpos = "topright")


###############
# Nitrogen = N
###############
N_valid = which(!is.na(peak_train$N))

N_model = plsr(peak_train$N[N_valid] ~ as.matrix(peak_train[N_valid, predictor_columns]),
               ncomp = 30, method = "oscorespls", validation = "CV", scale = F)

# Optimal number of components for each trait model
ncomp_N = selectNcomp(N_model, method = "onesigma", plot = T)
ncomp_N = 7 # Manually select ONC


################################################################################
# Bootstrap
################################################################################
# Set parameters
n_boot = 1000

# Define traits to predict
traits = c("LMA", "EWT", "C", "N")

# Select spectral column 
X_train = as.matrix(peak_train[,predictor_columns])
X_test  = as.matrix(peak_test[,predictor_columns])

# Input optimal number of components per trait
ncomp_list = c(LMA = ncomp_LMA, EWT = ncomp_EWT, C = ncomp_C, N = ncomp_N)

## --------------
# Trait loop
## --------------
for (trait in traits) {
  cat("Processing trait:", trait, "\n")
  
  # Get trait-specific ncomp
  ncomp = ncomp_list[[trait]]
  
  # Response vectors
  Y_train = peak_train[[trait]]
  Y_test  = peak_test[[trait]]
  
  # Initialize storage
  predictions = matrix(NA, nrow = nrow(X_test), ncol = n_boot)
  coef_list   = vector("list", n_boot)
  vip_list    = vector("list", n_boot)
  
  for (i in 1:n_boot) {
    cat("   boostrap: ", i, "\n")
    
    idx = sample(seq_len(nrow(X_train)), replace = TRUE)
    X_boot = X_train[idx, ]
    Y_boot = Y_train[idx]
    
    model = plsr(Y_boot ~ X_boot, ncomp = ncomp, method = "oscorespls", 
                  validation = "CV")
    
    pred = predict(model, newdata = X_test, ncomp = ncomp)
    predictions[, i] = pred[, , 1]
    
    coefs = coef(model, ncomp = ncomp, intercept = TRUE)
    
    # Combine Intercept and spectral names
    wavelengths = colnames(peak_test)[predictor_columns] 
    coef_names  = c("Intercept", wavelengths)
    
    # Assign named coefficients
    coef_list[[i]] = setNames(as.numeric(coefs), coef_names)
    
    # Calculate VIP
    vip_list[[i]] = plsVarSel::VIP(model, ncomp)
  }
  
  # Compute summary
  pred_mean <- rowMeans(predictions)
  pred_sd   <- apply(predictions, 1, sd)
  pred_ci95 <- apply(predictions, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
  
  result_df <- data.frame(
    Trait      = trait,
    Week       = peak_test$Week,
    Family     = peak_test$Family,
    Genus      = peak_test$Genus,
    Species    = peak_test$Species,
    Habit      = peak_test$Habit,
    Individual = peak_test$Individual,
    BranchID   = peak_test$Branch_ID,
    Measured   = Y_test,
    Predicted  = pred_mean,
    Pred_SD    = pred_sd,
    CI_0.025   = pred_ci95[1, ],
    CI_0.975   = pred_ci95[2, ]
  )
  
  coef_df = do.call(rbind, coef_list)
  vip_df  = do.call(rbind, vip_list)
  
  # Save and assign
  assign(paste0("results_", trait), result_df)
  assign(paste0("coef_", trait), coef_df)
  
  write.csv(coef_df, paste0("Data/Processed/Coefficients_and_Results/FM_peak_season/coef_boot_", trait, ".csv"), row.names = FALSE)
  write.csv(result_df, paste0("Data/Processed/Coefficients_and_Results/FM_peak_season/predictions_", trait, ".csv"), row.names = FALSE)
  write.csv(vip_df, paste0("Data/Processed/Coefficients_and_Results/FM_peak_season/vip_", trait, ".csv"), row.names = FALSE)
}

