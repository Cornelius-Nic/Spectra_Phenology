################################################################################
#                             Script README!
# This model was developed using "Week-as-covariate" for the all season data.
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

# Split the data by Branch_ID
train_idx = createDataPartition(fresh_spec_traits$Branch_ID, p = 0.75, list = FALSE)

fresh_train = fresh_spec_traits[train_idx, ]
fresh_test  = fresh_spec_traits[-train_idx, ]

# Week and spec: data to model
cols_week_and_spec = c(1, 15:ncol(fresh_spec_traits))

################################################################################
# Model tuning: to select the optimal number of components for bootstrapping.
################################################################################

x = as.matrix(fresh_train[ , cols_week_and_spec])

###########################
# Leaf Mass per Area = LMA
###########################
LMA_valid = which(!is.na(fresh_train$LMA))

LMA_model = plsr(fresh_train$LMA[LMA_valid] ~ x[LMA_valid, ],
                 ncomp = 30, method = "oscorespls", validation = "CV", scale = F)

# Select optimal number of components
ncomp_LMA  = selectNcomp(LMA_model, method = "onesigma", plot = T)

# Plot RMSEP to select optimal number of components
plot(RMSEP(LMA_model), legendpos = "topright")
#summary(LMA_model) # PLSR model examination

####################################
# Equivalent Water Thickness = EWT
####################################
EWT_valid = which(!is.na(fresh_train$EWT))

EWT_model = plsr(fresh_train$EWT[EWT_valid] ~ x[EWT_valid, ],
                ncomp = 30, method = "oscorespls", validation = "CV", scale = F)

# Optimal number of components
ncomp_EWT  = selectNcomp(EWT_model, method = "onesigma", plot = T)

# Plot RMSEP to select optimal number of components
plot(RMSEP(EWT_model), legendpos = "topright")

###############
# Carbon = C
###############
C_valid = which(!is.na(fresh_train$C))

C_model = plsr(fresh_train$C[C_valid] ~ x[C_valid, ],
               ncomp = 30, method = "oscorespls", validation = "CV", scale = F)

# Optimal number of components
ncomp_C = selectNcomp(C_model, method = "onesigma", plot = T)
ncomp_C = 5 # Manually select the minimum number of components

# Plot RMSEP to select optimal number of components
plot(RMSEP(C_model), legendpos = "topright")


###############
# Nitrogen = N
###############
N_valid = which(!is.na(fresh_train$N))

N_model = plsr(fresh_train$N[N_valid] ~ x[N_valid, ],
               ncomp = 30, method = "oscorespls", validation = "CV", scale = F)

# Optimal number of components for each trait model
ncomp_N = selectNcomp(N_model, method = "onesigma", plot = T)

# Plot RMSEP to select optimal number of components
plot(RMSEP(N_model), legendpos = "topright")


################################################################################
# Bootstrap
################################################################################
n_boot = 1000

# Define traits to predict
traits = c("LMA", "EWT", "C", "N")

# Input optimal number of components per trait
ncomp_list = c(LMA = ncomp_LMA, EWT = ncomp_EWT, C = ncomp_C, N = ncomp_N)

## --------------
# Trait loop
## --------------
# Trait loop
for (trait in traits) {
  cat("Processing trait:", trait, "\n")
  
  ncomp <- ncomp_list[[trait]]
  
  # Predictors with Week as factor
  X_train <- as.matrix(fresh_train[, cols_week_and_spec])
  X_test  <- as.matrix(fresh_test[, cols_week_and_spec])
  
  # Response vectors
  Y_train = fresh_train[[trait]]
  Y_test  = fresh_test[[trait]]
  
  # Storage
  predictions = matrix(NA, nrow = nrow(X_test), ncol = n_boot)
  coef_list   = vector("list", n_boot)
  vip_list    = vector("list", n_boot)

  for (i in 1:n_boot) {
    cat("   boostrap: ", i, "\n")
    
    idx <- sample(seq_len(nrow(X_train)), replace = TRUE)
    X_boot <- X_train[idx, ]
    Y_boot <- Y_train[idx]
    
    model <- plsr(Y_boot ~ X_boot, ncomp = ncomp, method = "oscorespls",
                  validation = "CV")
    
    pred <- predict(model, newdata = X_test, ncomp = ncomp)
    predictions[, i] <- pred[, , 1]
    
    coefs <- coef(model, ncomp = ncomp, intercept = TRUE)
    coef_list[[i]] <- as.numeric(coefs)
    names(coef_list[[i]]) <- c("Intercept", colnames(X_train))
    
    # Calculate VIP
    vip_list[[i]] = plsVarSel::VIP(model, ncomp)
  }
  
  # Summarize predictions
  pred_mean <- rowMeans(predictions)
  pred_sd   <- apply(predictions, 1, sd)
  pred_ci95 <- apply(predictions, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
  
  result_df <- data.frame(
    Trait      = trait,
    Week       = fresh_test$Week,
    Family     = fresh_test$Family,
    Genus      = fresh_test$Genus,
    Species    = fresh_test$Species,
    Habit      = fresh_test$Habit,
    Individual = fresh_test$Individual,
    BranchID   = fresh_test$Branch_ID,
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
  
  write.csv(coef_df, paste0("Data/Processed/Coefficients_and_Results/FM_week_as_covariate/coef_boot_", trait, ".csv"), row.names = FALSE)
  write.csv(result_df, paste0("Data/Processed/Coefficients_and_Results/FM_week_as_covariate/predictions_", trait, ".csv"), row.names = FALSE)
  write.csv(vip_df, paste0("Data/Processed/Coefficients_and_Results/FM_week_as_covariate/vip_", trait, ".csv"), row.names = FALSE)
}

