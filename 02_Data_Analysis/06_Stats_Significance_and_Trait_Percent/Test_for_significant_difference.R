################################################################################
# Packages
################################################################################
library("dplyr")
library("tidyr")
library("stringr")

#########################################
# Import data
#########################################
measured_traits    = readRDS("Data/Processed/fresh_spec_traits.RDS")
all_season_traits  = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_all_season.csv")
week_covar_traits  = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_week_as_covariate.csv")
peak_season_traits = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_peak_season.csv")
kothari_traits     = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_Kothari_2023.csv")


################################################################################
# Measured traits
# Test of trait significant differences between seasons
################################################################################
# Target traits
traits = c("LMA", "EWT", "N", "C")

############################################################
# Step 1: Filter to Weeks (1, 11, 23)
# PS. Weeks 1, 11, 23 = Weeks 22, 32, and 44 in Calendar year
############################################################

measured_weeks = measured_traits %>%
  filter(Week %in% c(1, 11, 23)) %>%
  mutate(Week = factor(Week))

############################################################
# Step 2: Create long-format dataframe (Species × Trait × Week)
############################################################

long_df = do.call(rbind, lapply(traits, function(trait) {
  measured_weeks %>%
    filter(!is.na(.data[[trait]])) %>%
    mutate(
      Value = .data[[trait]],
      Trait = trait
    ) %>%
    dplyr::select(Species, Week, Trait, Value)
}))

############################################################
# Step 3: ANOVA + TukeyHSD for Trait ~ Week (within Species)
############################################################

get_week_sig_table = function(df_sub, trait, species) {
  # Must have at least 2 week levels
  if (length(unique(df_sub$Week)) < 2) return(NULL)
  
  # Fit ANOVA model
  aov_model = aov(Value ~ Week, data = df_sub)
  aov_fit   = summary(aov_model)
  tukey_res = TukeyHSD(aov_model)$Week
  
  if (is.null(tukey_res)) return(NULL)
  
  sig_df = as.data.frame(tukey_res)
  
  # -------------------------------
  # MAP NUMERIC LEVELS → SEASONS
  # -------------------------------
  convert_week = function(x) {
    recode(as.character(x),
           "1"  = "Early",
           "11" = "Peak",
           "23" = "Late")
  }
  
  # Convert comparisons: "11-1" → "Peak - Early"
  convert_comparison = function(comp) {
    parts = unlist(strsplit(comp, "-"))
    paste(convert_week(parts[1]), "-", convert_week(parts[2]))
  }
  
  sig_df$Comparison = sapply(rownames(sig_df), convert_comparison)
  rownames(sig_df) = NULL
  
  # -------------------------------
  # ADD SIGNIFICANCE COLUMNS
  # -------------------------------
  sig_df = sig_df %>%
    dplyr::select(Comparison, `p adj`) %>%
    mutate(
      Trait = trait,
      Species = species,
      Significance = case_when(
        `p adj` < 0.001 ~ "***",
        `p adj` < 0.01  ~ "**",
        `p adj` < 0.05  ~ "*",
        TRUE            ~ "ns"
      )
    ) %>%
    dplyr::select(Trait, Species, Comparison, `p adj`, Significance)
  
  return(list(at = aov_fit, tt = sig_df))
}

############################################################
# Step 4: Apply across Traits × Species
############################################################
sig_all = list()
aov_all = list()

for (trait in traits) {
  for (sp in unique(long_df$Species)) {
    
    sub_df = long_df %>% filter(Trait == trait, Species == sp)
    
    if (nrow(sub_df) > 0 && length(unique(sub_df$Week)) > 1) {
      
      sig_res = get_week_sig_table(sub_df, trait, sp)
      
      if (!is.null(sig_res)) {
        
        # Store Tukey significance table
        sig_all[[paste(trait, sp, sep = "_")]] <- sig_res$tt
        
        # Store ANOVA summary with species included
        aov_all[[paste(trait, sp, sep = "_")]] <- data.frame(
          Trait   = trait,
          Species = sp,
          Term    = rownames(sig_res$at[[1]]),
          sig_res$at[[1]]
        )
      }
    }
  }
}

############################################################
# Step 5: Combine & Export
############################################################

sig_table = do.call(rbind, sig_all)
aov_table = do.call(rbind, aov_all)

write.csv(sig_table, "Data/Processed/Measured_Species_Significance.csv", row.names = FALSE)
write.csv(aov_table, "Data/Processed/Measured_ANOVA_Species_Summary.csv", row.names = FALSE)

message("✅ Week-level TukeyHSD (Trait ~ Week per Species) saved to Data/Processed/Measured_Species_Significance.csv")



################################################################################
# Trait Significant difference across seasons
# (1) Measured vs One Phenophase Models (Kothari et al. (2023) & Peak season)
################################################################################
#########################################
# Step 1: Combine datasets
#########################################
meas_season_data = list(
  Measured = measured_traits,
  Peak_season = peak_season_traits,
  Kothari_2023 = kothari_traits
)

# Step 2: Ensure column consistency
common_cols       = Reduce(intersect, lapply(meas_season_data, colnames))
meas_season_data  = lapply(meas_season_data, function(df) df[, common_cols])

###############################################
# Step 3: Combine into long dataframe
###############################################
long_data = do.call(rbind, lapply(names(meas_season_data), function(dataset) {
  df = meas_season_data[[dataset]]
  df$Dataset = dataset
  df
}))

################################################################
# Step 4: Filter to target weeks (1, 11, 23)
################################################################
filtered_data = long_data %>%
  filter(Week %in% c(1, 11, 23)) %>%
  mutate(Week = factor(Week))

############################################################
# Step 5: ANOVA + TukeyHSD for Trait ~ Week (within Species)
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


############################################################
# Step 6: Apply across Traits × Species
############################################################
sig_all = list()

for (trait in traits) {
  for (sp in unique(filtered_data$Species)) {
    
    sub_df = filtered_data %>%
      filter(!is.na(.data[[trait]]),
             Species == sp) %>%
      mutate(Value = .data[[trait]])
    
    res = get_dataset_sig_by_week(sub_df, trait, sp)
    
    if (!is.null(res)) {
      sig_all[[paste(trait, sp, sep = "_")]] <- res
    }
  }
}

############################################################
# Step 7: Combine & Export
############################################################
season_sig_table = do.call(rbind, sig_all)

write.csv(season_sig_table, "Data/Processed/Measured_vs_One_Phenophase_Models_Significance.csv", row.names = FALSE)



################################################################################
# Trait Significant difference across seasons
# (2) Measured vs All season vs Week as covariate
################################################################################
#########################################
# Step 1: Combine datasets
#########################################
meas_all_data = list(
  Measured = measured_traits,
  All_season = peak_season_traits,
  Week_covariate = week_covar_traits
)

# Step 2: Ensure column consistency
common_cols    = Reduce(intersect, lapply(meas_all_data, colnames))
meas_all_data  = lapply(meas_all_data, function(df) df[, common_cols])

###############################################
# Step 3: Combine into long dataframe
###############################################
mlong_data = do.call(rbind, lapply(names(meas_all_data), function(dataset) {
  df = meas_all_data[[dataset]]
  df$Dataset = dataset
  df
}))

###############################################
# Step 4: Filter to target weeks (1, 11, 23)
###############################################
filtered_data = mlong_data %>%
  filter(Week %in% c(1, 11, 23)) %>%
  mutate(Week = factor(Week))

############################################################
# Step 5: ANOVA + TukeyHSD for Trait ~ Week (within Species)
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

############################################################
# Step 6: Apply across Traits × Species
############################################################
sig_all = list()

for (trait in traits) {
  for (sp in unique(filtered_data$Species)) {
    
    sub_df = filtered_data %>%
      filter(!is.na(.data[[trait]]),
             Species == sp) %>%
      mutate(Value = .data[[trait]])
    
    res = get_dataset_sig_by_week(sub_df, trait, sp)
    
    if (!is.null(res)) {
      sig_all[[paste(trait, sp, sep = "_")]] <- res
    }
  }
}

############################################################
# Step 7: Combine & Export
############################################################
pheno_sig_table = do.call(rbind, sig_all)

write.csv(pheno_sig_table, "Data/Processed/Measured_vs_Complete_Phenology_Models_Significance.csv", row.names = FALSE)


#################################################################################
#################################################################################
