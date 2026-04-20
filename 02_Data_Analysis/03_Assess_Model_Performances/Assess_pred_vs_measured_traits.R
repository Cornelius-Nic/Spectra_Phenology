################################################################################
#                               Script README!
# This script evaluates model predictions on observed traits
# Models: all season, week as covariate, peak season, and Kothari et al. (2023) New Phytologist!
################################################################################
# Packages
################################################################################
library("dplyr")
library("ggplot2")
library("patchwork")

source("R/Utils/important_functions.R")
################################################################################
# Import data
################################################################################
measured_traits    = readRDS("Data/Processed/fresh_spec_traits.RDS")
all_season_traits  = readRDS("Data/Processed/Trait_Predictions/trait_prediction_summary_fresh_spectra_all_season.rds")
week_covar_traits  = readRDS("Data/Processed/Trait_Predictions/trait_prediction_summary_fresh_spectra_week_as_covariate.rds")
peak_season_traits = readRDS("Data/Processed/Trait_Predictions/trait_prediction_summary_fresh_spectra_peak_season.rds")
kothari_traits     = readRDS("Data/Processed/Trait_Predictions/trait_prediction_summary_fresh_spectra_Kothari_2023.rds")

# Remove Narcissus pseudonarcissus
kothari_traits    = lapply(kothari_traits, function(x){
  x[x$Species !="Narcissus pseudonarcissus", ]
})
  
################################################################################
# Evaluate the performance of trait model predictions on the temporal datasets
################################################################################
# All season
all_season_traits_lma = assess_prediction_performance(measured_traits$LMA, all_season_traits$LMA$Pred_mean)
all_season_traits_ewt = assess_prediction_performance(measured_traits$EWT, all_season_traits$EWT$Pred_mean)
all_season_traits_c   = assess_prediction_performance(measured_traits$C, all_season_traits$C$Pred_mean)
all_season_traits_n   = assess_prediction_performance(measured_traits$N, all_season_traits$N$Pred_mean)

# Week as a covariate
week_covar_traits_lma = assess_prediction_performance(measured_traits$LMA, week_covar_traits$LMA$Pred_mean)
week_covar_traits_ewt = assess_prediction_performance(measured_traits$EWT, week_covar_traits$EWT$Pred_mean)
week_covar_traits_c   = assess_prediction_performance(measured_traits$C, week_covar_traits$C$Pred_mean)
week_covar_traits_n   = assess_prediction_performance(measured_traits$N, week_covar_traits$N$Pred_mean)

# Peak season
peak_season_traits_lma = assess_prediction_performance(measured_traits$LMA, peak_season_traits$LMA$Pred_mean)
peak_season_traits_ewt = assess_prediction_performance(measured_traits$EWT, peak_season_traits$EWT$Pred_mean)
peak_season_traits_c   = assess_prediction_performance(measured_traits$C, peak_season_traits$C$Pred_mean)
peak_season_traits_n   = assess_prediction_performance(measured_traits$N, peak_season_traits$N$Pred_mean)

# Kothari et al. 2023
kothari_traits_lma = assess_prediction_performance(measured_traits$LMA, kothari_traits$LMA$Pred_mean)
kothari_traits_ewt = assess_prediction_performance(measured_traits$EWT, kothari_traits$EWT$Pred_mean)
kothari_traits_c   = assess_prediction_performance(measured_traits$C, kothari_traits$C$Pred_mean)
kothari_traits_n   = assess_prediction_performance(measured_traits$N, kothari_traits$N$Pred_mean)

###################################################
# Add performance metrics to prediction dataframes
###################################################
all_season_lma = add_metrics_to_df(all_season_traits$LMA, all_season_traits_lma)
all_season_ewt = add_metrics_to_df(all_season_traits$EWT, all_season_traits_ewt)
all_season_c   = add_metrics_to_df(all_season_traits$C, all_season_traits_c)
all_season_n   = add_metrics_to_df(all_season_traits$N, all_season_traits_n)

week_covar_lma = add_metrics_to_df(week_covar_traits$LMA, week_covar_traits_lma)
week_covar_ewt = add_metrics_to_df(week_covar_traits$EWT, week_covar_traits_ewt)
week_covar_c   = add_metrics_to_df(week_covar_traits$C, week_covar_traits_c)
week_covar_n   = add_metrics_to_df(week_covar_traits$N, week_covar_traits_n)

peak_season_lma = add_metrics_to_df(peak_season_traits$LMA, peak_season_traits_lma)
peak_season_ewt = add_metrics_to_df(peak_season_traits$EWT, peak_season_traits_ewt)
peak_season_c   = add_metrics_to_df(peak_season_traits$C, peak_season_traits_c)
peak_season_n   = add_metrics_to_df(peak_season_traits$N, peak_season_traits_n)

kothari_lma = add_metrics_to_df(kothari_traits$LMA, kothari_traits_lma)
kothari_ewt = add_metrics_to_df(kothari_traits$EWT, kothari_traits_ewt)
kothari_c   = add_metrics_to_df(kothari_traits$C, kothari_traits_c)
kothari_n   = add_metrics_to_df(kothari_traits$N, kothari_traits_n)

#####################################
# Add Measured traits to dataframe
#####################################
all_season_lma$Measured = measured_traits$LMA
all_season_ewt$Measured = measured_traits$EWT
all_season_c$Measured   = measured_traits$C
all_season_n$Measured   = measured_traits$N

week_covar_lma$Measured = measured_traits$LMA
week_covar_ewt$Measured = measured_traits$EWT
week_covar_c$Measured   = measured_traits$C
week_covar_n$Measured   = measured_traits$N

peak_season_lma$Measured = measured_traits$LMA
peak_season_ewt$Measured = measured_traits$EWT
peak_season_c$Measured   = measured_traits$C
peak_season_n$Measured   = measured_traits$N

kothari_lma$Measured = measured_traits$LMA
kothari_ewt$Measured = measured_traits$EWT
kothari_c$Measured   = measured_traits$C
kothari_n$Measured   = measured_traits$N

#########################################
# Export dataframes for additional plots
#########################################
write.csv(all_season_lma, "Data/Processed/Trait_Predictions/all_season_lma.csv", row.names = F)
write.csv(all_season_ewt, "Data/Processed/Trait_Predictions/all_season_ewt.csv", row.names = F)
write.csv(all_season_c, "Data/Processed/Trait_Predictions/all_season_c.csv", row.names = F)
write.csv(all_season_n, "Data/Processed/Trait_Predictions/all_season_n.csv", row.names = F)

write.csv(week_covar_lma, "Data/Processed/Trait_Predictions/week_as_covariate_lma.csv", row.names = F)
write.csv(week_covar_ewt, "Data/Processed/Trait_Predictions/week_as_covariate_ewt.csv", row.names = F)
write.csv(week_covar_c, "Data/Processed/Trait_Predictions/week_as_covariate_c.csv", row.names = F)
write.csv(week_covar_n, "Data/Processed/Trait_Predictions/week_as_covariate_n.csv", row.names = F)

write.csv(peak_season_lma, "Data/Processed/Trait_Predictions/peak_season_lma.csv", row.names = F)
write.csv(peak_season_ewt, "Data/Processed/Trait_Predictions/peak_season_ewt.csv", row.names = F)
write.csv(peak_season_c, "Data/Processed/Trait_Predictions/peak_season_c.csv", row.names = F)
write.csv(peak_season_n, "Data/Processed/Trait_Predictions/peak_season_n.csv", row.names = F)

write.csv(kothari_lma, "Data/Processed/Trait_Predictions/kothari_lma.csv", row.names = F)
write.csv(kothari_ewt, "Data/Processed/Trait_Predictions/kothari_ewt.csv", row.names = F)
write.csv(kothari_c, "Data/Processed/Trait_Predictions/kothari_c.csv", row.names = F)
write.csv(kothari_n, "Data/Processed/Trait_Predictions/kothari_n.csv", row.names = F)

################################
# Define range for square plot
################################
# All season
all_season_lma_range = range(c(all_season_lma$Pred_Mean, all_season_lma$Measured, all_season_lma$CI_0.025, all_season_lma$CI_0.975), na.rm = TRUE)
all_season_ewt_range = range(c(all_season_ewt$Pred_Mean, all_season_ewt$Measured, all_season_ewt$CI_0.025, all_season_ewt$CI_0.975), na.rm = TRUE)
all_season_c_range   = range(c(all_season_c$Pred_Mean, all_season_c$Measured, all_season_c$CI_0.025, all_season_c$CI_0.975), na.rm = TRUE)
all_season_n_range   = range(c(all_season_n$Pred_Mean, all_season_n$Measured, all_season_n$CI_0.025, all_season_n$CI_0.975), na.rm = TRUE)

# Week as variable
week_covar_lma_range = range(c(week_covar_lma$Pred_Mean, week_covar_lma$Measured, week_covar_lma$CI_0.025, week_covar_lma$CI_0.975), na.rm = TRUE)
week_covar_ewt_range = range(c(week_covar_ewt$Pred_Mean, week_covar_ewt$Measured, week_covar_ewt$CI_0.025, week_covar_ewt$CI_0.975), na.rm = TRUE)
week_covar_c_range   = range(c(week_covar_c$Pred_Mean, week_covar_c$Measured, week_covar_c$CI_0.025, week_covar_c$CI_0.975), na.rm = TRUE)
week_covar_n_range   = range(c(week_covar_n$Pred_Mean, week_covar_n$Measured, week_covar_n$CI_0.025, week_covar_n$CI_0.975), na.rm = TRUE)

# Peak season
peak_season_lma_range = range(c(peak_season_lma$Pred_Mean, peak_season_lma$Measured, peak_season_lma$CI_0.025, peak_season_lma$CI_0.975), na.rm = TRUE)
peak_season_ewt_range = range(c(peak_season_ewt$Pred_Mean, peak_season_ewt$Measured, peak_season_ewt$CI_0.025, peak_season_ewt$CI_0.975), na.rm = TRUE)
peak_season_c_range   = range(c(peak_season_c$Pred_Mean, peak_season_c$Measured, peak_season_c$CI_0.025, peak_season_c$CI_0.975), na.rm = TRUE)
peak_season_n_range   = range(c(peak_season_n$Pred_Mean, peak_season_n$Measured, peak_season_n$CI_0.025, peak_season_n$CI_0.975), na.rm = TRUE)

# Kothari et al. 2023
kothari_lma_range = range(c(kothari_lma$Pred_Mean, kothari_lma$Measured, kothari_lma$CI_0.025, kothari_lma$CI_0.975), na.rm = TRUE)
kothari_ewt_range = range(c(kothari_ewt$Pred_Mean, kothari_ewt$Measured, kothari_ewt$CI_0.025, kothari_ewt$CI_0.975), na.rm = TRUE)
kothari_c_range   = range(c(kothari_c$Pred_Mean, kothari_c$Measured, kothari_c$CI_0.025, kothari_c$CI_0.975), na.rm = TRUE)
kothari_n_range   = range(c(kothari_n$Pred_Mean, kothari_n$Measured, kothari_n$CI_0.025, kothari_n$CI_0.975), na.rm = TRUE)

# Define species colors
species_colors = c("Acer platanoides" = "navyblue", "Acer rubrum" = "slategray4", 
                   "Betula papyrifera" = "maroon2", "Prunus nigra" = "darkorange3", 
                   "Quercus rubra" = "darkorchid3", "Rhododendron catawbiense" = "darkred", 
                   "Rhododendron maximum" = "green4")


################################################################################
# Plots -- All season 
################################################################################
# LMA plot 
LMA_plot_all = ggplot(all_season_lma, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.014, y = 0.192,
           label = paste0("R² = ", round(all_season_lma$R2, 2), "\n%RMSE = ", round(all_season_lma$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.98 - 1.01"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(title = "All season",
    x = expression("Predicted LMA (kg m"^-2*")"),
    y = expression("Measured LMA (kg m"^-2*")")
  ) +
  coord_equal(xlim = all_season_lma_range, ylim = all_season_lma_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# EWT plot
EWT_plot_all = ggplot(all_season_ewt, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.0032, y = 0.0365,
           label = paste0("R² = ", round(all_season_ewt$R2, 2), "\n%RMSE = ", round(all_season_ewt$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.97 - 1.01"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted EWT (g cm"^-2*")"),
    y = expression("Measured EWT (g cm"^-2*")")
  ) +
  coord_equal(xlim = all_season_ewt_range, ylim = all_season_ewt_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 10, face = "bold")
  )


# C plot
C_plot_all = ggplot(all_season_c, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 28.2, y = 79.4,
           label = paste0("R² = ", round(all_season_c$R2, 2), "\n%RMSE = ", round(all_season_c$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.68 - 1.58"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted C (%)"),
    y = expression("Measured C (%)")
  ) +
  coord_equal(xlim = all_season_c_range, ylim = all_season_c_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# N plot
N_plot_all = ggplot(all_season_n, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -4.1, y = 6.3,
           label = paste0("R² = ", round(all_season_n$R2, 2), "\n%RMSE = ", round(all_season_n$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.86 - 1.02"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted N (%)"),
    y = expression("Measured N (%)")
  ) +
  coord_equal(xlim = all_season_n_range, ylim = all_season_n_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


################################################################################
# Plots -- Week as covariate
################################################################################
# LMA plot 
LMA_plot_week = ggplot(week_covar_lma, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.013, y = 0.191,
           label = paste0("R² = ", round(week_covar_lma$R2, 2), "\n%RMSE = ", round(week_covar_lma$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.98 - 1.01"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(title = "Week as covariate",
    x = expression("Predicted LMA (kg m"^-2*")"),
    y = expression("Measured LMA (kg m"^-2*")")
  ) +
  coord_equal(xlim = week_covar_lma_range, ylim = week_covar_lma_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# EWT plot
EWT_plot_week = ggplot(week_covar_ewt, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.0031, y = 0.0365,
           label = paste0("R² = ", round(week_covar_ewt$R2, 2), "\n%RMSE = ", round(week_covar_ewt$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.96 - 1.00"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted EWT (g cm"^-2*")"),
    y = expression("Measured EWT (g cm"^-2*")")
  ) +
  coord_equal(xlim = week_covar_ewt_range, ylim = week_covar_ewt_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 7, face = "italic"),
    legend.title = element_text(size = 8, face = "bold")
  )


# C plot
C_plot_week = ggplot(week_covar_c, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text",  x = 25.7, y = 79.5,
           label = paste0("R² = ", round(week_covar_c$R2, 2), "\n%RMSE = ", round(week_covar_c$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.72 - 1.06"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted C (%)"),
    y = expression("Measured C (%)")
  ) +
  coord_equal(xlim = week_covar_c_range, ylim = week_covar_c_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# N plot
N_plot_week = ggplot(week_covar_n, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -3.2, y = 6.2,
           label = paste0("R² = ", round(week_covar_n$R2, 2), "\n%RMSE = ", round(week_covar_n$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.83 - 0.99"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted N (%)"),
    y = expression("Measured N (%)")
  ) +
  coord_equal(xlim = week_covar_n_range, ylim = week_covar_n_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# Combine Plots 4 x 2
leaf_pheno = patchwork::wrap_plots(LMA_plot_all, LMA_plot_week, 
                                   EWT_plot_all, EWT_plot_week, 
                                   C_plot_all, C_plot_week, 
                                   N_plot_all, N_plot_week,
                                   ncol = 2, nrow = 4, guides = "collect")

# Save plot
ggsave("Figs/Leaf_phenology_models_vs_measured_assessments_plots.pdf", plot = leaf_pheno, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Leaf_phenology_models_vs_measured_assessments_plots.png", plot = leaf_pheno, width = 8.5, height = 10.5, dpi = 600)


################################################################################
# Plots -- Peak season 
################################################################################
# LMA plot
LMA_plot_peak = ggplot(peak_season_lma, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -0.74, y = 0.28,
           label = paste0("R² = ", round(peak_season_lma$R2, 2), "\n%RMSE = ", round(peak_season_lma$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.18 - 0.21"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(title = "Peak season",
    x = expression("Predicted LMA (kg m"^-2*")"),
    y = expression("Measured LMA (kg m"^-2*")")
  ) +
  coord_equal(xlim = peak_season_lma_range, ylim = peak_season_lma_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# EWT plot
EWT_plot_peak = ggplot(peak_season_ewt, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -0.011, y = 0.09,
           label = paste0("R² = ", round(peak_season_ewt$R2, 2), "\n%RMSE = ", round(peak_season_ewt$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.29 - 0.35"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted EWT (g cm"^-2*")"),
    y = expression("Measured EWT (g cm"^-2*")")
  ) +
  coord_equal(xlim = peak_season_ewt_range, ylim = peak_season_ewt_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 10, face = "bold")
  )


# C plot
C_plot_peak = ggplot(peak_season_c, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text",  x = -27, y = 157,
           label = paste0("R² = ", round(peak_season_c$R2, 2), "\n%RMSE = ", round(peak_season_c$perRMSE, 2),
                          "\nSlope 95% CI = ", "-0.18 - 0.50"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted C (%)"),
    y = expression("Measured C (%)")
  ) +
  coord_equal(xlim = peak_season_c_range, ylim = peak_season_c_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# N plot
N_plot_peak = ggplot(peak_season_n, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text",  x = -17, y = 52,
           label = paste0("R² = ", round(peak_season_n$R2, 2), "\n%RMSE = ", round(peak_season_n$perRMSE, 2),
                          "\nSlope 95% CI = ", "-0.10 - -0.05"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted N (%)"),
    y = expression("Measured N (%)")
  ) +
  coord_equal(xlim = peak_season_n_range, ylim = peak_season_n_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


################################################################################
# Plots -- Kothari et al. 2023 
################################################################################
# LMA Plot
LMA_plot_koth = ggplot(kothari_lma, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -0.5, y = 2.03,
           label = paste0("R² = ", round(kothari_lma$R2, 2), "\n%RMSE = ", round(kothari_lma$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.02 - 0.04"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(title = "Kothari 2023",
    x = expression("Predicted LMA (kg m"^-2*")"),
    y = expression("Measured LMA (kg m"^-2*")")
  ) +
  coord_equal(xlim = kothari_lma_range, ylim = kothari_lma_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# EWT plot
EWT_plot_koth = ggplot(kothari_ewt, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -0.018, y = 0.154,
           label = paste0("R² = ", round(kothari_ewt$R2, 2), "\n%RMSE = ", round(kothari_ewt$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.14 - 0.17"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted EWT (g cm"^-2*")"),
    y = expression("Measured EWT (g cm"^-2*")")
  ) +
  coord_equal(xlim = kothari_ewt_range, ylim = kothari_ewt_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# C plot
C_plot_koth = ggplot(kothari_c, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text",  x = 24.5, y = 81,
           label = paste0("R² = ", round(kothari_c$R2, 2), "\n%RMSE = ", round(kothari_c$perRMSE, 2),
                          "\nSlope 95% CI = ", "-0.35 - 0.62"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted C (%)"),
    y = expression("Measured C (%)")
  ) +
  coord_equal(xlim = kothari_c_range, ylim = kothari_c_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 7, face = "italic"),
    legend.title = element_text(size = 8, face = "bold")
  )


# N plot
N_plot_koth = ggplot(kothari_n, aes(x = Pred_mean, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -19.8, y = 6.95,
           label = paste0("R² = ", round(kothari_n$R2, 2), "\n%RMSE = ", round(kothari_n$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.09 - 0.16"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted N (%)"),
    y = expression("Measured N (%)")
  ) +
  coord_equal(xlim = kothari_n_range, ylim = kothari_n_range, expand = T) +
  scale_color_manual(values = species_colors) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# Combine Plots 4 x 2
season_specific = patchwork::wrap_plots(LMA_plot_peak, LMA_plot_koth, 
                                        EWT_plot_peak, EWT_plot_koth, 
                                        C_plot_peak, C_plot_koth, 
                                        N_plot_peak, N_plot_koth,
                                        ncol = 2, nrow = 4, guides = "collect")

# Save plot
ggsave("Figs/Season_specific_models_vs_measured_assessment_plots.pdf", plot = season_specific, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Season_specific_models_vs_measured_assessment_plots.png", plot = season_specific, width = 8.5, height = 10.5, dpi = 600)


###############################################
# Summary plots for trait evaluation
###############################################
# Combine all metrics
all_season_metrics  = do.call(rbind, list(all_season_traits_lma, all_season_traits_ewt, 
                                          all_season_traits_c, all_season_traits_n))

week_covar_metrics = do.call(rbind, list(week_covar_traits_lma, week_covar_traits_ewt, 
                                         week_covar_traits_c, week_covar_traits_n))

peak_season_metrics = do.call(rbind, list(peak_season_traits_lma, peak_season_traits_ewt, 
                                          peak_season_traits_c, peak_season_traits_n))

kothari_metrics     = do.call(rbind, list(kothari_traits_lma, kothari_traits_ewt, 
                                          kothari_traits_c, kothari_traits_n))

# Add trait column
all_season_metrics$Trait  = c("LMA", "EWT", "C", "N")
week_covar_metrics$Trait  = c("LMA", "EWT", "C", "N")
peak_season_metrics$Trait = c("LMA", "EWT", "C", "N")
kothari_metrics$Trait     = c("LMA", "EWT", "C", "N")

# Arrange trait in order
all_season_metrics$Trait  = factor(all_season_metrics$Trait, levels = c("LMA", "EWT", "C", "N"))
week_covar_metrics$Trait  = factor(week_covar_metrics$Trait, levels = c("LMA", "EWT", "C", "N"))
peak_season_metrics$Trait = factor(peak_season_metrics$Trait, levels = c("LMA", "EWT", "C", "N"))
kothari_metrics$Trait     = factor(kothari_metrics$Trait, levels = c("LMA", "EWT", "C", "N"))

# Combine metrics
all_season_metrics$Model  = "All season"
week_covar_metrics$Model  = "Week as covariate"
peak_season_metrics$Model = "Peak season"
kothari_metrics$Model     = "Kothari 2023"

combine_metrics = bind_rows(all_season_metrics, week_covar_metrics, peak_season_metrics, kothari_metrics)

# Make Model a factor
combine_metrics$Model = factor(combine_metrics$Model, levels = c("All season", "Week as covariate", "Peak season", "Kothari 2023"))

# Export metrics
write.csv(all_season_metrics, "Data/Processed/Coefficients_and_Results/FM_all_season/all_season_pred_vs_obs_assessment_metrics.csv", row.names = F)
write.csv(week_covar_metrics, "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/week_covar_pred_vs_obs_assessment_metrics.csv", row.names = F)
write.csv(peak_season_metrics, "Data/Processed/Coefficients_and_Results/FM_peak_season/peak_season_pred_vs_obs_assessment_metrics.csv", row.names = F)
write.csv(kothari_metrics, "Data/Processed/Coefficients_and_Results/Kothari_preds/kothari_pred_vs_obs_assessment_metrics.csv", row.names = F)

#############################
# Plot Metrics Summary
#############################
# Define colors
trait_colors = c("LMA" = "green4",
                 "EWT" = "royalblue",
                 "C" = "darkorchid2",
                 "N" = "darkorange4")

R2_sum_plot = ggplot(combine_metrics, aes(y = R2, x = Trait, fill = Trait)) +
  geom_col(alpha = 0.7, width = 0.6) +
  facet_wrap(~Model, nrow = 1) +
  scale_fill_manual(values = trait_colors) +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  labs(y = expression(italic("R"^2)), title = "R2"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))


perRMSE_sum_plot = ggplot(combine_metrics, aes(y = perRMSE, x = Trait, fill = Trait)) +
  geom_col(alpha = 0.7, width = 0.6) +  
  facet_wrap(~Model, nrow = 1) +
  scale_fill_manual(values = trait_colors) +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  labs(y = "%RMSE", x = "Trait", title = "%RMSE"
  ) +
  scale_y_continuous(expand = c(0, 0))


# Combine Plots 1 x 2
model_summary_plots = patchwork::wrap_plots(R2_sum_plot, perRMSE_sum_plot, 
                                            ncol = 1, nrow = 2, guides = "collect")

ggsave("Figs/Model_assessment_summary_plots.pdf", plot = model_summary_plots, width = 8.5, height = 6.5, dpi = 600)
ggsave("Figs/Model_assessment_summary_plots.png", plot = model_summary_plots, width = 8.5, height = 6.5, dpi = 600)


################################################################################
