################################################################################
#                               Script README!
# This script evaluates the 3 trait prediction models developed in Nichodemus and Meireles (....) New Phytologist!
################################################################################
# Packages
################################################################################
library("ggplot2")
library("dplyr")
library("readr")
library("patchwork")

source("R/Utils/important_functions.R")
################################################################################
# Read data
################################################################################

all_season_trait_pred_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_all_season/predictions_LMA.csv",
                                "EWT" = "Data/Processed/Coefficients_and_Results/FM_all_season/predictions_EWT.csv",
                                "C"   = "Data/Processed/Coefficients_and_Results/FM_all_season/predictions_C.csv",
                                "N"   = "Data/Processed/Coefficients_and_Results/FM_all_season/predictions_N.csv")

all_season_traits  = lapply(all_season_trait_pred_paths, read_csv)

week_covar_trait_pred_paths = c("LMA"  = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/predictions_LMA.csv",
                                "EWT"  = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/predictions_EWT.csv",
                                "C"    = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/predictions_C.csv",
                                "N"    = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/predictions_N.csv")

week_covar_traits = lapply(week_covar_trait_pred_paths, read_csv)

peak_season_trait_pred_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_peak_season/predictions_LMA.csv",
                                "EWT"  = "Data/Processed/Coefficients_and_Results/FM_peak_season/predictions_EWT.csv",
                                "C"    = "Data/Processed/Coefficients_and_Results/FM_peak_season/predictions_C.csv",
                                "N"    = "Data/Processed/Coefficients_and_Results/FM_peak_season/predictions_N.csv")

peak_season_traits = lapply(peak_season_trait_pred_paths, read_csv)


################################
# Evaluate Model Performances
################################
all_season_lma_metrics = assess_prediction_performance(all_season_traits$LMA$Measured, all_season_traits$LMA$Predicted)
all_season_ewt_metrics = assess_prediction_performance(all_season_traits$EWT$Measured, all_season_traits$EWT$Predicted)
all_season_c_metrics   = assess_prediction_performance(all_season_traits$C$Measured, all_season_traits$C$Predicted)
all_season_n_metrics   = assess_prediction_performance(all_season_traits$N$Measured, all_season_traits$N$Predicted)

week_covar_lma_metrics = assess_prediction_performance(week_covar_traits$LMA$Measured, week_covar_traits$LMA$Predicted)
week_covar_ewt_metrics = assess_prediction_performance(week_covar_traits$EWT$Measured, week_covar_traits$EWT$Predicted)
week_covar_c_metrics   = assess_prediction_performance(week_covar_traits$C$Measured, week_covar_traits$C$Predicted)
week_covar_n_metrics   = assess_prediction_performance(week_covar_traits$N$Measured, week_covar_traits$N$Predicted)

peak_season_lma_metrics = assess_prediction_performance(peak_season_traits$LMA$Measured, peak_season_traits$LMA$Predicted)
peak_season_ewt_metrics = assess_prediction_performance(peak_season_traits$EWT$Measured, peak_season_traits$EWT$Predicted)
peak_season_c_metrics   = assess_prediction_performance(peak_season_traits$C$Measured, peak_season_traits$C$Predicted)
peak_season_n_metrics   = assess_prediction_performance(peak_season_traits$N$Measured, peak_season_traits$N$Predicted)


###################################################
# Add performance metrics to prediction dataframes
###################################################
all_season_LMA_df = add_metrics_to_df(all_season_traits$LMA, all_season_lma_metrics)
all_season_EWT_df = add_metrics_to_df(all_season_traits$EWT, all_season_ewt_metrics)
all_season_C_df   = add_metrics_to_df(all_season_traits$C, all_season_c_metrics)
all_season_N_df   = add_metrics_to_df(all_season_traits$N, all_season_n_metrics)

week_covar_LMA_df = add_metrics_to_df(week_covar_traits$LMA, week_covar_lma_metrics)
week_covar_EWT_df = add_metrics_to_df(week_covar_traits$EWT, week_covar_ewt_metrics)
week_covar_C_df   = add_metrics_to_df(week_covar_traits$C, week_covar_c_metrics)
week_covar_N_df   = add_metrics_to_df(week_covar_traits$N, week_covar_n_metrics)

peak_season_LMA_df = add_metrics_to_df(peak_season_traits$LMA, peak_season_lma_metrics)
peak_season_EWT_df = add_metrics_to_df(peak_season_traits$EWT, peak_season_ewt_metrics)
peak_season_C_df   = add_metrics_to_df(peak_season_traits$C, peak_season_c_metrics)
peak_season_N_df   = add_metrics_to_df(peak_season_traits$N, peak_season_n_metrics)


################################
# Define range for square plot
################################
all_season_lma_range = range(c(all_season_LMA_df$Predicted, all_season_LMA_df$Measured, all_season_LMA_df$CI_0.025, all_season_LMA_df$CI_0.975), na.rm = TRUE)
all_season_ewt_range = range(c(all_season_EWT_df$Predicted, all_season_EWT_df$Measured, all_season_EWT_df$CI_0.025, all_season_EWT_df$CI_0.975), na.rm = TRUE)
all_season_c_range   = range(c(all_season_C_df$Predicted, all_season_C_df$Measured, all_season_C_df$CI_0.025, all_season_C_df$CI_0.975), na.rm = TRUE)
all_season_n_range   = range(c(all_season_N_df$Predicted, all_season_N_df$Measured, all_season_N_df$CI_0.025, all_season_N_df$CI_0.975), na.rm = TRUE)

week_covar_lma_range = range(c(week_covar_LMA_df$Predicted, week_covar_LMA_df$Measured, week_covar_LMA_df$CI_0.025, week_covar_LMA_df$CI_0.975), na.rm = TRUE)
week_covar_ewt_range = range(c(week_covar_EWT_df$Predicted, week_covar_EWT_df$Measured, week_covar_EWT_df$CI_0.025, week_covar_EWT_df$CI_0.975), na.rm = TRUE)
week_covar_c_range   = range(c(week_covar_C_df$Predicted, week_covar_C_df$Measured, week_covar_C_df$CI_0.025, week_covar_C_df$CI_0.975), na.rm = TRUE)
week_covar_n_range   = range(c(week_covar_N_df$Predicted, week_covar_N_df$Measured, week_covar_N_df$CI_0.025, week_covar_N_df$CI_0.975), na.rm = TRUE)

peak_season_lma_range = range(c(peak_season_LMA_df$Predicted, peak_season_LMA_df$Measured, peak_season_LMA_df$CI_0.025, peak_season_LMA_df$CI_0.975), na.rm = TRUE)
peak_season_ewt_range = range(c(peak_season_EWT_df$Predicted, peak_season_EWT_df$Measured, peak_season_EWT_df$CI_0.025, peak_season_EWT_df$CI_0.975), na.rm = TRUE)
peak_season_c_range   = range(c(peak_season_C_df$Predicted, peak_season_C_df$Measured, peak_season_C_df$CI_0.025, peak_season_C_df$CI_0.975), na.rm = TRUE)
peak_season_n_range   = range(c(peak_season_N_df$Predicted, peak_season_N_df$Measured, peak_season_N_df$CI_0.025, peak_season_N_df$CI_0.975), na.rm = TRUE)

# Define species colors
species_colors = c("Acer platanoides" = "navyblue", "Acer rubrum" = "slategray4", 
                   "Betula papyrifera" = "maroon2", "Prunus nigra" = "darkorange3", 
                   "Quercus rubra" = "darkorchid3", "Rhododendron catawbiense" = "darkred", 
                   "Rhododendron maximum" = "green4")

################################################################################
# Plots -- All season 
################################################################################
# Plot LMA
LMA_plot_all = ggplot(all_season_LMA_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.0195, y = 0.179,
           label = paste0("R² = ", round(all_season_LMA_df$R2, 2), "\n%RMSE = ", round(all_season_LMA_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.96 - 1.03"),
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
EWT_plot_all = ggplot(all_season_EWT_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.0034, y = 0.0257,
           label = paste0("R² = ", round(all_season_EWT_df$R2, 2), "\n%RMSE = ", round(all_season_EWT_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.92 - 1.02"),
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
C_plot_all = ggplot(all_season_C_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 28, y = 79.5,
           label = paste0("R² = ", round(all_season_C_df$R2, 2), "\n%RMSE = ", round(all_season_C_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.47 - 2.89"),
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
N_plot_all = ggplot(all_season_N_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -4.15, y = 6.27,
           label = paste0("R² = ", round(all_season_N_df$R2, 2), "\n%RMSE = ", round(all_season_N_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.60 - 0.99"),
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


# Combine Plots 2 x 2
all_season_plots = patchwork::wrap_plots(LMA_plot_all, EWT_plot_all, C_plot_all, N_plot_all,
                              ncol = 2, nrow = 2, guides = "collect")

# Save plot
#ggsave("Figs/All_season_plots/all_season_plots_model_fitting.pdf", plot = all_season_plots, width = 8.5, height = 8.5, dpi = 600)


################################################################################
# Plots -- Week as covariate
################################################################################
# Plot LMA
LMA_plot_week = ggplot(week_covar_LMA_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.013, y = 0.1765,
           label = paste0("R² = ", round(week_covar_LMA_df$R2, 2), "\n%RMSE = ", round(week_covar_LMA_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.97 - 1.02"),
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
EWT_plot_week = ggplot(week_covar_EWT_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.0036, y = 0.0257,
           label = paste0("R² = ", round(week_covar_EWT_df$R2, 2), "\n%RMSE = ", round(week_covar_EWT_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.90 - 0.98"),
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
    legend.position = "none",
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 10, face = "bold")
  )


# C plot
C_plot_week = ggplot(week_covar_C_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 28, y = 69.8,
           label = paste0("R² = ", round(week_covar_C_df$R2, 2), "\n%RMSE = ", round(week_covar_C_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.15 - 0.76"),
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
N_plot_week = ggplot(week_covar_N_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -1.82, y = 5,
           label = paste0("R² = ", round(week_covar_N_df$R2, 2), "\n%RMSE = ", round(week_covar_N_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.48 - 0.73"),
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


# Combine Plots 2 x 2
week_covar_plots = patchwork::wrap_plots(LMA_plot_week, EWT_plot_week, C_plot_week, N_plot_week,
                                         ncol = 2, nrow = 2, guides = "collect")

#ggsave("Figs/Week_covar_plots/week_covar_plots_model_fitting.pdf", plot = week_covar_plots, width = 8.5, height = 8.5, dpi = 600)

################################################################################
# Plots -- Peak season 
################################################################################
# LMA plot 
LMA_plot_peak = ggplot(peak_season_LMA_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.024, y = 0.155,
           label = paste0("R² = ", round(peak_season_LMA_df$R2, 2), "\n%RMSE = ", round(peak_season_LMA_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.92 - 1.01"),
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
EWT_plot_peak = ggplot(peak_season_EWT_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = 0.0061, y = 0.02475,
           label = paste0("R² = ", round(peak_season_EWT_df$R2, 2), "\n%RMSE = ", round(peak_season_EWT_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "0.92 - 1.05"),
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
C_plot_peak = ggplot(peak_season_C_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text",  x = 28.8, y = 63.4,
           label = paste0("R² = ", round(peak_season_C_df$R2, 2), "\n%RMSE = ", round(peak_season_C_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "-0.16 - 1.73"),
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
N_plot_peak = ggplot(peak_season_N_df, aes(x = Predicted, y = Measured, color = Species)) +
  geom_errorbarh(aes(xmin = Predicted - Pred_SD, xmax = Predicted + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text", x = -2.45, y = 6.78,
           label = paste0("R² = ", round(peak_season_N_df$R2, 2), "\n%RMSE = ", round(peak_season_N_df$perRMSE, 2),
                          "\nSlope 95% CI = ", "-0.07 - 0.65"),
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
    legend.position = "right",
    legend.text = element_text(size = 6, face = "italic"),
    legend.title = element_text(size = 8, face = "bold")
  )

# Combine Plots 2 x 2
peak_season_plots = patchwork::wrap_plots(LMA_plot_peak, EWT_plot_peak, C_plot_peak, N_plot_peak,
                                          ncol = 2, nrow = 2, guides = "collect")

#ggsave("Figs/Peak_season_plots/peak_season_plots_model_fitting.pdf", plot = peak_season_plots, width = 8.5, height = 8.5, dpi = 600)


################################################################
# Overall Combine plots for all models and traits 3 x 4
################################################################

combine_models_plots = patchwork::wrap_plots(LMA_plot_all, LMA_plot_week, LMA_plot_peak, 
                                             EWT_plot_all, EWT_plot_week, EWT_plot_peak,
                                             C_plot_all, C_plot_week, C_plot_peak,
                                             N_plot_all, N_plot_week, N_plot_peak,
                                             ncol = 3, nrow = 4, guides = "collect")

ggsave("Figs/Combine_model_fitting_plots.pdf", plot = combine_models_plots, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Combine_model_fitting_plots.png", plot = combine_models_plots, width = 8.5, height = 10.5, dpi = 600)


################################################
# Summary plots for trait evaluation
################################################
# Combine all metrics
all_season_metrics  = do.call(rbind, list(all_season_lma_metrics, all_season_ewt_metrics, 
                                          all_season_c_metrics, all_season_n_metrics))

week_covar_metrics  = do.call(rbind, list(week_covar_lma_metrics, week_covar_ewt_metrics, 
                                          week_covar_c_metrics, week_covar_n_metrics))

peak_season_metrics = do.call(rbind, list(peak_season_lma_metrics, peak_season_ewt_metrics, 
                                          peak_season_c_metrics, peak_season_n_metrics))

# Add trait column
all_season_metrics$Trait  = c("LMA", "EWT", "C", "N")
week_covar_metrics$Trait  = c("LMA", "EWT", "C", "N")
peak_season_metrics$Trait = c("LMA", "EWT", "C", "N")

# Arrange trait in order
all_season_metrics$Trait  = factor(all_season_metrics$Trait, levels = c("LMA", "EWT", "C", "N"))
week_covar_metrics$Trait  = factor(week_covar_metrics$Trait, levels = c("LMA", "EWT", "C", "N"))
peak_season_metrics$Trait = factor(peak_season_metrics$Trait, levels = c("LMA", "EWT", "C", "N"))

# Combine metrics
all_season_metrics$Model  = "All season"
week_covar_metrics$Model  = "Week as covariate"
peak_season_metrics$Model = "Peak season"

combine_metrics = bind_rows(all_season_metrics, week_covar_metrics, peak_season_metrics)

# Make Model a factor
combine_metrics$Model = factor(combine_metrics$Model, levels = c("All season", "Week as covariate", "Peak season"))

# Export metrics
write.csv(all_season_metrics, "Data/Processed/Coefficients_and_Results/FM_all_season/all_season_metrics.csv", row.names = F)
write.csv(week_covar_metrics, "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/week_as_covariate_metrics.csv", row.names = F)
write.csv(peak_season_metrics, "Data/Processed/Coefficients_and_Results/FM_peak_season/peak_season_metrics.csv", row.names = F)

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

ggsave("Figs/Model_fitting_summary_plots.pdf", plot = model_summary_plots, width = 8.5, height = 6.5, dpi = 600)
ggsave("Figs/Model_fitting_summary_plots.png", plot = model_summary_plots, width = 8.5, height = 6.5, dpi = 600)


########################### END! ################################################
