################################################################################
#                               Script README!
# This script evaluates model predictions on observed traits by week
# Models: all season, week as covariate, peak season, and Kothari et al. 2023
################################################################################
# Packages
################################################################################
library("dplyr")
library("ggplot2")
library("patchwork")

################################################################################
# Import data
################################################################################

all_season_lma = read.csv("Data/Processed/Trait_Predictions/all_season_lma.csv")
all_season_ewt = read.csv("Data/Processed/Trait_Predictions/all_season_ewt.csv")
all_season_c   = read.csv("Data/Processed/Trait_Predictions/all_season_c.csv")
all_season_n   = read.csv("Data/Processed/Trait_Predictions/all_season_n.csv")

week_covar_lma = read.csv("Data/Processed/Trait_Predictions/week_as_covariate_lma.csv")
week_covar_ewt = read.csv("Data/Processed/Trait_Predictions/week_as_covariate_ewt.csv")
week_covar_c   = read.csv("Data/Processed/Trait_Predictions/week_as_covariate_c.csv")
week_covar_n   = read.csv("Data/Processed/Trait_Predictions/week_as_covariate_n.csv")

peak_season_lma = read.csv("Data/Processed/Trait_Predictions/peak_season_lma.csv")
peak_season_ewt = read.csv("Data/Processed/Trait_Predictions/peak_season_ewt.csv")
peak_season_c   = read.csv("Data/Processed/Trait_Predictions/peak_season_c.csv")
peak_season_n   = read.csv("Data/Processed/Trait_Predictions/peak_season_n.csv")

kothari_lma   = read.csv("Data/Processed/Trait_Predictions/kothari_lma.csv")
kothari_ewt   = read.csv("Data/Processed/Trait_Predictions/kothari_ewt.csv")
kothari_c     = read.csv("Data/Processed/Trait_Predictions/kothari_c.csv")
kothari_n     = read.csv("Data/Processed/Trait_Predictions/kothari_n.csv")


#################################################################
# Add Week_Cal column to correspond to the time of the year
#################################################################
all_season_lma$Weeks = all_season_lma$Week + 21
all_season_ewt$Weeks = all_season_ewt$Week + 21
all_season_c$Weeks   = all_season_c$Week + 21
all_season_n$Weeks   = all_season_n$Week + 21

week_covar_lma$Weeks = week_covar_lma$Week + 21
week_covar_ewt$Weeks = week_covar_ewt$Week + 21
week_covar_c$Weeks   = week_covar_c$Week + 21
week_covar_n$Weeks   = week_covar_n$Week + 21

peak_season_lma$Weeks = peak_season_lma$Week + 21
peak_season_ewt$Weeks = peak_season_ewt$Week + 21
peak_season_c$Weeks   = peak_season_c$Week + 21
peak_season_n$Weeks   = peak_season_n$Week + 21

kothari_lma$Weeks = kothari_lma$Week + 21
kothari_ewt$Weeks = kothari_ewt$Week + 21
kothari_c$Weeks   = kothari_c$Week + 21
kothari_n$Weeks   = kothari_n$Week + 21

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


################################################################################
# Plots -- All season 
################################################################################
# LMA plot 
LMA_plot_all = ggplot(all_season_lma, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = all_season_lma_range, ylim = all_season_lma_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# EWT plot
EWT_plot_all = ggplot(all_season_ewt, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = all_season_ewt_range, ylim = all_season_ewt_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 10, face = "bold")
  )


# C plot
C_plot_all = ggplot(all_season_c, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = all_season_c_range, ylim = all_season_c_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# N plot
N_plot_all = ggplot(all_season_n, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = all_season_n_range, ylim = all_season_n_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Combine Plots 2 x 2
#all_season_plots = patchwork::wrap_plots(LMA_plot_all, EWT_plot_all, C_plot_all, N_plot_all,
#                                         ncol = 2, nrow = 2, guides = "collect")


################################################################################
# Plots -- Peak season 
################################################################################
# LMA plot 
LMA_plot_peak = ggplot(peak_season_lma, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = peak_season_lma_range, ylim = peak_season_lma_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# EWT plot
EWT_plot_peak = ggplot(peak_season_ewt, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = peak_season_ewt_range, ylim = peak_season_ewt_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 10, face = "bold")
  )


# C plot
C_plot_peak = ggplot(peak_season_c, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = peak_season_c_range, ylim = peak_season_c_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# N plot
N_plot_peak = ggplot(peak_season_n, aes(x = Pred_mean, y = Measured, color = Weeks)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text",  x = -17, y = 52,
           label = paste0("R² = ", round(peak_season_n$R2, 2), "\n%RMSE = ", round(peak_season_n$perRMSE, 2),
                          "\nSlope 95% CI = ", "-0.09 - -0.05"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted N (%)"),
    y = expression("Measured N (%)")
  ) +
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = peak_season_n_range, ylim = peak_season_n_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# Combine Plots 2 x 2
#peak_season_plots = patchwork::wrap_plots(LMA_plot_peak, EWT_plot_peak, C_plot_peak, N_plot_peak,
#                                          ncol = 2, nrow = 2, guides = "collect")


################################################################################
# Plots -- Kothari et al. 2023 
################################################################################
# LMA plot 
LMA_plot_koth = ggplot(kothari_lma, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = kothari_lma_range, ylim = kothari_lma_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# EWT plot 
EWT_plot_koth = ggplot(kothari_ewt, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = kothari_ewt_range, ylim = kothari_ewt_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# C plot
C_plot_koth = ggplot(kothari_c, aes(x = Pred_mean, y = Measured, color = Weeks)) +
  geom_errorbarh(aes(xmin = Pred_mean - Pred_SD, xmax = Pred_mean + Pred_SD),
                 height = 0, alpha = 0.3) +
  geom_point(size = 0.6, alpha = 0.7) +  # Points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 0.8) +  # Regression line
  annotate("text",  x = 24.5, y = 81.5,
           label = paste0("R² = ", round(kothari_c$R2, 2), "\n%RMSE = ", round(kothari_c$perRMSE, 2),
                          "\nSlope 95% CI = ", "-0.35 - 0.62"),
           hjust = 0, vjust = 1, size = 2.5) +
  labs(
    x = expression("Predicted C (%)"),
    y = expression("Measured C (%)")
  ) +
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = kothari_c_range, ylim = kothari_c_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 10, face = "bold")
  )


# N plot
N_plot_koth = ggplot(kothari_n, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = kothari_n_range, ylim = kothari_n_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Combine Plots 2 x 2
#kothari_plots = patchwork::wrap_plots(LMA_plot_koth, EWT_plot_koth, C_plot_koth, N_plot_koth,
#                                      ncol = 2, nrow = 2, guides = "collect")


################################################################################
# Combine model assessment into one big plot - Fig. 4
## This plot does not include Week as covariate model because it is similar to all season model.
## Check Supplementary material for "Week as covariate model".
################################################################################
# Combine Plots 4 x 3

combine_plots = patchwork::wrap_plots(LMA_plot_all, LMA_plot_peak, LMA_plot_koth,
                                      EWT_plot_all, EWT_plot_peak, EWT_plot_koth,
                                      C_plot_all, C_plot_peak, C_plot_koth,
                                      N_plot_all, N_plot_peak, N_plot_koth,
                                      ncol = 3, nrow = 4, guides = "collect")


# Save plot
ggsave("Figs/Overall_week_assessment_plots.pdf", plot = combine_plots, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Overall_week_assessment_plots.png", plot = combine_plots, width = 8.5, height = 10.5, dpi = 600)


################################################################################
# Plots -- Week as covariate
# See Supplementary Information of Nichodemus and Meireles (....) New Phytologist!
################################################################################
# LMA plot 
LMA_plot_week = ggplot(week_covar_lma, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = week_covar_lma_range, ylim = week_covar_lma_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# EWT plot
EWT_plot_week = ggplot(week_covar_ewt, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = week_covar_ewt_range, ylim = week_covar_ewt_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8, face = "italic"),
    legend.title = element_text(size = 10, face = "bold")
  )


# C plot
C_plot_week = ggplot(week_covar_c, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = week_covar_c_range, ylim = week_covar_c_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


# N plot
N_plot_week = ggplot(week_covar_n, aes(x = Pred_mean, y = Measured, color = Weeks)) +
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
  scale_color_gradientn(colors = c("turquoise3", "darkgreen", "darkgreen", "darkorange3"), limits = c(22,45)) +
  coord_equal(xlim = week_covar_n_range, ylim = week_covar_n_range, expand = T) +
  theme_minimal(base_size = 8) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Combine Plots 2 x 2
week_plots = patchwork::wrap_plots(LMA_plot_week, EWT_plot_week, C_plot_week, N_plot_week,
                                   ncol = 2, nrow = 2, guides = "collect")

# Save plot
ggsave("Figs/Week_as_covariate_plots/week_pred_by_week_assessments_plots.pdf", plot = week_plots, width = 8, height = 6, dpi = 600)
ggsave("Figs/Week_as_covariate_plots/week_pred_by_week_assessments_plots.png", plot = week_plots, width = 8, height = 6, dpi = 600)

