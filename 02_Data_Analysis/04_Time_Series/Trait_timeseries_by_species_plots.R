################################################################################
# Packages
################################################################################
library("ggplot2")
library("dplyr")
library("tidyr")

source("R/Utils/important_functions.R")

################################################################################
# Import data
################################################################################
fresh_spec_traits   = readRDS("Data/Processed/fresh_spec_traits.RDS")
all_season_traits   = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_all_season.csv")
week_covar_traits   = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_week_as_covariate.csv")
peak_season_traits  = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_peak_season.csv")
kothari_traits      = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_Kothari_2023.csv")

###############################
# Process data
###############################
# Add Weeks column to correspond to the time of the year
fresh_spec_traits$Weeks  = fresh_spec_traits$Week + 21
all_season_traits$Weeks  = all_season_traits$Week + 21
week_covar_traits$Weeks  = week_covar_traits$Week + 21
peak_season_traits$Weeks = peak_season_traits$Week + 21
kothari_traits$Weeks     = kothari_traits$Week + 21


# Split traits by Species
fresh_spec_traits   = split(fresh_spec_traits, fresh_spec_traits$Species)
all_season_traits   = split(all_season_traits, all_season_traits$Species)
peak_season_traits  = split(peak_season_traits, peak_season_traits$Species)
kothari_traits      = split(kothari_traits, kothari_traits$Species)


################################################################################
# Differences in trait predictions 
# Measured traits vs CON Models vs Kothari Models
################################################################################
# Combine all data sets
# Add source label to each
measured_traits_df = bind_rows(fresh_spec_traits, .id = "Species") %>%
  mutate(Source = "Measured traits")

kothari_pred_df = bind_rows(kothari_traits, .id = "Species") %>%
  mutate(Source = "Kothari 2023")

con_pred_all_df = bind_rows(all_season_traits, .id = "Species") %>%
  mutate(Source = "All season")

con_pred_peak_df = bind_rows(peak_season_traits, .id = "Species") %>%
  mutate(Source = "Peak season")

# Combine all into one long dataframe
combined_df = bind_rows(measured_traits_df, kothari_pred_df, con_pred_all_df, con_pred_peak_df)

# Ensure Week is numeric
combined_df$Week = as.numeric(as.character(combined_df$Week))

###############################
# Plots
###############################
# LMA
LMA_plot = ggplot(combined_df, aes(x = Weeks, y = LMA, color = Source, fill = Source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Measured traits" = "black", 
                                "Kothari 2023" = "darkorange3", 
                                "All season" = "forestgreen",
  #                              "Week as covariate" = "darkorchid3",
                                "Peak season" = "darkblue"),
  breaks = c("Measured traits", "All season", "Peak season", "Kothari 2023")) +
  scale_fill_manual(values = c("Measured traits" = "black", 
                               "Kothari 2023" = "darkorange3", 
                               "All season" = "forestgreen",
  #                             "Week as covariate" = "darkorchid3",
                               "Peak season" = "darkblue"),
  breaks = c("Measured traits", "All season", "Peak season", "Kothari 2023")) +
  labs(x = "Week of Year (WOY)", y = expression("LMA (kg m"^-2*")"), title = "LMA") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 7),
    legend.position = ""
  )


# EWT
EWT_plot = ggplot(combined_df, aes(x = Weeks, y = EWT, color = Source, fill = Source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Measured traits" = "black", 
                                "Kothari 2023" = "darkorange3", 
                                "All season" = "forestgreen",
   #                             "Week as covariate" = "darkorchid3",
                                "Peak season" = "darkblue"),
  breaks = c("Measured traits", "All season", "Peak season", "Kothari 2023")) +
  scale_fill_manual(values = c("Measured traits" = "black", 
                               "Kothari 2023" = "darkorange3", 
                               "All season" = "forestgreen",
 #                             "Week as covariate" = "darkorchid3",
                               "Peak season" = "darkblue"),
 breaks = c("Measured traits", "All season", "Peak season", "Kothari 2023")) +
  labs(x = "Week of Year (WOY)", y = expression(EWT~(g~cm^{-2})), title = "EWT") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 7),
    legend.position = "right"
  )


# Carbon
C_plot = ggplot(combined_df, aes(x = Weeks, y = C, color = Source, fill = Source)) +
  geom_smooth(method = "loess", se = TRUE, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Measured traits" = "black", 
                                "Kothari 2023" = "darkorange3", 
                                "All season" = "forestgreen",
  #                              "Week as covariate" = "darkorchid3",
                                "Peak season" = "darkblue"),
  breaks = c("Measured traits", "All season", "Peak season", "Kothari 2023")) +
  scale_fill_manual(values = c("Measured traits" = "black", 
                               "Kothari 2023" = "darkorange3", 
                               "All season" = "forestgreen",
  #                             "Week as covariate" = "darkorchid3",
                               "Peak season" = "darkblue"),
  breaks = c("Measured traits", "All season", "Peak season", "Kothari 2023")) +
  labs(x = "Week of Year (WOY)", y = "Carbon (%)", title = "Carbon") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 7),
    legend.position = ""
  )


# Nitrogen
N_plot = ggplot(combined_df, aes(x = Weeks, y = N, color = Source, fill = Source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Measured traits" = "black", 
                                "Kothari 2023" = "darkorange3", 
                                "All season" = "forestgreen",
  #                              "Week as covariate" = "darkorchid3",
                                "Peak season" = "darkblue"),
  breaks = c("Measured traits", "All season", "Peak season", "Kothari 2023")) +
  scale_fill_manual(values = c("Measured traits" = "black", 
                               "Kothari 2023" = "darkorange3", 
                               "All season" = "forestgreen",
  #                             "Week as covariate" = "darkorchid3",
                               "Peak season" = "darkblue"),
  breaks = c("Measured traits", "All season", "Peak season", "Kothari 2023")) +
  labs(x = "Week of Year (WOY)", y = "Nitrogen (%)", title = "Nitrogen") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 7),
    legend.position = "right"
  )

# Combine plots 0 x 4
combine_plot2 = patchwork::wrap_plots(LMA_plot, EWT_plot, C_plot, N_plot, 
                                     ncol = 4, guides = "collect") +
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("Figs/Timeseries_plots/Timeseries_LMA_EWT_C_and_N_plots.pdf", plot = combine_plot2, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Timeseries_plots/Timeseries_LMA_EWT_C_and_N_plots.png", plot = combine_plot2, width = 8.5, height = 10.5, dpi = 600)


################################################################################
# Week as covariate model
# We plotted this model separately because its trait prediction are similar to all season model
# Please see Supplementary Information of Nichodemus and Meireles (xxxx) New Phytologist!
################################################################################
# Define colors
species_colors = c("Acer platanoides" = "navyblue", "Acer rubrum" = "slategray4", 
                   "Betula papyrifera" = "maroon2", "Prunus nigra" = "darkorange3", 
                   "Quercus rubra" = "darkorchid3", "Rhododendron catawbiense" = "darkred", 
                   "Rhododendron maximum" = "green4")

# LMA
week_LMA_plot = ggplot(week_covar_traits, aes(x = Weeks, y = LMA, color = Species)) +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = expression("LMA (kg m"^-2*")"), title = "LMA") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 7),
    legend.position = "none"
  )

# EWT
week_EWT_plot = ggplot(week_covar_traits, aes(x = Weeks, y = EWT, color = Species)) +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = expression(EWT~(g~cm^{-2})), title = "EWT") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 7),
    legend.position = "none"
  )

# Carbon
week_C_plot = ggplot(week_covar_traits, aes(x = Weeks, y = C, color = Species)) +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Carbon (%)", title = "Carbon") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 7),
    legend.position = "none"
  )

# Nitrogen
week_N_plot = ggplot(week_covar_traits, aes(x = Weeks, y = N, color = Species)) +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Nitrogen (%)", title = "Nitrogen") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 7),
    legend.position = "none"
  )


combine_plot_S8 = patchwork::wrap_plots(week_LMA_plot, week_EWT_plot, week_C_plot, week_N_plot, 
                                      ncol = 4, guides = "collect") +
  patchwork::plot_layout(guides = "collect") #& theme(legend.position = "bottom")

ggsave("Figs/Timeseries_plots/Timeseries_week_as_covariate_plots.pdf", plot = combine_plot_S8, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Timeseries_plots/Timeseries_week_as_covariate_plots.png", plot = combine_plot_S8, width = 8.5, height = 10.5, dpi = 600)


################################################################################
# Plots for other traits from Kothari et al. (2023)
# See Supplementary Information of Nichodemus and Meireles (xxxx) New Phytologist!
################################################################################
kothari_traits_df  = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_Kothari_2023.csv")

# Add Weeks column to correspond to the time of the year
kothari_traits_df$Weeks = kothari_traits_df$Week + 21

##########################
# Total Chlorophyll
##########################
kothari_traits_df$Total_Chl = kothari_traits_df$ChlA + kothari_traits_df$ChlB

###################################################
# Fig.5: Plots for select species
###################################################
acer_rubrum_df   = subset(kothari_traits_df, Species == "Acer rubrum")
quercus_rubra_df = subset(kothari_traits_df, Species == "Quercus rubra")

acer_rub_plot = ggplot(acer_rubrum_df, aes(x = Weeks, y = Total_Chl, color = Species)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Total Chlorophyll (mg/g)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "italic", size = 10),
    legend.position = "none"
  )


quercus_rub_plot = ggplot(quercus_rubra_df, aes(x = Weeks, y = Sol, color = Species)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Soluble fractions (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "italic", size = 10),
    legend.position = "none"
  )


combine_fig5 = patchwork::wrap_plots(acer_rub_plot, quercus_rub_plot,
                                      ncol = 2, guides = "collect") #+
 # patchwork::plot_layout(guides = "collect") #& theme(legend.position = "bottom")

ggsave("Figs/Timeseries_plots/Timeseries_acer_quercus_plots.pdf", plot = combine_fig5, width = 8, height = 5, dpi = 600)
ggsave("Figs/Timeseries_plots/Timeseries_acer_quercus_plots.png", plot = combine_fig5, width = 8, height = 5, dpi = 600)


###########################################################
# Supplementary Plots for all 7 species and traits
###########################################################
#################################################
# Total Chlorophyll, Carotenoids, and Cellulose
#################################################
# Total Chlorophyll
Total_Chl_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = Total_Chl, color = Species)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Total Chlorophyll (mg/g)",
       title = "Total Chlorophyll") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )


# Carot
Carot_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = Carot, color = Species)) +
  #geom_point(alpha = 0.4, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Carotenoids (mg/g)",
       title = "Carotenoids") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )


# Cellulose
Cell_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = Cell, color = Species)) +
  #geom_point(alpha = 0.4, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  #geom_hline(yintercept = 100, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Cellulose (%)",
       title = "Cellulose") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )


combine_plot_S11 = patchwork::wrap_plots(Total_Chl_plot, Carot_plot, Cell_plot, 
                                      ncol = 3, guides = "collect") +
  patchwork::plot_layout(guides = "collect") #& theme(legend.position = "bottom")

ggsave("Figs/Timeseries_plots/Timeseries_TChl_Carot_and_Cell_plots.pdf", plot = combine_plot_S11, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Timeseries_plots/Timeseries_TChl_Carot_and_Cell_plots.png", plot = combine_plot_S11, width = 8.5, height = 10.5, dpi = 600)


#################################################
# LDMC, Lignin, and Soluble fractions
#################################################
# LDMC
LDMC_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = LDMC, color = Species)) +
  #geom_point(alpha = 0.4, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "LDMC (mg/g)",
       title = "Leaf Dry Matter Content") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )

# Lignin
Lignin_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = Lignin, color = Species)) +
  #geom_point(alpha = 0.4, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Lignin (%)",
       title = "Lignin") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )

# Soluble fractions
Soluble_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = Sol, color = Species)) +
  #geom_point(alpha = 0.4, size = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Soluble fractions (%)",
       title = "Soluble fractions") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )


combine_plot_S10 = patchwork::wrap_plots(LDMC_plot, Lignin_plot, Soluble_plot, 
                                      ncol = 3, guides = "collect") +
  patchwork::plot_layout(guides = "collect") #& theme(legend.position = "bottom")

ggsave("Figs/Timeseries_plots/Timeseries_LDMC_Lignin_Soluble_plots.pdf", plot = combine_plot_S10, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Timeseries_plots/Timeseries_LDMC_Lignin_Soluble_plots.png", plot = combine_plot_S10, width = 8.5, height = 10.5, dpi = 600)


#################################################
# Phosphorus, Potassium, and Hemicellulose
#################################################
# Phosphorus - P
P_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = P, color = Species)) +
  #geom_point(alpha = 0.4, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Phosphorus (mg/g)",
       title = "Phosphorus") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )

# Potassium - K
K_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = K, color = Species)) +
  #geom_point(alpha = 0.4, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Potassium (mg/g)",
       title = "Potassium") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )

# Hemicellulose - Hemic
Hemic_plot = ggplot(kothari_traits_df, aes(x = Weeks, y = Hemic, color = Species)) +
  #geom_point(alpha = 0.4, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  #geom_hline(yintercept = 100, linetype = "dashed", color = "darkred") +
  geom_smooth(method = "loess", se = T, lwd = 1, alpha = 0.2) +
  facet_wrap(~ Species, ncol = 1, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", y = "Hemicellulose (%)",
       title = "Hemicellulose") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic", size = 8),
    legend.position = "none"
  )


combine_plot_S12 = patchwork::wrap_plots(P_plot, K_plot, Hemic_plot, 
                                         ncol = 3, guides = "collect") +
  patchwork::plot_layout(guides = "collect") #& theme(legend.position = "bottom")

ggsave("Figs/Timeseries_plots/Timeseries_P_K_Hemic_plots.pdf", plot = combine_plot_S12, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/Timeseries_plots/Timeseries_P_K_Hemic_plots.png", plot = combine_plot_S12, width = 8.5, height = 10.5, dpi = 600)


################################################################################

############# END ############### END ################ END #####################

################################################################################
