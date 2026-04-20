################################################################################
# Packages
################################################################################
library("ggplot2")
library("dplyr")
library("tidyr")
library("readr")

################################################################################
# Read data
################################################################################
# All season
####################
all_vip_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_all_season/vip_LMA.csv",
                  "EWT" = "Data/Processed/Coefficients_and_Results/FM_all_season/vip_EWT.csv",
                  "C"   = "Data/Processed/Coefficients_and_Results/FM_all_season/vip_C.csv",
                  "N"   = "Data/Processed/Coefficients_and_Results/FM_all_season/vip_N.csv")

#all_season_vip = lapply(all_vip_paths, read_csv)

all_season_vip = lapply(names(all_vip_paths), function(trait) {
  df = read.csv(all_vip_paths[[trait]])
  df$Trait = trait
  df
}) 

# Combine into a dataframe
all_season_vip_df = bind_rows(all_season_vip)

all_vip_long_df = all_season_vip_df %>% 
  pivot_longer(cols = matches("^[X0-9]+$"),
               names_to = "Wavelength",
               values_to = "VIP") %>%
  mutate(Wavelength = readr::parse_number(Wavelength),
         Trait = factor(Trait, levels = c("LMA", "EWT", "C", "N")))

# Summarise VIP values
all_vip_summary_df  = all_vip_long_df %>% 
  group_by(Trait, Wavelength) %>%
  summarise(
    mean_VIP = mean(VIP, na.rm = T),
    sd_VIP   = sd(VIP, na.rm = T),
    .groups = "drop")

##################
# Plot VIP
##################
all_vip_plot = ggplot(all_vip_summary_df, aes(x = Wavelength, y = mean_VIP)) +
  geom_line(color = "darkblue", linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.6) +
  #scale_color_brewer(palette = "Set1") +
  facet_wrap(~ Trait, scales = "free_y", ncol = 1) +
  labs(y = "VIP", x = "Wavelength (nm)", title = "All season"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    panel.background = element_blank()
  )


################################################################################
# Week as covariate
################################################################################

week_vip_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/vip_LMA.csv",
                  "EWT"  = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/vip_EWT.csv",
                  "C"    = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/vip_C.csv",
                  "N"    = "Data/Processed/Coefficients_and_Results/FM_week_as_covariate/vip_N.csv")

#week_covar_vip = lapply(week_vip_paths, read_csv)
week_covar_vip = lapply(names(week_vip_paths), function(trait) {
  df = read.csv(week_vip_paths[[trait]])
  df$Trait = trait
  df
}) 

# Combine into a dataframe
week_vip_df = bind_rows(week_covar_vip)

week_vip_long_df = week_vip_df %>% 
  pivot_longer(cols = matches("^[X0-9]+$"),
               names_to = "Wavelength",
               values_to = "VIP") %>%
  mutate(Wavelength = readr::parse_number(Wavelength),
         Trait = factor(Trait, levels = c("LMA", "EWT", "C", "N")))

# Summarise VIP values
week_vip_summary_df  = week_vip_long_df %>% 
  group_by(Trait, Wavelength) %>%
  summarise(
    mean_VIP = mean(VIP, na.rm = T),
    sd_VIP   = sd(VIP, na.rm = T),
    .groups = "drop")

##################
# Plot VIP
##################
week_vip_plot = ggplot(week_vip_summary_df, aes(x = Wavelength, y = mean_VIP)) +
  geom_line(color = "darkblue", linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.6) +
  facet_wrap(~ Trait, scales = "free_y", ncol = 1) +
  labs(y = "", x = "Wavelength (nm)", title = "Week as covariate"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    panel.background = element_blank()
  )


################################################################################
# Peak season
################################################################################

peak_vip_paths = c("LMA" = "Data/Processed/Coefficients_and_Results/FM_peak_season/vip_LMA.csv",
                  "EWT"  = "Data/Processed/Coefficients_and_Results/FM_peak_season/vip_EWT.csv",
                  "C"    = "Data/Processed/Coefficients_and_Results/FM_peak_season/vip_C.csv",
                  "N"    = "Data/Processed/Coefficients_and_Results/FM_peak_season/vip_N.csv")

#peak_season_vip = lapply(peak_vip_paths, read_csv)
peak_season_vip = lapply(names(peak_vip_paths), function(trait) {
  df = read.csv(peak_vip_paths[[trait]])
  df$Trait = trait
  df
}) 

# Combine into a dataframe
peak_season_vip_df = bind_rows(peak_season_vip)

peak_vip_long_df = peak_season_vip_df %>% 
  pivot_longer(cols = matches("^[X0-9]+$"),
               names_to = "Wavelength",
               values_to = "VIP") %>%
  mutate(Wavelength = readr::parse_number(Wavelength),
         Trait = factor(Trait, levels = c("LMA", "EWT", "C", "N")))

# Summarise VIP values
peak_vip_summary_df  = peak_vip_long_df %>% 
  group_by(Trait, Wavelength) %>%
  summarise(
    mean_VIP = mean(VIP, na.rm = T),
    sd_VIP   = sd(VIP, na.rm = T),
    .groups = "drop")

##################
# Plot VIP
##################
peak_vip_plot = ggplot(peak_vip_summary_df, aes(x = Wavelength, y = mean_VIP)) +
  geom_line(color = "darkblue", linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.6) +
  facet_wrap(~ Trait, scales = "free_y", ncol = 1) +
  labs(y = "", x = "Wavelength (nm)", title = "Peak season"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    panel.background = element_blank()
  )

############################
# Combine plots 1 x 3
############################
combine_vip_plot = patchwork::wrap_plots(all_vip_plot, week_vip_plot, peak_vip_plot,
                                     ncol = 3, guides = "collect")

ggsave("Figs/VIP_plots/combine_vip_plot.pdf", plot = combine_vip_plot, width = 8.5, height = 10.5, dpi = 600)
ggsave("Figs/VIP_plots/combine_vip_plot.png", plot = combine_vip_plot, width = 8.5, height = 10.5, dpi = 600)

################################################################################
