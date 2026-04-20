################################################################################
#                             Script README!
# This script contains all spectra related plots per Species per Week 
# and their corresponding coefficient of variation (CV).
################################################################################
# Package(s)
################################################################################
library("spectrolab")
library("tidyverse")
library("ggplot2")
library("dplyr")
library("tidyr")
library("purrr")
library("stringr")
library("readr")

################################################################################
# Import data
################################################################################
fresh_spec  = readRDS("Data/Processed/spectra_fresh.RDS")

##################
# Process spectra
##################
# Remove Narcissus pseudonarcissus
fresh_spec = lapply(fresh_spec, function(x){
  x[x$Species !="Narcissus pseudonarcissus", ] 
})

################################################################################
# Spectral changes over phenological stages
# Species mean reflectance spectra per season stage and overall CV 
################################################################################
# Step 1: Build long data & weekly means 
get_week_num = function(nm) readr::parse_number(nm)

spectral_long = purrr::imap_dfr(fresh_spec, ~{
  this_week = get_week_num(.y)
  .x %>%
    tidyr::pivot_longer(
      cols = matches("^\\d+$"),
      names_to = "Wavelength",
      values_to = "Reflectance"
    ) %>%
    mutate(Week = this_week,
           Wavelength = as.numeric(Wavelength))
})

spectral_long = spectral_long %>% select(Species, Week, Wavelength, Reflectance)

# Step 2: Mean spectrum per Species × Week × Wavelength
weekly_mean = spectral_long %>%
  group_by(Species, Week, Wavelength) %>%
  summarise(Reflectance_mean = mean(Reflectance, na.rm = TRUE), .groups = "drop")

# Step 3: CV spectrum (sd/mean across weeks at each wavelength)
cv_spectrum = weekly_mean %>%
  group_by(Species, Wavelength) %>%
  summarise(
    CV = sd(Reflectance_mean, na.rm = TRUE) / mean(Reflectance_mean, na.rm = TRUE),
    .groups = "drop"
  )

###########################################################
# Fig. 1: (A) Mean spectra for select 2 species
# "Acer platanoides", "Rhododendron maximum"
# To see the changes in all 7 species in SI1, 
# comment out line 71 and modify line 74
###########################################################
# Step 4: Select weeks and relabel as "season" 
wk_keep  = c(1, 11, 23)
spp_keep = c("Acer platanoides", "Rhododendron maximum")

plot_means = weekly_mean %>%
  filter(Week %in% wk_keep, Species %in% spp_keep) %>%
  mutate(
    season = dplyr::recode(as.character(Week),
                           `1`  = "early",
                           `11` = "peak",
                           `23` = "late"),
    season = factor(season, levels = c("early", "peak", "late"))
  )

# Color season
season_cols = c(early = "lightgreen", peak  = "darkgreen", late  = "darkorange3")

# Step 5: Spectra Plot
mean_spp_spec = ggplot(plot_means,
            aes(x = Wavelength, y = Reflectance_mean, color = season, group = season)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ Species, scales = "fixed", ncol = 3) +
  scale_color_manual(values = season_cols,
                     labels = c("early season", "peak season", "late season")) + # legend title
  labs(x = "Wavelength (nm)", y = "Mean reflectance", color = NULL) +
  theme_classic(base_size = 11) +
  theme(
    legend.position   = "right",
    strip.text        = element_text(face = "italic"), # italicize species names
    #strip.background  = element_blank(),       # remove frame behind species names
    panel.grid = element_line(color = "gray90", size = 0.5),
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

ggsave("Figs/Spectra_plots/7Species_spectra_plots.pdf", plot = mean_spp_spec, width = 8, height = 8.5, dpi = 600)


########################################################
# Fig. 1: (B) CV spectra for all species
########################################################

# Define colors
species_colors = c("Acer platanoides" = "navyblue", "Acer rubrum" = "slategray4", "Betula papyrifera" = "maroon2", 
  "Prunus nigra" = "darkorange3", "Quercus rubra" = "darkorchid3", "Rhododendron catawbiense" = "darkred", 
  "Rhododendron maximum" = "green4")

# Step 5: Spectra Plot
cv_plot = ggplot(cv_spectrum, aes(x = Wavelength, y = CV, color = Species)) +
  geom_line(linewidth = 0.8) +
  #facet_wrap(~ Species, scales = "free_y", ncol = 3) +
  scale_color_manual(values = species_colors) + 
  labs(x = "Wavelength (nm)", y = "Coefficient of variation (CV)") +
  theme_classic(base_size = 11) +
  theme(
    legend.position   = "right",
    strip.text        = element_text(face = "italic"), # italicize species names
    panel.grid.major  = element_line(color = "grey80"),  # remove frame behind species names
    # keep a frame around the plot panels:
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.6),
    legend.text = element_text(size = 7, face = "italic"),
    legend.title = element_text(size = 8, face = "bold")
  )

ggsave("Figs/Spectra_plots/CV_plots.pdf", plot = cv_plot, width = 8, height = 5, dpi = 600)

##############################################################################################