################################################################################
# Packages
################################################################################
library("ggplot2")
library("dplyr")
library("tidyr")

###############################
# Import data
###############################
measured_traits  = readRDS("Data/Processed/fresh_spec_traits.RDS")

###############################
# Process data
###############################
# Add Weeks column to correspond to the time of the year
measured_traits$Weeks = measured_traits$Week + 21

# Define colors for species
species_colors = c("Acer platanoides" = "navyblue", "Acer rubrum" = "slategray4", 
                   "Betula papyrifera" = "maroon2", "Prunus nigra" = "darkorange3", 
                   "Quercus rubra" = "darkorchid3", "Rhododendron catawbiense" = "darkred", 
                   "Rhododendron maximum" = "green4")

#################
# Plots - Fig. 2
#################
# Leaf dry mass per area
LMA_plot = ggplot(measured_traits, aes(x = Weeks, y = LMA, color = Species, fill = Species)) +
  geom_smooth(method = "loess", se = T, lwd = 1, level = 0.95, alpha = 0.2) +
  #facet_wrap(~ Species, ncol = 4, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  labs(x = "", 
    y = expression("LMA (kg m"^-2*")")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic"),
    legend.position = "right",
    legend.text = element_text(size = 6, face = "italic"),
    legend.title = element_text(size = 8, face = "bold")
  )


# Equivalent Water Thickness
EWT_plot = ggplot(measured_traits, aes(x = Weeks, y = EWT, color = Species, fill = Species)) +
  geom_smooth(method = "loess", se = T, lwd = 1, level = 0.95, alpha = 0.2) +
  #facet_wrap(~ Species, ncol = 4, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  labs(x = "", 
       y = expression("EWT (g cm"^-2*")")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic"),
    legend.position = "right",
    legend.text = element_text(size = 6, face = "italic"),
    legend.title = element_text(size = 8, face = "bold")
  )


# Carbon
C_plot = ggplot(measured_traits, aes(x = Weeks, y = C, color = Species, fill = Species)) +
  geom_smooth(method = "loess", se = T, lwd = 1, level = 0.95, alpha = 0.2) +
  #facet_wrap(~ Species, ncol = 4, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", 
    y = expression("Carbon (%)")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic"),
    legend.position = "right",
    legend.text = element_text(size = 6, face = "italic"),
    legend.title = element_text(size = 8, face = "bold")
  )


# Nitrogen
N_plot = ggplot(measured_traits, aes(x = Weeks, y = N, color = Species, fill = Species)) +
  geom_smooth(method = "loess", se = T, lwd = 1, level = 0.95, alpha = 0.2) +
  #facet_wrap(~ Species, ncol = 4, scales = "free_y") +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  labs(x = "Week of Year (WOY)", 
    y = expression("Nitrogen (%)")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "italic"),
    legend.position = "right",
    legend.text = element_text(size = 6, face = "italic"),
    legend.title = element_text(size = 8, face = "bold")
  )

# Combine plots 2 x 2
combine_plot = patchwork::wrap_plots(LMA_plot, EWT_plot, C_plot, N_plot,
                                     ncol = 2, nrow = 2, guides = "collect")


ggsave("Figs/Combine_measured_traits.pdf", plot = combine_plot, width = 8, height = 6.5, dpi = 600)
ggsave("Figs/Combine_measured_traits.png", plot = combine_plot, width = 8, height = 6.5, dpi = 600)


############################## END #############################################

################################################################################
