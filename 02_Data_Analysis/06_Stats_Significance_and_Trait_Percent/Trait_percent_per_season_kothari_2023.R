!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#                                 SCRIPT README!
# POST HOC ANALYSIS TO ASSESS THE VALIDITY OF ONE PHENOPHASE MODELS IN 
# THE ABSENCE OF DIRECT MEASUREMENTS
# We used a widely accepted model - Kothari et al. (2023) New Phytologist!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

################################################################################
# Packages
################################################################################
library("ggplot2")
library("dplyr")
library("tidyr")

source("R/Utils/important_functions.R")

###############################
# Import data
###############################
kothari_traits  = read.csv("Data/Processed/Trait_Predictions/trait_prediction_fresh_spectra_Kothari_2023.csv")

######################################
# Process data - Confusion Matrix
######################################
# Add Weeks column to correspond to the time of the year
#kothari_traits$Weeks = kothari_traits$Week + 21

##########################
# Total Chlorophyll
##########################
kothari_traits$Total_Chl = kothari_traits$ChlA + kothari_traits$ChlB

# Traits
traits = c("LDMC", "Total_Chl", "Carot", "Sol", "Lignin", "Cell", "P", "K", "Hemic")

######################################
# Scoring function
######################################
score_traits = function(df) {
  df %>%
    mutate(across(all_of(traits), ~ {
      colname <- cur_column()
      
      # Rule for trait values < 0
      if (colname %in% c("LDMC", "Total_Chl", "Carot", "P", "K", "Hemic")) {
        return(ifelse(.x < 0, 0, 1))
      }
      
      # Rule for trait values > 100
      if (colname %in% c("Sol", "Lignin", "Cell")) {
        return(ifelse(.x > 100 | .x < 0, 0, 1))
      }
      
      # Otherwise leave as NA
      return(NA)
    }))
}

######################################################
# Create trait percentage per season
######################################################
get_reasonable_percent_traits = function(df){
  scored = score_traits(df)
  wide = scored %>% select(all_of(traits))
  colSums(wide) / nrow(wide)
}

#####################################################################
# Split data into seasons
# PS. Weeks 1, 11, 23 = Weeks 22, 32, and 44 in Calendar year
#####################################################################
early_df = kothari_traits %>% filter(Week == 1)
peak_df  = kothari_traits %>% filter(Week == 11)
late_df  = kothari_traits %>% filter(Week == 23)

#########################################
# Create three Confusion matrices
#########################################
conf_early = get_reasonable_percent_traits(early_df)
conf_peak  = get_reasonable_percent_traits(peak_df)
conf_late  = get_reasonable_percent_traits(late_df)

#########################################
# Convert named vectors to dataframes
# Select only peak and late season
#########################################
conf_peak_df = data.frame(
  Trait = names(conf_peak),
  Match = as.numeric(conf_peak),
  Season = "Peak season"
)

conf_late_df = data.frame(
  Trait = names(conf_late),
  Match = as.numeric(conf_late),
  Season = "Late season"
)

#####################################
# Combine for plotting
#####################################
conf_all = bind_rows(conf_peak_df, conf_late_df)

conf_all$Season = factor(conf_all$Season,
                         levels = c("Peak season", "Late season"))

#####################################
# Column Plot
#####################################
column_plot = ggplot(conf_all, aes(x = Trait, y = Match, fill = Season)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  labs(x = "Traits", y = "Percentage of reasonable estimates", fill = "Season"
  ) +
  scale_fill_manual(values = c("Peak season" = "blue",
                               "Late season" = "firebrick")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.7),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "right"
  )

ggsave("Figs/Percent_Column_plot.pdf", plot = column_plot, width = 8, height = 5, dpi = 600)

################################################################################

