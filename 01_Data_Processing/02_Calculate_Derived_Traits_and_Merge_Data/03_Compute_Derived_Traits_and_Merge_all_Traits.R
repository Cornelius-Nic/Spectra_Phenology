##############################################################
# Packages
##############################################################
library("readr")

##############################################################
# Import data  
# QY, fresh_dry_weight, leaf_area_data, carbon & nitrogen
##############################################################

fresh_dry_weight = readr::read_csv("Data/Processed/fresh_dry_weight_single_leaf.csv")
qy_fluorpen      = readr::read_csv("Data/Processed/qy_fluorpen.csv")
leaf_area_data   = readr::read_csv("Data/Processed/leaf_area_data_single_leaf.csv")
carbon_nitrogen  = readr::read_csv("Data/Processed/carbon_nitrogen.csv")


# Assemble the data frames into a list
merge_list  = list(fresh_dry_weight, qy_fluorpen, leaf_area_data, carbon_nitrogen)

# Merge all the data frames into one
merge_data  = Reduce(function(x,y) merge(x,y, all = T), merge_list)

# Rearrange the data frame
# To follow an orderly weekly arrangement
merge_data  = merge_data[order(merge_data$Week), ]

################################################################################
# Unit Conversion
################################################################################
# Dry weight (g to kg)
merge_data$Dry_Weight_kg = merge_data$Dry_Weight / 1000

# Dry Weight (g to mg)
merge_data$Dry_Weight_mg = merge_data$Dry_Weight * 1000

################################################################################
# Derived traits
################################################################################
##########################
# Calculate LMA (kg/m^2)
##########################
merge_data$LMA = merge_data$Dry_Weight_kg / merge_data$Leaf_Area_m2

##########################
# Calculate EWT (g/cm^2)
##########################
merge_data$EWT = (merge_data$Fresh_Weight - merge_data$Dry_Weight) / merge_data$Leaf_Area_cm2

##########################
# Calculate LDMC (mg/g)
##########################
merge_data$LDMC = merge_data$Dry_Weight_mg / merge_data$Fresh_Weight

####################
# Calculate MC (%)
####################
merge_data$MC = ((merge_data$Fresh_Weight - merge_data$Dry_Weight) / merge_data$Fresh_Weight) * 100

# Save/Export the merged traits
readr::write_csv(merge_data, "Data/Processed/merged_traits.csv")

################################################################################
# NOTE:
# LMA is the ratio of leaf dry weight (DW) and leaf area (LA)
# Mathematically, LMA = DW/LA; unit = kg/m2
# The unit was changed from g/cm2 to kg/m2 for uniformity with existing models and databases

################################################################################
