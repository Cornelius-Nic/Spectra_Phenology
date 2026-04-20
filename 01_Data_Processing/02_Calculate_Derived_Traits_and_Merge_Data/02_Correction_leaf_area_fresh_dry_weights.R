# Script READ ME!
# All calculated traits (LMA and MC) are on a 2 leaf basis except Ind 22 in Wk 1.
# This script tend to correct and create uniformity in generated values to meet
# global trait databases standards

##############################################################
# Packages
##############################################################
library("readr")

##############################################################
# Import data: fresh_dry_weight & leaf_area_data
##############################################################
fresh_dry_weight = readr::read_csv("Data/Processed/fresh_dry_weight.csv")
leaf_area_data   = readr::read_csv("Data/Processed/leaf_area_data.csv")


# Individual 22 in Wk 1 had only 1 leaf (it was lost during measurement)
# To correct this discrepancy in data and create uniformity, 
# we doubled the leaf area value for only Ind 22 in Week 1.

# Select affected rows
pick_row = which(leaf_area_data$Individual == 22 & leaf_area_data$Week == 1)
# Double the leaf area
leaf_area_data[pick_row, c("Leaf_Area_cm2", "Leaf_Area_m2")] = leaf_area_data[pick_row, c("Leaf_Area_cm2", "Leaf_Area_m2")] * 2 

# To meet global trait databases standards with 1 trait value per single leaf,
# we divide through our leaf area data and fresh and dry weights by 2
 
# Convert leaf area to one leaf basis (Divide by 2)
leaf_area_data$Leaf_Area_cm2 = leaf_area_data$Leaf_Area_cm2 / 2
leaf_area_data$Leaf_Area_m2  = leaf_area_data$Leaf_Area_m2 / 2

# Convert fresh and dry weights to one leaf basis (Divide by 2)
fresh_dry_weight$Fresh_Weight = fresh_dry_weight$Fresh_Weight / 2
fresh_dry_weight$Dry_Weight   = fresh_dry_weight$Dry_Weight / 2

# Export data
readr::write_csv(fresh_dry_weight, "Data/Processed/fresh_dry_weight_single_leaf.csv")
readr::write_csv(leaf_area_data, "Data/Processed/leaf_area_data_single_leaf.csv")
