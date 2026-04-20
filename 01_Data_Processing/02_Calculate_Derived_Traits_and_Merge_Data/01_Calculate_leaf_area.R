################################################################################
# Packages
################################################################################
library("magick")
library("reshape2")
library("stringr")
source("R/Utils/Utils.R")

########################################
# List leaf image data
########################################

# Image data
base_path_scans  = "Data/Processed/Leaf_Area_Scans"
week_dirs        = dir(base_path_scans, pattern = "Week", full.names = TRUE)
names(week_dirs) = basename(week_dirs)
image_paths      = lapply(week_dirs, dir, full.names = TRUE)

# Size standard
size_standard    = file.path(base_path_scans, "3inx3in.jpeg")


###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
###                         IMPORTANT!!!
###
### These leaf area measurements are for two leaves! 
###
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###


################################################################################
# Process data and export images
################################################################################

########################################
# Standard area -- 9 in2
########################################

x = magick::image_read(size_standard)
y = magick::image_quantize(x, max = 2, colorspace = "gray")
z = table(as.raster(y))


black_pixel_name  = "#000000ff"
n_pixels_standard = z[black_pixel_name]
area_standard_cm2 = 58.0644   #3in x 3in = 9in2 = 58.0644 cm2


########################################
# Leaf data
########################################

## Generate base dataframe

leaf_area_data           = reshape2::melt(image_paths)
names(leaf_area_data)    = c("Path", "Week")
leaf_area_data$Branch_ID = gsub(".jpeg", "", basename(leaf_area_data$Path))

leaf_area_data = add_individual_id(leaf_area_data, branch_id_col = "Branch_ID")
leaf_area_data = add_species(leaf_area_data)

leaf_area_data$Leaf_Area_cm2 = NA
leaf_area_data$Leaf_Area_m2  = NA

# Extract the basename of the week column
leaf_area_data = leaf_area_data %>% dplyr::mutate(Week = as.numeric(stringr::str_extract(Week, "\\d+")))

# Arrange the week column from Week 1 to 24
leaf_area_data = leaf_area_data %>% dplyr::arrange(Week)


# Remove suffix "-1" and "-2" from Branch_ID
# In order to identify duplicated branches whose areas should be averaged
leaf_area_data$Branch_ID = gsub("-(1|2)$", "", leaf_area_data$Branch_ID)
leaf_area_data$Branch_ID = trimws(leaf_area_data$Branch_ID)

## Compute leaf area
for(i in seq.int(nrow(leaf_area_data))){
  message(i)
  x = magick::image_read(leaf_area_data[[i, "Path"]])
  y = magick::image_quantize(x, max = 2, colorspace = "gray")
  z = table(as.raster(y))
  
  leaf_area_data[[i, "Leaf_Area_cm2"]] = (z[[black_pixel_name]] * area_standard_cm2) / n_pixels_standard

  # Convert cm2 to m2 by dividing cm2 by 10,000
  leaf_area_data[[i, "Leaf_Area_m2"]]  = leaf_area_data[[i, "Leaf_Area_cm2"]] / 10000
  
}

# Remove the prefix "Week" from the Week column 
#leaf_area_data$Week = gsub("^Week ","",leaf_area_data$Week)

# # Remove the suffix "-1 or -2" from the Branch_ID column
# leaf_area_data$Branch_ID = gsub("-\\d+$", "", leaf_area_data$Branch_ID)

# Confirm the labels for Week and Branch_ID
# to be sure they are all unique and correct
unique(leaf_area_data$Week)
unique(leaf_area_data$Branch_ID)

# Calculate the sum value for Leaf_Area_cm2  
# By merging rows with the same Branch_ID
leaf_area_data  = aggregate(cbind(Leaf_Area_cm2, Leaf_Area_m2) 
                            ~ Family + Genus + Species + Individual + Week + Branch_ID, data = leaf_area_data, sum)

########################################
# Write out leaf area data
########################################
readr::write_csv(leaf_area_data, "Data/Processed/leaf_area_data.csv")
