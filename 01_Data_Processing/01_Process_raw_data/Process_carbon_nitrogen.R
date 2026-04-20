######################################################################
# Package(s)
######################################################################
library("readxl")

# Manually added species IDs to the file
# Manually changed values above 100 to NAs
##################
# Read data
##################
carb_nit = readxl::read_excel("Data/Raw/CN/CN_Results_250528.xlsx")

# Create Week column using Sample ID
carb_nit$Week = as.integer(sub("w(\\d+)_.*", "\\1", carb_nit$`Sample ID`))

# Rename columns
names(carb_nit)[names(carb_nit) == "% C"] = "C"
names(carb_nit)[names(carb_nit) == "% N"] = "N"

################################################################################
# Export
################################################################################
write.csv(carb_nit, "Data/Processed/carbon_nitrogen.csv", row.names = FALSE)
