################################################################################
# Packages
################################################################################

library("readr")
library("tibble")

source("R/Utils/Utils.R")

################################################################################
# Import data
################################################################################

fd_path = "Data/Raw/Weight/Fresh_Dry_Weight.csv"
raw_fd  = readr::read_csv(file = fd_path)

################################################################################
# Process 
################################################################################

# Create column with individual number
fd_ind_id = add_individual_id(x = raw_fd, branch_id_col = "Branch_ID")

# Add species names to dataset
fd = add_species(fd_ind_id)

################################################################################
# Export
################################################################################

readr::write_csv(x = fd, file = "Data/Processed/fresh_dry_weight.csv")

