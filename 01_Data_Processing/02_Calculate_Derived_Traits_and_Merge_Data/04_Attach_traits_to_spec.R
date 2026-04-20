################################################################################
# Packages
################################################################################
library("spectrolab")
library("readr")
library("dplyr")

################################################################################
# Read spectra and trait data
################################################################################
# spectra
fresh_spec = readRDS("Data/Processed/spectra_fresh.RDS")
dry_spec   = readRDS("Data/Processed/spectra_dry_black.RDS") # dry spec after 1 year

# traits
traits     = readr::read_csv("Data/Processed/merged_traits.csv")


# Add ScanDate for dry spec to traits dataframe
traits = traits %>%
  mutate(
    ScanDate = case_when(
      Week %in% 1:3   ~ as.Date("2024-08-06"),
      Week %in% 4:6   ~ as.Date("2024-08-09"),
      Week %in% 7:9   ~ as.Date("2024-08-13"),
      Week %in% 10:11 ~ as.Date("2024-08-15"),
      Week %in% 12:14 ~ as.Date("2024-08-19"),
      Week %in% 15:17 ~ as.Date("2024-08-21"),
      Week %in% 18:20 ~ as.Date("2024-08-23"),
      Week %in% 21:24 ~ as.Date("2024-08-26"),
      TRUE            ~ as.Date(NA)
    )
  )


# Split trait dataframe by Week
trait_list    = split(traits, traits$Week)

##############################
# Attach traits to fresh spec
##############################

# Select columns from each element in "Spec" and add to the corresponding elements in "traits"
fresh_list = setNames(
  Map(function(trait_list, fresh_spec) {
    # Ensure both data frames have an 'ID' column
    if (!"Branch_ID" %in% colnames(trait_list) | !"Branch_ID" %in% colnames(fresh_spec)) {
      stop("Both trait_list and fresh_spec must have an 'Branch_ID' column.")
    }
    
    # Select columns from trait_list, including the ID
    selected_columns = trait_list[, c("Branch_ID", "Date_Fresh", "LMA", "EWT", "MC", "LDMC", "C", "N"), drop = FALSE]
    
    # Merge with fresh_spec based on ID
    merged = merge(fresh_spec, selected_columns, by = "Branch_ID", all.x = TRUE) 
    merged
  }, trait_list, fresh_spec),
  names(fresh_spec) # Retain names from fresh_spec
)


# Add plant type
fresh_list = lapply(fresh_list, function(x) {
  x$Habit  = NA  # initialize column
  x$Habit[x$Species == "Acer rubrum"] = "deciduous"
  x$Habit[x$Species == "Acer platanoides"] = "deciduous"
  x$Habit[x$Species == "Betula papyrifera"] = "deciduous"
  x$Habit[x$Species == "Prunus nigra"] = "deciduous"
  x$Habit[x$Species == "Quercus rubra"] = "deciduous"
  x$Habit[x$Species == "Rhododendron catawbiense"] = "evergreen"
  x$Habit[x$Species == "Rhododendron maximum"] = "evergreen"
  x$Habit[x$Species == "Narcissus pseudonarcissus"] = "monocot"
  return(x)
})


# Change the column name "Date_Fresh" to "ScanDate"
fresh_list = lapply(fresh_list, function(x) {
  names(x)[names(x) == "Date_Fresh"] = "ScanDate"
  return(x)
})


# Function to rearrange columns
rearrange_columns = function(x) {
  # No specific reason - just keeping the old arrangement
  priority_cols = c("Family", "Genus", "Species", "Habit", "Individual", "Branch_ID", 
                    "ScanDate", "LMA", "EWT", "MC", "LDMC", "C", "N")
  # Identify integer columns
  integer_cols  = names(x)[sapply(x, is.integer)]
  # Remaining columns (if there is any)
  other_cols    = setdiff(names(x), c(priority_cols, integer_cols))
  # Combine column order
  new_col_order = c(priority_cols, integer_cols, other_cols)
  # Reorder columns in the data frame
  x[, intersect(new_col_order, names(x)), drop = FALSE] 
}

# Apply the rearrangement to each element of the list
fresh_spec_traits = lapply(fresh_list, rearrange_columns)

# Combine weeks into a single data frame
fresh_df  = Reduce(rbind, fresh_spec_traits)

# Add week column
row_per_week = sapply(fresh_spec_traits, nrow) 
week_cols    = rep(names(row_per_week), times = row_per_week)

all_fresh = cbind("Week" =  as.numeric(gsub("Week_", "", week_cols)),
                  fresh_df)


# Remove Narcissus pseudonarcissus because its measurement ended in week 11
fresh_spec_traits = all_fresh[all_fresh$Species !="Narcissus pseudonarcissus", ] 

################################################################################
# Export traits data as tibble
################################################################################
saveRDS(object = fresh_spec_traits, file = "Data/Processed/fresh_spec_traits.RDS")


##############################
# Do the same for dry spectra
# Attach traits to spec
##############################

# Select columns from each element in "Spec" and add to the corresponding elements in "traits"
dry_list = setNames(
  Map(function(trait_list, dry_spec) {
    # Ensure both data frames have an 'ID' column
    if (!"Branch_ID" %in% colnames(trait_list) | !"Branch_ID" %in% colnames(dry_spec)) {
      stop("Both trait_list and dry_spec must have an 'Branch_ID' column.")
    }
    
    # Select columns from trait_list, including the ID
    selected_columns = trait_list[, c("Branch_ID", "ScanDate", "LMA", "EWT", "MC", "LDMC", "C", "N"), drop = FALSE]
    
    # Merge with dry_spec based on ID
    merged = merge(dry_spec, selected_columns, by = "Branch_ID", all.x = TRUE) 
    merged
  }, trait_list, dry_spec),
  names(dry_spec) # Retain names from dry_spec
)


# Add plant type
dry_list = lapply(dry_list, function(x) {
  x$Habit  = NA  # initialize column
  x$Habit[x$Species == "Acer rubrum"] = "deciduous"
  x$Habit[x$Species == "Acer platanoides"] = "deciduous"
  x$Habit[x$Species == "Betula papyrifera"] = "deciduous"
  x$Habit[x$Species == "Prunus nigra"] = "deciduous"
  x$Habit[x$Species == "Quercus rubra"] = "deciduous"
  x$Habit[x$Species == "Rhododendron catawbiense"] = "evergreen"
  x$Habit[x$Species == "Rhododendron maximum"] = "evergreen"
  x$Habit[x$Species == "Narcissus pseudonarcissus"] = "monocot"
  return(x)
})

# Apply the rearrangement to each element of the list
dry_spec_traits = lapply(dry_list, rearrange_columns)

# Combine weeks into a single data frame
dry_df    = Reduce(rbind, dry_spec_traits)

# Add week column
row_per_week = sapply(dry_spec_traits, nrow) 
week_cols    = rep(names(row_per_week), times = row_per_week)

all_dry = cbind("Week" =  as.numeric(gsub("Week_", "", week_cols)),
                dry_df)


# Remove Narcissus pseudonarcissus because its measurement ended in week 11
dry_spec_traits = all_dry[all_dry$Species !="Narcissus pseudonarcissus", ] 

################################################################################
# Export traits data as tibble
################################################################################
saveRDS(object = dry_spec_traits, file = "Data/Processed/dry_spec_traits.RDS")

