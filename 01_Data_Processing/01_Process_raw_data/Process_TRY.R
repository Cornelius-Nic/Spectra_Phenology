################################################################################
# Package(s)
################################################################################
library("data.table")

################################################################################
# Import data
################################################################################
#try_1 = fread("Data/Raw/TRY/13403.txt") # LMA, Nitrogen
#try_2 = fread("Data/Raw/TRY/43807.txt") # Carbon, Leaf Water Content

try_1 = fread("C:/Users/corne/OneDrive - University of Maine System/Desktop/Data/TRY/13403.txt")
try_2 = fread("C:/Users/corne/OneDrive - University of Maine System/Desktop/Data/TRY/43807.txt")

# Study species
species = c("Acer platanoides", "Acer rubrum", "Betula papyrifera", "Prunus nigra", 
            "Quercus rubra", "Rhododendron catawbiense", "Rhododendron maximum")

# Subset TRY datasets to contain only study species
try_1 = try_1[try_1$SpeciesName %in% species, ]
try_2 = try_2[try_2$SpeciesName %in% species, ]

##########################
# Calculate LMA summary
##########################
A   = aggregate(StdValue ~ SpeciesName * TraitName, data = try_1, FUN = summary)
lma = A[grep("Leaf area per leaf dry mass", A$TraitName), ]
lma[, 3:ncol(lma)] = 1/lma[, 3:ncol(lma)]

# Range of TRY LMA values
try_lma_range = range(lma$StdValue, na.rm = T)
# 0.008 - 0.2045

##########################
# Calculate EWT summary
##########################
B  = aggregate(StdValue ~ SpeciesName * TraitName, data = try_2, FUN = summary)
#ewt  = B[grep("Leaf water content per leaf dry mass (not saturated)", B$TraitName), ]
#ewt[, 3:ncol(ewt)] = ewt[, 3:ncol(ewt)]

# Range of TRY EWT values
#try_lma_range = range(B$StdValue, na.rm = T)

##########################
# Calculate Carbon summary
##########################
B = aggregate(StdValue ~ SpeciesName * TraitName, data = try_2, FUN = summary)
carb = B[B$TraitName == "Leaf carbon (C) content per leaf dry mass", ]
carb[, 3:ncol(carb)] = carb[, 3:ncol(carb)] * 0.1

# Range of TRY Carbon values
try_carb_range = range(carb$StdValue, na.rm = T)
# 39.4 - 52.9

##########################
# Calculate Nitrogen summary
##########################
A = aggregate(StdValue ~ SpeciesName * TraitName, data = try_1, FUN = summary)
nit = A[A$TraitName == "Leaf nitrogen (N) content per leaf dry mass", ]
nit[, 3:ncol(nit)] = nit[, 3:ncol(nit)] * 0.1

# Range of TRY Nitrogen values
try_nit_range = range(nit$StdValue, na.rm = T)
# 0.49 - 4.92

##########################
# Calculate LDMC summary
##########################
B = aggregate(StdValue ~ SpeciesName * TraitName, data = try_2, FUN = summary)
ldmc = B[B$TraitName == "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)", ]
ldmc = B[B$StdValue  == "mg/g", ]
ldmc[, 3:ncol(ldmc)] = ldmc[, 3:ncol(ldmc)] 

# Range of TRY Nitrogen values
try_ldmc_range = range(ldmc$StdValue, na.rm = T)
# 0.49 - 4.92