
add_individual_id = function(x, branch_id_col = NULL){
  require("dplyr")
  require("stringr")
  real_ind = as.numeric(stringr::str_extract(x[[branch_id_col]], "[0-9]+"))
  dplyr::bind_cols("Individual" = real_ind, x)
}



add_species = function(x,
                       sp_lookup_path = "Data/Raw/species lookup.csv",
                       by             = "Individual"){
  require("readr")
  require("dplyr")
  
  l = readr::read_csv(file = sp_lookup_path)
  dplyr::inner_join(x = l, y = x, by = by)
}
