################################################################################
# Packages
################################################################################
library("tiff")
library("magick")

################################################################################
# List leaf image data
################################################################################

# Image data
base_path_scans  = "Data/Raw/Leaf_Area_Scans"
week_dirs        = dir(base_path_scans, pattern = "Week", full.names = TRUE)
names(week_dirs) = basename(week_dirs)
image_paths      = lapply(week_dirs, dir, full.names = TRUE)

# Size standard
#size_standard    = file.path(base_path_scans, "3inx3in.tiff")

################################################################################
# Process data and export images
################################################################################


########################################
# Leaf data
########################################

# Create folders to hold processed images
dirs_out = gsub(pattern = "Raw", "Processed", week_dirs)

lapply(dirs_out, dir.create, recursive = TRUE)

for(i in seq_along(image_paths)){
  
  for(j in image_paths[[i]]){
    message(j)
    
    x = try({
      # Reading the file with the tiff package because the magick
      # package randomly throws a segfault when reading from the
      # disk
      a = readTIFF(j)
      magick::image_read(path = a)
    })
    
    if(inherits(x, "try-error")){
      x = magick::image_read(path = j)
    } 
    
    y = magick::image_normalize(x)
    z = magick::image_median(y, radius = 3)
    w1 = magick::image_threshold(z, "black", threshold = "70%")
    w2 = magick::image_quantize(w1, max = 2, colorspace = "gray")
    s  = file.path(dirs_out[[i]], basename(j))
    s  = gsub(".tif", ".jpeg", s)
    magick::image_write(w2, s, format = "jpeg")
  }
}

########################################
# Size standard
########################################

# x = magick::image_read(readTIFF(size_standard))
# y = magick::image_normalize(x)
# z = magick::image_median(y, radius = 3)
# w1 = magick::image_threshold(z, "black", threshold = "60%")
# w2 = magick::image_quantize(w1, max = 2, colorspace = "gray")
# s1 = gsub(".tiff", ".jpeg", size_standard)
# s2 = gsub("Raw", "Processed", s1)
# magick::image_write(w2, s2, format = "jpeg")
