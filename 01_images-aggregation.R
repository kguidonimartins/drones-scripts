# ipak function: install and load multiple R packages.
# Check to see if packages are installed.
# Install them if they are not, then load them into the R session.
# Forked from: https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}


ipak(c(
  "httr",
  "XML",
  "purrr",
  "future",
  "furrr",
  "parallel",
  "doParallel",
  "raster",
  "tidyverse"
))


# list files
files <- list.files(path = ".", 
                    pattern = "*dblbnd.adf", 
                    full.names = TRUE,
                    recursive = TRUE)


# function to aggregate images
tiff_aggregator <- function(file_path, overwrite = FALSE){
  
  # read raster
  f <- raster(file_path)
  
  # define aggregation factors: see ?raster::aggregate
  vec_agr <- c(2, 4, 8, 16, 32, 64)
  
  # loop over factors
  for (i in seq_along(vec_agr)){
    
    # vector with the i aggregation factor
    fct_agr <- vec_agr[i]
    
    # aggregate image by i factor
    f1 <- raster::aggregate(x = f, fact = fct_agr, func = mean)
    
    # extract the file entry name
    file_entry_name <- f@file@name
    
    # extract file extension
    file_extension_name <- basename(file_entry_name)
    
    # pathname to save file
    file_path_name <- str_remove_all(file_entry_name, file_extension_name)
    
    # 1st filename
    file_name_to_save <- basename(file_path_name)
    
    # final filename
    final_file <- paste0(file_path_name, file_name_to_save, "_aggregated_by_", fct_agr, ".tiff")
    
    # save image
    writeRaster(x = f1, filename = final_file, overwrite = overwrite)
    
    # inform
    message("Saving at ", final_file)
    
  }
  
}


# loop over files
# plan
plan(multiprocess)
# run
furrr::future_map(.x = files, .f = tiff_aggregator, overwrite = TRUE, .progress = TRUE)


