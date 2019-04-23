ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# load spatial packages

ipak(c("raster",
       "rgdal",
       "rgeos",
       "RColorBrewer"))

# turn off factors
options(stringsAsFactors = FALSE)

# list files 
(fts <- list.files(path = ".", pattern = "*.JPG$"))

# batch process
for(i in seq_along(fts)){
  # read file i
  foto <- stack(x = fts[i])
  # process file i
  foto.br <- brick(foto)
  # calculate NDVI
  naip_ndvi <- (foto.br[[3]] - foto.br[[1]]) / (foto.br[[3]] + foto.br[[1]])
  # plot NDVI
  plot(naip_ndvi,
       main = paste("EMAS", fts[i]),
       axes = FALSE, box = FALSE)
  # save NDVI
  writeRaster(x = naip_ndvi, filename = paste0("NDVI_", gsub(pattern = ".JPG$", replacement = "", x = fts[i]), ".tiff"))
}




