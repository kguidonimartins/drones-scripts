#' ---
#' output: html_document
#' ---

#' Fator de agregacao: 
#' Aggregation factor expressed as number of cells in each direction (horizontally and 
#' vertically). Or two integers (horizontal and vertical aggregation factor) or three 
#' integers (when also aggregating over layers)
#' 

set.seed(123)
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

theme_set(theme_light())

# count valid pixels to n sampling 
count_valid_pixel <- function(r){
  ra <- raster(r)
  ra[is.na(ra)] <- 0
  ra <- ra[ra != 0]
  df <- data.frame(ncell(ra))
  return(df)
}

# sampling pixel
fastRandomPoints <- function(r, n) {
  # n <- 50000
  a <- raster(r)
  df <- data.frame(matrix(data = NA, nrow = n, ncol = 2))
  nome <-gsub(
    pattern = "/Volumes/Seagate Expansion Drive/arcgis/NOTCALIBRATED/",
    replacement = "",
    x = raster::filename(a)
  )
  df[,2] <- rep(nome, n)
  v <- raster::getValues(a)
  v[is.na(v)] <- 0
  v.d <- v[v != 0]
  df[,1] <- sample(v.d, n)
  names(df) <- c("NDVI_values", "source_file")
  # return(a)
  return(df)
}

# function make info
make_infos <- function(df){
  df_info <- df %>% 
    mutate(
      year_month = str_extract(source_file, "[0-9]+_[A-Z]+"),
      year = str_extract(year_month, "[0-9]+"),
      month = str_extract(year_month, "[A-Z]+"),
      vegetation_type = ifelse(test = str_detect(source_file, "cac"), yes = "cac", no = "cna"), 
      sampling_point = str_extract(source_file, "dr[0-9]+"),
      scale_agr = str_extract(source_file, "aggregated_by_[0-9]+")
    ) %>% 
    select(
      NDVI_values,
      sampling_point,
      vegetation_type,
      month,
      year,
      year_month,
      scale_agr,
      source_file
    )
  
  return(df_info)
}

plot(raster(x = "2019_JANEIRO/cac_0119_dr23/cac_0119_dr23_aggregated_by_2.tif"), main = "Fator de agregacao = 2")
plot(raster(x = "2019_JANEIRO/cna_0119_dr23/cna_0119_dr23_aggregated_by_2.tif"), add = TRUE)

files <- list.files(path = ".", pattern = "0119_dr23_aggregated_by_2", full.names = TRUE, recursive = TRUE)

# plan
plan(multiprocess)
# run
valid_pixel <- furrr::future_map_dfr(.x = files, .f = count_valid_pixel, .progress = TRUE)

n <- min(valid_pixel$ncell.ra.)

# plan
plan(multiprocess)
# run
valid_pixel_sampled <- furrr::future_map_dfr(.x = files, .f = fastRandomPoints, n, .progress = TRUE)

# make info
make_infos(valid_pixel_sampled) %>% 
  ggplot(aes(x = month, y = NDVI_values, fill = vegetation_type)) +
  geom_violin()



plot(raster(x = "2019_JANEIRO/cac_0119_dr23/cac_0119_dr23_aggregated_by_4.tif"), main = "Fator de agregacao = 4")
plot(raster(x = "2019_JANEIRO/cna_0119_dr23/cna_0119_dr23_aggregated_by_4.tif"), add = TRUE)

files <- list.files(path = ".", pattern = "0119_dr23_aggregated_by_4", full.names = TRUE, recursive = TRUE)

# plan
plan(multiprocess)
# run
valid_pixel <- furrr::future_map_dfr(.x = files, .f = count_valid_pixel, .progress = TRUE)

n <- min(valid_pixel$ncell.ra.)

# plan
plan(multiprocess)
# run
valid_pixel_sampled <- furrr::future_map_dfr(.x = files, .f = fastRandomPoints, n, .progress = TRUE)

# make info
make_infos(valid_pixel_sampled) %>% 
  ggplot(aes(x = month, y = NDVI_values, fill = vegetation_type)) +
  geom_violin()

plot(raster(x = "2019_JANEIRO/cac_0119_dr23/cac_0119_dr23_aggregated_by_8.tif"), main = "Fator de agregacao = 8")
plot(raster(x = "2019_JANEIRO/cna_0119_dr23/cna_0119_dr23_aggregated_by_8.tif"), add = TRUE)

files <- list.files(path = ".", pattern = "0119_dr23_aggregated_by_8", full.names = TRUE, recursive = TRUE)

# plan
plan(multiprocess)
# run
valid_pixel <- furrr::future_map_dfr(.x = files, .f = count_valid_pixel, .progress = TRUE)

n <- min(valid_pixel$ncell.ra.)

# plan
plan(multiprocess)
# run
valid_pixel_sampled <- furrr::future_map_dfr(.x = files, .f = fastRandomPoints, n, .progress = TRUE)

# make info
make_infos(valid_pixel_sampled) %>% 
  ggplot(aes(x = month, y = NDVI_values, fill = vegetation_type)) +
  geom_violin()

plot(raster(x = "2019_JANEIRO/cac_0119_dr23/cac_0119_dr23_aggregated_by_16.tif"), main = "Fator de agregacao = 16")
plot(raster(x = "2019_JANEIRO/cna_0119_dr23/cna_0119_dr23_aggregated_by_16.tif"), add = TRUE)

files <- list.files(path = ".", pattern = "0119_dr23_aggregated_by_16", full.names = TRUE, recursive = TRUE)

# plan
plan(multiprocess)
# run
valid_pixel <- furrr::future_map_dfr(.x = files, .f = count_valid_pixel, .progress = TRUE)

n <- min(valid_pixel$ncell.ra.)

# plan
plan(multiprocess)
# run
valid_pixel_sampled <- furrr::future_map_dfr(.x = files, .f = fastRandomPoints, n, .progress = TRUE)

# make info
make_infos(valid_pixel_sampled) %>% 
  ggplot(aes(x = month, y = NDVI_values, fill = vegetation_type)) +
  geom_violin()

plot(raster(x = "2019_JANEIRO/cac_0119_dr23/cac_0119_dr23_aggregated_by_32.tif"), main = "Fator de agregacao = 32")
plot(raster(x = "2019_JANEIRO/cna_0119_dr23/cna_0119_dr23_aggregated_by_32.tif"), add = TRUE)

files <- list.files(path = ".", pattern = "0119_dr23_aggregated_by_32", full.names = TRUE, recursive = TRUE)

# plan
plan(multiprocess)
# run
valid_pixel <- furrr::future_map_dfr(.x = files, .f = count_valid_pixel, .progress = TRUE)

n <- min(valid_pixel$ncell.ra.)

# plan
plan(multiprocess)
# run
valid_pixel_sampled <- furrr::future_map_dfr(.x = files, .f = fastRandomPoints, n, .progress = TRUE)

# make info
make_infos(valid_pixel_sampled) %>% 
  ggplot(aes(x = month, y = NDVI_values, fill = vegetation_type)) +
  geom_violin()

plot(raster(x = "2019_JANEIRO/cac_0119_dr23/cac_0119_dr23_aggregated_by_64.tif"), main = "Fator de agregacao = 64")
plot(raster(x = "2019_JANEIRO/cna_0119_dr23/cna_0119_dr23_aggregated_by_64.tif"), add = TRUE)

files <- list.files(path = ".", pattern = "0119_dr23_aggregated_by_64", full.names = TRUE, recursive = TRUE)

# plan
plan(multiprocess)
# run
valid_pixel <- furrr::future_map_dfr(.x = files, .f = count_valid_pixel, .progress = TRUE)

n <- min(valid_pixel$ncell.ra.)

# plan
plan(multiprocess)
# run
valid_pixel_sampled <- furrr::future_map_dfr(.x = files, .f = fastRandomPoints, n, .progress = TRUE)

# make info
make_infos(valid_pixel_sampled) %>% 
  ggplot(aes(x = month, y = NDVI_values, fill = vegetation_type)) +
  geom_violin()
