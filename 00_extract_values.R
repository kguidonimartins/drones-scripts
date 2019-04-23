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

# list files
files <- list.files(path = ".", 
                    pattern = "*dblbnd.adf", 
                    full.names = TRUE,
                    recursive = TRUE)

# count valid pixels to n sampling 
count_valid_pixel <- function(r){
  ra <- raster(r)
  ra[is.na(ra)] <- 0
  ra <- ra[ra != 0]
  df <- data.frame(ncell(ra))
  return(df)
}

# plan
plan(multiprocess)
# run
valid_pixel <- furrr::future_map_dfr(.x = files, .f = count_valid_pixel, .progress = TRUE)

# minimum valid pixel
min(valid_pixel$ncell.ra.)

(n_sampling <- 500000)

# sampling pixel
fastRandomPoints <- function(r, n) {
  n <- 50000
  a <- raster(r)
  df <- data.frame(matrix(data = NA, nrow = n, ncol = 2))
  nome <-gsub(
    pattern = "/Volumes/Seagate Expansion Drive/arcgis/CALIBRATED/",
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

# exemplo <- fastRandomPoints(files[1], 100)

# extracted_values <- map_df(files[1:3], fastRandomPoints, 1)

# plan
plan(multiprocess)
# run
valid_pixel <- furrr::future_map_dfr(.x = files, .f = fastRandomPoints, .progress = TRUE)
# save file
# write.csv(x = valid_pixel, file = "./extracted_pixels.csv", row.names = FALSE)

valid_pixel %>% 
  tail() %>% 
  mutate(
    year_month = str_extract(source_file, "[0-9]+_[A-Z]+"),
    year = str_extract(year_month, "[0-9]+"),
    month = str_extract(year_month, "[A-Z]+"),
    vegetation_type = ifelse(test = str_detect(source_file, "cna"), yes = "cna", no = "cac"), 
    sampling_point = str_extract(source_file, "dr[0-9]+")
    ) %>% 
  select(NDVI_values, sampling_point, vegetation_type, month, year, year_month, source_file)

processed_df <- 
  valid_pixel %>% 
    mutate(
      year_month = str_extract(source_file, "[0-9]+_[A-Z]+"),
      year = str_extract(year_month, "[0-9]+"),
      month = str_extract(year_month, "[A-Z]+"),
      vegetation_type = ifelse(test = str_detect(source_file, "cna"), yes = "cna", no = "cac"), 
      sampling_point = str_extract(source_file, "dr[0-9]+")
    ) %>% 
  select(
    NDVI_values, 
    sampling_point, 
    vegetation_type, 
    month, 
    year, 
    year_month, 
    source_file
    )

# write.csv(x = processed_df, file = "processed_valid_pixel_df.csv", row.names = FALSE)

theme_set(theme_light())

processed_df %>% 
  ggplot(aes(x = month, y = NDVI_values, fill = vegetation_type)) +
  geom_boxplot()

processed_df %>% 
  ggplot(aes(x = sampling_point, y = NDVI_values, fill = vegetation_type)) +
  geom_boxplot()

processed_df %>% 
  ggplot(aes(x = sampling_point, y = NDVI_values, fill = vegetation_type)) +
  geom_boxplot() +
  facet_wrap(~ month)

ggsave(
  filename = "ndvi_by_sampling_point-color_by_vegetation-facet_by_month.pdf",
  plot = last_plot(), 
  width = 20, 
  height = 10, 
  dpi = 200
  )


processed_df %>% 
  ggplot(aes(x = sampling_point, y = NDVI_values, colour = vegetation_type, fill = month)) +
  geom_boxplot()

ggsave(
  filename = "ndvi_by_sampling_point-color_by_vegetation-facet_by_month.pdf",
  plot = last_plot(), 
  width = 20, 
  height = 10, 
  dpi = 200
)

#------------------------------------------------------------------------------------------

processed_df %>% 
  ggplot(aes(x = sampling_point, y = NDVI_values, fill = vegetation_type)) +
  geom_boxplot()

alt_data <-
  processed_df %>%
  mutate(point_veg_type = paste0(sampling_point, vegetation_type))

data_avg <- processed_df %>% 
  group_by(sampling_point, vegetation_type, year_month, month) %>%
  summarize(
    Avg = base::mean(NDVI_values),
    SD = stats::sd(NDVI_values),
    N = n()
  ) %>%
  ungroup() %>%
  mutate(L_CI = Avg - 1.96 * sqrt(Avg * (1 - Avg) / N)) %>%
  mutate(U_CI = Avg + 1.96 * sqrt(Avg * (1 - Avg) / N)) 

data_avg

data_avg %>%
  ggplot(aes(sampling_point, Avg, colour = vegetation_type)) +
  geom_point(aes(x=sampling_point, y=Avg), size = 3) +
  geom_errorbar(aes(ymin = L_CI, ymax = U_CI), width = .1, size = 1) +
  labs(
    y = "Média +- IC"
  )

new_order <- c('MAIO', 'JUNHO', 'SETEMBRO', 'OUTUBRO', 'JANEIRO')

data_avg %>%
  mutate(month = factor(month, levels = new_order)) %>% 
  ggplot(aes(sampling_point, Avg, colour = vegetation_type)) +
  geom_point(aes(x=sampling_point, y=Avg), size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = L_CI, ymax = U_CI), width = .5) +
  labs(
    y = "Média +- IC"
  ) + 
  facet_wrap(~month, ncol = 1) +
  scale_color_manual(values = c('red', 'blue'))

data_avg %>%
  mutate(month = factor(month, levels = new_order)) %>% 
  ggplot(aes(sampling_point, Avg, colour = vegetation_type)) +
  geom_violin(data = processed_df, mapping = aes(sampling_point, NDVI_values)) +
  geom_point(aes(x=sampling_point, y=Avg), size = 3, alpha = 0.5) +
  geom_errorbar(aes(ymin = L_CI, ymax = U_CI), width = .5) +
  labs(
    y = "Média +- IC"
  ) + 
  facet_wrap(~month, ncol = 1) +
  scale_color_manual(values = c('red', 'blue'))

ggsave(
  filename = "ndvi_by_sampling_point-color_by_vegetation-facet_by_month_violin.pdf",
  plot = last_plot(), 
  width = 20, 
  height = 10, 
  dpi = 200
)
