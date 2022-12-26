
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, crayon, tidyverse, usdm, cptcity, dismo, rmapshaper, outliers, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Fonts -------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto Condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
spce <- 'Agrilus planipennis'
spce <- 'Sirex noctilio'
spce <- 'Dryocosmus kuriphilus'
spce <- 'Ganoderma boninense'

avrg <- terra::rast(glue('./models/maxent/{spce}/run_1/raster_model_avrg.tif'))
stdt <- terra::rast(glue('./models/maxent/{spce}/run_1/raster_model_stdt.tif'))
pnts <- read_csv(glue('./tbl/speciesInput/{spce}_swd_otl_vrs_back.csv'))
pnts <- filter(pnts, pb == 1)

# World data
wrld <- ne_countries(returnclass = 'sf', scale = 50, type = 'countries') %>% filter(name != 'Antarctiva')

# Raster to table ---------------------------------------------------------
stck <- c(avrg, stdt)
tble <- terra::as.data.frame(x = stck, xy = T) %>% as_tibble %>% setNames(c('x', 'y', 'average', 'sdtd'))

# To make the map ---------------------------------------------------------
# name <- 'Ophelimus maskeli'
name <- spce

# Color
"yellow"
find_cpt('fruit')
find_cpt('red')
image(matrix(1:100), col = cpt("neota_food_red_candy"))

# Average
gavg <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = average)) + 
  scale_fill_gradientn(colors = cpt(pal = 'imagej_gyr_centre', n = 10, rev = TRUE)) +
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.2) + 
  ggtitle(label = name) +
  coord_sf() +
  labs(fill = 'Suitability score') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 60, hjust = 0.5), 
        legend.position = 'bottom', 
        legend.text = element_text(family = 'Roboto', size = 40, color = 'grey50'), 
        legend.title = element_text(family = 'Roboto', size = 50, face = 'bold', color = 'grey50'),
        legend.key.width = unit(3, 'line'))  

ggsave(plot = gavg, filename = glue('./png/mapsOutput/{name}/{name}.png'), units = 'in', width = 9, height = 7, dpi = 300)

# Standart desviation

gstd <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = sdtd)) + 
  scale_fill_gradientn(colors = cpt(pal = 'neota_food_red_candy', n = 10, rev = TRUE)) +
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.2) + 
  ggtitle(label = name) +
  coord_sf() +
  labs(fill = 'Standart desviation - suitability score') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 60, hjust = 0.5), 
        legend.position = 'bottom', 
        legend.text = element_text(family = 'Roboto', size = 40, color = 'grey50'), 
        legend.title = element_text(family = 'Roboto', size = 50, face = 'bold', color = 'grey50'),
        legend.key.width = unit(3, 'line'))  

ggsave(plot = gstd, filename = glue('./png/mapsOutput/{name}/{name}_stdt.png'), units = 'in', width = 9, height = 7, dpi = 300)

# Average + presence points
gpnt <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = average)) + 
  scale_fill_gradientn(colors = cpt(pal = 'imagej_gyr_centre', n = 10, rev = TRUE)) +
  geom_point(data = pnts, aes(x = lon, y = lat), col = '#2E2E2E', size = 0.03) + 
  geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.2) + 
  ggtitle(label = name) +
  coord_sf() +
  labs(fill = 'Suitability score') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 60, hjust = 0.5), 
        legend.position = 'bottom', 
        legend.text = element_text(family = 'Roboto', size = 40, color = 'grey50'), 
        legend.title = element_text(family = 'Roboto', size = 50, face = 'bold', color = 'grey50'),
        legend.key.width = unit(3, 'line'))  

ggsave(plot = gpnt, filename = glue('./png/mapsOutput/{name}/{name}_average_points.png'), units = 'in', width = 9, height = 7, dpi = 300)
