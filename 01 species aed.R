

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Functions ---------------------------------------------------------------
findGBIF <- function(spc){
  
  spc <- 'Cryphonectria parasitica' # Eucalyptus snout beetle
  # spc <- 'Eucalyptus snout beetle'
  spc <- 'Dendroctonus ponderosae'
  spc <- 'Cameraria ohridella'
  spc <- 'Basidiomicetos'
  
  try(expr = {
    
    cat(spc, '\n')
    rsl <- occ_data(scientificName = spc, limit = 2e5, hasCoordinate = T, hasGeospatialIssue = F)
    rsl <- rsl[[2]]
    rsl <- dplyr::select(rsl, key, scientificName, decimalLongitude, decimalLatitude, issues, year, month, day, eventDate, country)
    write.csv(rsl, glue('./tbl/speciesGBIF/{spc}.csv'), row.names = FALSE)
    return(rsl)  
    
  })
  
}
makeMap <- function(tbl){
  
  # tbl <- tbls[[1]]
  
  tbl <- drop_na(tbl)
  sft <- st_as_sf(x = tbl, coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326)
  sft <- st_transform(sft, crs = st_crs("ESRI:54030"))
  
  nme <- unique(sft$scientificName)
  nme <- str_split(string = nme, pattern = ',')
  nme <- nme[[1]][[1]]
  
  gmp <- ggplot() + 
    geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.3) + 
    geom_sf(data = sft, col = '#943B3B', size = 0.3) + 
    coord_sf(crs = st_crs("ESRI:54030")) + 
    ggtitle(label = nme) +
    theme_void() + 
    theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 40, hjust = 0.5)) 
  
  out <- glue('../png/speciesLocation/{nme}.png')
  ggsave(plot = gmp, filename = out, units = 'in', width = 9, height = 5.3, dpi = 300)
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------
spcs <- read_excel('../tbl/species.xlsx')
colnames(spcs)[2] <- 'scientificName'

spcs_list <- spcs$scientificName
spcs_list <- unique(spcs_list) %>% sort()

# World shapefile ---------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)

# To find in RGBIF --------------------------------------------------------
tbls <- map(.x = spcs_list, .f = findGBIF)
issu <- c('C parasitica', 'C ribicola', 'Lymantria dispar', 'Ophiostoma ultmi', 'Oryctes rhinoceros beetles', 'Phytophtora', 'Phytophtora ramorum', 'Pineus pini and Pineus  boerneri', 'Puccinia psidii complex')

# Check the results -------------------------------------------------------
tbls <- dir_ls('../tbl/speciesGBIF', regexp = '.csv$')
tbls <- map(tbls, read_csv)

# Fonts -------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto Condensed')
showtext_auto()

# To make the static map  -------------------------------------------------
map(tbls, makeMap)

# Endothia parasitica y cryponectria parastica ----------------------------




