

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, rmapshaper, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load libraries ----------------------------------------------------------
fles <- dir_ls('./kml') %>% as.character()
fles <- grep('Ophelimus', fles, value = T)
srze <- st_read(fles[1])
srzn <- st_read(fles[2])

fles <- c('./kml/Agrilus plannippennis (Emerald Ash Borer).kml', './kml/Emerald Ash Borer (CABI).kml')
fles <- './kml/Dryocosmus kuriphilus (Chestnut gall wasp) (1).kml'
shpf <- st_read(fles)
shp1 <- st_read(fles[1])
shp2 <- st_read(fles[2])

# Convert to table  -------------------------------------------------------
srze
unique(srze$Name)
srze_tble <- st_coordinates(srze) %>% as_tibble() %>% mutate(name = 'Agrilus plannippennis') %>% setNames(c('x', 'y', 'z', 'name')) %>% dplyr::select(-z)
srze_tble
srzn_tble <- st_coordinates(srzn) %>% as_tibble() %>% mutate(name = 'Agrilus plannippennis') %>% setNames(c('x', 'y', 'z', 'name')) %>% dplyr::select(-z)

tble <- st_coordinates(shpf) %>% as_tibble() %>% mutate(name = 'Dryocosmus kuriphilus') %>% setNames(c('x', 'y', 'z', 'name')) %>% dplyr::select(-z)

unique(srzn_tble$name)
unique(srze_tble$name)
crd1_tble <- st_coordinates(shp1) %>% as_tibble() %>% mutate(name = 'Agrilus plannippennis') %>% setNames(c('x', 'y', 'z', 'name')) %>% dplyr::select(-z)
crd2_tble <- st_coordinates(shp2) %>% as_tibble() %>% mutate(name = 'Agrilus plannippennis') %>% setNames(c('x', 'y', 'z', 'name')) %>% dplyr::select(-z)
crds <- rbind(crd1_tble, crd2_tble)
# crds <- st_coordinates(shpf) %>% as_tibble() %>% mutate(name = 'Agrilus plannippennis') %>% setNames(c('x', 'y', 'z', 'name')) %>% dplyr::select(-z)
shpf <- st_as_sf(x = crds, coords = c('x', 'y'), crs = st_crs(4326))
shpf <- st_as_sf(x = tble, coords = c('x', 'y'), crs = st_crs(4326))

# World shapefile ---------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)

# Fonts -------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto Condensed')
showtext_auto()

# Read excel file ---------------------------------------------------------
file <- 'tbl/speciesGBIF/GanodermaB-GBIF.xlsx'
tble <- read_excel(file)
tble <- tble[,c('species', 'Longitude', 'Latitude')]
colnames(tble) <- c('name', 'lon', 'lat')

shpf <- st_as_sf(tble, coords = c('lon', 'lat'), crs = 4326)

# Maps --------------------------------------------------------------------

# To make the map
gmp_agrlz <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.3) + 
  geom_sf(data = shpf, col = '#943B3B', size = 0.3) + 
  coord_sf(crs = st_crs("ESRI:54030")) + 
  ggtitle(label = 'Agrilus plannippennis') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 40, hjust = 0.5)) 

gmp_gndrm <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.3) + 
  geom_sf(data = shpf, col = '#943B3B', size = 3) + 
  coord_sf(crs = st_crs("ESRI:54030")) + 
  ggtitle(label = 'Ganoderma boninense') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 40, hjust = 0.5)) 


gmp_dryoc <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.3) + 
  geom_sf(data = shpf, col = '#943B3B', size = 0.3) + 
  coord_sf(crs = st_crs("ESRI:54030")) + 
  ggtitle(label = 'Dryocosmus kuriphilus') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 40, hjust = 0.5)) 

gmp_sirez_noctilio <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.3) + 
  geom_sf(data = srzn, col = '#943B3B', size = 0.3) + 
  coord_sf(crs = st_crs("ESRI:54030")) + 
  ggtitle(label = 'Sirez noctilio') +
  theme_void() + 
  theme(plot.title = element_text(family = 'Roboto', color = 'grey50', face = 'bold.italic', size = 40, hjust = 0.5)) 

ggsave(plot = gmp_agrlz, filename = './png/speciesLocation/Agrilus plannippennis.png', units = 'in', width = 9, height = 5.3, dpi = 300)
ggsave(plot = gmp_sirez_noctilio, filename = './png/speciesLocation/Sirez noctilio.png', units = 'in', width = 9, height = 5.3, dpi = 300)
ggsave(plot = gmp_sirez_noctilio, filename = './png/speciesLocation/Sirez noctilio.png', units = 'in', width = 9, height = 5.3, dpi = 300)
ggsave(plot = gmp_dryoc , filename = './png/speciesLocation/Dryocosmus kuriphilus.png', units = 'in', width = 9, height = 5.3, dpi = 300)
ggsave(plot = gmp_gndrm , filename = './png/speciesLocation/Ganoderma boninesne.png', units = 'in', width = 9, height = 5.3, dpi = 300)

srze_tble
srzn_tble
srzn_alld <- rbind(srze_tble, srzn_tble)
colnames(srzn_alld) <- c('decimalLongitude', 'decimalLatitude', 'scientificName')
srzn_rawd <- read_csv('./tbl/speciesGBIF/Sirex noctilio.csv')
fnal <- bind_rows(srzn_rawd, srzn_alld)

fnal <- crds
write.csv(fnal, './tbl/speciesGBIF/Agrilus plannippennis.csv', row.names = FALSE)
write.csv(tble, './tbl/speciesGBIF/Dryocosmus kuriphilus.csv', row.names = FALSE)

write.csv(tble, './tbl/speciesGBIF/Ganoderma boninense.csv', row.names = FALSE)

