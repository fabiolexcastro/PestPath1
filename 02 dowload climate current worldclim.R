
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, rmapshaper, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load libraries ----------------------------------------------------------

# Download administrative data
wrld <- ne_countries(returnclass = 'sf', scale = 50, type = 'countries')
wrld <- terra::vect(wrld)

# Climate download

# 5 km --------------------------------------------------------------------
prec <- geodata::worldclim_global(var = 'prec', res = 2.5, path = './tmpr' )
tmax <- geodata::worldclim_global(var = 'tmax', res = 2.5, path = './tmpr' )
tavg <- geodata::worldclim_global(var = 'tavg', res = 2.5, path = './tmpr' )
tmin <- geodata::worldclim_global(var = 'tmin', res = 2.5, path = './tmpr' )

dout <- './tif/climate/wc21/5km'
dir_create(dout)

terra::writeRaster(x = prec, filename = glue('{dout}/prec.tif'), overwrite = TRUE)
terra::writeRaster(x = tmax, filename = glue('{dout}/tmax.tif'), overwrite = TRUE)
terra::writeRaster(x = tavg, filename = glue('{dout}/tavg.tif'), overwrite = TRUE)
terra::writeRaster(x = tmin, filename = glue('{dout}/tmin.tif'), overwrite = TRUE)

# To write these files ----------------------------------------------------

# 10 km 
prec <- geodata::worldclim_global(var = 'prec', res = 5, path = './tmpr' )
tmax <- geodata::worldclim_global(var = 'tmax', res = 5, path = './tmpr' )
tavg <- geodata::worldclim_global(var = 'tavg', res = 5, path = './tmpr' )
tmin <- geodata::worldclim_global(var = 'tmin', res = 5, path = './tmpr' )
bioc <- geodata::worldclim_global(var = 'bio', res = 5, path = './tmpr' )

dout <- './tif/climate/wc21/10km'
dir_create(dout)

terra::writeRaster(x = prec, filename = glue('{dout}/prec.tif'), overwrite = TRUE)
terra::writeRaster(x = tmax, filename = glue('{dout}/tmax.tif'), overwrite = TRUE)
terra::writeRaster(x = tavg, filename = glue('{dout}/tavg.tif'), overwrite = TRUE)
terra::writeRaster(x = tmin, filename = glue('{dout}/tmin.tif'), overwrite = TRUE)
terra::writeRaster(x = bioc, filename = glue('{dout}/bioc.tif'), overwrite = TRUE)


