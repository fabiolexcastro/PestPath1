

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, crayon, tidyverse, usdm, dismo, rmapshaper, outliers, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

options(java.parameters = "-Xmx8000m") # Source: https://stackoverflow.com/questions/34624002/r-error-java-lang-outofmemoryerror-java-heap-space
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
options(java.parameters = "-Xmx16g")

# Fonts -------------------------------------------------------------------
font_add_google(family = 'Roboto', name = 'Roboto Condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
pnts <- read_csv('./tbl/speciesInput/Sirex noctilio_swd_otl_vrs_back.csv')
pnts <- read_csv('./tbl/speciesInput/Lethal yelllwoing disease_swd_otl_vrs_back.csv')
pnts <- read_csv('./tbl/speciesInput/Ganoderma boninense_swd_otl_vrs_back.csv')
# pnts <- read_csv('./tbl/speciesInput/Leptocybe invasa_swd_otl_vrs_back.csv')
# pnts <- read_csv('./tbl/speciesInput/Ophelimus maskeli_swd_otl_vrs_back.csv')
# pnts <- read_csv('./tbl/speciesInput/Agrilus planipennis_swd_otl_vrs_back.csv')
# pnts <- read_csv('./tbl/speciesInput/Dryocosmus kuriphilus_swd_otl_vrs_back.csv')
bioc <- terra::rast('./tif/climate/wc21/10km/bioc.tif')
names(bioc) <- glue('bio_{1:19}')

# spce <- 'Dryocosmus kuriphilus'
spce <- 'Sirex noctilio'
spce <- 'Lethal yellowing disease'
spce <- 'Ganoderma boninense'

vars <- colnames(dplyr::select(pnts, starts_with('bio')))
bioc <- bioc[[grep(paste0(vars, collapse = '|'), names(bioc), value = F)]]

# World data
wrld <- ne_countries(returnclass = 'sf', scale = 50, type = 'countries')
wrld <- filter(wrld, name != 'Antarctica')

# To extract by mask climate  ---------------------------------------------
bioc <- terra::crop(bioc, vect(wrld)) %>% terra::mask(., vect(wrld))

# K fold - crossvalidation ------------------------------------------------
occr <- filter(pnts, pb == 1)
back <- filter(pnts, pb == 0)

fld_occ <- kfold(occr, k = 10)
fld_bck <- kfold(back, k = 10)

# spce <- 'Sirex noctilio'
spce

# To make the model -------------------------------------------------------
mdls <- purrr::map(.x = 1:2, .f = function(i){
  
  # Filtering cross - validation
  cat(green(i), '\n')
  tst <- occr[fld_occ == i,]
  trn <- occr[fld_occ != i,]
  tst_bck <- back[fld_bck == i,]
  trn_bck <- back[fld_bck != i,]
  
  # Presences and pseudo-absences
  env <- rbind(trn, trn_bck)
  y <- c(trn$pb, trn_bck$pb)
  
  # Output directory 
  out <- glue('./models/maxent/{spce}/run_1/model_{i}')
  ifelse(!file.exists(out), dir_create(out), print('Directorio existe'))
  
  # Make the model and predict
  mxn <- maxent(env[,4:ncol(env)], y,  argcs = c('addsamplestobackground=true', 'responsecurves'), path = out)
  rst <- terra::predict(mxn, bioc)
  evl <- evaluate(mxn, p = data.frame(tst[,4:ncol(tst)]), a = data.frame(tst_bck[,4:ncol(tst_bck)]))
  prc <- as.data.frame(mxn@results)
  prc <- data.frame(variables = vars, percentage = prc[grep('contribution', rownames(prc)),], rutin = i)
  auc <- evl@auc
  tss <- evl@TPR + evl@TNR - 1
  tss <- evl@t[which.max(tss)]
  dfm <- data.frame(routine = i, threshold = tss, auc = auc)
  
  # To write the results
  saveRDS(object = mxn, file = glue('{out}/mxn_{i}.rds'))
  terra::writeRaster(x = rst, filename = glue('{out}/predict_mdl_current_{i}.tif'), overwrite = TRUE)
  write.csv(prc, file = glue('{out}/percentage_importance.csv'), row.names = FALSE)
  write.csv(dfm, file = glue('{out}/auc_threshold.csv'), row.names = FALSE)
  cat('Done!\n')
  return(list(rst, prc, dfm))
  
})

# Join all the tables / contribution 
# Raster
trra <- map(mdls, 1)

# Tables
dfrm <- map(mdls, 3)
dfrm <- bind_rows(dfrm)

prcn <- lapply(mdls, '[[', 2) %>% 
  bind_rows() %>%
  as_tibble()

# Write these files
dir.create('.')
write.csv(dfrm, './models/maxent/Ganoderma boninense/run_1/threshold_auc.csv', row.names = FALSE)
write.csv(prcn, './models/maxent/Ganoderma boninense/run_1/percentage_each_rutin.csv', row.names = FALSE)
writeRaster(do.call('c', trra), './models/maxent/Ganoderma boninense/run_1/raster_model_runs.tif', overwrite = TRUE)

# To calculate the average from each run ----------------------------------
trra <- do.call('c', trra)
trra_avrg <- terra::mean(trra)
writeRaster(trra_avrg, './models/maxent/Ganoderma boninense/run_1/raster_model_avrg.tif', overwrite = TRUE) 

# To calculate the standar deviation --------------------------------------
trra_stdt <- terra::app(x = trra, fun = 'sd')
writeRaster(trra_stdt, './models/maxent/Ganoderma boninense/run_1/raster_model_stdt.tif', overwrite = F)

# check leptocybe 
# rslt <- terra::rast('./models/maxent/Leptocybe invasa/run_1/raster_model_runs.tif')
# rslt <- mean(rslt)
# terra::writeRaster(x = rslt, './models/maxent/Leptocybe invasa/run_1/raster_model_avrg.tif', overwrite = T)


