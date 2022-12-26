

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, crayon, tidyverse, usdm, dismo, rmapshaper, outliers, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------
fles <- dir_ls('tbl/speciesInput') %>% as.character %>% grep('.csv', ., value = T) %>% grep('_swd.csv', ., value = T)
fles <- fles[1:7]
tbls <- map(fles, read_csv)

vif(as.data.frame(tbls[[1]])[,4:22])
usdm::vif(as.data.frame(tbls[[1]][,4:22]))

rslt <- purrr::map(.x = 1:length(tbls), .f = function(i){
  
  cat(i, '\n')
  tble <- tbls[[i]]
  tble <- as.data.frame(tble)
  spce <- unique(tble$scientificName)[1]
  
  viff <- vif(tble[,4:22])
  viff <- mutate(viff, VIF = round(VIF, 1))
  colnames(viff)[2] <- spce
  return(viff)
  
})

colnames(rslt[[7]])[2] <- 'Sirex noctilio'
map(rslt, colnames)

rslt <- rslt %>% purrr::reduce(., inner_join)
rslt
write.xlsx(x = rslt, file = 'tbl/output/vif.xlsx', sheetName = 'VIF', row.names = F)

# Get the variables from the csv ------------------------------------------
fles <- dir_ls('tbl/speciesInput') %>% grep('_vrs_', ., value = T) %>% as.character()
fles <- fles[1:8]
tbls <- map(fles, read_csv)
tbls <- map(1:length(tbls), function(i){
  tibble(bios = colnames(tbls[[i]])[4:ncol(tbls[[i]])], specie = basename(fles[i]))
})
tbls <- bind_rows(tbls)
write.xlsx(x = as.data.frame(tbls), file = 'tbl/output/variables.xlsx', sheetName = 'vars', row.names = FALSE)
