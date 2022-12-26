
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, xlsx, crayon, tidyverse, usdm, dismo, rmapshaper, outliers, showtext, extrafont, glue, colourpicker, ggspatial, rnaturalearthdata, rnaturalearth, gtools, rgbif, readxl)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999)

# Load data ---------------------------------------------------------------
path <- dir_ls('./models/maxent') %>% as.character()
fles <- glue('{path}/run_1') %>% dir_ls() %>% as.character() %>% grep('.csv', ., value = T)

# Percentage --------------------------------------------------------------
prcn <- grep('percentage', fles, value = T)
spcs <- dirname(fles) %>% unique()
spcs <- str_split(spcs, pattern = '/') %>% purrr::map(., 4) %>% unlist()
prcn <- map(prcn, read_csv)
prcn <- map(1:length(prcn), .f = function(i) mutate(prcn[[i]], specie = spcs[i]))
prcn <- bind_rows(prcn)

# Threshold - AUC - metrics -----------------------------------------------
thrs <- grep('threshold_auc', fles, value = T)
thrs <- map(thrs, read_csv)
thrs <- map(1:length(thrs), .f = function(i) mutate(thrs[[i]], specie = spcs[i]))
thrs <- bind_rows(thrs)
thrs <- thrs %>% group_by(specie) %>% summarise(threshold = mean(threshold), auc = mean(auc)) %>% ungroup()

# To write ----------------------------------------------------------------
write.xlsx(x = as.data.frame(prcn), file = 'tbl/output/metrics_species.xlsx', sheetName = 'PercentageContribution', append = FALSE, row.names = FALSE)
write.xlsx(x = as.data.frame(thrs), file = 'tbl/output/metrics_species.xlsx', sheetName = 'Metrics', append = TRUE, row.names = FALSE)

# Summarize percentage ----------------------------------------------------
smmr <- prcn %>% group_by(variables, specie) %>% dplyr::summarise(percentage = mean(percentage, na.rm = T)) %>% ungroup()
smmr %>% arrange(specie)
write.xlsx(x = as.data.frame(smmr), file = 'tbl/output/metrics_species.xlsx', sheetName = 'AggregatePercentage', append = TRUE, row.names = FALSE)
