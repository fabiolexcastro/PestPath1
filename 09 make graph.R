
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, hrbrthemes, readxl, gtools, rgeos)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
excel_sheets('./tbl/output/metrics_species.xlsx')
tble <- read_excel('./tbl/output/metrics_species.xlsx', 'AggregatePercentage')
vars <- unique(tble$variables) %>% mixedsort()
tble <- mutate(tble, variables = factor(variables, levels = vars))

lbls <- tble %>% distinct(specie) %>% mutate(Especie = c('Agrilus planipennis', 'Cryphonectria parasitica', 'Dryocosmus kuriphilus', 'Ganoderma boninense', 'Lethal yellowing disease', 'Sirex noctilio', 'Lecptocybe invasa', 'Ophelimus maskeli'))
tble <- inner_join(tble, lbls, by = c('specie'))
tble <- mutate(tble, variables = gsub('_', ' ', variables))

tble <- mutate(tble, variables = factor(variables, levels = mixedsort(unique(pull(tble, variables)))))

# To make the graph -------------------------------------------------------

# Importance --------------------------------------------------------------
gcol <- ggplot(data = tble, aes(x = variables, y = percentage)) + 
  geom_col() +
  facet_wrap(~Especie, scales = 'free_x') + 
  labs(x = 'Bioclimatic variables', y = 'Percentage importance') + 
  theme_ipsum_ps() + 
  theme(strip.text.x = element_text(face = 'bold.italic'), 
        axis.text.x = element_text(angle = 45, vjust = 0.5))
gcol

ggsave(plot = gcol, filename = './png/graphs/importanceVariables.png', units = 'in', width = 11.5, height = 7, dpi = 300)

# AUC graph ---------------------------------------------------------------
aucs <- read_excel('tbl/output/metrics_species.xlsx', sheet = 'Metrics')
aucs <- inner_join(aucs, lbls, by = 'specie')

gbar <- ggplot(data = aucs, aes(x = Especie, y = auc)) + 
  geom_col() + 
  theme_ipsum_ps() + 
  labs(x = '', y = 'AUC') +
  theme(axis.title.y = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave(plot = gbar, filename = './png/graphs/aucSpecies.png', units = 'in', width = 8, height = 6.5, dpi = 300)
