library(tidyverse)
library(ggthemes)
library(sf)
library(GGally)

source("fonctions_bases.R")

# --------------------------- Hypothèse 2 : décroissance préalable, RGPP -------------------------------
demographie <- read.csv("BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_demographie.csv", stringsAsFactors = FALSE,
                        encoding = "Latin1") %>%
  as_tibble()

rgpp_nb_evo <- read.csv("BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_RGPP_nb_equip_2009-2018.csv", stringsAsFactors = FALSE,
                        encoding = "Latin1") %>%
  as_tibble()

tab_decroissance_rgpp_services_2009_2013 <- rgpp_nb_evo %>%
  filter(annees == "2009-2013") %>%
  pivot_wider(id_cols = AU2010:tailles_2016, names_from = RGPP, values_from = TCAM) %>%
  left_join(., y = demographie %>% select(-pop2016, -pop2013, -pop1990:-pop_2009_2013), by = "AU2010")


# vue globale :
tab_decroissance_rgpp_services_2009_2013 %>%
  select(`non `, `oui `, pop_2006_2009, pop_1999_2006) %>%
  ggpairs()

# en ne travaillant que sur les changements (et non la stricte évolution des services visés par la RGPP) :
tab_decroissance_rgpp_services_2009_2013 %>%
  filter(pop_2006_2009 < 30) %>% # virer les Sables-d'Olonne
  filter(`oui ` != 0) %>% # ce qui fait 322 aires urbaines sur 578
  select(`non `, `oui `, pop_2006_2009, pop_1999_2006) %>%
  ggpairs()
