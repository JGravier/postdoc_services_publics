library(tidyverse)
library(Cairo)
library(GGally)


source("fonctions_bases.R")

# -------------------- data ---------------------
sf_sp_communes <- read.csv("BDD_services_publics/data_sorties/services_publics_communes_des_aires_urbaines_long.csv",
                                               stringsAsFactors = FALSE, header = TRUE, encoding = "Latin1")

tab_taille <- sf_sp_communes %>%
  group_by(AU2010) %>%
  select(POPULATION) %>%
  summarise_if(is.numeric, sum) %>%
  rename(tailles_2016_pop = POPULATION)

sf_sp_communes <- sf_sp_communes %>%
  as_tibble() %>%
  mutate(INSEE_REG = as.character(INSEE_REG),
         dep = as.character(dep),
         annee = as.character(annee),
         ID = as.character(ID)) %>%
  left_join(., y = tab_taille, by = "AU2010") %>%
  mutate(tailles_2016 = if_else(tailles_2016_pop < 30000, "petite\n(< 30.000 hab.)",
                                if_else(tailles_2016_pop > 200000, "grande\n(> 200.000 hab.)", "moyenne"))) %>%
  filter(ID == "2")

rm(tab_taille)

tab_poste_dec <- sf_sp_communes %>%
  pivot_wider(id_cols = c("depcom", "INSEE_REG", "dep", "pop2009", "pop1999", "pop1990", "pop1975",
                          "LIBGEO", "LIBAU2010", "geometry", "tailles_2016"), 
              names_from = "annee", values_from = "nb_equip") %>%
  mutate(`2009` = if_else(is.na(`2009`), 0, as.double(`2009`)),
         `2013` = if_else(is.na(`2013`), 0, as.double(`2013`)),
         `2018` = if_else(is.na(`2018`), 0, as.double(`2018`))) %>%
  mutate(pop_99_2009 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 10),
         pop_90_2009 = TCAM(datefin = pop2009, datedebut = pop1990, nbannee = 19),
         pop_75_2009 = TCAM(datefin = pop2009, datedebut = pop1975, nbannee = 34)) %>%
  mutate(poste_tcam_2009_2018 = TCAM(datefin = `2018`, datedebut = `2009`, nbannee = 9),
         poste_vol_2009_2018 = `2018` - `2009`)

tab_poste_dec <- tab_poste_dec %>%
  mutate(poste_tcam_2009_2018 = if_else(poste_tcam_2009_2018 == "NaN", 0, poste_tcam_2009_2018)) %>%
  # cas où l'on a en 2009 et en 2018 pas de bureau (mais souvent 1 bureau en 2013)
  mutate(poste_tcam_2009_2018 = if_else(poste_tcam_2009_2018 == "Inf", 100, poste_tcam_2009_2018)) %>%
  # cas de création d'un bureau à l'exeption de la commune depcom = 49307 pour laquelle on passe de 0 à 3 bureaux sur la période
  mutate(poste_tcam_2009_2018 = if_else(depcom == 49307, 300, poste_tcam_2009_2018))

tab_poste_dec %>%
  filter(poste_tcam_2009_2018 != 0) %>%
  nrow()
# 1/3 des communes où l'on a un changement en termes de nb de bureaux de poste

tab_poste_dec %>%
  filter(poste_tcam_2009_2018 != 0) %>%
  select(pop_99_2009:poste_vol_2009_2018) %>%
  ggpairs()
  
ggsave(filename = "ggpairs.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/LaPoste/dec", dpi = 300,  width = 30, height = 25, units = "cm")

tab_poste_dec %>%
  filter(poste_tcam_2009_2018 != 0 & pop2009 > 500) %>%
  select(pop_99_2009:poste_vol_2009_2018) %>%
  ggpairs()

ggsave(filename = "ggpairs_communes_plus500hab.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/LaPoste/dec", dpi = 300,  width = 30, height = 25, units = "cm")

tab_poste_dec %>%
  filter(poste_tcam_2009_2018 != 0 & poste_tcam_2009_2018 > -99 & poste_tcam_2009_2018 < 99) %>%
  select(pop_99_2009:poste_vol_2009_2018) %>%
  ggpairs()

ggsave(filename = "ggpairs_communes_hors_apparition_disparition.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/LaPoste/dec", dpi = 300,  width = 30, height = 25, units = "cm")
