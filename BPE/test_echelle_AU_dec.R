library(tidyverse)
library(Cairo)
library(GGally)


source("fonctions_bases.R")

# -------------------- data ---------------------
sf_services_publics_aires_urbaines <- read.csv("BDD_services_publics/data_sorties/services_publics_aires_urbaines_long.csv",
                                               stringsAsFactors = FALSE, header = TRUE, encoding = "Latin1")

typologie <- read.csv("BDD_services_publics/data_entrees/services_publics.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

typologie <- typologie %>%
  select(ID, validite_temporelle, RGPP, regalien) %>%
  mutate(ID = as.character(ID))

sf_services_publics_aires_urbaines <- sf_services_publics_aires_urbaines %>%
  as_tibble() %>%
  mutate(AU2010 = as.character(AU2010),
         TAU2016 = as.character(TAU2016),
         annee = as.character(annee),
         ID = as.character(ID)) %>%
  left_join(., y = typologie, by = "ID") %>%
  mutate(tailles_2016 = if_else(pop2016 < 30000, "petite\n(< 30.000 hab.)",
                                if_else(pop2016 > 200000, "grande\n(> 200.000 hab.)", "moyenne"))) %>%
  mutate(tailles_1999 = if_else(pop1999 < 30000, "petite\n(< 30.000 hab.)",
                                if_else(pop1999 > 200000, "grande\n(> 200.000 hab.)", "moyenne")))

rm(typologie)


# différences entre 1999 et 2016
sf_services_publics_aires_urbaines %>%
  select(pop2016:pop1968, LIBAU2010, tailles_2016, tailles_1999) %>%
  unique() %>%
  filter(tailles_2016 != tailles_1999)
# compte tenu de la modification communale au 1er janvier 2019 des Sables-d'Olonne, on a un beug dans les données population
# voir : http://www.insee.fr/fr/metadonnees/cog/commune/COM85194-les-sables-d-olonne
sf_services_publics_aires_urbaines <- sf_services_publics_aires_urbaines %>%
  mutate(pop2013 = if_else(LIBAU2010 == "Les Sables-d'Olonne", true = as.integer(NA), false = pop2013)) %>%
  mutate(pop2009 = if_else(LIBAU2010 == "Les Sables-d'Olonne", true = as.integer(48036), false = pop2009)) %>%
  # d'après les données de l'INSEE en géographie 2017, année 2010 (et non 2009)
  mutate(pop1999 = if_else(LIBAU2010 == "Les Sables-d'Olonne", true = as.integer(42933), false = pop1999)) %>%
  # ibid année 1999
  mutate(pop1990 = if_else(LIBAU2010 == "Les Sables-d'Olonne", true = as.integer(38769), false = pop1990)) %>%
  # ibid année 1990
  mutate(pop1982 = if_else(LIBAU2010 == "Les Sables-d'Olonne", true = as.integer(35318), false = pop1982)) %>%
  # ibid année 1982
  mutate(pop1975 = if_else(LIBAU2010 == "Les Sables-d'Olonne", true = as.integer(33205), false = pop1975)) %>%
  # ibid année 1975
  mutate(pop1968 = if_else(LIBAU2010 == "Les Sables-d'Olonne", true = as.integer(30417), false = pop1968)) %>%
  # ibid année 1975
  mutate(tailles_2016 = if_else(pop2016 < 30000, "petite\n(< 30.000 hab.)",
                                if_else(pop2016 > 200000, "grande\n(> 200.000 hab.)", "moyenne"))) %>%
  mutate(tailles_1999 = if_else(pop1999 < 30000, "petite\n(< 30.000 hab.)",
                                if_else(pop1999 > 200000, "grande\n(> 200.000 hab.)", "moyenne")))

sf_services_publics_aires_urbaines <- sf_services_publics_aires_urbaines %>%
  filter(ID == "7")


tab_education_secondaire <- sf_services_publics_aires_urbaines %>%
  pivot_wider(id_cols = c("pop2009", "pop1999", "pop1990", "pop1975",
                          "LIBAU2010", "geometry", "tailles_2016"), 
              names_from = "annee", values_from = "nb_equip") %>%
  mutate(`2009` = if_else(is.na(`2009`), 0, as.double(`2009`)),
         `2013` = if_else(is.na(`2013`), 0, as.double(`2013`)),
         `2018` = if_else(is.na(`2018`), 0, as.double(`2018`))) %>%
  mutate(pop_99_2009 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 10),
         pop_90_2009 = TCAM(datefin = pop2009, datedebut = pop1990, nbannee = 19),
         pop_75_2009 = TCAM(datefin = pop2009, datedebut = pop1975, nbannee = 34)) %>%
  mutate(educ_tcam_2009_2018 = TCAM(datefin = `2018`, datedebut = `2009`, nbannee = 9),
         educ_vol_2009_2018 = `2018` - `2009`)

tab_education_secondaire <- tab_education_secondaire %>%
  mutate(educ_tcam_2009_2018 = if_else(educ_tcam_2009_2018 == "Inf", 100, educ_tcam_2009_2018)) # cas de création d'un établissement pour 3 AU

tab_education_secondaire %>%
  filter(educ_tcam_2009_2018 != 0) %>%
  nrow()
# 22% des villes où l'on a un changement en termes de nb d'établissements

tab_education_secondaire %>%
  filter(educ_tcam_2009_2018 != 0) %>%
  select(pop_99_2009:educ_vol_2009_2018) %>%
  ggpairs()

ggsave(filename = "education_secondaire_ggpairs.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/demographie_services_publics/figures", dpi = 300,  width = 30, height = 25, units = "cm")

tab_education_secondaire %>%
  filter(educ_tcam_2009_2018 != 0 & educ_tcam_2009_2018 != 100) %>% # vire val extrêmes
  select(pop_99_2009:educ_vol_2009_2018) %>%
  ggpairs()

ggsave(filename = "education_secondaire_ggpairs_sans_val_extremes.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/demographie_services_publics/figures", dpi = 300,  width = 30, height = 25, units = "cm")
