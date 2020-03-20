library(tidyverse)
library(ggthemes)
library(sf)
library(tmap)

source("fonctions_bases.R")

# ----------------------------- tailles AU -----------------------------------
sf_services_publics_aires_urbaines <- read.csv("BDD_services_publics/data_sorties/services_publics_aires_urbaines_long.csv",
                                               stringsAsFactors = FALSE, header = TRUE, encoding = "Latin1")

typologie <- read.csv("BDD_services_publics/data_entrees/services_publics.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

typologie <- typologie %>%
  select(ID, RGPP, regalien) %>%
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

# voir de nouveau la différence de taille des AU au fil du temps
sf_services_publics_aires_urbaines %>%
  select(pop2016:pop1968, LIBAU2010, tailles_2016, tailles_1999) %>%
  unique() %>%
  filter(tailles_2016 != tailles_1999)

# sortie
evo_taille <- sf_services_publics_aires_urbaines %>%
  select(LIBAU2010, pop2016, pop1999, tailles_2016, tailles_1999) %>%
  unique() %>%
  filter(tailles_2016 != tailles_1999)
write.csv(evo_taille, "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evolution_tailles_villes_1999-2016.csv", row.names = FALSE)
rm(evo_taille)




