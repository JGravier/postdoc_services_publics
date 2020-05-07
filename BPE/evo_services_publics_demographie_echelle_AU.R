library(tidyverse)
library(ggthemes)
library(sf)
library(patchwork)
library(Cairo)


source("fonctions_bases.R")

# --------------------------- Hypothèse 2 : décroissance préalable, RGPP
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

france <- read_sf("BPE/data_communes_au/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse


# --------------------- panorama croissance et décroissance perso ---------------------------------
tabl_panorama <- sf_services_publics_aires_urbaines %>%
  select(AU2010:LIBAU2010, geometry) %>%
  unique() %>%
  mutate(pop_75_99 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop_99_09 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 20))

tab_classes <- tabl_panorama %>%
  rename(`1999-2009` = pop_99_09,
         `1975-1999` = pop_75_99) %>%
  pivot_longer(cols = `1975-1999`:`1999-2009`, names_to = "periodes", values_to = "tcam")

classes <- classIntervals(var = tab_classes$tcam, n = 7, style = "jenks")

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = tab_classes %>% 
            st_as_sf(wkt = "geometry", crs = 2154), 
          aes(fill = cut(tcam, classes$brks, include.lowest = TRUE)), show.legend = TRUE, size = 0.2) +
  scale_fill_brewer(name = "TCAM", palette = "RdYlBu", drop = FALSE, direction = -1) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2009, 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Situation démographique") +
  facet_wrap(~periodes)

ggsave(filename = "evo_pop_avant_2009.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/demographie_services_publics/figures", dpi = 300,  width = 30, height = 20, units = "cm")


# visu perso en qualitatif pour que ce soit plus claire
# à partir de Wollf 2017 (seuils croissance/maintien/déclin) : il table sur du -0.15 + 0.15 lui
tab_classes <- tab_classes %>%
  mutate(situation = if_else(tcam < -0.25, "décroissance",
                             if_else(tcam > 0.25, "croissance", "maintien")))

scales::show_col(tableau_color_pal(palette = "Traffic")(20))
palette <- c("#bd0a36", "#2c69b0", "#ffda66")

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = tab_classes %>% 
            st_as_sf(wkt = "geometry", crs = 2154), 
          aes(fill = situation), show.legend = TRUE, size = 0.2) +
  scale_fill_manual(values = palette) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2009, 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Situation démographique") +
  facet_wrap(~periodes)

ggsave(filename = "evo_pop_avant_2009_qualitatif.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/demographie_services_publics/figures", dpi = 300,  width = 30, height = 20, units = "cm")
