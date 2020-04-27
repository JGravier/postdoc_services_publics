library(tidyverse)
library(ggthemes)
library(sf)
library(classInt)

source("fonctions_bases.R")

# ----------------------------- data : tailles AU -----------------------------------
sf_services_publics_aires_urbaines <- read.csv("BPE/sorties/tailles_villes_places_services_publics/entrees_data/services_publics_aires_urbaines_long.csv",
                                               stringsAsFactors = FALSE, header = TRUE, encoding = "Latin1")

typologie <- read.csv("BPE/sorties/tailles_villes_places_services_publics/entrees_data/services_publics.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

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

sf_services_publics_aires_urbaines <- st_as_sf(x = sf_services_publics_aires_urbaines, wkt = "geometry", crs = 2154)

# -------- analyse des emplois sans conseillers spécialisés : ---------
data_communes_des_au <- read.csv(file = "BPE/sorties/tailles_villes_places_services_publics/entrees_data/services_publics_communes_des_aires_urbaines_long.csv", 
                                 header = TRUE, encoding = "LAtin1", stringsAsFactors = FALSE) %>%
  as_tibble()

data_communes_des_au <- data_communes_des_au %>%
  mutate(depcom = if_else(nchar(depcom) == 4, paste(0, depcom, sep = ""), as.character(depcom))) %>%
  mutate(AU2010 = as.character(AU2010)) %>%
  left_join(., y = sf_services_publics_aires_urbaines %>% select(AU2010, tailles_2016) %>% st_drop_geometry() %>% unique(), by = "AU2010")

data_communes_des_au <- st_as_sf(x = data_communes_des_au, wkt = "geometry", crs = 2154)

data_communes_des_au_emploi <- data_communes_des_au %>%
  filter(typologie == "service de l'emploi sans conseiller spécialisé") %>%
  mutate(espace_au = if_else(CATAEU2010 %in% c(111, 211, 221), "Pôle", "Couronne"))

data_communes_des_au_emploi %>%
  group_by(tailles_2016, espace_au, annee) %>%
  summarise(sum(nb_equip)) %>%
  rename(nb_equip = `sum(nb_equip)`) %>%
  ungroup() %>%
  group_by(tailles_2016, annee) %>%
  mutate(pourc_tailles = nb_equip/sum(nb_equip)*100) %>%
  ggplot(aes(x = as.character(annee), y = pourc_tailles, fill = espace_au, label = nb_equip)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = pourc_tailles + 5), position = position_dodge(width = 1), color = "grey20", size = 3.5) +
  scale_fill_tableau(palette = "Green-Orange-Teal", name = "Espace intra-urbain") +
  theme_julie() +
  theme(axis.title.x = element_blank()) +
  ylab("Pourcentage") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution des services de l'emploi sans conseiller spécialisé") +
  facet_wrap(~ fct_rev(tailles_2016))

ggsave(filename = "evo_emplois_sans_conseiller.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 25, height = 15, units = "cm")

# cartographie
data_communes_des_au_emploi_2 <- data_communes_des_au_emploi %>%
  select(depcom, annee, nb_equip, INSEE_REG, LIBGEO) %>%
  st_drop_geometry() %>%
  pivot_wider(id_cols = depcom, names_from = "annee", values_from = "nb_equip") %>%
  mutate(`2018` = if_else(is.na(`2018`), 0, as.double(`2018`)),
         `2013` = if_else(is.na(`2013`), 0, as.double(`2013`))) %>%
  mutate(evo_emplois = (`2018`-`2013`)/`2013`*100) %>%
  mutate(evo_emplois = if_else(evo_emplois == "Inf", 100, as.double(evo_emplois))) %>%
  left_join(., x = data_communes_des_au_emploi %>% select(depcom, geometry) %>% unique(), by = "depcom") %>%
  st_as_sf()

classes <- classIntervals(var = data_communes_des_au_emploi_2$evo_emplois, n = 5, style = "jenks") # discrétisation de Jenks
classes$brks[4] <- -1
classes$brks[5] <- 1

ma_palette <- c("#4575b4", "#91bfdb", "#e0f3f8", "#ffffbf", "#d73027")


france <- read_sf("BPE/data_communes_au/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse

evo_pop_aire_urbaine <- sf_services_publics_aires_urbaines %>%
  select(AU2010:LIBAU2010, tailles_2016, geometry) %>%
  unique()

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = evo_pop_aire_urbaine, fill = "grey80", color = "grey70") +
  geom_sf(data = data_communes_des_au_emploi_2, 
          aes(fill = cut(evo_emplois, classes$brks, include.lowest = TRUE))) +
  scale_fill_manual(name = "Taux de variation", values = ma_palette) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services de l'emploi sans conseiller spécialisé")

ggsave(filename = "evo_emplois_sans_conseiller_carto.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 25, height = 30, units = "cm")

# Bretagne :
ggplot() +
  geom_sf(data = france %>% filter(NOM_REG == "Bretagne"), fill = "grey98", color = "grey50") +
  geom_sf(data = evo_pop_aire_urbaine %>%
            left_join(., y = data_communes_des_au %>% 
                        select(AU2010, INSEE_REG) %>%
                        st_drop_geometry() %>%
                        unique(), by = "AU2010") %>%
            filter(INSEE_REG == 53), 
          fill = "grey80", color = "grey70") +
  geom_sf(data = data_communes_des_au_emploi_2 %>%
            left_join(., y = data_communes_des_au %>% 
                        select(depcom, INSEE_REG) %>%
                        st_drop_geometry() %>%
                        unique(), by = "depcom") %>%
            filter(INSEE_REG == 53), 
          aes(fill = cut(evo_emplois, classes$brks, include.lowest = TRUE))) +
  scale_fill_manual(name = "Taux de variation", values = ma_palette) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services de l'emploi sans conseiller spécialisé en Bretagne")

ggsave(filename = "evo_emplois_sans_conseiller_carto_bretagne.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 20, height = 15, units = "cm")

# Aire urbaine de Paris :
departements <- read_sf("BPE/data_communes_au/DEPARTEMENT.shp", stringsAsFactors = FALSE, options = "ENCODING=Latin1") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(CODE_DEPT %ni% c("2A", "2B")) # sans la Corse


ggplot() +
  geom_sf(data = evo_pop_aire_urbaine %>% filter(LIBAU2010 == "Paris"), fill = "grey80", color = "grey70") +
  geom_sf(data = departements %>% 
            filter(NOM_REGION == "ILE-DE-FRANCE"), 
          fill = "grey80", color = "grey50", alpha = 0.1) +
  geom_sf(data = data_communes_des_au_emploi_2 %>%
            left_join(., y = data_communes_des_au %>% 
                        select(depcom, AU2010, INSEE_REG) %>%
                        st_drop_geometry() %>%
                        unique(), by = "depcom") %>%
            filter(AU2010 == 1), 
          aes(fill = cut(evo_emplois, classes$brks, include.lowest = TRUE))) +
  scale_fill_manual(name = "Taux de variation", values = ma_palette) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services de l'emploi sans conseiller spécialisé dans l'aire urbaine de Paris")

ggsave(filename = "evo_emplois_sans_conseiller_carto_AU_Paris.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 20, height = 15, units = "cm")


# -------- emplois avec et sans conseillers spécialisés : ---------
data_communes_des_au_emploi <- data_communes_des_au %>%
  filter(typologie %in% c("service de l'emploi sans conseiller spécialisé", "service de l'emploi avec conseiller spécialisé")) %>%
  mutate(espace_au = if_else(CATAEU2010 %in% c(111, 211, 221), "Pôle", "Couronne"))

data_communes_des_au_emploi %>%
  group_by(tailles_2016, espace_au, annee, typologie) %>%
  summarise(sum(nb_equip)) %>%
  rename(nb_equip = `sum(nb_equip)`) %>%
  ungroup() %>%
  group_by(tailles_2016, annee, typologie) %>%
  mutate(pourc_tailles = nb_equip/sum(nb_equip)*100) %>%
  ggplot(aes(x = as.character(annee), y = pourc_tailles, fill = espace_au, label = nb_equip)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = pourc_tailles + 5), position = position_dodge(width = 1), color = "grey20", size = 3.5) +
  scale_fill_tableau(palette = "Green-Orange-Teal", name = "Espace intra-urbain") +
  theme_julie() +
  theme(axis.title.x = element_blank()) +
  ylab("Pourcentage") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution des services de l'emploi") +
  facet_grid(typologie ~ fct_rev(tailles_2016))

ggsave(filename = "evo_emplois_avec_et_sans_conseiller.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 30, height = 25, units = "cm")

# cartographie
data_communes_des_au_emploi_2 <- data_communes_des_au_emploi %>%
  select(depcom, annee, nb_equip, INSEE_REG, LIBGEO, typologie) %>%
  mutate(depcom2 = paste(depcom, typologie, sep = "_")) %>%
  st_drop_geometry() %>%
  pivot_wider(id_cols = depcom2, names_from = "annee", values_from = "nb_equip") %>%
  mutate(`2018` = if_else(is.na(`2018`), 0, as.double(`2018`)),
         `2013` = if_else(is.na(`2013`), 0, as.double(`2013`))) %>%
  mutate(evo_emplois = (`2018`-`2013`)/`2013`*100) %>%
  mutate(evo_emplois = if_else(evo_emplois == "Inf", 100, as.double(evo_emplois))) %>%
  separate(col = depcom2, into = c("depcom", "typologie"), sep = "_") %>%
  left_join(., x = data_communes_des_au_emploi %>% select(depcom, geometry) %>% unique(), by = "depcom") %>%
  st_as_sf()

classes <- classIntervals(var = data_communes_des_au_emploi_2$evo_emplois, n = 7, style = "jenks") # discrétisation de Jenks
classes$brks[5] <- -1
classes$brks[6] <- +1

ma_palette <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#fee090", "#f46d43", "#a50026")

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = evo_pop_aire_urbaine, fill = "grey80", color = "grey70") +
  geom_sf(data = data_communes_des_au_emploi_2, 
          aes(fill = cut(evo_emplois, classes$brks, include.lowest = TRUE))) +
  scale_fill_manual(name = "Taux de variation", values = ma_palette) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services de l'emploi") +
  facet_wrap(~typologie)

ggsave(filename = "evo_emplois_avec_et_sans_conseiller_carto.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 25, height = 20, units = "cm")

# Bretagne :
ggplot() +
  geom_sf(data = france %>% filter(NOM_REG == "Bretagne"), fill = "grey98", color = "grey50") +
  geom_sf(data = evo_pop_aire_urbaine %>%
            left_join(., y = data_communes_des_au %>% 
                        select(AU2010, INSEE_REG) %>%
                        st_drop_geometry() %>%
                        unique(), by = "AU2010") %>%
            filter(INSEE_REG == 53), 
          fill = "grey80", color = "grey70") +
  geom_sf(data = data_communes_des_au_emploi_2 %>%
            left_join(., y = data_communes_des_au %>% 
                        select(depcom, INSEE_REG) %>%
                        st_drop_geometry() %>%
                        unique(), by = "depcom") %>%
            filter(INSEE_REG == 53), 
          aes(fill = cut(evo_emplois, classes$brks, include.lowest = TRUE))) +
  scale_fill_manual(name = "Taux de variation", values = ma_palette) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services de l'emploi") +
  facet_wrap(~typologie)

ggsave(filename = "evo_emplois_avec_et_sans_conseiller_carto_bretagne.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 20, height = 20, units = "cm")

# voir la différence avec les données pole emploi
pole_emploi_bretagne <- read_sf("BPE/sorties/tailles_villes_places_services_publics/entrees_data/pole_emploi.shp", stringsAsFactors = FALSE, options = "ENCODING=Latin1") %>%
  st_transform(crs = 2154) %>%
  mutate(typologie = "service de l'emploi avec conseiller spécialisé")

MSAP_pole_emploi <- read_sf("BPE/sorties/tailles_villes_places_services_publics/entrees_data/MSAP-20180627_geolocalisation_pole_emploi.shp", stringsAsFactors = FALSE, options = "ENCODING=Latin1") %>%
  st_transform(crs = 2154) %>%
  mutate(typologie = "service de l'emploi sans conseiller spécialisé")

reseau_revu <- read_sf("BPE/sorties/tailles_villes_places_services_publics/entrees_data/reseau_partenarial_pole_emploi_2018.shp", stringsAsFactors = FALSE, options = "ENCODING=Latin1") %>%
  st_set_crs(2154) %>%
  mutate(typologie = "service de l'emploi sans conseiller spécialisé") %>%
  st_centroid()

reseau_revu_2013 <- read_sf("BPE/sorties/tailles_villes_places_services_publics/entrees_data/reseau_partenarial_pole_emploi_2013.shp", stringsAsFactors = FALSE, options = "ENCODING=Latin1") %>%
  st_set_crs(2154) %>%
  mutate(typologie = "service de l'emploi sans conseiller spécialisé") %>%
  st_centroid()

ggplot() +
  geom_sf(data = france %>% filter(NOM_REG == "Bretagne"), fill = "grey98", color = "grey50") +
  geom_sf(data = evo_pop_aire_urbaine %>%
            left_join(., y = data_communes_des_au %>% 
                        select(AU2010, INSEE_REG) %>%
                        st_drop_geometry() %>%
                        unique(), by = "AU2010") %>%
            filter(INSEE_REG == 53), 
          fill = "grey80", color = "grey70") +
  geom_sf(data = data_communes_des_au_emploi_2 %>%
            left_join(., y = data_communes_des_au %>% 
                        select(depcom, INSEE_REG) %>%
                        st_drop_geometry() %>%
                        unique(), by = "depcom") %>%
            filter(INSEE_REG == 53), 
          aes(fill = cut(evo_emplois, classes$brks, include.lowest = TRUE))) +
  geom_sf(data = pole_emploi_bretagne, size = 1) +
  geom_sf(data = reseau_revu_2013 %>% filter(reg == "53"), aes(color = typequ), size = 1) +
  scale_color_tableau(palette = "Traffic", name = "Type équipement en 2013") +
  scale_fill_manual(name = "Taux de variation", values = ma_palette) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services de l'emploi") +
  facet_wrap(~typologie)

ggsave(filename = "evo_emplois_avec_et_sans_conseiller_carto_bretagne_donnees_pole_emploi_revu_2013.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 25, height = 25, units = "cm")

# AU de PAris
ggplot() +
  geom_sf(data = evo_pop_aire_urbaine %>% filter(LIBAU2010 == "Paris"), fill = "grey80", color = "grey70") +
  geom_sf(data = departements %>% 
            filter(NOM_REGION == "ILE-DE-FRANCE"), 
          fill = "grey80", color = "grey50", alpha = 0.1) +
  geom_sf(data = data_communes_des_au_emploi_2 %>%
            left_join(., y = data_communes_des_au %>% 
                        select(depcom, AU2010, INSEE_REG) %>%
                        st_drop_geometry() %>%
                        unique(), by = "depcom") %>%
            filter(AU2010 == 1), 
          aes(fill = cut(evo_emplois, classes$brks, include.lowest = TRUE))) +
  scale_fill_manual(name = "Taux de variation", values = ma_palette) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services de l'emploidans l'aire urbaine de Paris") +
  facet_wrap(~typologie)

ggsave(filename = "evo_emplois_avec_et_sans_conseiller_carto_AU_Paris.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.4.services_emplois_sans_conseiller", device = "png",
       width = 25, height = 20, units = "cm")

