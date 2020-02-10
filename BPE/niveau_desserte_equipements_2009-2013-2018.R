library(tidyverse) # need tidyr version>= 1.0.0
library(sf)
library(readxl)
library(ggthemes)
library(tmap)
library(patchwork)

source("fonctions_bases.R")
options(scipen=10000)

# -------------------------------- ECHELLE COMMUNALE ------------------------------------------------------

# -------------- import et réorganisation des données BPE 2018 ------------------
## fond France
france <- read_sf("BPE/data_communes_au/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse

## communes : population 2016 dans la délimitation 2019
communes_2019 <- read_sf("BPE/data_communes_au/COMMUNES.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  filter(INSEE_DEP %ni% c("2A", "2B")) %>% # sans la Corse
  st_set_crs(2154)

## BPE 2018 : ensemble et éducation
bpe_ensemble <- read.csv2("BPE/data/bpe18_ensemble.csv", stringsAsFactors = FALSE)
metadonnees_ensemble <- read.csv2("BPE/data/varmod_bpe18_ensemble.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# grouper les IRIS et lier avec les métadonnées pour sélectionner les ensembles qui nous intéressent
bpe_ensemble <- bpe_ensemble %>%
  group_by(DEPCOM, REG, TYPEQU) %>%
  summarise(NB_EQUIP = sum(NB_EQUIP)) %>%
  filter(REG %ni% c("1", "2", "3", "4", "6", "94")) # sans dom-tom & la Corse

bpe_ensemble <- left_join(x = bpe_ensemble, y = metadonnees_ensemble %>% select(COD_MOD, LIB_MOD), by = c("TYPEQU" = "COD_MOD"))

bpe_2018 <- left_join(x = bpe_ensemble, y = communes_2019 %>% select(INSEE_COM, POPULATION, TYPE, NOM_COM), by = c("DEPCOM" = "INSEE_COM")) %>%
  st_as_sf()

rm(bpe_ensemble)


# ---------------- part des communes équipées sur part de la pop desservie 2018 -------
bpe_2018_wide <- bpe_2018 %>%
  mutate(equip_1 = 1) %>%
  st_drop_geometry() %>%
  filter(TYPEQU %in% c("A101", "A104", "A105", "A106", "A107", "A108", "A109", "A115", "A199", "A120", 
                       "A121", "A122", "A123", "A124", "A125", "A126", "A206", "A207", "A208", "C201", 
                       "C301","C302", "C303", "C401", "D101", "D102", "D103", "D106", "D107")) %>%
  select(-TYPEQU, -NB_EQUIP) %>%
  pivot_wider(names_from = LIB_MOD, values_from = equip_1)

part_com_equipees_2018 <- bpe_2018_wide %>%
  ungroup() %>%
  select(-DEPCOM:-NOM_COM) %>% 
  fonction_part_equip_recup(x = ., y = communes_2019 %>% rename(id = "INSEE_COM"))

part_com_pop_equipees_2018 <- bpe_2018_wide %>%
  ungroup() %>%
  select(DEPCOM, Gendarmerie:`Cour d’appel (CA)`) %>%
  rename("id" = DEPCOM) %>%
  fonction_part_pop_recup(x = ., y = communes_2019 %>% st_drop_geometry() %>% rename(id = "INSEE_COM", pop = "POPULATION"))


tableau_global_2018_part <- bind_rows(part_com_equipees_2018[[2]], part_com_pop_equipees_2018[[2]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2018")

tableau_global_2018_volume <- bind_rows(part_com_equipees_2018[[1]], part_com_pop_equipees_2018[[1]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(com_equipee = "V1", pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2018")

write.csv2(part_com_equipees_2018[[1]], "BPE/sorties/part_unites_spatiales_equipements/communes_equipees_2018.csv")
write.csv2(part_com_pop_equipees_2018[[1]], "BPE/sorties/part_unites_spatiales_equipements/pop_equipee_2018.csv")

# ------------------ graphique 2018 ----------------------
tableau_global_2018_part %>%
  ggplot(aes(part_com_equipee, part_pop_equipee)) +
  geom_point() +
  xlab("Part des communes équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2018 (Insee), ADMIN EXPRESS 2019 (IGN)",
       subtitle = "Population desservie et communes équipées de services publics en France métropolitaine en 2018") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  )


# -------------- idem BPE 2009 et 2013 ------------------
## données population : géographie 2017 - ajout des arrondissements de Lyon et Marseille en 2009 et 2013 et suppression des 2 communes
population <- read_excel("BPE/data/populations_communales_arrondissements_donnees_temporelles_geographie_2017.xlsx") %>%
  filter(REG %ni% c("01", "02", "03", "04", "06", "94")) # sans dom-tom & la Corse

## BPE 2009
bpe_ensemble <- read_excel("BPE/data/bpe2009.xlsx") %>%
  mutate(dep = substr(INSEECOM, 1, 2), # récupération du numéro du département
         dep_3 = substr(INSEECOM, 1, 3)) %>% 
  filter(dep %ni% c("2A", "2B")) %>% # sans la Corse
  filter(dep_3 %ni% c("971", "972", "973", "974", "976")) %>% # sans régions & dep d'outre mer
  select(-dep, -dep_3)

bpe_2009 <- left_join(x = bpe_ensemble, y = communes_2019 %>% select(INSEE_COM, TYPE, NOM_COM), by = c("INSEECOM" = "INSEE_COM")) %>%
  st_as_sf()
rm(bpe_ensemble)

# ------------------ évolution 2009-2018 ----------------------------------
bpe_2009 <- bpe_2009 %>%
  select(INSEECOM, TYPE, NOM_COM, `Bureau de poste`, Gendarmerie, Police, `Collège`,
         `Etablissement santé court séjour`, `Etablissement santé long séjour`,
         `Etablissement santé moyen séjour`, `Maternité`, Urgences) %>%
  left_join(., y = population %>% select(CODGEO, PMUN09), by = c("INSEECOM" = "CODGEO")) %>%
  st_drop_geometry() %>%
  pivot_longer(names_to = "type_equip", values_to = "pres_abs", `Bureau de poste`:Urgences) %>%
  mutate(pres_abs = ifelse(is.na(pres_abs), NA, 1)) %>%
  pivot_wider(names_from = "type_equip", values_from = "pres_abs")

part_com_equipees_2009 <- bpe_2009 %>%
  select(-INSEECOM :-PMUN09) %>% 
  fonction_part_equip_recup(x = ., y = communes_2019 %>% rename(id = "INSEE_COM"))

communes_2019 <- left_join(x = communes_2019, y = population, by = c("INSEE_COM" = "CODGEO"))

part_com_pop_equipees_2009 <- bpe_2009 %>%
  select(INSEECOM, `Bureau de poste`:Urgences) %>%
  rename(id = "INSEECOM") %>%
  fonction_part_pop_recup(x = ., y = communes_2019 %>% st_drop_geometry() %>% rename(id = "INSEE_COM", pop = "PMUN09"))


tableau_global_2009 <- bind_rows(part_com_equipees_2009[[2]], part_com_pop_equipees_2009[[2]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2009")

# volume
tableau_global_2009_volume <- bind_rows(part_com_equipees_2009[[1]], part_com_pop_equipees_2009[[1]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(com_equipee = "V1", pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(pop_equipee = pop_equipee/1000000) %>%
  mutate(date = "2009")

write.csv2(part_com_equipees_2009[[1]], "BPE/sorties/part_unites_spatiales_equipements/communes_equipees_2009.csv")
write.csv2(part_com_pop_equipees_2009[[1]], "BPE/sorties/part_unites_spatiales_equipements/pop_equipee_2009.csv")

## graphique
tableau_global_2009 %>%
  ggplot(aes(part_com_equipee, part_pop_equipee)) +
  geom_point() +
  xlab("Part des communes équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2009 (Insee), ADMIN EXPRESS 2019 (IGN)",
       subtitle = "Population desservie et communes équipées de services publics en France métropolitaine en 2009") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  )


# ---------------- tableau global : 2009-2018 pour comparaison des parts --------------------
tableau_global_part <- tableau_global_2018_part %>%
  filter(type %in% c("Gendarmerie", "Bureau de poste", "Police", "Collège", "Établissement santé court séjour",
                     "Établissement santé long séjour", "Établissement santé moyen séjour", "Maternité", "Urgences")) %>%
  bind_rows(., tableau_global_2009)

# En volume :
tableau_global_volume <- tableau_global_2018_volume %>% 
  filter(type %in% c("Gendarmerie", "Bureau de poste", "Police", "Collège", "Établissement santé court séjour",
                     "Établissement santé long séjour", "Établissement santé moyen séjour", "Maternité", "Urgences")) %>%
  mutate(pop_equipee = pop_equipee/1000000) %>%
  bind_rows(., tableau_global_2009_volume)


## visualisation
a <- tableau_global_part %>%
  ggplot(aes(part_com_equipee, part_pop_equipee, color = date)) +
  geom_point() +
  scale_color_tableau(palette = "Tableau 10") +
  xlab("Part des communes équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  theme(legend.position = "none") + # forcer le fait de retirer la légende
  labs(subtitle = "Population desservie et communes équipées de services publics en France métropolitaine") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type, color = date), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  ) +
  expand_limits(x = 0, y = 0)


b <- tableau_global_volume %>%
  ggplot(aes(com_equipee, pop_equipee, color = date)) +
  geom_point() +
  scale_color_tableau(palette = "Tableau 10") +
  xlab("Nombre de communes équipées") +
  ylab("Population directement desservie (en millions)") +
  theme_julie() +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2009, 2018 (Insee), ADMIN EXPRESS 2019 (IGN)") +
  ggrepel::geom_text_repel(
    mapping = aes(com_equipee, pop_equipee, label = type, color = date), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  ) +
  expand_limits(x = 0, y = 0)


a + b


# -------------------- exploration cartographique : évolution 2009-2018 --------------------------
bpe_2009 <- left_join(x = bpe_2009, y = communes_2019 %>% select(INSEE_COM), by = c("INSEECOM" = "INSEE_COM")) %>%
  st_as_sf()

bpe_2018_wide <- bpe_2018_wide %>% 
  left_join(., y = communes_2019, by = c("DEPCOM" = "INSEE_COM")) %>% ungroup() %>%
  st_as_sf()

## empty geometries : communes qui ont fusionnées avec d'autres entre 2009 et 2019
any(is.na(st_dimension(bpe_2009)))
empty_2009 <- bpe_2009[st_is_empty(bpe_2009), , drop = FALSE] # cela concerne donc 4.62 % des communes
empty_2009 %>% filter(PMUN09 > 1000) %>% nrow() # 40 communes de plus de 1000 habitants en 2009

empty_2018 <- bpe_2018_wide[st_is_empty(bpe_2018_wide), , drop = FALSE] # soit 0.62 % des communes


## exploration carto par type de service

bpe_2009 %>% select(Police, INSEECOM) %>% 
  filter(Police == 1) %>%
  mutate(date = "2009") -> `2009`
bpe_2018_wide %>% 
  select(Police, DEPCOM) %>% 
  filter(Police == 1) %>%
  mutate(date = "2018") -> `2018`

tmap_mode("view")
tm_shape(`2009`) +
  tm_fill(col = "darkgreen", alpha = 1) +
  tm_borders(col = "black") +
  tm_shape(`2018`) +
  tm_fill(col = "darkorange", alpha = 1) +
  tm_borders(col = "black")


## visualisation : sorties cartographiques
periode_2009 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = `2009`, fill = "darkgreen", color = "darkgreen") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Communes desservies par un poste de police") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(subtitle = "2009")

periode_2018 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = `2018`, fill = "darkgreen", color = "darkgreen") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(subtitle = "2018")


diff_2009_2018 <- left_join(x = `2009`, y = `2018` %>% st_drop_geometry(), by = c("INSEECOM" = "DEPCOM"))
diff_2018_2009 <- left_join(x = `2018`, y = `2009` %>% st_drop_geometry(), by = c("DEPCOM" = "INSEECOM"))

couleurs <- c("disparition" = "firebrick4", "apparition" = "dodgerblue4")
ecart_2009_2018 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = diff_2009_2018 %>% filter(is.na(date.y)), aes(fill = "disparition", color = "disparition")) +
  geom_sf(data = diff_2018_2009 %>% filter(is.na(date.y)), aes(fill = "apparition", color = "apparition")) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : BPE 2009, 2018 (Insee), ADMIN EXPRESS 2019 (IGN)",
       subtitle = "Différence entre 2009 et 2018") +
  scale_fill_manual(name = "", values = couleurs) + # ajout légende manuellement
  scale_color_manual(name = "", values = couleurs) # idem pour les contours

# patchwork :
periode_2009 + periode_2018 + ecart_2009_2018

