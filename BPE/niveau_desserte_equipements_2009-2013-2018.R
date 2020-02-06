library(tidyverse) # need tidyr version>= 1.0.0
library(sf)
library(readxl)
library(ggthemes)
library(tmap)

source("fonctions_bases.R")

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


tableau_global_2018 <- bind_rows(part_com_equipees_2018[[2]], part_com_pop_equipees_2018[[2]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2018")

write.csv2(part_com_equipees_2018[[1]], "BPE/sorties/part_unites_spatiales_equipements/communes_equipees_2018.csv")
write.csv2(part_com_pop_equipees_2018[[1]], "BPE/sorties/part_unites_spatiales_equipements/pop_equipee_2018.csv")

# ------------------ graphique 2018 ----------------------
tableau_global_2018 %>%
  ggplot(aes(part_com_equipee, part_pop_equipee)) +
  geom_point() +
  xlab("Part des communes équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2018, Insee",
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

# exemple perso à revoir plus tard
bpe_2009 <- bpe_2009 %>%
  select(INSEECOM, TYPE, NOM_COM, `Bureau de poste`, Gendarmerie, Police) %>%
  left_join(., y = population %>% select(CODGEO, PMUN09), by = c("INSEECOM" = "CODGEO")) %>%
  pivot_longer(names_to = "type_equip", values_to = "pres_abs", `Bureau de poste`:Police) %>%
  mutate(pres_abs = ifelse(is.na(pres_abs), NA, 1)) %>%
  pivot_wider(names_from = "type_equip", values_from = "pres_abs")

## ---------- étude saint-julien
part_com_equipees_2009 <- bpe_2009 %>%
  select(-INSEECOM :-PMUN09) %>% 
  fonction_part_equip_recup(x = ., y = communes_2019 %>% rename(id = "INSEE_COM"))

communes_2019 <- left_join(x = communes_2019, y = population, by = c("INSEE_COM" = "CODGEO"))

part_com_pop_equipees_2009 <- bpe_2009 %>%
  select(INSEECOM, `Bureau de poste`:Police) %>%
  rename(id = "INSEECOM") %>%
  fonction_part_pop_recup(x = ., y = communes_2019 %>% st_drop_geometry() %>% rename(id = "INSEE_COM", pop = "PMUN09"))


tableau_global_2009 <- bind_rows(part_com_equipees_2009[[2]], part_com_pop_equipees_2009[[2]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
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
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2009, Insee",
       subtitle = "Population desservie et communes équipées de services publics en France métropolitaine en 2009") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  )



# --------- comparaison rapide 2009-2018 certains éléments
tableau_global <- tableau_global_2018 %>%
  filter(type %in% c("Gendarmerie", "Bureau de poste", "Police")) %>%
  bind_rows(., tableau_global_2009)


tableau_global %>%
  ggplot(aes(part_com_equipee, part_pop_equipee, color = date)) +
  geom_point() +
  xlab("Part des communes équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2009, Insee",
       subtitle = "Population desservie et communes équipées de services publics en France métropolitaine") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type, color = date), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  )


# carto rapide : à revoir
tmap_mode("view")

bpe_2009 %>% st_as_sf() %>% select(`Bureau de poste`) %>% filter(`Bureau de poste` == 1) %>%
  mutate(date = "2009") -> `2009`
bpe_2018_wide %>% 
  left_join(., y = communes_2019, by = c("DEPCOM" = "INSEE_COM")) %>% ungroup() %>%
  st_as_sf() %>% select(`Agence postale`) %>% 
  filter(`Agence postale` == 1) %>%
  mutate(date = "2018") -> `2018`

tm_shape(`2009`) +
  tm_fill(col = "darkgreen", alpha = 1) +
  tm_borders(col = "black") +
  tm_shape(`2018`) +
  tm_fill(col = "darkorange", alpha = 1) +
  tm_borders(col = "black")

## BPE 2013
bpe_ensemble <- read_excel("BPE/data/bpe2013.xlsx") %>%
  mutate(dep = substr(depcom, 1, 2)) %>% # récupération du numéro du département
  filter(dep %ni% c("2A", "2B")) %>% # sans la Corse
  select(-dep)

bpe_2013 <- left_join(x = bpe_ensemble, y = communes_2019 %>% select(INSEE_COM, TYPE, NOM_COM), by = c("depcom" = "INSEE_COM")) %>%
  st_as_sf()

rm(bpe_ensemble)


population <- population %>%
  filter(REG != "94")




