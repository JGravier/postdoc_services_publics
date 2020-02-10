library(tidyverse) # need tidyr version>= 1.0.0
library(sf)
library(readxl)
library(ggthemes)
library(tmap)
library(patchwork)
library(gplots)

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
  fonction_part_equip_recup(x = ., y = communes_2019 %>% 
                              filter(NOM_COM %ni% c("Paris", "Lyon", "Marseille")) %>%
                              rename(id = "INSEE_COM"))

part_com_pop_equipees_2018 <- bpe_2018_wide %>%
  ungroup() %>%
  select(DEPCOM, Gendarmerie:`Cour d’appel (CA)`) %>%
  rename("id" = DEPCOM) %>%
  fonction_part_pop_recup(x = ., y = communes_2019 %>% 
                            filter(NOM_COM %ni% c("Paris", "Lyon", "Marseille")) %>% 
                            # virer les communes car il y a aussi les arrondissements
                            st_drop_geometry() %>% rename(id = "INSEE_COM", pop = "POPULATION"))


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
  fonction_part_equip_recup(x = ., y = communes_2019 %>% 
                              filter(NOM_COM %ni% c("Paris", "Lyon", "Marseille")) %>%
                              rename(id = "INSEE_COM"))

communes_2019 <- left_join(x = communes_2019, y = population, by = c("INSEE_COM" = "CODGEO"))

part_com_pop_equipees_2009 <- bpe_2009 %>%
  select(INSEECOM, `Bureau de poste`:Urgences) %>%
  rename(id = "INSEECOM") %>%
  fonction_part_pop_recup(x = ., y = communes_2019 %>% 
                            filter(NOM_COM %ni% c("Paris", "Lyon", "Marseille")) %>%
                            st_drop_geometry() %>% rename(id = "INSEE_COM", pop = "PMUN09"))


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


# --------------------- ECHELLE DES COMMUNES DANS AIRES URBAINES ------------------------------------------
# ------------------------------ import et création des AU ------------------------------------------------
communes_2019_au_2010 <- read_excel("BPE/data_communes_au/AU2010_au_01-01-2019.xls", sheet = "data_composition_communale")
# PAris, Lyon et Marseille sont ici 3 communes ; pour faire le lien avec les arrondissements, on supprime ces trois communes
# puis ajout des lignes relatives aux arrondissements (création d'abord d'un tableau arrondissements)
paris <- tibble(
  dep = "751",
  com = c(1:20)
) %>%
  mutate(com = if_else(nchar(as.character(com)) == 1, true = paste("0", com, sep = ""), false = as.character(com))) %>%
  mutate(CODGEO = str_c(dep, com, sep = "")) %>%
  mutate(LIBGEO = "Paris",
         AU2010 = "001",
         LIBAU2010 = "Paris",
         CATAEU2010 = "111",
         DEP = "75",
         REG = "11") %>%
  select(-dep, -com)

marseille <- tibble(
  dep = "132",
  com = c(1:16)
) %>%
  mutate(com = if_else(nchar(as.character(com)) == 1, true = paste("0", com, sep = ""), false = as.character(com))) %>%
  mutate(CODGEO = str_c(dep, com, sep = "")) %>%
  mutate(LIBGEO = "Marseille",
         AU2010 = "003",
         LIBAU2010 = "Marseille",
         CATAEU2010 = "111",
         DEP = "13",
         REG = "93") %>%
  select(-dep, -com)

lyon <- tibble(
  dep = "693",
  com = c(81:89)
) %>%
  mutate(com = if_else(nchar(as.character(com)) == 1, true = paste("0", com, sep = ""), false = as.character(com))) %>%
  mutate(CODGEO = str_c(dep, com, sep = "")) %>%
  mutate(LIBGEO = "Lyon",
         AU2010 = "002",
         LIBAU2010 = "Lyon",
         CATAEU2010 = "111",
         DEP = "69",
         REG = "11") %>%
  select(-dep, -com)

arrondissements <- bind_rows(paris, marseille, lyon)

# lien avec le tableau initial
communes_2019_au_2010 <- communes_2019_au_2010 %>%
  filter(LIBGEO %ni% c("Paris", "Lyon", "Marseille")) %>%
  bind_rows(., arrondissements)
rm(paris, marseille, lyon, arrondissements)


## suite : AU
au_2010_2019 <- read_excel("BPE/data_communes_au/AU2010_au_01-01-2019.xls", sheet = "data_AU2010")

# lier les informations sur les AU à la composition communale, en l'occurrence "TAU2016", soit la tranche d'aire urbaine 2016
au_2010_2019_v2 <- au_2010_2019 %>%
  select(AU2010, TAU2016)

communes_2019_au_2010 <- left_join(x = communes_2019_au_2010, y = au_2010_2019_v2, by = "AU2010")
rm(au_2010_2019_v2)

# on n'a pas le même nombre de communes dans les deux tableaux (.shp = 34886 et .xls = 34970), 
# est-ce dû au fait que l'on a les communes situées hors France métropolitaine dans le fichier sur les aires urbaines ?
communes_2019_au_2010 <- communes_2019_au_2010 %>%
  filter(DEP %ni% c("971", "972", "973", "974", "976")) %>% # on retire les communes appartenant aux région et dép d'outre mer
  filter(DEP %ni% c("2A", "2B")) %>% # on retire la Corse
  mutate(INSEE_COM = CODGEO)
# une différence de 3 unités spatiales entre communes_2019 et ce fichiers du fait que l sf communes_2019
# a à la fois les arrondissements de Paris, Lyon, Marseille ET les communes

# création d'un objet sf des AU en France métropolitaine
communes_2019_au_2010 <- left_join(x = communes_2019_au_2010, y = communes_2019, by = "INSEE_COM") %>%
  st_sf()

au_2010_pop <- communes_2019_au_2010 %>%
  filter(CATAEU2010 %ni% c("300", "400", "120")) %>% # soit celle n'étant ni 300 = "Autre commune multipolarisée", 
  # ni 400 = "Commune isolée hors influence des pôles"
  # ni les 120 = "multipolarisées des grands pôles"
  st_set_precision(1000000) %>% 
  lwgeom::st_make_valid() %>% # il y a un pb dans les données de l'IGN, en gros une topologie qui n'est pas
  # bien enregistrée (avec une auto-intersection) ; solution trouvée sur https://github.com/r-spatial/sf/issues/603
  # l'idée étant d'amoindrir la précision spatiale pour que le group_by puisse fonctionner (sachant qu'on a ici une précision très très fine,
  # de l'odre du "subsubsubsub-centimetric if we are speaking of a metric projection" pour reprendre la phrase Lorenzo Busetto)
  group_by(AU2010) %>%
  summarise(population_2016 = sum(POPULATION), population_2009 = sum(PMUN09)) %>%
  left_join(x = ., y = au_2010_2019, by = "AU2010") %>%
  st_cast(x = ., to = "MULTIPOLYGON") # ce qui est bien pour faire de l'export en shp si besoin


## communes des aires urbaines
# création d'un objet sf des AU en France métropolitaine
communes_2019_au_2010 <- communes_2019_au_2010 %>%
  filter(CATAEU2010 %ni% c("300", "400", "120"))

# ------------------------------ exploration cartographique ------------------------------------------------
# BPE liée aux communes des aires urbaines
bpe_2009_au <- left_join(x = bpe_2009 %>% st_drop_geometry(), y = communes_2019_au_2010, by = c("INSEECOM" = "CODGEO")) %>%
  st_as_sf() %>%
  filter(!is.na(REG.y)) # virer les communes pas dans les aires urbaines

bpe_2018_wide_au <- bpe_2018_wide %>% 
  select(-ID:-PTOT1876) %>%
  st_drop_geometry() %>%
  left_join(., y = communes_2019_au_2010, by = c("DEPCOM" = "CODGEO")) %>% ungroup() %>%
  st_as_sf() %>%
  filter(!is.na(REG.y)) # virer les communes pas dans les aires urbaines

## empty geometries : communes qui ont fusionnées avec d'autres entre 2009 et 2019
any(is.na(st_dimension(bpe_2018_wide_au)))
# il n'y a pas de vide


## exploration carto par type de service

bpe_2009_au %>% select(Urgences, INSEECOM, LIBAU2010, STATUT, CATAEU2010) %>% 
  filter(Urgences == 1) %>%
  mutate(date = "2009") -> `2009`
bpe_2018_wide_au %>% 
  select(Urgences, DEPCOM, LIBAU2010, STATUT, CATAEU2010) %>% 
  filter(Urgences == 1) %>%
  mutate(date = "2018") -> `2018`

bpe_2009_au %>% select(Police, INSEECOM) %>% 
  filter(Police == 1) %>%
  mutate(date = "2009") -> police_2009

## visualisation : sorties cartographiques
periode_2009 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = `2009`, fill = "darkgreen", color = "darkgreen") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Communes desservies par un service d'urgences\ndans les aires urbaines") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(subtitle = "2009")

periode_2018 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
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
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
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


# ------------------------------ exploration statistique (bi) ------------------------------------------------
## Police et gendarmerie
diff_2009_2018 <- left_join(x = `2009`, y = `2018` %>% st_drop_geometry(), by = c("INSEECOM" = "DEPCOM")) %>%
  select(-LIBAU2010.y:-CATAEU2010.y) %>%
  st_drop_geometry()
diff_2018_2009 <- left_join(x = `2018`, y = `2009` %>% st_drop_geometry(), by = c("DEPCOM" = "INSEECOM")) %>%
  rename("INSEECOM" = "DEPCOM") %>%
  select(-LIBAU2010.y:-CATAEU2010.y) %>%
  filter(is.na(date.y)) %>%
  st_drop_geometry()

gendarmerie_police <- diff_2009_2018 %>% 
  # tableau des gendarmerie existantes en 2009 > adjonction de 2018
  bind_rows(., diff_2018_2009) %>% 
  # adjonction des lignes du tableau des "apparitions" entre 2009 et 2018
  mutate(evolution = if_else(condition = is.na(date.y) & date.x == "2009", 
                             true = "disparition", 
                             false = if_else(
                               condition = is.na(date.y) & date.x == "2018",
                               true = "apparition",
                               false = "maintien"
                             ))) %>%
  # création conditionnelle de l'évolution en 3 modalités > apparition, disparition, maintien
  mutate(type_commune_dans_au = if_else(CATAEU2010.x %in% c("111", "211", "221"), # voir métadonnées INSEE
                                        true = "pôle urbain", 
                                        false = "couronne")) %>%
  mutate(evolution_type_espace = str_c(evolution, type_commune_dans_au, sep = " ")) %>%
  left_join(., y = police_2009 %>% st_drop_geometry(), by = "INSEECOM") %>%
  mutate(pres_abs_police = if_else(is.na(Police), "non", "oui"))


# sortie du tableau
write.csv2(gendarmerie_police %>%
             select(-Gendarmerie.x, -Gendarmerie.y, -Police:-date) %>%
             rename(LIBAU2010 = LIBAU2010.x,
                    Statut = STATUT.x,
                    CATAEU2010 = CATAEU2010.x,
                    date1 = date.x,
                    date2 = date.y), 
           "BPE/sorties/part_unites_spatiales_equipements/khi_2_gendarmerie_police_tableau_elementaire.csv",
           fileEncoding = "UTF-8", row.names=FALSE)


gendarmerie_police <- table(gendarmerie_police$evolution_type_espace, gendarmerie_police$pres_abs_police)
khi_deux_gendarmerie_police <- gendarmerie_police %>% chisq.test()

khi_deux_gendarmerie_police[1:3]

write.csv2(khi_deux_gendarmerie_police$observed, "BPE/sorties/part_unites_spatiales_equipements/khi_2_gendarmerie_police_tableau.csv")
write.csv2(khi_deux_gendarmerie_police$expected, "BPE/sorties/part_unites_spatiales_equipements/khi_2_gendarmerie_police_attendu.csv")
write.csv2(khi_deux_gendarmerie_police$residuals, "BPE/sorties/part_unites_spatiales_equipements/khi_2_gendarmerie_police_residus_standardises.csv")

my_palette <- colorRampPalette(c("#ff7f0e", "white", "#1f83b4"))(n = 30)  # création de sa propre palette de couleur
heatmap.2(khi_deux_gendarmerie_police$residuals, Rowv = FALSE, Colv = FALSE, # on ne réordonne pas les lignes et les colonnes
          col = my_palette, # on utilise la palette de couleur que l'on a créé
          key = TRUE, denscol = "black", keysize = 1.2, key.title = "résidus\nstandardisés", density.info = "none",
          dendrogram = 'none', # on ne veut ni de réordonnancement des lignes et des colonnes, ni de dendrogramme tracé
          trace = 'none',
          margins = c(7, 14), cexRow = 1, cexCol = 1,
          main = "Communes des aires urbaines\n en France métropolitaine",
          xlab = "Présence d'un poste de Police en 2009",
          ylab = "Évolution de la présence d'une gendarmerie\n dans la commune entre 2009 et 2018")


# -------------------- part des communes équipées sur part de la pop desservie 2018 ------------------------------------------------
bpe_2018_wide_au <- bpe_2018_wide_au %>%
  st_drop_geometry()

part_com_au_equipees_2018 <- bpe_2018_wide_au %>%
  select(-DEPCOM:-NOM_COM.x, -LIBGEO.x:-PTOT1876) %>% 
  fonction_part_equip_recup(x = ., y = communes_2019_au_2010 %>%
                              rename(id = "INSEE_COM"))

part_com_pop_au_equipees_2018 <- bpe_2018_wide_au %>%
  select(-REG.x.x:-NOM_COM.x, -LIBGEO.x:-PTOT1876) %>%
  rename("id" = DEPCOM) %>%
  fonction_part_pop_recup(x = ., y = communes_2019_au_2010 %>%
                            st_drop_geometry() %>% 
                            rename(id = "INSEE_COM", pop = "POPULATION"))


tableau_global_au_2018_part <- bind_rows(part_com_au_equipees_2018[[2]], part_com_pop_au_equipees_2018[[2]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2018")

tableau_global_au_2018_volume <- bind_rows(part_com_au_equipees_2018[[1]], part_com_pop_au_equipees_2018[[1]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(com_equipee = "V1", pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2018")

write.csv2(part_com_au_equipees_2018[[1]], "BPE/sorties/part_unites_spatiales_equipements/communes_au_equipees_2018.csv")
write.csv2(part_com_pop_au_equipees_2018[[1]], "BPE/sorties/part_unites_spatiales_equipements/pop_equipee_au_2018.csv")

# ------------------ graphique 2018 : toutes les communes et uniquement celles des AU ----------------------
tab_com_au_2018_part_desservie <- tableau_global_au_2018_part %>%
  mutate(`communes des aires urbaines ?` = "oui, uniquement") %>%
  bind_rows(., tableau_global_2018_part %>% mutate(`communes des aires urbaines ?` = "toutes les communes"))


## graphique comparatif : dans AU et en général... pas très passionnant comme question !
# au moins on retrouve des évidences..
tab_com_au_2018_part_desservie %>%
  ggplot(aes(part_com_equipee, part_pop_equipee, color = `communes des aires urbaines ?`)) +
  geom_point() +
  scale_color_tableau(palette = "Tableau 10") +
  xlab("Part des communes équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  labs(subtitle = "Population desservie et communes équipées de services publics en France métropolitaine") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type, color = `communes des aires urbaines ?`), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  ) +
  expand_limits(x = 0, y = 0)

# -------------------- evo part et volume communes des AU : 2009-2018 --------------------------------------
part_com_au_equipees_2009 <- bpe_2009_au %>%
  st_drop_geometry() %>%
  select(`Bureau de poste`:Urgences) %>% 
  fonction_part_equip_recup(x = ., y = communes_2019_au_2010 %>%
                              rename(id = "INSEE_COM"))

part_com_pop_au_equipees_2009 <- bpe_2009_au %>%
  st_drop_geometry() %>%
  select(INSEECOM, `Bureau de poste`:Urgences) %>%
  rename(id = "INSEECOM") %>%
  fonction_part_pop_recup(x = ., y = communes_2019_au_2010 %>%
                            st_drop_geometry() %>% rename(id = "INSEE_COM", pop = "PMUN09"))


tableau_global_au_2009 <- bind_rows(part_com_au_equipees_2009[[2]], part_com_pop_au_equipees_2009[[2]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2009")

# volume
tableau_global_au_2009_volume <- bind_rows(part_com_au_equipees_2009[[1]], part_com_pop_au_equipees_2009[[1]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(com_equipee = "V1", pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(pop_equipee = pop_equipee/1000000) %>%
  mutate(date = "2009")

write.csv2(part_com_au_equipees_2009[[1]], "BPE/sorties/part_unites_spatiales_equipements/communes_au_equipees_2009.csv")
write.csv2(part_com_pop_au_equipees_2009[[1]], "BPE/sorties/part_unites_spatiales_equipements/pop_equipee_au_2009.csv")



##---------- tableau global relatif aux AU : 2009-2018
tableau_global_au_part <- tableau_global_au_2018_part %>%
  filter(type %in% c("Gendarmerie", "Bureau de poste", "Police", "Collège", "Établissement santé court séjour",
                     "Établissement santé long séjour", "Établissement santé moyen séjour", "Maternité", "Urgences")) %>%
  bind_rows(., tableau_global_au_2009)

# En volume :
tableau_global_au_volume <- tableau_global_au_2018_volume %>% 
  filter(type %in% c("Gendarmerie", "Bureau de poste", "Police", "Collège", "Établissement santé court séjour",
                     "Établissement santé long séjour", "Établissement santé moyen séjour", "Maternité", "Urgences")) %>%
  mutate(pop_equipee = pop_equipee/1000000) %>%
  bind_rows(., tableau_global_au_2009_volume)


## visualisation
a <- tableau_global_au_part %>%
  ggplot(aes(part_com_equipee, part_pop_equipee, color = date)) +
  geom_point() +
  scale_color_tableau(palette = "Traffic") +
  xlab("Part des communes équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  theme(legend.position = "none") + # forcer le fait de retirer la légende
  labs(subtitle = "Communes des aires urbaines équipées de services publics en France métropolitaine") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type, color = date), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  ) +
  expand_limits(x = 0, y = 0)


b <- tableau_global_au_volume %>%
  ggplot(aes(com_equipee, pop_equipee, color = date)) +
  geom_point() +
  scale_color_tableau(palette = "Traffic") +
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

# --------------------- ECHELLE DES AIRES URBAINES ------------------------------------------
bpe_2018_echelle_aire_urbaine <- bpe_2018_wide_au %>% 
  select(Gendarmerie:AU2010) %>% 
  group_by(AU2010) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  pivot_longer(cols = Gendarmerie:`Cour d’appel (CA)`, names_to = "name", values_to = "value") %>%
  mutate(value = if_else(condition = value > 0, true = 1, false = 0)) %>%
  pivot_wider(names_from = "name", values_from = "value")

bpe_2009_echelle_aire_urbaine <- bpe_2009_au %>%
  st_drop_geometry() %>%
  select(`Bureau de poste`:AU2010) %>%
  group_by(AU2010) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  pivot_longer(cols = `Bureau de poste`:Urgences, names_to = "name", values_to = "value") %>%
  mutate(value = if_else(condition = value > 0, true = 1, false = 0)) %>%
  pivot_wider(names_from = "name", values_from = "value")


# -------- part en 2018 des différents services publics --------------
part_au_equipee_2018 <- bpe_2018_echelle_aire_urbaine %>%
  select(-AU2010) %>% 
  fonction_part_equip_recup(x = ., y = au_2010_pop %>%
                              rename(id = "AU2010"))

part_com_pop_equipees_2018 <- bpe_2018_echelle_aire_urbaine %>%
  rename(id = "AU2010") %>%
  fonction_part_pop_recup(x = ., y = au_2010_pop %>%
                            st_drop_geometry() %>%
                            rename(id = "AU2010", pop = "population_2016"))


part_au_2018 <- bind_rows(part_au_equipee_2018[[2]], part_com_pop_equipees_2018[[2]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2018")

volume_au_2018 <- bind_rows(part_au_equipee_2018[[1]], part_com_pop_equipees_2018[[1]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(com_equipee = "V1", pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2018")

write.csv2(part_au_equipee_2018[[1]], "BPE/sorties/part_unites_spatiales_equipements/AU_equipees_2018.csv",
           fileEncoding = "UTF-8", row.names=FALSE)
write.csv2(part_com_pop_equipees_2018[[1]], "BPE/sorties/part_unites_spatiales_equipements/AU_pop_equipee_2018.csv",
           fileEncoding = "UTF-8", row.names=FALSE)

## graphique
part_au_2018 %>%
  ggplot(aes(part_com_equipee, part_pop_equipee)) +
  geom_point() +
  xlab("Part des aires urbaines équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2018 (Insee), ADMIN EXPRESS 2019 (IGN)",
       subtitle = "Population desservie et AU équipées de services publics en France métropolitaine en 2018") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  ) +
  expand_limits(x = 0, y = 0)

# -------------------- evo part et volume des AU équipées : 2009-2018 --------------------------------------
part_au_equipee_2009 <- bpe_2009_echelle_aire_urbaine %>%
  select(`Bureau de poste`:Urgences) %>% 
  fonction_part_equip_recup(x = ., y = au_2010_pop %>%
                              rename(id = "AU2010"))

part_pop_au_equipee_2009 <- bpe_2009_echelle_aire_urbaine %>%
  rename(id = "AU2010") %>%
  fonction_part_pop_recup(x = ., y = au_2010_pop %>%
                            st_drop_geometry() %>%
                            rename(id = "AU2010", pop = "population_2009"))


part_au_2009 <- bind_rows(part_au_equipee_2009[[2]], part_pop_au_equipee_2009[[2]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(date = "2009")

# volume
volume_au_2009 <- bind_rows(part_au_equipee_2009[[1]], part_pop_au_equipee_2009[[1]]) %>%
  t() %>%
  as.data.frame() %>%
  rename(com_equipee = "V1", pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as_tibble() %>%
  mutate(pop_equipee = pop_equipee/1000000) %>%
  mutate(date = "2009")

write.csv2(part_au_equipee_2009[[1]], "BPE/sorties/part_unites_spatiales_equipements/au_equipees_2009.csv",
           fileEncoding = "UTF-8", row.names=FALSE)
write.csv2(part_pop_au_equipee_2009[[1]], "BPE/sorties/part_unites_spatiales_equipements/au_pop_equipee_2009.csv",
           fileEncoding = "UTF-8", row.names=FALSE)



##---------- tableau global relatif aux AU : 2009-2018
au_global_part <- part_au_2018 %>%
  filter(type %in% c("Gendarmerie", "Bureau de poste", "Police", "Collège", "Établissement santé court séjour",
                     "Établissement santé long séjour", "Établissement santé moyen séjour", "Maternité", "Urgences")) %>%
  bind_rows(., part_au_2009)

# En volume :
au_global_volume <- volume_au_2018 %>% 
  filter(type %in% c("Gendarmerie", "Bureau de poste", "Police", "Collège", "Établissement santé court séjour",
                     "Établissement santé long séjour", "Établissement santé moyen séjour", "Maternité", "Urgences")) %>%
  mutate(pop_equipee = pop_equipee/1000000) %>%
  bind_rows(., volume_au_2009)


## visualisation
a <- au_global_part %>%
  ggplot(aes(part_com_equipee, part_pop_equipee, color = date)) +
  geom_point() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Part des aires urbaines équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  theme_julie() +
  theme(legend.position = "none") + # forcer le fait de retirer la légende
  labs(subtitle = "Aires urbaines équipées de services publics en France métropolitaine") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type, color = date), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  )


b <- au_global_volume %>%
  ggplot(aes(com_equipee, pop_equipee, color = date)) +
  geom_point() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Nombre d'aires urbaines équipées") +
  ylab("Population directement desservie (en millions)") +
  theme_julie() +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2009, 2018 (Insee), ADMIN EXPRESS 2019 (IGN)") +
  ggrepel::geom_text_repel(
    mapping = aes(com_equipee, pop_equipee, label = type, color = date), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  )


a + b
