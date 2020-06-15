library(tidyverse) # need tidyr version>= 1.0.0
library(sf)
library(readxl)
library(ggthemes)
library(tmap)
library(patchwork)
library(gplots)
library(classInt)

source("fonctions_bases.R")

# -------------------------------- IMPORT DONNEES, MISE EN FORME ------------------------------------------------------

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

bpe_2018_wide <- bpe_2018 %>%
  mutate(equip_1 = 1) %>%
  st_drop_geometry() %>%
  filter(TYPEQU %in% c("A101", "A104", "A105", "A106", "A107", "A108", "A109", "A115", "A199", "A120", 
                       "A121", "A122", "A123", "A124", "A125", "A126", "A206", "A207", "A208", "C201", 
                       "C301","C302", "C303", "C401", "D101", "D102", "D103", "D106", "D107")) %>%
  select(-TYPEQU, -NB_EQUIP) %>%
  pivot_wider(names_from = LIB_MOD, values_from = equip_1)

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

bpe_2009 <- bpe_2009 %>%
  select(INSEECOM, TYPE, NOM_COM, `Bureau de poste`, Gendarmerie, Police, `Collège`,
         `Etablissement santé court séjour`, `Etablissement santé long séjour`,
         `Etablissement santé moyen séjour`, `Maternité`, Urgences) %>%
  left_join(., y = population %>% select(CODGEO, PMUN09), by = c("INSEECOM" = "CODGEO")) %>%
  st_drop_geometry() %>%
  pivot_longer(names_to = "type_equip", values_to = "pres_abs", `Bureau de poste`:Urgences) %>%
  mutate(pres_abs = ifelse(is.na(pres_abs), NA, 1)) %>%
  pivot_wider(names_from = "type_equip", values_from = "pres_abs")

# --------------------- ECHELLE DES COMMUNES DANS AIRES URBAINES ------------------------------------------
# ------------------------------ import et création des AU ------------------------------------------------
communes_2019 <- left_join(x = communes_2019, y = population, by = c("INSEE_COM" = "CODGEO"))
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
bpe_2009_au <- left_join(x = bpe_2009, y = communes_2019_au_2010, by = c("INSEECOM" = "CODGEO")) %>%
  st_as_sf() %>%
  filter(!is.na(REG.y)) # virer les communes pas dans les aires urbaines

bpe_2018_wide_au <- bpe_2018_wide %>% 
  left_join(., y = communes_2019_au_2010, by = c("DEPCOM" = "CODGEO")) %>% ungroup() %>%
  st_as_sf() %>%
  filter(!is.na(REG.y)) # virer les communes pas dans les aires urbaines

## empty geometries : communes qui ont fusionnées avec d'autres entre 2009 et 2019
any(is.na(st_dimension(bpe_2009_au)))
# il n'y a pas de vide


## exploration carto par type de service
bpe_2009_au %>% select(`Bureau de poste`, INSEECOM, LIBAU2010, STATUT, CATAEU2010) %>% 
  filter(`Bureau de poste` == 1) %>%
  mutate(date = "2009") -> `2009`
bpe_2018_wide_au %>% 
  select(`Bureau de poste`, DEPCOM, LIBAU2010, STATUT, CATAEU2010) %>% 
  filter(`Bureau de poste` == 1) %>%
  mutate(date = "2018") -> `2018`


## visualisation : sorties cartographiques
periode_2009 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = au_2010_pop, fill = "grey85", color = "grey80", size = 0.1) +
  geom_sf(data = `2009`, fill = "#252525", color = "#252525", size = 0.02) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Communes desservies par un bureau de Poste\ndans les aires urbaines") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(subtitle = "2009")

periode_2018 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = `2018`, fill = "#252525", color = "#252525", size = 0.02) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(subtitle = "2018")


diff_2009_2018 <- left_join(x = `2009`, y = `2018` %>% st_drop_geometry(), by = c("INSEECOM" = "DEPCOM"))
diff_2018_2009 <- left_join(x = `2018`, y = `2009` %>% st_drop_geometry(), by = c("DEPCOM" = "INSEECOM"))

couleurs <- c("disparition" = "#b2182b", "apparition" = "#2166ac")
ecart_2009_2018 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = diff_2009_2018 %>% filter(is.na(date.y)), 
          aes(fill = "disparition", color = "disparition"), size = 0.02) +
  geom_sf(data = diff_2018_2009 %>% filter(is.na(date.y)), 
          aes(fill = "apparition", color = "apparition"), size = 0.02) +
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
(periode_2009 | periode_2018)
library(Cairo)

ggsave(filename = "poste_bureau_evo1.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/CIST2020_revu/", dpi = 300,  width = 30, height = 20, units = "cm")

ecart_2009_2018
ggsave(filename = "poste_bureau_evo2.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/CIST2020_revu/", dpi = 300,  width = 20, height = 15, units = "cm")

# ----------------------------- ECHELLE DES AIRES URBAINES -----------------------------------------------------
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
  select(-AU2010, -geometry) %>% 
  fonction_part_equip_recup(x = ., y = au_2010_pop %>%
                              rename(id = "AU2010"))

part_com_pop_equipees_2018 <- bpe_2018_echelle_aire_urbaine %>%
  select(-geometry) %>%
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
ggsave(filename = "Figure1.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/CIST2020_revu/", dpi = 300,  width = 30, height = 20, units = "cm")


# ------------------------------ exploration cartographique ------------------------------------------------
# BPE liée aux aires urbaines
bpe_2009_echelle_aire_urbaine <- bpe_2009_echelle_aire_urbaine %>%
  left_join(x = ., y = au_2010_pop, by = "AU2010") %>%
  st_as_sf()

bpe_2018_echelle_aire_urbaine <- bpe_2018_echelle_aire_urbaine %>% 
  left_join(., y = au_2010_pop, by = "AU2010") %>%
  st_as_sf()

## empty geometries : communes qui ont fusionnées avec d'autres entre 2009 et 2019
any(is.na(st_dimension(bpe_2009_echelle_aire_urbaine)))
# il n'y a pas de vide


## exploration carto par type de service
bpe_2009_echelle_aire_urbaine %>% 
  select(AU2010, Maternité) %>% 
  filter(Maternité == 1) %>%
  mutate(date = "2009") -> `2009`
bpe_2018_echelle_aire_urbaine %>% 
  select(AU2010, Maternité) %>% 
  filter(Maternité == 1) %>%
  mutate(date = "2018") -> `2018`


## visualisation : sorties cartographiques
periode_2009 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = `2009`, fill = "gray30", color = "gray30") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Aires urbaines desservies par\nune maternité") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(subtitle = "2009")

periode_2018 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = `2018`, fill = "gray30", color = "gray30") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(subtitle = "2018")


diff_2009_2018 <- left_join(x = `2009`, y = `2018` %>% st_drop_geometry(), by = "AU2010")
diff_2018_2009 <- left_join(x = `2018`, y = `2009` %>% st_drop_geometry(), by = "AU2010")

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


# ---------------- PART DES COMMUNES où les habitants vont vers 1 autre commune pour accéder à 1 ou des services ------
## exploration carto par type de service
communes_2019_au_2010 %>% 
  st_drop_geometry() %>%
  select(CODGEO, AU2010) %>%
  left_join(., y = bpe_2018_wide_au, by = c("CODGEO" = "DEPCOM")) %>%
  select(`Bureau de poste`, CODGEO, AU2010.x, LIBAU2010, STATUT, CATAEU2010) %>%
  mutate(avec_sans = if_else(
    condition = `Bureau de poste` == 1, true = "avec_equipements", false = "sans_equipements"
  )) %>% 
  mutate(avec_sans_boolean = if_else(is.na(avec_sans), true = 1, false = 0)) %>%
  group_by(AU2010.x) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(date = "2018") -> communes_avec_sans_equipement_au_2018

communes_2019_au_2010 %>% 
  st_drop_geometry() %>%
  select(CODGEO, AU2010) %>%
  left_join(., y = bpe_2009_au, by = c("CODGEO" = "INSEECOM")) %>%
  select(`Bureau de poste`, CODGEO, AU2010.x, LIBAU2010, STATUT, CATAEU2010) %>%
  mutate(avec_sans = if_else(
    condition = `Bureau de poste` == 1, true = "avec_equipements", false = "sans_equipements"
  )) %>%
  mutate(avec_sans_boolean = if_else(is.na(avec_sans), true = 1, false = 0)) %>%
  group_by(AU2010.x) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(date = "2009") -> communes_avec_sans_equipement_au_2009

communes_avec_sans_equipement_au_2009_2018 <- bind_rows(communes_avec_sans_equipement_au_2018, communes_avec_sans_equipement_au_2009) %>%
  left_join(., y = au_2010_pop, by = c("AU2010.x" = "AU2010")) %>%
  mutate(pourc_sans_equipements = avec_sans_boolean/NB_COM*100) %>%
  filter(pourc_sans_equipements != 0) %>%
  st_as_sf()

classes <- classIntervals(var = communes_avec_sans_equipement_au_2009_2018$pourc_sans_equipements, n = 7, style = "jenks") # discrétisation de Jenks

# France : cartographie
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = au_2010_pop, fill = "darkorange", color = "grey40", size = 0.15) +
  geom_sf(data = communes_avec_sans_equipement_au_2009_2018, 
          aes(fill = cut(pourc_sans_equipements, classes$brks, include.lowest = TRUE)), 
          show.legend = TRUE,
          size = 0.15) +
  scale_fill_brewer(name = "Part des communes où les habitants\nvont vers une autre commune\npour accéder au service étudié\n(en orange : aires entièrement équipées)", 
                    palette = "Purples", drop = FALSE, direction = 1) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme_julie() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : BPE 2009, 2018 (Insee), ADMIN EXPRESS 2019 (IGN)") +
  facet_wrap(~date)

ggsave(filename = "Figure4.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/CIST2020_revu/", dpi = 300,  width = 30, height = 20, units = "cm")

# ----------------------- Bureaux de poste : comparaison tailles de villes 2009-2018 -------------------------------------
bpe_2009_2018_poste <- left_join(x = bpe_2009_au %>% st_drop_geometry() %>% select(INSEECOM:`Bureau de poste`, LIBGEO.x, AU2010, LIBAU2010, POPULATION), 
                                 y = bpe_2018_wide_au %>% st_drop_geometry() %>% select(DEPCOM, `Bureau de poste`), 
                                 by = c("INSEECOM" = "DEPCOM"))
  
  
bpe_2009_2018_poste <- bpe_2009_2018_poste %>%
  rename(bureau_2009 = `Bureau de poste.x`,
         bureau_2018 = `Bureau de poste.y`)

bpe_2009_2018_poste_au <- bpe_2009_2018_poste %>%
  group_by(AU2010) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(bureau_2009 = ifelse(bureau_2009 == 0 & bureau_2018 != 0, 0.001, bureau_2009),
         # nécessaire de transformer les 0 en 0.001 dans les cas où il n'y avait pas d'activité postale en 2013
         # mais qu'il y en a une en 2018. Sinon le calcul des TCAM ne peut pas être effectué dans ce cas de croissance
         TCAM = TCAM(datefin = bureau_2018, datedebut = bureau_2009, nbannee = 8)) # taux de croissance annuel moyen

bpe_2009_2018_poste_au <- bpe_2009_2018_poste_au %>%
  mutate(tailles_2009 = if_else(PMUN09.x < 30000, "petite\n(< 30.000 hab.)",
                                if_else(PMUN09.x > 200000, "grande\n(> 200.000 hab.)", "moyenne"))) %>%
  mutate(tailles_2018 = if_else(POPULATION < 30000, "petite\n(< 30.000 hab.)",
                                if_else(POPULATION > 200000, "grande\n(> 200.000 hab.)", "moyenne")))

#### distributions
bpe_2009_2018_poste_au %>%
  filter(AU2010 != "001" & TCAM != "NaN") %>% # enlever Paris car peu pertinent il me semble
  rename(`2009` = tailles_2009, `2018` = tailles_2018) %>%
  pivot_longer(names_to = "annee", values_to = "population", cols = starts_with("20")) %>%
  ggplot(aes(x = population, y = TCAM, fill = population)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 10") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2019-2018, Insee") +
  ggtitle("Bureaux de poste des aires urbaines entre 2009 et 2018 (hors Paris)") +
  facet_wrap(~ annee)

# distributions sans les valeurs extrêmes de croissance
bpe_2009_2018_poste_au %>%
  filter(TCAM != "NaN") %>% # enlever Paris car peu pertinent il me semble
  filter(TCAM < 100 & TCAM > -100) %>% 
  ggplot(aes(x = tailles_2009, y = TCAM, fill = tailles_2009)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 10") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2009-2018, Insee")

ggsave(filename = "Figure2.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/CIST2020_revu/", dpi = 300,  width = 20, height = 15, units = "cm")




#-------- Police et gendarmerie -----------
## exploration carto par type de service
bpe_2009_au %>% 
  select(Gendarmerie, Police, INSEECOM, LIBAU2010, STATUT, CATAEU2010) %>% 
  filter(Police == 1) %>%
  mutate(date = "2009") -> `2009`

## visualisation : sorties cartographiques
periode_2009 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = au_2010_pop, fill = "grey85", color = "grey80", size = 0.1) +
  geom_sf(data = `2009`, fill = "#252525", color = "#252525", size = 0.1) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Communes desservies par un poste de Police\ndans les aires urbaines") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(subtitle = "en 2009")


bpe_2009_au %>% 
  select(Gendarmerie, Police, INSEECOM, LIBAU2010, STATUT, CATAEU2010) %>% 
  filter(Gendarmerie == 1) %>%
  mutate(date = "2009") -> `2009`
bpe_2018_wide_au %>% 
  select(Gendarmerie, Police, DEPCOM, LIBAU2010, STATUT, CATAEU2010) %>% 
  filter(Gendarmerie == 1) %>%
  mutate(date = "2018") -> `2018`


diff_2009_2018 <- left_join(x = `2009`, y = `2018` %>% st_drop_geometry(), by = c("INSEECOM" = "DEPCOM"))
diff_2018_2009 <- left_join(x = `2018`, y = `2009` %>% st_drop_geometry(), by = c("DEPCOM" = "INSEECOM"))

couleurs <- c("disparition" = "#b2182b", "apparition" = "#2166ac")
ecart_2009_2018 <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = diff_2009_2018 %>% filter(is.na(date.y)), 
          aes(fill = "disparition", color = "disparition"), size = 0.1) +
  geom_sf(data = diff_2018_2009 %>% filter(is.na(date.y)), 
          aes(fill = "apparition", color = "apparition"), size = 0.1) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : BPE 2009, 2018 (Insee), ADMIN EXPRESS 2019 (IGN)",
       subtitle = "Gendarmerie : évolution 2009-2018") +
  scale_fill_manual(name = "", values = couleurs) + # ajout légende manuellement
  scale_color_manual(name = "", values = couleurs) # idem pour les contours

# patchwork :
periode_2009 + ecart_2009_2018

ggsave(filename = "Figure5.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/CIST2020_revu/", dpi = 300,  width = 30, height = 30, units = "cm")




