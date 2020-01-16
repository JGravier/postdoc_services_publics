library(tidyverse)
library(sf)
library(tmap)
library(readxl)
library(ggthemes)
library(foreign) # lecture de dbf
library(GGally)
library(classInt)
library(explor)
library(ade4)
library(patchwork)
library(ggsn) # pour les échelles (carto)

source("fonctions_bases.R")

# --------------------------------- fond de carte France ----------------
france <- read_sf("BPE/data_communes_au/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse

# -------------------------------------- AU en sf ---------------------
communes_2019 <- read_sf("BPE/data_communes_au/COMMUNES.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154)

communes_2019_au_2010 <- read_excel("BPE/data_communes_au/AU2010_au_01-01-2019.xls", sheet = "data_composition_communale")
au_2010_2019 <- read_excel("BPE/data_communes_au/AU2010_au_01-01-2019.xls", sheet = "data_AU2010")

# lier les informations sur les AU à la composition communale, en l'occurrence "TAU2016", soit la tranche d'aire urbaine 2016
au_2010_2019_v2 <- au_2010_2019 %>%
  select(AU2010, TAU2016)

communes_2019_au_2010 <- left_join(x = communes_2019_au_2010, y = au_2010_2019_v2, by = "AU2010")
rm(au_2010_2019_v2)

# on n'a pas le même nombre de communes dans les deux tableaux (.shp = 34886 et .xls = 34970), 
# est-ce dû au fait que l'on a les communes situées hors France métropolitaine dans le fichier sur les aires urbaines ?
communes_2019_au_2010 <- communes_2019_au_2010 %>%
  filter(DEP %ni% c("971", "972", "973", "974", "976")) %>% # on retire les communes appartenant aux DOM
  mutate(INSEE_COM = CODGEO)
# maintenant le fichier contient 34886 individus, soit 45 de moins que dans le fichier spatial de l'IGN
# cela est dû au fait que d'un côté Paris, Marseille et Lyon sont groupées en communes (fichier AU de l'Insee),
# tandis que de l'autre on a les arrondissements de ces communes (fichier de l'IGN)

# création d'un objet sf des AU en France métropolitaine
communes_2019_au_2010 <- left_join(x = communes_2019_au_2010, y = communes_2019, by = "INSEE_COM") %>%
  st_sf()

au_2010_pop <- communes_2019_au_2010 %>%
  filter(CATAEU2010 %ni% c("300", "400", "120"), INSEE_DEP %ni% c("2A", "2B")) %>% # soit celle n'étant ni 300 = "Autre commune multipolarisée", 
  # ni 400 = "Commune isolée hors influence des pôles"
  # ni les 120 = "multipolarisées des grands pôles"
  st_set_precision(1000000) %>% 
  lwgeom::st_make_valid() %>% # il y a un pb dans les données de l'IGN, en gros une topologie qui n'est pas
  # bien enregistrée (avec une auto-intersection) ; solution trouvée sur https://github.com/r-spatial/sf/issues/603
  # l'idée étant d'amoindrir la précision spatiale pour que le group_by puisse fonctionner (sachant qu'on a ici une précision très très fine,
  # de l'odre du "subsubsubsub-centimetric if we are speaking of a metric projection" pour reprendre la phrase Lorenzo Busetto)
  group_by(AU2010) %>%
  summarise(population = sum(POPULATION)) %>%
  left_join(x = ., y = au_2010_2019, by = "AU2010") %>%
  st_cast(x = ., to = "MULTIPOLYGON") # ce qui est bien pour faire de l'export en shp si besoin

# vérification de la validité de la création des aires urbaines
tmap_mode("view")
tm_shape(shp = au_2010_pop) +
  tm_fill(col = "grey", alpha = 0.1) +
  tm_borders(col = "black")


# -------------------- données BPE : read ---------------------
bpe_evolution <- read.dbf("BPE/data/bpe1318_nb_equip_au.dbf", as.is = TRUE)
Encoding(bpe_evolution$TYPEQU) <- "UTF-08"
metadonnees <- read.dbf("BPE/data/varmod_bpe1318_nb_equip_au.dbf", as.is = TRUE)
metadonnees <- metadonnees %>%
  mutate(COD_MOD = as.character(COD_MOD))

bpe_evolution <- left_join(x = bpe_evolution, y = metadonnees, by = c("TYPEQU" = "COD_MOD"))
bpe_evolution <- bpe_evolution %>% select(-COD_VAR, -LIB_VAR, -TYPE_VAR, -LONG_VAR)
rm(metadonnees)

# -------------------- La Poste ---------------------
# construction d'un tableau spécifique
bpe_poste <- bpe_evolution %>%
  filter(TYPEQU %in% c("A206", "A207", "A208")) %>%
  left_join(., y = au_2010_pop, by = c("ID_AU2010" = "AU2010")) %>%
  filter(!is.na(population)) %>% # si on a pas de données pop des AU, c'est que ce sont des activités postales situées hors AU
  # donc on les supprime
  st_as_sf()

# enrichissement du tableau : densité et TCAM
bpe_poste <- bpe_poste %>%
  mutate(densite_2013 = NB_2013/population*10000, # densité pour 10 000 habitants 
         densite_2018 = NB_2018/population*10000, # densité pour 10 000 habitants
         NB_2013_2 = ifelse(NB_2013 == 0 & NB_2018 != 0, 0.001, NB_2013),
         # nécessaire de transformer les 0 en 0.001 dans les cas où il n'y avait pas d'activité postale en 2013
         # mais qu'il y en a une en 2018. Sinon le calcul des TCAM ne peut pas être effectué dans ce cas de croissance
         TCAM = TCAM(datefin = NB_2018, datedebut = NB_2013_2, nbannee = 5)) # taux de croissance annuel moyen


# ------------------ densité 2018 des activités postales : activité par activité -----------------------

# ------------------------------------- Les bureaux de poste :
classes <- bpe_poste %>% filter(TYPEQU == "A206")  # sélection des bureaux
classes <- classIntervals(var = classes$densite_2018, n = 5, style = "jenks") # discrétisation de Jenks en 5 classes

# France : cartographie
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A206"), 
          aes(fill = cut(densite_2018, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "densité pour\n10 000 hab", palette = "RdYlGn", direction = -1,  # inversion de la palette : direction
                    drop = FALSE) +
  scalebar(data = bpe_poste, dist = 100, dist_unit = "km", transform = FALSE, st.size = 3, border.size = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Bureaux de poste")


# Grand Est : cartographie (zoom à partir des classes calculées pour la France entière)
guides <- st_bbox(france %>% filter(INSEE_REG == "44")) # zoom sur la région Grand-Est

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A206"), 
          aes(fill = cut(densite_2018, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "densité pour\n10 000 hab", palette = "RdYlGn", direction = -1,  # inversion de la palette : direction
                    drop = FALSE) +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) + # zoom régional
  scalebar(x.min = guides[1], x.max = guides[3], y.min = guides[2], y.max = guides[4],
           dist = 20, dist_unit = "km", transform = FALSE, st.size = 3, border.size = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Bureaux de poste")



# ------------------------------------- Les relais de poste :
classes <- bpe_poste %>% filter(TYPEQU == "A207") 
classes <- classIntervals(var = classes$densite_2018, n = 5, style = "jenks")

# France : cartographie
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A207"), 
          aes(fill = cut(densite_2018, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "densité pour\n10 000 hab", palette = "RdYlGn", direction = -1,  # inversion de la palette : direction
                    drop = FALSE) +
  scalebar(data = bpe_poste, dist = 100, dist_unit = "km", transform = FALSE, st.size = 3, border.size = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Relais de poste")


# Grand Est : cartographie (zoom à partir des classes calculées pour la France entière)
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A207"), 
          aes(fill = cut(densite_2018, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "densité pour\n10 000 hab", palette = "RdYlGn", direction = -1,  # inversion de la palette : direction
                    drop = FALSE) +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) + # zoom régional
  scalebar(x.min = guides[1], x.max = guides[3], y.min = guides[2], y.max = guides[4],
           dist = 20, dist_unit = "km", transform = FALSE, st.size = 3, border.size = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Relais de poste")


# ------------------------------------- Les agences postales :
classes <- bpe_poste %>% filter(TYPEQU == "A208")
classes <- classIntervals(var = classes$densite_2018, n = 5, style = "jenks")

# France : cartographie
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A208"), 
          aes(fill = cut(densite_2018, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "densité pour\n10 000 hab", palette = "RdYlGn", direction = -1,  # inversion de la palette : direction
                    drop = FALSE) +
  scalebar(data = bpe_poste, dist = 100, dist_unit = "km", transform = FALSE, st.size = 3, border.size = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Agences postales communales")


# Grand Est : cartographie (zoom à partir des classes calculées pour la France entière)
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A208"), 
          aes(fill = cut(densite_2018, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "densité pour\n10 000 hab", palette = "RdYlGn", direction = -1,  # inversion de la palette : direction
                    drop = FALSE) +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) + # zoom régional
  scalebar(x.min = guides[1], x.max = guides[3], y.min = guides[2], y.max = guides[4],
           dist = 20, dist_unit = "km", transform = FALSE, st.size = 3, border.size = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Agences postales communales")


# ------------------ densité 2018 des activités postales : globalement
classes <- classIntervals(var = bpe_poste$densite_2018, n = 5, style = "jenks")

# France : cartographie
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste, 
          aes(fill = cut(densite_2018, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "densité pour\n10 000 hab", palette = "RdYlGn", direction = -1,  # inversion de la palette : direction
                    drop = FALSE) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Réseau postal en 2018") +
  facet_wrap(~LIB_MOD)

# Grand Est : cartographie (à partir des classes calculées pour la France entière et les 3 types d'activités postale)
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste, 
          aes(fill = cut(densite_2018, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "densité pour\n10 000 hab", palette = "RdYlGn", direction = -1,  # inversion de la palette : direction
                    drop = FALSE) +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Réseau postal en 2018") +
  facet_wrap(~LIB_MOD)


# ------------------ TCAM des activités postales -----------------------

# ------------------------------------- Par type d'activité

# TCAM 
classes <- bpe_poste %>% filter(TYPEQU == "A208" & TCAM != "NaN") # type d'activité et 
# "NaN" : cas où il n'y avait pas d'équipement en 2013, ni en 2018
# A206 : bureau de poste, A207 : relais de poste, A208 : agence postale communale
classes <- classIntervals(var = classes$TCAM, n = 7, style = "jenks")
classes$brks[2] <- -99 # changer la discrétisation de la première classe [-100 ; -100) en [-100 ; -99 )
# pour être en mesure d'utiliser les breaks dans le geom_sf()

# France : cartographie
# NOTE : revoir la visu sémio & le découpage en classe
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A208"), 
          aes(fill = cut(TCAM, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "RdYlBu", drop = FALSE) +
  scalebar(data = bpe_poste, dist = 100, dist_unit = "km", transform = FALSE, st.size = 3, border.size = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Agences postales communales")

# Région Grand Est : cartographie (zoom)
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A208"), 
          aes(fill = cut(TCAM, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "RdYlBu", drop = FALSE) +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) + # zoom régional
  scalebar(x.min = guides[1], x.max = guides[3], y.min = guides[2], y.max = guides[4],
           dist = 20, dist_unit = "km", transform = FALSE, st.size = 3, border.size = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Agences postales communales")


# ------------------ evo. Poste et hypothèses d'évolution ---------------------
# explorations de l'évolution selon différents critères : taille des villes, évolution démo précédente (décroissance/croissance),
# situation géographique (indicateurs de distance à un/plusieurs éléments, appartenance territoriale), etc.


# ----------------------- analyse selon la taille de villes (hors Paris)
# enrichissement du tableau relatif aux activités postales
bpe_poste <- bpe_poste %>%
  mutate(TAU2016_group = ifelse(TAU2016 %in% c("01", "02", "03", "04"), "petite", # petite : groupe de taille [inf à 15 000 à 35 000 hab)
                                ifelse(TAU2016 %in% c("08", "09"), "grande", # grande : 200.000 à 2.400.000 hab
                                ifelse(TAU2016 == "10", "Paris", "moyenne")))) %>% # moyenne : entre 35.000 et 200.000 hab
  mutate(TAU2016_Insee = ifelse(TAU2016 == "01", "1: < 15.000", # les types de tailles d'AU selon l'Insee
                                ifelse(TAU2016 == "02", "2: 15.000-20.000",
                                       ifelse(TAU2016 == "03", "3: 20.000-25.000",
                                              ifelse(TAU2016 == "04", "4: 25.000-35.000",
                                              ifelse(TAU2016 == "05", "5: 35.000-50.000",
                                              ifelse(TAU2016 == "06", "6: 50.000-100.000",
                                              ifelse(TAU2016 == "07", "7: 100.000-200.000",
                                              ifelse(TAU2016 == "08", "8: 200.000-500.000",
                                              ifelse(TAU2016 == "09", "9: 500.000-2.400.000", "Paris")))))))))) %>%
  mutate(reordonner = ifelse(TAU2016 %in% c("01", "02", "03", "04"), "a",
                             ifelse(TAU2016 %in% c("08", "09"), "c",
                             ifelse(TAU2016 == "10", "Paris", "b"))))


# Trois taille de villes : petite, moyenne, grande
bpe_poste %>%
  filter(TAU2016 != "10" & TCAM != "NaN") %>% # enlever Paris car peu pertinent il me semble
  ggplot(aes(x = reorder(TAU2016_group, reordonner), y = TCAM, fill = TAU2016_group)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 10") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2020.\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)


# sans les valeurs extrêmes de croissance
bpe_poste %>%
  filter(TAU2016 != "10" & TCAM != "NaN") %>% # enlever Paris car peu pertinent il me semble
  filter(TCAM < 100) %>%
  ggplot(aes(x = reorder(TAU2016_group, reordonner), y = TCAM, fill = TAU2016_group)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 10") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2020.\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)

# ANOVA sur ces groupes : les bureaux de postes, puis relais, puis agences postales
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A208") %>% st_drop_geometry()

AOV_bureau_poste <- AOV_bureau_poste %>% filter(TCAM != Inf) # nécessaire relais poste
AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_group)
summary(AOV_bureau_poste_2)

Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_group, TCAM) %>%
  group_by(TAU2016_group) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))

write.csv2(Tab_AOV_bureau_poste, "sorties/LaPoste/ANOVA_agence_postale.csv")
rm(Tab_AOV_bureau_poste)

Summary_AOV <- summary(AOV_bureau_poste_2)
Summary_AOV <- unlist(Summary_AOV)
SCT <- Summary_AOV[3] + Summary_AOV[4] # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)
R_deux <- Summary_AOV[3]/SCT

# selon typologie de l'Insee des Aires Urbaines : TAU
bpe_poste %>%
  filter(TAU2016 != "10") %>% # enlever Paris car peu pertinent il me semble
  ggplot(aes(x = TAU2016_Insee, y = TCAM, fill = TAU2016_Insee)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 20") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2020.\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD.C.87)

# ANOVA sur ces groupes : les bureaux de postes, puis relais, puis agences postales
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206") %>% st_drop_geometry()

AOV_bureau_poste <- AOV_bureau_poste %>% filter(TCAM != Inf) # nécessaire relais poste
AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016)
summary(AOV_bureau_poste_2)

Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016, TCAM) %>%
  group_by(TAU2016) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))

write.csv2(Tab_AOV_bureau_poste, "sorties/LaPoste/ANOVA_TAU_INSEE_agence_postale.csv")
rm(Tab_AOV_bureau_poste)

Summary_AOV <- summary(AOV_bureau_poste_2)
Summary_AOV <- unlist(Summary_AOV)
SCT <- Summary_AOV[3] + Summary_AOV[4] # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)
R_deux <- Summary_AOV[3]/SCT

# note personnelle : en fait, il ne faut pas travailler forcément sur l'évolution mais sur le changement
# là où il y a changement, comment se fait-il ? (donc en virant tous les TCAM = 0)
# ANOVA sur les 3 grpes de villes en prenant QUE le changement 
# des bureaux de postes, puis relais, puis agences postales
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A208" & TCAM != 0) %>% st_drop_geometry()

AOV_bureau_poste <- AOV_bureau_poste %>% filter(TCAM != Inf) # nécessaire relais poste
AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_group)
summary(AOV_bureau_poste_2)

Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_group, TCAM) %>%
  group_by(TAU2016_group) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))

write.csv2(Tab_AOV_bureau_poste, "sorties/LaPoste/ANOVA_agence_postale.csv")
rm(Tab_AOV_bureau_poste)

Summary_AOV <- summary(AOV_bureau_poste_2)
Summary_AOV <- unlist(Summary_AOV)
SCT <- Summary_AOV[3] + Summary_AOV[4] # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)
R_deux <- Summary_AOV[3]/SCT

# ANOVA sur les groupes INSEE : les bureaux de postes, puis relais, puis agences postales
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A208" & TCAM != 0) %>% st_drop_geometry()

AOV_bureau_poste <- AOV_bureau_poste %>% filter(TCAM != Inf) # nécessaire relais poste
AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016)
summary(AOV_bureau_poste_2)

Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016, TCAM) %>%
  group_by(TAU2016) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))

write.csv2(Tab_AOV_bureau_poste, "sorties/LaPoste/ANOVA_TAU_INSEE_agence_postale.csv")
rm(Tab_AOV_bureau_poste)

Summary_AOV <- summary(AOV_bureau_poste_2)
Summary_AOV <- unlist(Summary_AOV)
SCT <- Summary_AOV[3] + Summary_AOV[4] # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)
R_deux <- Summary_AOV[3]/SCT

# ----------------------- les appartenances régionales (actuelles)


# ----------------------- selon la situation des aires urbaines : croissance/décroissance


# ----------------------- corr. plot

# ------------> corrélations par type d'activité postale (hors Paris)

bpe_poste %>% filter(TYPEQU == "A207" & LIBAU2010 != "Paris") %>% 
  select(population, densite_2013, densite_2018) %>%
  st_drop_geometry() %>% 
  ggpairs()

bpe_poste %>% filter(TYPEQU == "A207" & LIBAU2010 != "Paris") %>% 
  select(population, TCAM) %>% 
  st_drop_geometry() %>% 
  ggpairs()

# -----> évolution des bureaux de poste selon la population :
bpe_poste %>% filter(TYPEQU == "A206" & LIBAU2010 != "Paris") %>%
  select(population, TCAM) %>% 
  st_drop_geometry() %>% ggpairs()

# idem mais en virant cette fois-ci les outliers
bpe_poste %>% filter(TYPEQU == "A206" & LIBAU2010 != "Paris" & TCAM > -50) %>%
  select(population, TCAM) %>% 
  st_drop_geometry() %>% ggpairs()

# -----> changements des bureaux de poste selon la population :
bpe_poste %>% filter(TYPEQU == "A208" & LIBAU2010 != "Paris" & TCAM != 0) %>%
  select(population, TCAM) %>% 
  st_drop_geometry() %>% ggpairs()

# transformation de TCAM en log10 > valeurs négatives, donc on doit d'abord "translate puis transform"
# solution : add a constant value to the data prior to applying the log transform
# The transformation is therefore log(Y+a) where a is the constant.
# Some people like to choose a so that min(Y+a) is a very small positive number (like 0.001). 
# Others choose a so that min(Y+a) = 1. 
# You can show that a = b – min(Y), where b is either a small number or is 1
bpe_poste %>% filter(TYPEQU == "A206" & LIBAU2010 != "Paris" & TCAM > -50 & TCAM != 0) %>% # bureau de poste
  mutate(population = log10(population + 1 - min(population)), 
         TCAM = log10(TCAM + 1 - min(TCAM))) %>%
  select(population, TCAM) %>% 
  st_drop_geometry() %>% ggpairs()


# ---------------------- fermetures : sortie tableau et carto
fermeture_poste <- bpe_poste %>% 
  filter(TCAM == -100)

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = fermeture_poste, fill = "red") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~LIB_MOD.C.87) +
  labs(subtitle = "Fermetures")

write.csv2(fermeture_poste %>% st_drop_geometry(), "sorties/LaPoste/fermetures.csv")
rm(fermeture_poste)


# -------------------- très fortes diminution : sortie tableau et carto
fortes_diminution_poste <- bpe_poste %>% 
  filter(TCAM < -10)

classes <- classIntervals(var = fortes_diminution_poste$TCAM, n = 5, style = "jenks")
classes$brks[2:6]
# par type : Grand Est
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = fortes_diminution_poste, 
          aes(fill = cut(TCAM, classes$brks[2:6])), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "YlOrRd", drop = FALSE, direction = -1) + #inverser la palette : direction
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~LIB_MOD.C.87) +
  labs(subtitle = "Fortes diminutions")

write.csv2(fortes_diminution_poste %>% st_drop_geometry(), 
           "sorties/LaPoste/diminution_inferieures_10_pourcents.csv")
rm(fortes_diminution_poste)



# ------------------ Urgences et maternités ----------------------
bpe_maternite <- bpe_evolution %>%
  filter(TYPEQU %in% c("D106", "D107")) %>%
  left_join(., y = au_2010_pop, by = c("ID_AU2010" = "AU2010")) %>%
  filter(!is.na(population)) %>%
  st_as_sf()

# densité pour 10 000 habitants
bpe_maternite <- bpe_maternite %>%
  mutate(densite_2013 = NB_2013/population*10000,
         densite_2018 = NB_2018/population*10000,
         TCAM = TCAM(datefin = densite_2018, datedebut = densite_2013, nbannee = 5),
         qualit_TCAM = ifelse(TCAM == -100, "Perte", 
                              ifelse(TCAM < 0, "Diminution",
                                     ifelse(TCAM == 0 & densite_2013 != 0, "Maintien", 
                                            ifelse(TCAM > 0, "Croissance", TCAM))
                              )
                              ))

# corr plot
bpe_maternite %>% filter(TYPEQU == "D106" & LIBAU2010 != "Paris") %>% 
  select(population, densite_2013, densite_2018) %>% st_drop_geometry() %>% ggpairs()

bpe_maternite %>% filter(TYPEQU == "D106" & LIBAU2010 != "Paris") %>% 
  select(population, TCAM) %>% st_drop_geometry() %>% ggpairs()

# cartographie des densités en 2013 et 2018
classes <- bpe_maternite %>% filter(TYPEQU == "D106") 
classes <- classIntervals(var = classes$TCAM, n = 7, style = "jenks")
classes$brks[2:8]

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_maternite %>% filter(TYPEQU == "D106") %>% gather(key = "densite", value = "donnees", densite_2013:densite_2018), 
          aes(fill = donnees), show.legend = TRUE) +
  scale_fill_viridis_c(name = "pour 10 000 hab.", option = "magma", direction = -1) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~densite) +
  ggtitle("Urgences")

# cartographie des classes d'évolution entre 2013 et 2018
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_maternite, 
          aes(fill = qualit_TCAM), show.legend = TRUE) +
  scale_fill_tableau(name = "pour 10 000 hab.", palette = "Tableau 10") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~LIB_MOD.C.87)

# région Grand Est
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_maternite, 
          aes(fill = qualit_TCAM), show.legend = TRUE) +
  scale_fill_tableau(name = "pour 10 000 hab.", palette = "Tableau 10") +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~LIB_MOD.C.87)


# ------------------ BPE évolution générale ---------------
bpe_evo_au <- bpe_evolution %>%
  left_join(., y = au_2010_pop, by = c("ID_AU2010" = "AU2010")) %>%
  filter(!is.na(population)) %>%
  st_as_sf()

bpe_evo_au <- bpe_evo_au %>%
  mutate(densite_2013 = NB_2013/population*10000,
       densite_2018 = NB_2018/population*10000,
       TCAM = TCAM(datefin = densite_2018, datedebut = densite_2013, nbannee = 5),
       qualit_TCAM = ifelse(TCAM == -100, "Perte", 
                            ifelse(TCAM < 0, "Diminution",
                                   ifelse(TCAM == 0 & densite_2013 != 0, "Maintien", 
                                          ifelse(TCAM > 0, "Croissance", TCAM))
                            )
       ))


# cartographie des classes d'évolution entre 2013 et 2018
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_evo_au %>% filter(TYPEQU %in% c("D401", "D601", "D603")), 
          aes(fill = qualit_TCAM), show.legend = TRUE) +
  scale_fill_tableau(palette = "Tableau 10", name = "Évolution") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~LIB_MOD.C.87)

# région Grand Est
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_evo_au %>% filter(TYPEQU %in% c("D401", "D601", "D603")), 
          aes(fill = qualit_TCAM), show.legend = TRUE) +
  scale_fill_tableau(palette = "Tableau 10", name = "Évolution") +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~LIB_MOD.C.87)


# ------------- test groupes ----------------
bpe_evo_au_groupe <- bpe_evo_au %>%
  filter(Groupe != "autre") %>%
  group_by(ID_AU2010, Groupe) %>%
  summarise(sum2013 = sum(NB_2013), sum2018 = sum(NB_2018)) %>%
  left_join(., y = au_2010_pop %>% st_drop_geometry(), by = c("ID_AU2010" = "AU2010")) %>%
  mutate(densite_2013 = sum2013/population*10000,
         densite_2018 = sum2018/population*10000,
         TCAM = TCAM(datefin = densite_2018, datedebut = densite_2013, nbannee = 5),
         qualit_TCAM = ifelse(TCAM == -100, "Perte", 
                              ifelse(TCAM < 0, "Diminution",
                                     ifelse(TCAM == 0 & densite_2013 != 0, "Maintien", 
                                            ifelse(TCAM > 0, "Croissance", TCAM))
                              )
         ))

# cartographie des classes d'évolution entre 2013 et 2018
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_evo_au_groupe, 
          aes(fill = qualit_TCAM), show.legend = TRUE) +
  scale_fill_tableau(palette = "Tableau 10", name = "Évolution") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~Groupe)

# exploration
bpe_evo_au_groupe %>% st_drop_geometry() %>% ungroup() %>% 
  select(TCAM, Groupe) %>% ggpairs()

options(scipen=10000)
bpe_evo_au_groupe %>% 
  ggplot(aes(y = population, x = TCAM)) +
  geom_point() +
  geom_hline(yintercept = 20000, color = "darkorange") +
  geom_hline(yintercept = 200000, color = "darkgreen") +
  scale_y_log10() +
  facet_wrap(~Groupe)

bpe_evo_au_groupe %>% 
  ggplot(aes(y = population, x = qualit_TCAM)) +
  geom_boxplot() +
  geom_hline(yintercept = 20000, color = "darkorange") +
  geom_hline(yintercept = 200000, color = "darkgreen") +
  xlab("") +
  scale_y_log10() +
  facet_wrap(~Groupe)

sf_grand_est <- st_join(x = france %>% filter(INSEE_REG %in% c("44")), y = bpe_evo_au_groupe, join = st_intersects) %>%
  st_drop_geometry() %>%
  filter(LIBAU2010 != "Paris" & ID_AU2010 %ni% c("073", "025", "336", "469")) %>%
  select(ID_AU2010)

sf_grand_est <- inner_join(x = bpe_evo_au_groupe, y = sf_grand_est, by = "ID_AU2010")

sf_grand_est <- ungroup(sf_grand_est) %>%
  mutate(geom = geometry) %>%
  st_drop_geometry() %>%
  unique() %>%
  st_as_sf()

tmap_mode("view")
tm_shape(shp = sf_grand_est %>% select(geometry) %>% unique()) +
  tm_fill(col = "grey", alpha = 0.1) +
  tm_borders(col = "black")

sf_grand_est %>%
  ggplot(aes(y = population, x = qualit_TCAM)) +
  geom_boxplot() +
  geom_hline(yintercept = 20000, color = "darkorange") +
  geom_hline(yintercept = 200000, color = "darkgreen") +
  xlab("") +
  scale_y_log10() +
  facet_wrap(~Groupe)

sf_grand_est %>%
  ggplot(aes(y = population, x = TCAM)) +
  geom_point() +
  geom_hline(yintercept = 20000, color = "darkorange") +
  geom_hline(yintercept = 200000, color = "darkgreen") +
  scale_y_log10() +
  facet_wrap(~Groupe)

# région Grand Est
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = sf_grand_est, 
          aes(fill = qualit_TCAM), show.legend = TRUE) +
  scale_fill_tableau(palette = "Tableau 10", name = "Évolution") +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~Groupe)

# -------- analyse densité : France -----------
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_evo_au_groupe %>% 
            filter(Groupe == "service public régalien à destination du maintien de l’ordre interne à l’État") %>% 
            gather(key = "densite", value = "donnees", densite_2013:densite_2018), 
          aes(fill = donnees), show.legend = TRUE) +
  scale_fill_viridis_c(name = "pour 10 000 hab.", option = "magma", direction = -1) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~densite) +
  ggtitle("service public régalien à destination du maintien de l’ordre interne à l’État")

pour_acp_densite <- bpe_evo_au_groupe %>% 
  ungroup() %>%
  spread(key = "Groupe", value = densite_2013) %>% 
  st_drop_geometry()
pour_acp_densite <- pour_acp_densite %>%
  select(ID_AU2010,`service public régalien à destination du maintien de l’ordre interne à l’État`:`service socioculturel sportif`) %>%
  group_by(ID_AU2010) %>%
  summarise_at(vars(`service public régalien à destination du maintien de l’ordre interne à l’État`:`service socioculturel sportif`),
               sum, na.rm = TRUE)

acp_densite2013 <- dudi.pca(pour_acp_densite %>% select(-ID_AU2010), scannf = FALSE, center = TRUE,
                            scale = TRUE, nf = ncol(pour_acp_densite %>% select(-ID_AU2010)))

explor(acp_densite2013)
cah_sur_acp_densite2013 <- CAH_sur_coord_ACP(pour_acp_densite %>% select(-ID_AU2010))

plot(cah_sur_acp_densite2013, hang = -1, cex = 0.6, 
     main = "Dendrogramme",
     xlab = "aires urbaines")
typo <- cutree(cah_sur_acp_densite2013, k = 6)

tab_standardise_classes_densite2013 <- pour_acp_densite %>% select(-ID_AU2010) %>%
  Standar() %>%
  mutate(Cluster = factor(typo, levels = 1:6))
rownames(tab_standardise_classes_densite2013) <- pour_acp_densite$ID_AU2010
head(tab_standardise_classes_densite2013)
write.csv(tab_standardise_classes_densite2013, "sorties/tableau/tab_standardise_densite2013.csv")

tab_standardise_mean <- tab_standardise_classes_densite2013 %>%
  group_by(Cluster)
tab_standardise_mean <- tab_standardise_mean %>%
  summarise_each(funs(mean))
write.csv(tab_standardise_mean, "sorties/tableau/tab_meanCluster.csv")

tab_standardise_mean %>%
  gather(key = "clef", value = donnees, -Cluster) %>%
  ggplot() +
  geom_bar(aes(clef, donnees, fill = Cluster), stat = "identity") +
  theme_julie() +
  scale_fill_tableau(palette = "Color Blind", breaks = NULL) +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  labs(subtitle = "Classe de périodes : CAH (distance euclidienne)") +
  ylab("Moyennes des fréquences standardisées par classe") +
  facet_wrap(~Cluster) +
  coord_flip()

tab_standardise_classes_densite2013$AU2010 <- rownames(tab_standardise_classes_densite2013)
tab_standardise_classes_densite2013 <- left_join(x = au_2010_pop, y = tab_standardise_classes_densite2013,
                                                 by = "AU2010")

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = tab_standardise_classes_densite2013, 
          aes(fill = Cluster), show.legend = TRUE) +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) +
  scale_fill_tableau(palette = "Color Blind", name = "Type") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

tab_standardise_classes_densite2013 %>%
  ggplot(aes(y = population, x = Cluster)) +
  geom_boxplot() +
  geom_hline(yintercept = 20000, color = "darkorange") +
  geom_hline(yintercept = 200000, color = "darkgreen") +
  xlab("Types") +
  scale_y_log10()


# ----------------- militaires ---------------------
militaire <- read_excel("data/fermetures_casernes.xlsx")

militaire_sum <- militaire %>%
  group_by(AU2010, date_fermeture_prevue) %>%
  summarise(effectifs_tot = sum(effectifs)) %>%
  left_join(., y = au_2010_pop, by = "AU2010") %>%
  mutate(volume = effectifs_tot/population*100) %>%
  st_as_sf()

# cartographie
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = militaire_sum, aes(fill = effectifs_tot), show.legend = TRUE) +
  scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging", trans = "reverse",
                               name = "Effectifs") +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~date_fermeture_prevue)

militaire <- militaire %>%
  group_by(AU2010) %>%
  summarise(effectifs_tot = sum(effectifs)) %>%
  left_join(., y = au_2010_pop, by = "AU2010") %>%
  mutate(volume = effectifs_tot/population*100) %>%
  st_as_sf()

militaire %>% filter(LIBAU2010 != "Paris") %>% select(population, volume) %>% 
  st_drop_geometry() %>% ggpairs()

militaire %>% filter(LIBAU2010 != "Paris") %>%
  ggplot(aes(x = population, y = volume, label = LIBAU2010)) +
  geom_point() +
  ggrepel::geom_text_repel(data = subset(militaire, volume > 3)) +
  theme_julie() +
  xlab("Population des aires urbaines") +
  ylab("Militaires partis (part de la population)")

# grand est
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = militaire_sum, aes(fill = volume), show.legend = TRUE) +
  scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging", trans = "reverse",
                               name = "Part de la pop") +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~date_fermeture_prevue)