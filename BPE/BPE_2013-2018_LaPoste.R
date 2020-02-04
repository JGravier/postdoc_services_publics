<<<<<<< HEAD:BPE/BPE_2013-2018_LaPoste.R
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.3) +
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
classes <- bpe_poste %>% filter(TYPEQU == "A206" & TCAM != "NaN") # type d'activité et 
# "NaN" : cas où il n'y avait pas d'équipement en 2013, ni en 2018
# A206 : bureau de poste, A207 : relais de poste, A208 : agence postale communale
classes <- classIntervals(var = classes$TCAM, n = 6, style = "jenks")
classes$brks[2] <- -99 # changer la discrétisation de la première classe [-100 ; -100) en [-100 ; -99 )
# pour être en mesure d'utiliser les breaks dans le geom_sf() - si nécessaire

# France : cartographie
# NOTE : revoir la visu sémio & le découpage en classe
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A206"), 
          aes(fill = cut(TCAM, classes$brks, include.lowest = TRUE)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "BuPu", drop = FALSE, direction = -1) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Bureaux de poste")

# Région Grand Est : cartographie (zoom)
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A206"), 
          aes(fill = cut(TCAM, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "Blues", drop = FALSE, direction = -1) +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) + # zoom régional
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Bureaux de poste")


# ------------------ corrélations linéaires : évo. ---------------------
### Par type d'activité postale
bpe_poste %>% filter(TYPEQU == "A207" & LIBAU2010 != "Paris") %>% 
  select(population, TCAM) %>% 
  st_drop_geometry() %>% 
  ggpairs()

# en virant les outliers
bpe_poste %>% filter(TYPEQU == "A207" & LIBAU2010 != "Paris" & TCAM > -50 & TCAM < 200) %>%
  select(population, TCAM) %>% 
  st_drop_geometry() %>% 
  ggpairs()


# ------------------ evo. poste et hypothèses d'évolution ---------------------
# explorations de l'évolution selon différents critères : taille des villes, évolution démo précédente (décroissance/croissance),
# situation géographique (indicateurs de distance à un/plusieurs éléments, appartenance territoriale), etc.


# ----------------------- analyse selon la taille des villes (hors Paris)
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


#-------------------------- Trois tailles de villes : petite, moyenne, grande
#### distributions
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
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)

# distributions sans les valeurs extrêmes de croissance
bpe_poste %>%
  filter(TAU2016 != "10" & TCAM != "NaN") %>% 
  filter(TCAM < 100) %>% # hors valeurs extrêmes
  ggplot(aes(x = reorder(TAU2016_group, reordonner), y = TCAM, fill = TAU2016_group)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 10") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)


#### ANOVA sur ce groupe de tailles villes par type d'activité
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206") %>% 
  st_drop_geometry()

AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_group)
summary(AOV_bureau_poste_2)

# tableau des moyennes de classe de taille de villes
Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_group, TCAM) %>%
  group_by(TAU2016_group) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))
write.csv2(Tab_AOV_bureau_poste, "BPE/sorties/LaPoste/ANOVA_3_tail-villes_bureaux_tab_mean.csv")

# résumé de l'ANOVA, calcul r² et sortie
summary_AOV_poste <- summary(AOV_bureau_poste_2)
summary_AOV_poste <- unlist(summary_AOV_poste)
r_deux <- summary_AOV_poste[3]/(summary_AOV_poste[3] + summary_AOV_poste[4]) # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)

sortie_summary <- as.data.frame(summary_AOV_poste) %>% t() %>% as.data.frame()
sortie_summary$r_deux <- r_deux
write.csv2(sortie_summary, "BPE/sorties/LaPoste/ANOVA_3_tail-villes_bureaux_summary.csv")

rm(Tab_AOV_bureau_poste, sortie_summary, AOV_bureau_poste, AOV_bureau_poste_2)


#----------- 9 tailles de villes : typologie de l'Insee TAU2016 (sans Paris)
#### distributions
bpe_poste %>%
  filter(TAU2016 != "10" & TCAM != "NaN") %>% # enlever Paris car peu pertinent il me semble
  ggplot(aes(x = TAU2016_Insee, y = TCAM, fill = TAU2016_Insee)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 20") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)

# distributions sans les valeurs extrêmes de croissance
bpe_poste %>%
  filter(TAU2016 != "10" & TCAM != "NaN") %>% 
  filter(TCAM < 100) %>% # hors valeurs extrêmes
  ggplot(aes(TAU2016_Insee, y = TCAM, fill = TAU2016_Insee)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 20") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)


#### ANOVA sur ce groupe de villes par type d'activité
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206") %>% 
  st_drop_geometry()
AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_Insee)
summary(AOV_bureau_poste_2)

# tableau des moyennes de classe de taille de villes
Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_Insee, TCAM) %>%
  group_by(TAU2016_Insee) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))
write.csv2(Tab_AOV_bureau_poste, "BPE/sorties/LaPoste/ANOVA_9_tail-villes_bureaux_tab_mean.csv")

# résumé de l'ANOVA, calcul r² et sortie
summary_AOV_poste <- summary(AOV_bureau_poste_2)
summary_AOV_poste <- unlist(summary_AOV_poste)
r_deux <- summary_AOV_poste[3]/(summary_AOV_poste[3] + summary_AOV_poste[4]) # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)

sortie_summary <- as.data.frame(summary_AOV_poste) %>% t() %>% as.data.frame()
sortie_summary$r_deux <- r_deux
write.csv2(sortie_summary, "BPE/sorties/LaPoste/ANOVA_9_tail-villes_bureaux_summary.csv")

rm(Tab_AOV_bureau_poste, sortie_summary, AOV_bureau_poste, AOV_bureau_poste_2)


# ------------------ changement poste et hypothèses d'évolution ---------------------
# note personnelle : en fait, il ne faut pas travailler forcément sur l'évolution mais sur le changement
# là où il y a changement, comment se fait-il ? (donc en virant tous les TCAM = 0 %)
# ANOVA sur les 3 grpes de villes en prenant QUE le changement 
# des bureaux de postes, puis relais, puis agences postales


#### étude des changements selon les 3 tailles de villes : petite, moyenne, grandes
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206" & TCAM != 0) %>% # hors Paris, sans TCAM = 0 % et selon les types d'activités
  st_drop_geometry()

AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_group)
summary(AOV_bureau_poste_2)

# tableau des moyennes de classe de taille de villes
Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_group, TCAM) %>%
  group_by(TAU2016_group) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))
write.csv2(Tab_AOV_bureau_poste, "BPE/sorties/LaPoste/ANOVA_changement_3_tail-villes_bureaux_tab_mean.csv")

# résumé de l'ANOVA, calcul r² et sortie
summary_AOV_poste <- summary(AOV_bureau_poste_2)
summary_AOV_poste <- unlist(summary_AOV_poste)
r_deux <- summary_AOV_poste[3]/(summary_AOV_poste[3] + summary_AOV_poste[4]) # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)

sortie_summary <- as.data.frame(summary_AOV_poste) %>% t() %>% as.data.frame()
sortie_summary$r_deux <- r_deux
write.csv2(sortie_summary, "BPE/sorties/LaPoste/ANOVA_changement_3_tail-villes_bureaux_summary.csv")

rm(Tab_AOV_bureau_poste, sortie_summary, AOV_bureau_poste, AOV_bureau_poste_2)


#### changements selon les 9 tailles de villes : typo de l'Insee
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206" & TCAM != 0) %>% # type d'activité
  st_drop_geometry()

AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_Insee)
summary(AOV_bureau_poste_2)

# tableau des moyennes de classe de taille de villes
Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_Insee, TCAM) %>%
  group_by(TAU2016_Insee) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))
write.csv2(Tab_AOV_bureau_poste, "BPE/sorties/LaPoste/ANOVA_changement_9_tail-villes_bureaux_tab_mean.csv")

# résumé de l'ANOVA, calcul r² et sortie
summary_AOV_poste <- summary(AOV_bureau_poste_2)
summary_AOV_poste <- unlist(summary_AOV_poste)
r_deux <- summary_AOV_poste[3]/(summary_AOV_poste[3] + summary_AOV_poste[4]) # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)

sortie_summary <- as.data.frame(summary_AOV_poste) %>% t() %>% as.data.frame()
sortie_summary$r_deux <- r_deux
write.csv2(sortie_summary, "BPE/sorties/LaPoste/ANOVA_changement_9_tail-villes_bureaux_summary.csv")

rm(Tab_AOV_bureau_poste, sortie_summary, AOV_bureau_poste, AOV_bureau_poste_2)


# ---------------------- fermetures -----------------
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
  facet_wrap(~LIB_MOD) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018",
       subtitle = "Fermetures")

write.csv2(fermeture_poste %>% st_drop_geometry(), "BPE/sorties/LaPoste/fermetures.csv")
rm(fermeture_poste)


# -------------------- très fortes diminutions & fermetures ------------
fortes_diminution_poste <- bpe_poste %>% 
  filter(TCAM < -5)

classes <- classIntervals(var = fortes_diminution_poste$TCAM, n = 5, style = "jenks")
classes$brks[2] <- -99 # changer la discrétisation de la première classe [-100 ; -100) en [-100 ; -99 )
# pour être en mesure d'utiliser les breaks dans le geom_sf() - si nécessaire

# par type : France entière
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = fortes_diminution_poste, 
          aes(fill = cut(TCAM, classes$brks, include.lowest = TRUE)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "YlOrRd", drop = FALSE, direction = -1) + #inverser la palette : direction
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~LIB_MOD) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018",
       subtitle = "Fortes diminutions et fermetures")

write.csv2(fortes_diminution_poste %>% st_drop_geometry(), 
           "BPE/sorties/LaPoste/diminution_inferieures_10_pourcents.csv")
rm(fortes_diminution_poste)
=======
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5) +
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
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.3) +
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
classes <- bpe_poste %>% filter(TYPEQU == "A206" & TCAM != "NaN") # type d'activité et 
# "NaN" : cas où il n'y avait pas d'équipement en 2013, ni en 2018
# A206 : bureau de poste, A207 : relais de poste, A208 : agence postale communale
classes <- classIntervals(var = classes$TCAM, n = 6, style = "jenks")
classes$brks[2] <- -99 # changer la discrétisation de la première classe [-100 ; -100) en [-100 ; -99 )
# pour être en mesure d'utiliser les breaks dans le geom_sf() - si nécessaire

# France : cartographie
# NOTE : revoir la visu sémio & le découpage en classe
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A206"), 
          aes(fill = cut(TCAM, classes$brks, include.lowest = TRUE)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "BuPu", drop = FALSE, direction = -1) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Bureaux de poste")

# Région Grand Est : cartographie (zoom)
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = bpe_poste %>% filter(TYPEQU == "A206"), 
          aes(fill = cut(TCAM, classes$brks)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "Blues", drop = FALSE, direction = -1) +
  coord_sf(xlim = guides[c(1,3)], ylim = guides[c(2,4)]) + # zoom régional
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.5) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018") +
  ggtitle("Bureaux de poste")


# ------------------ corrélations linéaires : évo. ---------------------
### Par type d'activité postale
bpe_poste %>% filter(TYPEQU == "A207" & LIBAU2010 != "Paris") %>% 
  select(population, TCAM) %>% 
  st_drop_geometry() %>% 
  ggpairs()

# en virant les outliers
bpe_poste %>% filter(TYPEQU == "A207" & LIBAU2010 != "Paris" & TCAM > -50 & TCAM < 200) %>%
  select(population, TCAM) %>% 
  st_drop_geometry() %>% 
  ggpairs()


# ------------------ evo. poste et hypothèses d'évolution ---------------------
# explorations de l'évolution selon différents critères : taille des villes, évolution démo précédente (décroissance/croissance),
# situation géographique (indicateurs de distance à un/plusieurs éléments, appartenance territoriale), etc.


# ----------------------- analyse selon la taille des villes (hors Paris)
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


#-------------------------- Trois tailles de villes : petite, moyenne, grande
#### distributions
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
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)

# distributions sans les valeurs extrêmes de croissance
bpe_poste %>%
  filter(TAU2016 != "10" & TCAM != "NaN") %>% 
  filter(TCAM < 100) %>% # hors valeurs extrêmes
  ggplot(aes(x = reorder(TAU2016_group, reordonner), y = TCAM, fill = TAU2016_group)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 10") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)


#### ANOVA sur ce groupe de tailles villes par type d'activité
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206") %>% 
  st_drop_geometry()

AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_group)
summary(AOV_bureau_poste_2)

# tableau des moyennes de classe de taille de villes
Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_group, TCAM) %>%
  group_by(TAU2016_group) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))
write.csv2(Tab_AOV_bureau_poste, "BPE/sorties/LaPoste/ANOVA_3_tail-villes_bureaux_tab_mean.csv")

# résumé de l'ANOVA, calcul r² et sortie
summary_AOV_poste <- summary(AOV_bureau_poste_2)
summary_AOV_poste <- unlist(summary_AOV_poste)
r_deux <- summary_AOV_poste[3]/(summary_AOV_poste[3] + summary_AOV_poste[4]) # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)

sortie_summary <- as.data.frame(summary_AOV_poste) %>% t() %>% as.data.frame()
sortie_summary$r_deux <- r_deux
write.csv2(sortie_summary, "BPE/sorties/LaPoste/ANOVA_3_tail-villes_bureaux_summary.csv")

rm(Tab_AOV_bureau_poste, sortie_summary, AOV_bureau_poste, AOV_bureau_poste_2)


#----------- 9 tailles de villes : typologie de l'Insee TAU2016 (sans Paris)
#### distributions
bpe_poste %>%
  filter(TAU2016 != "10" & TCAM != "NaN") %>% # enlever Paris car peu pertinent il me semble
  ggplot(aes(x = TAU2016_Insee, y = TCAM, fill = TAU2016_Insee)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 20") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)

# distributions sans les valeurs extrêmes de croissance
bpe_poste %>%
  filter(TAU2016 != "10" & TCAM != "NaN") %>% 
  filter(TCAM < 100) %>% # hors valeurs extrêmes
  ggplot(aes(TAU2016_Insee, y = TCAM, fill = TAU2016_Insee)) +
  geom_violin() +
  theme_julie() +
  scale_fill_tableau(name = "Taille des villes", palette = "Tableau 20") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités..\n Sources : BPE 2013-2018, Insee") +
  ggtitle("Réseau postal des aires urbaines entre 2013 et 2018 (hors Paris)") +
  facet_wrap(~ LIB_MOD)


#### ANOVA sur ce groupe de villes par type d'activité
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206") %>% 
  st_drop_geometry()
AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_Insee)
summary(AOV_bureau_poste_2)

# tableau des moyennes de classe de taille de villes
Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_Insee, TCAM) %>%
  group_by(TAU2016_Insee) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))
write.csv2(Tab_AOV_bureau_poste, "BPE/sorties/LaPoste/ANOVA_9_tail-villes_bureaux_tab_mean.csv")

# résumé de l'ANOVA, calcul r² et sortie
summary_AOV_poste <- summary(AOV_bureau_poste_2)
summary_AOV_poste <- unlist(summary_AOV_poste)
r_deux <- summary_AOV_poste[3]/(summary_AOV_poste[3] + summary_AOV_poste[4]) # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)

sortie_summary <- as.data.frame(summary_AOV_poste) %>% t() %>% as.data.frame()
sortie_summary$r_deux <- r_deux
write.csv2(sortie_summary, "BPE/sorties/LaPoste/ANOVA_9_tail-villes_bureaux_summary.csv")

rm(Tab_AOV_bureau_poste, sortie_summary, AOV_bureau_poste, AOV_bureau_poste_2)


# ------------------ changement poste et hypothèses d'évolution ---------------------
# note personnelle : en fait, il ne faut pas travailler forcément sur l'évolution mais sur le changement
# là où il y a changement, comment se fait-il ? (donc en virant tous les TCAM = 0 %)
# ANOVA sur les 3 grpes de villes en prenant QUE le changement 
# des bureaux de postes, puis relais, puis agences postales


#### étude des changements selon les 3 tailles de villes : petite, moyenne, grandes
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206" & TCAM != 0) %>% # hors Paris, sans TCAM = 0 % et selon les types d'activités
  st_drop_geometry()

AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_group)
summary(AOV_bureau_poste_2)

# tableau des moyennes de classe de taille de villes
Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_group, TCAM) %>%
  group_by(TAU2016_group) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))
write.csv2(Tab_AOV_bureau_poste, "BPE/sorties/LaPoste/ANOVA_changement_3_tail-villes_bureaux_tab_mean.csv")

# résumé de l'ANOVA, calcul r² et sortie
summary_AOV_poste <- summary(AOV_bureau_poste_2)
summary_AOV_poste <- unlist(summary_AOV_poste)
r_deux <- summary_AOV_poste[3]/(summary_AOV_poste[3] + summary_AOV_poste[4]) # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)

sortie_summary <- as.data.frame(summary_AOV_poste) %>% t() %>% as.data.frame()
sortie_summary$r_deux <- r_deux
write.csv2(sortie_summary, "BPE/sorties/LaPoste/ANOVA_changement_3_tail-villes_bureaux_summary.csv")

rm(Tab_AOV_bureau_poste, sortie_summary, AOV_bureau_poste, AOV_bureau_poste_2)


#### changements selon les 9 tailles de villes : typo de l'Insee
AOV_bureau_poste <- bpe_poste %>%
  filter(TAU2016 != "10" & TYPEQU == "A206" & TCAM != 0) %>% # type d'activité
  st_drop_geometry()

AOV_bureau_poste_2 <- aov(AOV_bureau_poste$TCAM ~ AOV_bureau_poste$TAU2016_Insee)
summary(AOV_bureau_poste_2)

# tableau des moyennes de classe de taille de villes
Tab_AOV_bureau_poste <- AOV_bureau_poste %>%
  select(TAU2016_Insee, TCAM) %>%
  group_by(TAU2016_Insee) %>%
  summarise(Nombre = n(), Moyenne = mean(TCAM, na.rm = TRUE))
write.csv2(Tab_AOV_bureau_poste, "BPE/sorties/LaPoste/ANOVA_changement_9_tail-villes_bureaux_tab_mean.csv")

# résumé de l'ANOVA, calcul r² et sortie
summary_AOV_poste <- summary(AOV_bureau_poste_2)
summary_AOV_poste <- unlist(summary_AOV_poste)
r_deux <- summary_AOV_poste[3]/(summary_AOV_poste[3] + summary_AOV_poste[4]) # récup SCE (somme des carrés estimés) et SCR (somme des carrés résiduels)

sortie_summary <- as.data.frame(summary_AOV_poste) %>% t() %>% as.data.frame()
sortie_summary$r_deux <- r_deux
write.csv2(sortie_summary, "BPE/sorties/LaPoste/ANOVA_changement_9_tail-villes_bureaux_summary.csv")

rm(Tab_AOV_bureau_poste, sortie_summary, AOV_bureau_poste, AOV_bureau_poste_2)


# ---------------------- fermetures -----------------
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
  facet_wrap(~LIB_MOD) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018",
       subtitle = "Fermetures")

write.csv2(fermeture_poste %>% st_drop_geometry(), "BPE/sorties/LaPoste/fermetures.csv")
rm(fermeture_poste)


# -------------------- très fortes diminutions & fermetures ------------
fortes_diminution_poste <- bpe_poste %>% 
  filter(TCAM < -5)

classes <- classIntervals(var = fortes_diminution_poste$TCAM, n = 5, style = "jenks")
classes$brks[2] <- -99 # changer la discrétisation de la première classe [-100 ; -100) en [-100 ; -99 )
# pour être en mesure d'utiliser les breaks dans le geom_sf() - si nécessaire

# par type : France entière
ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = au_2010_pop, fill = "grey80", color = "grey70") +
  geom_sf(data = fortes_diminution_poste, 
          aes(fill = cut(TCAM, classes$brks, include.lowest = TRUE)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "YlOrRd", drop = FALSE, direction = -1) + #inverser la palette : direction
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~LIB_MOD) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources : INSEE, BPE 2013-2018",
       subtitle = "Fortes diminutions et fermetures")

write.csv2(fortes_diminution_poste %>% st_drop_geometry(), 
           "BPE/sorties/LaPoste/diminution_inferieures_10_pourcents.csv")
rm(fortes_diminution_poste)
>>>>>>> 5276285a6d154762997c3ede399b209d8f8082fc:BPE/BPE_2013-2018.R
