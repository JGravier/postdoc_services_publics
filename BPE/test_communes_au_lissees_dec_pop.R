library(tidyverse)
library(sf)
library(areal) # interpolation spatiale
library(classInt)
library(ade4)
library(explor)
library(ggthemes)
library(patchwork)
library(Cairo)


source("fonctions_bases.R")

# ---------------------- data générales pour visualisation spatiale ---------------------------
france <- read_sf("BPE/data_communes_au/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse

aires_urbaines <- read.csv("BDD_services_publics/data_sorties/services_publics_aires_urbaines.csv",
                           stringsAsFactors = FALSE, header = TRUE, encoding = "Latin1") %>%
  as_tibble()

aires_urbaines <- aires_urbaines %>%
  select(geometry) %>%
  unique() %>%
  st_as_sf(., wkt = "geometry", crs = 2154)

# -------------------- data des communes des AU ayant des services publics étudiés ---------------------
sf_sp_communes <- read.csv("BDD_services_publics/data_sorties/services_publics_communes_des_aires_urbaines_long.csv",
                           stringsAsFactors = FALSE, header = TRUE, encoding = "Latin1")

sf_sp_communes <- sf_sp_communes %>%
  as_tibble() %>%
  mutate(INSEE_REG = as.character(INSEE_REG),
         dep = as.character(dep),
         dep = if_else(nchar(dep) == 1, paste0("0", dep, sep = ""), dep),
         depcom = as.character(depcom),
         depcom = if_else(nchar(depcom) == 4, paste0("0", depcom, sep = ""), depcom),
         annee = as.character(annee),
         ID = as.character(ID),
         AU2010 = as.character(AU2010)) %>%
  filter(validite_temporelle == "2009-2013-2018") %>% # sélection des services étudiables aux 3 dates
  filter(annee != "2013")

# création d'un objet sf à partir du wkt "geometry" du tableau initial
sf_sp_communes <- st_as_sf(x = sf_sp_communes, wkt = "geometry", crs = 2154)

# création d'une grille hexagonale :
grid_hexa <- st_make_grid(x = sf_sp_communes, 
                          cellsize = 80*80, # mesures selon le système de projection
                          square = FALSE, # if FALSE : create hexagonal
                          what = "centers")

ggplot() +
  geom_sf(data = grid_hexa) +
  ggspatial::annotation_scale(location = "bl",  width_hint = 0.3)


#---------------> interpolation spatiale des données
grid_hexa <- grid_hexa %>%
  st_as_sf() %>% # transfo en sf (st_make_grid créant un sfc)
  mutate(id = seq(from = 1, to = length(grid_hexa), 1)) %>% # ajout d'une colonne d'id nécessaire pour la fonction aw_interpolate
  st_set_precision(1000000) %>% # probleme de géometrie initialement : voir https://github.com/r-spatial/sf/issues/603
  lwgeom::st_make_valid()

# summarise du nb_equipement par année, puis sélection de 2009 ou 2018 pour création de l'interpolation
sf_sp_communes_summarise <- sf_sp_communes %>%
  group_by(depcom, annee, pop2016, pop2016, pop2009, pop1999, pop1975) %>%
  summarise_if(is.numeric, sum) %>%
  select(depcom:nb_equip)

# tableau en 2009
sf_sp_communes_2009 <- sf_sp_communes_summarise %>%
  filter(annee == "2009") %>%
  ungroup()

# tableau en 2018
sf_sp_communes_2018 <- sf_sp_communes_summarise %>%
  filter(annee == "2018") %>%
  ungroup()

interpo_grid_2009 <- aw_interpolate(.data = grid_hexa, tid = id, # la grille hexagonale consituée
                                 source = sf_sp_communes_2009, sid = depcom, # les données à interpoler
                                 weight = "sum",
                                 output = "tibble", 
                                 extensive = c("pop2016", "pop2009", "pop1999", "pop1975", "nb_equip")) # extensive interpolation : soit somme


interpo_grid_2018 <- aw_interpolate(.data = grid_hexa, tid = id,
                                    source = sf_sp_communes_2018, sid = depcom,
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = "nb_equip") # cette fois-ci, on ne veut que le nb_equip puisqu'on a déjà les pops


# nouveau tableau : grille hexagonale aux deux dates
interpo_grid <- grid_hexa %>%
  mutate(geom = x) %>%
  st_drop_geometry() %>%
  left_join(.,
            y = interpo_grid_2009,
            by = "id") %>%
  left_join(., 
            y = interpo_grid_2018 %>% 
              rename(nb_equip_2018 = nb_equip), 
            by = "id") %>%
  mutate(nb_equip = if_else(is.na(nb_equip), 0, as.double(nb_equip))) %>% # quand NA implique qu'il n'y a pas d'équipement
  mutate(nb_equip_2018 = if_else(is.na(nb_equip_2018), 0, as.double(nb_equip_2018))) %>% # idem
  st_as_sf(crs = 2154)

# enrichissement du tableau avec tcam
interpo_grid <- interpo_grid %>%
  mutate(pop1975_1999 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop1999_2009 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 10),
         pop2009_2016 = TCAM(datefin = pop2016, datedebut = pop2009, nbannee = 7),
         sp_2009_2018 = TCAM(datefin = nb_equip_2018, datedebut = nb_equip, nbannee = 9))

# il y a 8 hexagones où l'on a pas de service en 2009 et qui apparaissent en 2018
# il faudra travailler à les traiter (pour l'instant non car ils ne représentent mois de 0.1 % des hexagones)
interpo_grid <- interpo_grid %>%
  filter(!is.na(pop1975))

# ---------- visualisation carto générale --------------------------
# création discrétisation
classes <- classIntervals(var = interpo_grid$sp_2009_2018, n = 7, style = "quantile")
classes

classes$brks[5] <- 0
classes$brks[6] <- 1
classes$brks[7] <- 4

classes

ma_palette <- c("#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fee090", "#fdae61", "#f46d43")

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey90", size = 0.3) +
  geom_sf(data = aires_urbaines, fill = "grey95", color = "grey80", size = 0.3) +
  geom_sf(data = interpo_grid, 
          aes(fill = cut(sp_2009_2018, classes$brks, include.lowest = TRUE)), 
          show.legend = TRUE, size = 0.15) +
  scale_fill_manual(values = ma_palette, name = "TCAM 2009-2018") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2009, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Évolution des services publics dans les aires urbaines françaises")

ggsave(filename = "cartographie_2009-2018_TCAM_sp_lissage_85_85.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM/interpolation/", dpi = 300,  width = 30, height = 25, units = "cm")

# pour se repérer avec les aires urbaines
a <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey90", size = 0.3) +
  geom_sf(data = aires_urbaines, fill = "grey95", color = "grey80", size = 0.3) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey90", size = 0.3) +
  geom_sf(data = aires_urbaines, fill = "grey95", color = "grey80", size = 0.3) +
  geom_sf(data = interpo_grid, 
          aes(fill = cut(sp_2009_2018, classes$brks, include.lowest = TRUE)), 
          show.legend = TRUE, size = 0.15) +
  scale_fill_manual(values = ma_palette, name = "TCAM 2009-2018") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2009, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Évolution des services publics dans les aires urbaines françaises")


a + b

ggsave(filename = "cartographie_2009-2018_TCAM_sp_lissage_reperage.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM/interpolation/", dpi = 300,  width = 40, height = 25, units = "cm")


# ---------- exploration relation : pop & dec sur changement ---------------------
# ----------> en linéaire sur La Poste
# summarise du nb_equipement par année, puis sélection de 2009 ou 2018 pour création de l'interpolation
sf_sp_communes_summarise <- sf_sp_communes %>%
  filter(ID == "2") %>% # sélection des bureaux de poste
  group_by(depcom, annee, pop2016, pop2016, pop2009, pop1999, pop1975) %>%
  summarise_if(is.numeric, sum) %>%
  select(depcom:nb_equip)

# tableau en 2009
sf_sp_communes_2009 <- sf_sp_communes_summarise %>%
  filter(annee == "2009") %>%
  ungroup()

# tableau en 2018
sf_sp_communes_2018 <- sf_sp_communes_summarise %>%
  filter(annee == "2018") %>%
  ungroup()

interpo_grid_2009 <- aw_interpolate(.data = grid_hexa, tid = id, # la grille hexagonale consituée
                                    source = sf_sp_communes_2009, sid = depcom, # les données à interpoler
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = c("pop2016", "pop2009", "pop1999", "pop1975", "nb_equip")) # extensive interpolation : soit somme


interpo_grid_2018 <- aw_interpolate(.data = grid_hexa, tid = id,
                                    source = sf_sp_communes_2018, sid = depcom,
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = "nb_equip") # cette fois-ci, on ne veut que le nb_equip puisqu'on a déjà les pops


# nouveau tableau : grille hexagonale aux deux dates
interpo_grid <- grid_hexa %>%
  mutate(geom = x) %>%
  st_drop_geometry() %>%
  left_join(.,
            y = interpo_grid_2009,
            by = "id") %>%
  left_join(., 
            y = interpo_grid_2018 %>% 
              rename(nb_equip_2018 = nb_equip), 
            by = "id") %>%
  mutate(nb_equip = if_else(is.na(nb_equip), 0, as.double(nb_equip))) %>% # quand NA implique qu'il n'y a pas d'équipement
  mutate(nb_equip_2018 = if_else(is.na(nb_equip_2018), 0, as.double(nb_equip_2018))) %>% # idem
  st_as_sf(crs = 2154)

# enrichissement du tableau avec tcam
interpo_grid <- interpo_grid %>%
  mutate(pop1975_1999 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop1999_2009 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 10),
         pop2009_2016 = TCAM(datefin = pop2016, datedebut = pop2009, nbannee = 7),
         sp_2009_2018 = TCAM(datefin = nb_equip_2018, datedebut = nb_equip, nbannee = 9))

# il y a 8 hexagones où l'on a pas de service en 2009 et qui apparaissent en 2018
# il faudra travailler à les traiter (pour l'instant non car ils ne représentent mois de 0.1 % des hexagones)
interpo_grid <- interpo_grid %>%
  filter(!is.na(pop1975))

# corr sur hexagones avec changement
interpo_grid %>%
  filter(sp_2009_2018 != 0) %>%
  select(pop1975_1999, pop1999_2009, sp_2009_2018) %>%
  st_drop_geometry() %>%
  ggpairs()



# ----------> sur les services d'éducation secondaire
# summarise du nb_equipement par année, puis sélection de 2009 ou 2018 pour création de l'interpolation
sf_sp_communes_summarise <- sf_sp_communes %>%
  filter(ID == "7") %>% # sélection des bureaux de poste
  group_by(depcom, annee, pop2016, pop2016, pop2009, pop1999, pop1975) %>%
  summarise_if(is.numeric, sum) %>%
  select(depcom:nb_equip)

# tableau en 2009
sf_sp_communes_2009 <- sf_sp_communes_summarise %>%
  filter(annee == "2009") %>%
  ungroup()

# tableau en 2018
sf_sp_communes_2018 <- sf_sp_communes_summarise %>%
  filter(annee == "2018") %>%
  ungroup()

interpo_grid_2009 <- aw_interpolate(.data = grid_hexa, tid = id, # la grille hexagonale consituée
                                    source = sf_sp_communes_2009, sid = depcom, # les données à interpoler
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = c("pop2016", "pop2009", "pop1999", "pop1975", "nb_equip")) # extensive interpolation : soit somme


interpo_grid_2018 <- aw_interpolate(.data = grid_hexa, tid = id,
                                    source = sf_sp_communes_2018, sid = depcom,
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = "nb_equip") # cette fois-ci, on ne veut que le nb_equip puisqu'on a déjà les pops


# nouveau tableau : grille hexagonale aux deux dates
interpo_grid <- grid_hexa %>%
  mutate(geom = x) %>%
  st_drop_geometry() %>%
  left_join(.,
            y = interpo_grid_2009,
            by = "id") %>%
  left_join(., 
            y = interpo_grid_2018 %>% 
              rename(nb_equip_2018 = nb_equip), 
            by = "id") %>%
  mutate(nb_equip = if_else(is.na(nb_equip), 0, as.double(nb_equip))) %>% # quand NA implique qu'il n'y a pas d'équipement
  mutate(nb_equip_2018 = if_else(is.na(nb_equip_2018), 0, as.double(nb_equip_2018))) %>% # idem
  st_as_sf(crs = 2154)

# enrichissement du tableau avec tcam
interpo_grid <- interpo_grid %>%
  mutate(pop1975_1999 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop1999_2009 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 20),
         pop2009_2016 = TCAM(datefin = pop2016, datedebut = pop2009, nbannee = 7),
         sp_2009_2018 = TCAM(datefin = nb_equip_2018, datedebut = nb_equip, nbannee = 9))

# il y a 8 hexagones où l'on a pas de service en 2009 et qui apparaissent en 2018
# il faudra travailler à les traiter (pour l'instant non car ils ne représentent mois de 0.1 % des hexagones)
interpo_grid <- interpo_grid %>%
  filter(!is.na(pop1975))

# corr sur hexagones avec changement
interpo_grid %>%
  filter(sp_2009_2018 != 0) %>%
  select(pop1975_1999, pop1999_2009, sp_2009_2018) %>%
  st_drop_geometry() %>%
  ggpairs()


# ---------------------- exploration générale typologique (revenir à l'ensemble des services) ---------------
# idem execute l.64-199
sf_sp_communes_summarise <- sf_sp_communes %>%
  group_by(depcom, annee, pop2016, pop2016, pop2009, pop1999, pop1975) %>%
  summarise_if(is.numeric, sum) %>%
  select(depcom:nb_equip)

# tableau en 2009
sf_sp_communes_2009 <- sf_sp_communes_summarise %>%
  filter(annee == "2009") %>%
  ungroup()

# tableau en 2018
sf_sp_communes_2018 <- sf_sp_communes_summarise %>%
  filter(annee == "2018") %>%
  ungroup()

interpo_grid_2009 <- aw_interpolate(.data = grid_hexa, tid = id, # la grille hexagonale consituée
                                    source = sf_sp_communes_2009, sid = depcom, # les données à interpoler
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = c("pop2016", "pop2009", "pop1999", "pop1975", "nb_equip")) # extensive interpolation : soit somme


interpo_grid_2018 <- aw_interpolate(.data = grid_hexa, tid = id,
                                    source = sf_sp_communes_2018, sid = depcom,
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = "nb_equip") # cette fois-ci, on ne veut que le nb_equip puisqu'on a déjà les pops


# nouveau tableau : grille hexagonale aux deux dates
interpo_grid <- grid_hexa %>%
  mutate(geom = x) %>%
  st_drop_geometry() %>%
  left_join(.,
            y = interpo_grid_2009,
            by = "id") %>%
  left_join(., 
            y = interpo_grid_2018 %>% 
              rename(nb_equip_2018 = nb_equip), 
            by = "id") %>%
  mutate(nb_equip = if_else(is.na(nb_equip), 0, as.double(nb_equip))) %>% # quand NA implique qu'il n'y a pas d'équipement
  mutate(nb_equip_2018 = if_else(is.na(nb_equip_2018), 0, as.double(nb_equip_2018))) %>% # idem
  st_as_sf(crs = 2154)

# enrichissement du tableau avec tcam
interpo_grid <- interpo_grid %>%
  mutate(pop1975_1999 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop1999_2009 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 10),
         pop2009_2016 = TCAM(datefin = pop2016, datedebut = pop2009, nbannee = 7),
         sp_2009_2018 = TCAM(datefin = nb_equip_2018, datedebut = nb_equip, nbannee = 9)) %>%
  # ajout de la densité pour 10 000 habitants en 2009
  mutate(densite = nb_equip/pop2009*1000)

# il y a 8 hexagones où l'on a pas de service en 2009 et qui apparaissent en 2018
# il faudra travailler à les traiter (pour l'instant non car ils ne représentent mois de 0.1 % des hexagones)
interpo_grid <- interpo_grid %>%
  filter(!is.na(pop1975))


# ------------------ cah : exploration typo du changement ------------------------------------
acp_tableau <- interpo_grid %>% 
  select(pop1975_1999, pop1999_2009, sp_2009_2018, densite) %>%
  filter(sp_2009_2018 != 0) %>%
  st_drop_geometry()

ggpairs(acp_tableau)

acp <- dudi.pca(acp_tableau, scannf = FALSE, nf = ncol(acp_tableau))
explor(acp)

cah_acp <- acp_tableau %>%
  CAH_sur_coord_ACP()

plot(cah_acp, hang = -1, cex = 0.6, 
     main = "Dendro",
     xlab = "villes")

inertie <- sort(cah_acp$height, decreasing = TRUE)
plot(inertie, type = "h", xlab = "nombre de classes", ylab = "inertie")

inertie <- inertie/sum(inertie)*100
barplot(inertie[1:20], col = "#454847", border = "#454847", names.arg = seq(0, 19, 1),
        xlab = "nombre de classes",
        ylab = "part de l'inertie totale (%)")
# donc pour le présent cas, on peut découper en 5 classes



# ------- visu cah --------------------
typo_tab_general <- cutree(cah_acp, k = 5)  # k = nombre de classes

acp_tableau_enrichi <- interpo_grid %>% 
  select(pop1975_1999, pop1999_2009, sp_2009_2018, densite) %>%
  filter(sp_2009_2018 != 0) %>%
  st_drop_geometry() %>%
  Standar() %>%  # nécessaire de standardiser les valeurs du tableau initial
  mutate(cluster = factor(typo_tab_general, levels = 1:6)) %>%
  as_tibble()

acp_tableau_visu <- acp_tableau_enrichi %>% # nouvelle colonne avec appartenance des lignes i par classe
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)

a <- acp_tableau_visu %>%
  rename(`densité sp en 2009` = densite,
         `evo. services` = sp_2009_2018,
         `evo. pop. 1975-1999` = pop1975_1999,
         `evo. pop. 1999-2009` = pop1999_2009) %>%
  pivot_longer(cols = -cluster, names_to = "variables", values_to = "moyenne_cluster") %>%
  ggplot() +
  geom_bar(aes(x = moyenne_cluster, y = variables, fill = cluster), stat = "identity", show.legend = FALSE) +
  scale_fill_tableau(palette = "Tableau 10") +
  xlab("Moyennes des valeurs standardisées par classe") +
  theme_julie() +
  theme(axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Classe de périodes : CAH (distance euclidienne)") +
  facet_wrap(~cluster)

# carto
acp_tableau_enrichi <- acp_tableau_enrichi %>%
  bind_cols(., y = interpo_grid %>% 
              filter(sp_2009_2018 != 0) %>%
              select(geom)) %>%
  st_as_sf(crs = 2154)

b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey90", size = 0.3) +
  geom_sf(data = aires_urbaines, fill = "grey95", color = "grey80", size = 0.3) +
  geom_sf(data = acp_tableau_enrichi, aes(fill = cluster), show.legend = FALSE, size = 0.15) +
  scale_fill_tableau(palette = "Tableau 10") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Caractérisation des changements des services publics dans les aires urbaines")

b / a + plot_layout(heights = c(2, 1))

ggsave(filename = "carto_cah_80_80.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/dans_au_interpolation", dpi = 300,  width = 25, height = 30, units = "cm")

