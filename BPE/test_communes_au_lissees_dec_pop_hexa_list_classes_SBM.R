library(tidyverse)
library(sf)
library(areal) # interpolation spatiale
library(ggthemes)
library(patchwork)
library(Cairo)


source("fonctions_bases.R")


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

sf_sp_communes_summarise <- sf_sp_communes %>%
  filter(ID %in% c("2", "7")) %>% # sélection des bureaux de poste et des ét. d'éduc. secondaire
  group_by(depcom, annee, ID, INSEE_REG, pop2016, pop2009, pop1999, pop1990, pop1975) %>%
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



### ouverture de boucle pour des tests sur plusieurs tailles d'hexagones et types de services
services_choisis <- c("2", "7")
tailles_choisies <- seq(5000, 35000, 2500) # de 5km à 35km de côté, tout les 2.5km (ici mesure selon syst. de projection, soit mètres)

### tableau vide à remplir avec la boucle
tableau_iteration <- tibble(
  taille = numeric(),
  service = character(),
  nombre = numeric(),
  val_khi2_pop90_2009 = numeric(),
  df_pop90_2009 = numeric(),
  p_value_pop90_2009 = numeric(),
  observe_pop90_2009 = list(),
  attendu_pop90_2009 = list(),
  residus_pearson_pop90_2009 = list(),
  visu_pop90_2009 = list(),
  val_khi2_pop99_2009 = numeric(),
  df_pop99_2009 = numeric(),
  p_value_pop99_2009 = numeric(),
  observe_pop99_2009 = list(),
  attendu_pop99_2009 = list(),
  residus_pearson_pop99_2009 = list(),
  visu_pop99_2009 = list()
)


for (identifiant_service in services_choisis) {
  for (taille_hexa in tailles_choisies) {

# utilisation pour test de la chaîne avant traitement de boucle :
# identifiant_service <- "2"
# taille_hexa <- 6000 # en mètres


# création d'une grille hexagonale :
grid_hexa <- st_make_grid(x = sf_sp_communes %>%
                            filter(ID == identifiant_service), 
                          cellsize = taille_hexa, # mesures selon le système de projection
                          square = FALSE, # if FALSE : create hexagonal
                          what = "centers")


#---------------> interpolation spatiale des données
grid_hexa <- grid_hexa %>%
  st_as_sf() %>% # transfo en sf (st_make_grid créant un sfc)
  mutate(id = seq(from = 1, to = length(grid_hexa), 1)) %>% # ajout d'une colonne d'id nécessaire pour la fonction aw_interpolate
  st_set_precision(1000000) %>% # probleme de géometrie initialement : voir https://github.com/r-spatial/sf/issues/603
  lwgeom::st_make_valid()




interpo_grid_2009 <- aw_interpolate(.data = grid_hexa, tid = id, # la grille hexagonale consituée
                                    source = sf_sp_communes_2009 %>% 
                                      filter(ID == identifiant_service),
                                    sid = depcom, # les données à interpoler
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = c("pop2009", "pop1999", "pop1990", "pop1975", "nb_equip")) # extensive interpolation : soit somme


interpo_grid_2018 <- aw_interpolate(.data = grid_hexa, tid = id,
                                    source = sf_sp_communes_2018  %>% 
                                      filter(ID == identifiant_service), 
                                    sid = depcom,
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
  mutate(pop1990_2009 = TCAM(datefin = pop2009, datedebut = pop1990, nbannee = 19),
         pop1999_2009 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 10),
         sp_2009_2018 = TCAM(datefin = nb_equip_2018, datedebut = nb_equip, nbannee = 9))

# on regarde la situation 2018 par rapport à celle de 2009, quand il y a un service en 2009
interpo_grid <- interpo_grid %>%
  filter(!is.na(pop2009))

# nombre d'heaxagones étudiés
nombre_hexa <- nrow(interpo_grid)


# nouveau tableau pour khi-2 sur hexagones
cl_1 <- -100
cl_2 <- -0.15
cl_3 <- 0.15

tableau_pour_khi2 <- interpo_grid %>%
  select(pop1990_2009, pop1999_2009, sp_2009_2018) %>%
  st_drop_geometry() %>%
  mutate(pop1990_2009_classes = if_else(pop1990_2009 <= -0.15, "pop_décroissance", 
                                        if_else(pop1990_2009 >= 0.15, "pop_croissance", "pop_maintien")),
         pop1999_2009_classes = if_else(pop1999_2009 <= -0.15, "pop_décroissance", 
                                        if_else(pop1999_2009 >= 0.15, "pop_croissance", "pop_maintien"))) %>%
  mutate(sp_2009_2018_classes = if_else(sp_2009_2018 == cl_1, "sp_fermeture",
                                        if_else(sp_2009_2018 <= cl_2 & sp_2009_2018 > cl_1, "sp_décroissance",
                                                if_else(sp_2009_2018 >= cl_3, "sp_croissance", "sp_maintien"))))
  

# calcul du khi-2 et extraction de résultats pour tableau de sortie:
# sur tableau avec pop évolution : 1990-2009
tableau_khi2_pop90_2009 <- table(tableau_pour_khi2$pop1990_2009_classes, tableau_pour_khi2$sp_2009_2018_classes)
khi2_pop90_2009 <- chisq.test(tableau_khi2_pop90_2009)

val_khi2_pop90_2009 <- khi2_pop90_2009$statistic[[1]]
degre_liberte_pop90_2009 <- khi2_pop90_2009$parameter[[1]]
p_value_pop90_2009 <- khi2_pop90_2009$p.value
tab_observe_pop90_2009 <- list(khi2_pop90_2009$observed)
tab_attendu_pop90_2009 <- list(khi2_pop90_2009$expected)
tab_residus_pearson_pop90_2009 <- list(khi2_pop90_2009$residuals)

# sur tableau avec pop évolution : 1999-2009
tableau_khi2_pop99_2009 <- table(tableau_pour_khi2$pop1999_2009_classes, tableau_pour_khi2$sp_2009_2018_classes)
khi2_pop99_2009 <- chisq.test(tableau_khi2_pop99_2009)

val_khi2_pop99_2009 <- khi2_pop99_2009$statistic[[1]]
degre_liberte_pop99_2009 <- khi2_pop99_2009$parameter[[1]]
p_value_pop99_2009 <- khi2_pop99_2009$p.value
tab_observe_pop99_2009 <- list(khi2_pop99_2009$observed)
tab_attendu_pop99_2009 <- list(khi2_pop99_2009$expected)
tab_residus_pearson_pop99_2009 <- list(khi2_pop99_2009$residuals)

nombre_hexa
taille_hexa
identifiant_service
val_khi2_pop90_2009
degre_liberte_pop90_2009
p_value_pop90_2009
tab_observe_pop90_2009
tab_attendu_pop90_2009
tab_residus_pearson_pop90_2009
val_khi2_pop99_2009
degre_liberte_pop99_2009
p_value_pop99_2009
tab_observe_pop99_2009
tab_attendu_pop99_2009
tab_residus_pearson_pop99_2009


# création visualisation 1990-2009, résidus pearson en color
residus_visu_90_2009 <- as.data.frame(khi2_pop90_2009$residuals)

visu90_2009 <- residus_visu_90_2009 %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_text(aes(x = Var1, y = Var2, label = round(Freq, 2)), color = "grey20", size = 3.5) +
  scale_fill_fermenter(name = "Résidus", palette = "RdYlGn", direction = -1) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  ggtitle(label = taille_hexa/1000, subtitle = round(x = p_value_pop90_2009, digits = 25))

visu90_2009

# création visualisation 1999-2009, résidus pearson en color
residus_visu_99_2009 <- as.data.frame(khi2_pop99_2009$residuals)

visu99_2009 <- residus_visu_99_2009 %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_text(aes(x = Var1, y = Var2, label = round(Freq, 2)), color = "grey20", size = 3.5) +
  scale_fill_fermenter(name = "Résidus", palette = "RdYlGn", direction = -1) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  ggtitle(label = taille_hexa/1000, subtitle = round(x = p_value_pop99_2009, digits = 25))

visu99_2009


resultat_iteration <- tibble(
  taille = taille_hexa,
  service = identifiant_service,
  nombre = nombre_hexa,
  val_khi2_pop90_2009 = val_khi2_pop90_2009,
  df_pop90_2009 = degre_liberte_pop90_2009,
  p_value_pop90_2009 = p_value_pop90_2009,
  observe_pop90_2009 = tab_observe_pop90_2009,
  attendu_pop90_2009 = tab_attendu_pop90_2009,
  residus_pearson_pop90_2009 = tab_residus_pearson_pop90_2009,
  visu_pop90_2009 = list(visu90_2009),
  val_khi2_pop99_2009 = val_khi2_pop99_2009,
  df_pop99_2009 = degre_liberte_pop99_2009,
  p_value_pop99_2009 = p_value_pop99_2009,
  observe_pop99_2009 = tab_observe_pop99_2009,
  attendu_pop99_2009 = tab_attendu_pop99_2009,
  residus_pearson_pop99_2009 = tab_residus_pearson_pop99_2009,
  visu_pop99_2009 = list(visu99_2009)
)

# fin de la boucle
# remplissage du tableau vide de départ
tableau_iteration <- tableau_iteration %>%
  bind_rows(resultat_iteration)

  }
}

# export en RData
save(tableau_iteration, file = "BPE/sorties/demographie_services_publics/data/iteration_khi2_poste_educ_v1.RData")
warnings()

# lecture plot :
data_frame <- tibble(
  Var1 = c("pop_croissance", "pop_décroissance", "pop_maintien", "pop_croissance", "pop_décroissance", "pop_maintien",
           "pop_croissance", "pop_décroissance", "pop_maintien", "pop_croissance", "pop_décroissance", "pop_maintien"),
  Var2 = c("sp_croissance", "sp_croissance", "sp_croissance", "sp_décroissance", "sp_décroissance", "sp_décroissance",
           "sp_fermeture", "sp_fermeture", "sp_fermeture", "sp_maintien", "sp_maintien", "sp_maintien"),
  Freq = c(4.14351334, -3.1979002, -1.948255, -1.623280, 0.3295008, -0.424722, 0.057627, 0.5128524, 3.1979002, 1.948255, -1.623280, -0.245555)
)

data_frame <- data_frame %>%
  mutate(Freq_quali = if_else(Freq >= 4, "(Très) forte\nassociation", 
                              if_else(Freq <= 4 & Freq > 2, "Forte association",
                                      if_else(Freq <= -2, "(Très) forte\nopposition", "Opposition/association faible"))))

data_frame %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Freq), show.legend = FALSE) +
  geom_text(aes(x = Var1, y = Var2, label = Freq_quali), color = "grey20", size = 4.5, show.legend = FALSE) +
  scale_fill_fermenter(name = "Résidus", palette = "RdYlGn", direction = -1) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  ggtitle(label = "Taille en km de chaque maille hexagonale", 
          subtitle = "p-value") +
  xlab("Évolution de la population par catégories : 1999-2009") +
  ylab("Évolution du service par catégories : 2009-2018")

ggsave(filename = "wrap_plot_lecture.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/demographie_services_publics/figures/khi2_SBM", dpi = 300, 
       width = 30, height = 20, units = "cm")

# plot poste
plots_finaux <- tableau_iteration %>% 
  filter(service == "2")

wrap_plots(plots_finaux$visu_pop99_2009)

plots_finaux <- plots_finaux %>% 
  filter(taille <= 20000)

wrap_plots(plots_finaux$visu_pop99_2009)

ggsave(filename = "wrap_plot_poste_khi2_residus_pearson_maille_5_20km.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/demographie_services_publics/figures/khi2_SBM", dpi = 300, 
       width = 40, height = 35, units = "cm")
