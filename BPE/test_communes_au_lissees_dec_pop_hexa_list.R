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
  filter(ID %in% c("2", "7", "11")) %>% # sélection des bureaux de poste et des ét. d'éduc. secondaire
  filter(INSEE_REG %in% c("44", "75", "76", "84")) %>% # sélection de 4 régions : Grand Est, Nlle Aquitaine, Occitanie et Auvergne-Rhône-Alpes
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


tableau_iteration <- tibble(
  taille = numeric(),
  service = character(),
  regions = character(),
  nombre = integer(),
  correlation_sp_pop_evo_90_09 = numeric(),
  correlation_sp_pop_evo_99_09 = numeric()
)

tous_services <- c("2", "7", "11")
toutes_tailles <- seq(from = 2500, to = 35000, by = 2500) # test 1 de taille des hexagones (ici en mètres)
toutes_tailles <- seq(from = 5000, to = 50000, by = 5000) # test 2
regions_selection <- c("44", "75", "76", "84")

for (taille_hexa in toutes_tailles){
  for (identifiant_service in tous_services){
    for (regions in regions_selection){
# identifiant_service <- "2"
# taille_hexa <- 6000
# regions <- "44"
      
# création d'une grille hexagonale :
grid_hexa <- st_make_grid(x = sf_sp_communes %>%
                            filter(ID == identifiant_service, INSEE_REG == regions), 
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
                                      filter(ID == identifiant_service & INSEE_REG == regions),
                                    sid = depcom, # les données à interpoler
                                    weight = "sum",
                                    output = "tibble", 
                                    extensive = c("pop2009", "pop1999", "pop1990", "pop1975", "nb_equip")) # extensive interpolation : soit somme


interpo_grid_2018 <- aw_interpolate(.data = grid_hexa, tid = id,
                                    source = sf_sp_communes_2018  %>% 
                                      filter(ID == identifiant_service & INSEE_REG == regions), 
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
  mutate(pop1975_2009 = TCAM(datefin = pop2009, datedebut = pop1975, nbannee = 34),
         pop1990_2009 = TCAM(datefin = pop2009, datedebut = pop1990, nbannee = 19),
         pop1999_2009 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 10),
         sp_2009_2018 = TCAM(datefin = nb_equip_2018, datedebut = nb_equip, nbannee = 9))

# il y a 8 hexagones où l'on a pas de service en 2009 et qui apparaissent en 2018
# il faudra travailler à les traiter (pour l'instant non car ils ne représentent mois de 0.1 % des hexagones)
interpo_grid <- interpo_grid %>%
  filter(!is.na(pop1975))

nombre_hexa <- nrow(interpo_grid)


# corr sur hexagones avec changement
variable <- interpo_grid %>%
  filter(sp_2009_2018 != 0) %>%
  select(pop1975_2009, pop1990_2009, pop1999_2009, sp_2009_2018) %>%
  st_drop_geometry()

cor_90_09 <- cor(variable$sp_2009_2018, variable$pop1990_2009)
cor_99_09 <- cor(variable$sp_2009_2018, variable$pop1999_2009)
nombre_hexa
taille_hexa
identifiant_service
regions

resultat_iteration <- tibble(
  taille = taille_hexa,
  service = identifiant_service,
  regions = regions,
  nombre = nombre_hexa,
  correlation_sp_pop_evo_90_09 = cor_90_09,
  correlation_sp_pop_evo_99_09 = cor_99_09
)

tableau_iteration <- tableau_iteration %>%
  bind_rows(resultat_iteration)
# Fin boucle
  }
  }
}

tableau_iteration_base <- tableau_iteration %>%
  mutate(taille = taille/1000) %>% # la taille en km
  mutate(service = if_else(service == "2", "bureaux de poste", 
                           if_else(service == "7", "collèges et lycées publics", "Établissements de santé"))) %>%
  mutate(regions = if_else(regions == "44", "Grand Est", 
                           if_else(regions == "75", "Nouvelle Aquitaine",
                                   if_else(regions == "76", "Occitanie", "Auvergne-Rhône-Alpes"))))

tableau_iteration_test2 <- tableau_iteration %>%
  mutate(taille = taille/1000) %>%
  mutate(service = if_else(service == "2", "bureaux de poste", "collèges et lycées publics")) %>%
  mutate(regions = if_else(regions == "44", "Grand Est", 
                           if_else(regions == "75", "Nouvelle Aquitaine",
                                   if_else(regions == "76", "Occitanie", "Auvergne-Rhône-Alpes"))))

write.csv2(tableau_iteration_base, 
           file = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM/interpolation/postes_educ_sante_4regions_petite_maille.csv",
           row.names = FALSE)

write.csv2(tableau_iteration_test2, 
           file = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM/interpolation/postes_educ_sec_4regions_grande_maille.csv",
           row.names = FALSE)

tableau_iteration_base %>%
  gather(key = "type", value = "correlation", -taille, -service, -regions, -nombre) %>%
  mutate(type = if_else(type == "correlation_sp_pop_evo_75_09", "... TCAM pop. entre 1975 et 2009",
                        if_else(type == "correlation_sp_pop_evo_90_09",
                                "... TCAM pop. entre 1990 et 2009",
                                "... TCAM pop. entre 1999 et 2009"
                        ))) %>%
  ggplot() +
  aes(x = taille, y = correlation, colour = type, group = type) +
  scale_color_tableau(palette = "Traffic", name = "Corrélation des TCAM\ndu service étudié avec...") +
  geom_line() + 
  geom_point() +
  xlab("Taille des mailles hexagonales en km") +
  ylab("Coefficient de corrélation") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2009, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services publics dans les aires urbaines") +
  facet_grid(service ~ regions)

ggsave(filename = "correlations_hexagones_postes_educ_sante_4regions_petite_maille_revu.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM/interpolation/", dpi = 300, 
       width = 40, height = 25, units = "cm")

tableau_iteration_test2 %>%
  gather(key = "type", value = "correlation", -taille, -service, -regions, -nombre) %>%
  mutate(type = if_else(type == "correlation_sp_pop_evo_75_09", "... TCAM pop. entre 1975 et 2009",
                        if_else(type == "correlation_sp_pop_evo_90_09",
                                "... TCAM pop. entre 1990 et 2009",
                                "... TCAM pop. entre 1999 et 2009"
                        ))) %>%
  ggplot() +
  aes(x = taille, y = correlation, colour = type, group = type) +
  scale_color_tableau(palette = "Traffic", name = "Corrélation des TCAM\ndu service étudié avec...") +
  geom_line() + 
  geom_point() +
  xlab("Taille des mailles hexagonales en km") +
  ylab("Coefficient de corrélation") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2009, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Les services publics dans les aires urbaines") +
  facet_grid(service ~ regions)

ggsave(filename = "correlations_hexagones_postes_educ_sec_4regions_grande_maille.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM/interpolation/", dpi = 300, 
       width = 40, height = 25, units = "cm")

