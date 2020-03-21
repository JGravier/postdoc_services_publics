library(tidyverse)
library(ggthemes)
library(sf)
library(classInt)
library(colorspace)

source("fonctions_bases.R")

# ----------------------------- data : tailles AU -----------------------------------
sf_services_publics_aires_urbaines <- read.csv("BDD_services_publics/data_sorties/services_publics_aires_urbaines_long.csv",
                                               stringsAsFactors = FALSE, header = TRUE, encoding = "Latin1")

typologie <- read.csv("BDD_services_publics/data_entrees/services_publics.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

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

# voir de nouveau la différence de taille des AU au fil du temps
sf_services_publics_aires_urbaines %>%
  select(pop2016:pop1968, LIBAU2010, tailles_2016, tailles_1999) %>%
  unique() %>%
  filter(tailles_2016 != tailles_1999)

# sortie
evo_taille <- sf_services_publics_aires_urbaines %>%
  select(LIBAU2010, pop2016, pop1999, tailles_2016, tailles_1999) %>%
  unique() %>%
  filter(tailles_2016 != tailles_1999)
write.csv(evo_taille, "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evolution_tailles_villes_1999-2016.csv", row.names = FALSE)
rm(evo_taille)


sf_services_publics_aires_urbaines <- sf_services_publics_aires_urbaines %>%
  mutate(densite_equip = if_else(annee == "2018", true = nb_equip/pop2016*10000, 
                                           if_else(annee == "2013", 
                                                   true = nb_equip/pop2013*10000, 
                                                   false = nb_equip/pop1999*10000))) %>%
  mutate(typologie = if_else(typologie == "équipements juridictionnels d'ordre judiciaire",
                             true = "équipements juridictionnels\nd'ordre judiciaire",
                             false = typologie)) %>%
  mutate(typologie = if_else(typologie == "établissement public d'éducation supérieure (filières de formation dans des établissements exclusivement publics)",
                             true = "établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)",
                             false = typologie)) %>%
  mutate(typologie = if_else(typologie == "établissement public d'éducation supérieure (filières de formation dans des établissements publics ou privés)",
                             true = "établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)",
                             false = typologie)) %>%
  mutate(typologie = if_else(typologie == "service postal de remplacement des bureaux de poste",
                             true = "service postal de remplacement\ndes bureaux de poste",
                             false = typologie)) %>%
  mutate(typologie = if_else(typologie == "service de l'emploi avec conseiller spécialisé",
                             true = "service de l'emploi avec\nconseiller spécialisé",
                             false = typologie)) %>%
  mutate(typologie = if_else(typologie == "service de l'emploi sans conseiller spécialisé",
                             true = "service de l'emploi sans\nconseiller spécialisé",
                             false = typologie))



# ---------------------- explo : densité de services publics -------------------------------
# évolution 2009-2013-2018 : violin plot
scales::show_col(tableau_color_pal(palette = "Color Blind")(20))
scales::show_col(tableau_color_pal(palette = "Summer")(20))
ma_palette_2009_2018 <- c("#1170aa", "#a3acb9", "#b60a1c", "#fc7d0b", "#ffbc79", "#309143", "#57606c")

# selon les tailles des villes et la typologie de sp
sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  ggplot() +
  geom_violin(aes(x = typologie, y = densite_equip, fill = typologie)) +
  scale_fill_manual(values = ma_palette_2009_2018) +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("densité de service pour 10 000 habitants") +
  facet_grid(annee ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution des services publics selon les tailles des aires urbaines en France métropolitaine")

sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  filter(tailles_2016 != "petite\n(< 30.000 hab.)") %>%
  ggplot() +
  geom_violin(aes(x = typologie, y = densite_equip, fill = typologie)) +
  scale_fill_manual(values = ma_palette_2009_2018) +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("densité de service pour 10 000 habitants") +
  facet_grid(annee ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution des services publics des moyennes et grandes aires urbaines en France métropolitaine")


# selon les tailles de villes (en fin de période) et l'impact direct de la RGPP :
sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  ggplot() +
  geom_violin(aes(x = RGPP, y = densite_equip, fill = RGPP)) +
  scale_fill_manual(values = ma_palette_2009_2018) +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("densité de service pour 10 000 habitant") +
  facet_grid(annee ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution des services publics selon les tailles des aires urbaines en France métropolitaine")
# on ne voit pas d'évolution très notable, ce qui est d'ailleurs très intéressant ! c'est contre intuitif à échelle si agrégée

# et si on regarde en volume ? (pas intéressant)
# et en prenant à la fois la typo mais également la RGPP ?
sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  ggplot() +
  geom_violin(aes(x = typologie, y = densite_equip, fill = RGPP)) +
  scale_fill_manual(values = ma_palette_2009_2018) +
  coord_flip() +
  theme_julie() +
  ylab("densité de service pour 10 000 habitants") +
  facet_grid(annee ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution des services publics des moyennes et petites aires urbaines en France métropolitaine")



# évolution 2013-2018 : violin plot
scales::show_col(tableau_color_pal(palette = "Color Blind")(20))
scales::show_col(tableau_color_pal(palette = "Summer")(20))
ma_palette_2013_2018 <- c("#1170aa", "#a3acb9", "#b60a1c", "#fc7d0b", "#ffbc79", "#309143", "#57606c")

# selon les tailles des villes et la typologie de sp
sf_services_publics_aires_urbaines %>%
  ggplot() +
  geom_violin(aes(x = typologie, y = densite_equip, fill = typologie), show.legend = FALSE) +
  scale_fill_tableau(palette = "Tableau 20") +
  coord_flip() +
  theme_julie() +
  ylab("densité de service pour 10 000 habitants") +
  facet_grid(annee ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution des services publics selon les tailles des aires urbaines en France métropolitaine")

sf_services_publics_aires_urbaines %>%
  filter(tailles_2016 != "petite\n(< 30.000 hab.)") %>%
  ggplot() +
  geom_violin(aes(x = typologie, y = densite_equip, fill = typologie)) +
  scale_fill_tableau(palette = "Tableau 20") +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("densité de service pour 10 000 habitants") +
  facet_grid(annee ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution des services publics des moyennes et grandes aires urbaines en France métropolitaine")


# ---------------------- explo : TCAM des services publics -------------------------------
# transformation du tableau long en tableau wide pour calculer les TCAM:
sf_sp_au_wide_nb_equip <- sf_services_publics_aires_urbaines %>%
  select(-ID, -RGPP, -regalien, -densite_equip, -validite_temporelle) %>%
  mutate(nb_equip = as.double(nb_equip)) %>%
  pivot_wider(names_from = typologie, values_from = nb_equip)

# fonction aux 3 dates : transformer les NA en 0 quand c'est nécessaire afin de voir les disparitions
na_en_zero_2009_a_2018 <- function(x, annee){
  x <- if_else(condition = annee == "2009" & is.na(x), true = 0, false = x)
  x <- if_else(condition = annee == "2013" & is.na(x), true = 0, false = x)
  x <- if_else(condition = annee == "2018" & is.na(x), true = 0, false = x)
  return(x)
}

na_en_zero_2013_a_2018 <- function(x, annee){
  x <- if_else(condition = annee == "2013" & is.na(x), true = 0, false = x)
  x <- if_else(condition = annee == "2018" & is.na(x), true = 0, false = x)
  return(x)
}

sf_sp_au_wide_nb_equip <- sf_sp_au_wide_nb_equip %>%
  mutate(`police et gendarmerie nationales` = na_en_zero_2009_a_2018(x = `police et gendarmerie nationales`, annee = annee),
         `bureau de poste` = na_en_zero_2009_a_2018(x = `bureau de poste`, annee = annee),
         `équipements juridictionnels\nd'ordre judiciaire` = na_en_zero_2009_a_2018(x = `équipements juridictionnels\nd'ordre judiciaire`, annee = annee),
         `établissement public d'éducation secondaire` = na_en_zero_2009_a_2018(x = `établissement public d'éducation secondaire`, annee = annee),
         `établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)`, annee = annee),
         `établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)`, annee = annee),
         `service de l'emploi avec\nconseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi avec\nconseiller spécialisé`, annee = annee),
         `direction des finances publiques` = na_en_zero_2013_a_2018(x = `direction des finances publiques`, annee = annee),
         `service postal de remplacement\ndes bureaux de poste` = na_en_zero_2013_a_2018(x = `service postal de remplacement\ndes bureaux de poste`, annee = annee),
         `service de l'emploi sans\nconseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi sans\nconseiller spécialisé`, annee = annee),
         `établissement public ou privé de santé` = na_en_zero_2009_a_2018(x = `établissement public ou privé de santé`, annee = annee))


# maintenant : calcul des TCAM 2009-2013
sf_sp_au_wide_nb_equip <- sf_sp_au_wide_nb_equip %>%
  select(-AU2010:-pop1968, -TAU2016, -NB_COM, -tailles_1999) %>%
  pivot_longer(cols = `police et gendarmerie nationales`:`direction des finances publiques`, 
               names_to = "typologie", values_to = "nb_equip") %>%
  mutate(typologie = str_c(annee, typologie, sep = "_")) %>%
  select(-annee) %>%
  pivot_wider(names_from = typologie, values_from = nb_equip)


# flemme d'écrire tous les TCAM en ligne de code...
# mais je là ne vois pas d'autres solutions (il y en a forcément.. groupmf) :
sf_sp_au_wide_nb_equip <- sf_sp_au_wide_nb_equip %>%
  mutate(`police et gendarmerie nationales 2009-2013` = TCAM(datefin = `2013_police et gendarmerie nationales`, 
                                                             datedebut = `2009_police et gendarmerie nationales`, 
                                                             nbannee = 4)) %>%
  mutate(`bureau de poste 2009-2013` = TCAM(datefin = `2013_bureau de poste`, 
                                                             datedebut = `2009_bureau de poste`, 
                                                             nbannee = 4)) %>%
  mutate(`équipements juridictionnels\nd'ordre judiciaire 2009-2013` = TCAM(datefin = `2013_équipements juridictionnels\nd'ordre judiciaire`, 
                                                             datedebut = `2009_équipements juridictionnels\nd'ordre judiciaire`, 
                                                             nbannee = 4)) %>%
  mutate(`établissement public d'éducation secondaire 2009-2013` = TCAM(datefin = `2013_établissement public d'éducation secondaire`, 
                                            datedebut = `2009_établissement public d'éducation secondaire`, 
                                            nbannee = 4)) %>%
  mutate(`établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés) 2009-2013` = TCAM(datefin = `2013_établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)`, 
                                                                        datedebut = `2009_établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)`, 
                                                                        nbannee = 4)) %>%
  mutate(`établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics) 2009-2013` = TCAM(datefin = `2013_établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)`, 
                                                                                                                                            datedebut = `2009_établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)`, 
                                                                                                                                            nbannee = 4)) %>%
  mutate(`établissement public ou privé de santé 2009-2013` = TCAM(datefin = `2013_établissement public ou privé de santé`, 
                                                                            datedebut = `2009_établissement public ou privé de santé`, 
                                                                            nbannee = 4)) %>%
  mutate(`police et gendarmerie nationales 2013-2018` = TCAM(datefin = `2018_police et gendarmerie nationales`, 
                                                             datedebut = `2013_police et gendarmerie nationales`, 
                                                             nbannee = 5)) %>%
  mutate(`bureau de poste 2013-2018` = TCAM(datefin = `2018_bureau de poste`, 
                                            datedebut = `2013_bureau de poste`, 
                                            nbannee = 5)) %>%
  mutate(`équipements juridictionnels\nd'ordre judiciaire 2013-2018` = TCAM(datefin = `2018_équipements juridictionnels\nd'ordre judiciaire`, 
                                                                            datedebut = `2013_équipements juridictionnels\nd'ordre judiciaire`, 
                                                                            nbannee = 5)) %>%
  mutate(`établissement public d'éducation secondaire 2013-2018` = TCAM(datefin = `2018_établissement public d'éducation secondaire`, 
                                                                        datedebut = `2013_établissement public d'éducation secondaire`, 
                                                                        nbannee = 5)) %>%
  mutate(`établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés) 2013-2018` = TCAM(datefin = `2018_établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)`, 
                                                                                                                                            datedebut = `2013_établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)`, 
                                                                                                                                            nbannee = 5)) %>%
  mutate(`établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics) 2013-2018` = TCAM(datefin = `2018_établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)`, 
                                                                                                                                                datedebut = `2013_établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)`, 
                                                                                                                                                nbannee = 5)) %>%
  mutate(`établissement public ou privé de santé 2013-2018` = TCAM(datefin = `2018_établissement public ou privé de santé`, 
                                                                   datedebut = `2013_établissement public ou privé de santé`, 
                                                                   nbannee = 5)) %>%
  mutate(`service de l'emploi avec\nconseiller spécialisé 2013-2018` = TCAM(datefin = `2018_service de l'emploi avec\nconseiller spécialisé`, 
                                                                   datedebut = `2013_service de l'emploi avec\nconseiller spécialisé`, 
                                                                   nbannee = 5)) %>%
  mutate(`service de l'emploi sans\nconseiller spécialisé 2013-2018` = TCAM(datefin = `2018_service de l'emploi sans\nconseiller spécialisé`, 
                                                                   datedebut = `2013_service de l'emploi sans\nconseiller spécialisé`, 
                                                                   nbannee = 5)) %>%
  mutate(`direction des finances publiques 2013-2018` = TCAM(datefin = `2018_direction des finances publiques`, 
                                                                   datedebut = `2013_direction des finances publiques`, 
                                                                   nbannee = 5)) %>%
  mutate(`service postal de remplacement\ndes bureaux de poste 2013-2018` = TCAM(datefin = `2018_service postal de remplacement\ndes bureaux de poste`, 
                                                                   datedebut = `2013_service postal de remplacement\ndes bureaux de poste`, 
                                                                   nbannee = 5))


# ----------------> analyse des 7 sp étudiables sur toute la durée de l'étude : 2009-2018
evolution2009_2018 <- sf_sp_au_wide_nb_equip[,37:50]
evolution2009_2018 <- sf_sp_au_wide_nb_equip %>%
  select(LIBAU2010:tailles_2016) %>%
  bind_cols(evolution2009_2018)

evolution2009_2018 <- evolution2009_2018 %>%
  pivot_longer(cols = `police et gendarmerie nationales 2009-2013`:`établissement public ou privé de santé 2013-2018`,
               names_to = "typologie", values_to = "TCAM") %>%
  mutate(annees = str_extract_all(string = typologie, pattern = "2009-2013", simplify = TRUE)) %>% # évite le NA ici pour ligne suivante
  mutate(annees = if_else(annees != "2009-2013", "2013-2018", "2009-2013")) %>%
  mutate(typologie = str_remove_all(string = typologie, pattern = "2009-2013")) %>%
  mutate(typologie = str_remove_all(string = typologie, pattern = "2013-2018"))

evolution2009_2018 %>% filter(TCAM == "NaN") # ceux avec en début et en fin de période 0 équipement

evolution2009_2018 %>%
  filter(TCAM > -20 & TCAM < 20) %>%
  ggplot() +
  geom_violin(aes(x = typologie, y = TCAM, fill = typologie)) +
  scale_fill_manual(values = ma_palette_2009_2018) +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("taux de croissance annuel moyen du nombre de services (sup. à -20% et inf. à +20%)") +
  facet_grid(annees ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Services publics des aires urbaines en France métropolitaine")


# ----> cartographie
# création d'un objet sf
geometry1 <- st_as_sfc(evolution2009_2018$geometry, crs = 2154)
evolution2009_2018 <- evolution2009_2018 %>%
  mutate(geometry = geometry1) %>%
  st_as_sf()
rm(geometry1)

france <- read_sf("BPE/data_communes_au/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse


# Police et gendarmerie nationale
classes <- evolution2009_2018 %>% 
  filter(typologie == "police et gendarmerie nationales ") %>%
  filter(TCAM != "NaN") %>%
  filter(TCAM != "Inf")
classes <- classIntervals(var = classes$TCAM, n = 7, style = "jenks") # discrétisation de Jenks
classes$brks[2] <- -99 # change "duplicate"
classes$brks
classes$brks[4] <- -2 
classes$brks[5] <- 0 

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = evolution2009_2018 %>% select(LIBAU2010) %>% unique(), fill = "grey80", color = "grey70") +
  geom_sf(data = evolution2009_2018 %>% 
            filter(typologie == "police et gendarmerie nationales "), 
          aes(fill = cut(TCAM, classes$brks, include.lowest = TRUE)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "RdYlGn", drop = FALSE, direction = 1) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2009, 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Police et gendarmerie nationale") +
  facet_wrap(~annees)


# équipements juridictionnels\nd'ordre judiciaire
classes <- evolution2009_2018 %>% 
  filter(typologie == "équipements juridictionnels\nd'ordre judiciaire ") %>%
  filter(TCAM != "NaN") %>%
  filter(TCAM != "Inf")
classes <- classIntervals(var = classes$TCAM, n = 5, style = "jenks") # discrétisation de Jenks
classes$brks[2] <- -99 # change "duplicate"
classes$brks
classes$brks[4] <- 0 

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = evolution2009_2018 %>% select(LIBAU2010) %>% unique(), fill = "grey80", color = "grey70") +
  geom_sf(data = evolution2009_2018 %>% 
            filter(typologie == "police et gendarmerie nationales "), 
          aes(fill = cut(TCAM, classes$brks, include.lowest = TRUE)), show.legend = TRUE) +
  scale_fill_brewer(name = "TCAM", palette = "RdYlGn", drop = FALSE, direction = 1) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources:  BPE 2009, 2013, 2018 (Insee),\ndélim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Équipements juridictionnels d'ordre judiciaire") +
  facet_wrap(~annees)
