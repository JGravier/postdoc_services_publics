library(tidyverse)
library(ggthemes)
library(sf)
library(classInt)

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



# ---------------------- densité de services publics -------------------------------
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


# ---------------------- TCAM nb d'équipements services publics -------------------------------
# transformation du tableau long en tableau wide pour calculer les TCAM:
sf_sp_au_wide_nb_equip <- sf_services_publics_aires_urbaines %>%
  select(-ID, -RGPP, -regalien, -densite_equip, -validite_temporelle) %>%
  mutate(nb_equip = as.double(nb_equip)) %>%
  pivot_wider(names_from = typologie, values_from = nb_equip)

# fonction aux 3 dates : transformer les NA en 0 quand c'est nécessaire afin de voir les disparitions
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
         `établissement public ou privé de santé` = na_en_zero_2009_a_2018(x = `établissement public ou privé de santé`, annee = annee))


# maintenant : calcul des TCAM 2009-2013
sf_sp_au_wide_nb_equip <- sf_sp_au_wide_nb_equip %>%
  select(-pop2016:-pop1968, -TAU2016, -NB_COM, -tailles_1999) %>%
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
  mutate(`direction des finances publiques 2013-2018` = TCAM(datefin = `2018_direction des finances publiques`, 
                                                                   datedebut = `2013_direction des finances publiques`, 
                                                                   nbannee = 5)) %>%
  mutate(`service postal de remplacement\ndes bureaux de poste 2013-2018` = TCAM(datefin = `2018_service postal de remplacement\ndes bureaux de poste`, 
                                                                   datedebut = `2013_service postal de remplacement\ndes bureaux de poste`, 
                                                                   nbannee = 5))


# ----------------> analyse des 7 sp étudiables sur toute la durée de l'étude : 2009-2018
evolution2009_2018 <- sf_sp_au_wide_nb_equip[,35:48]
evolution2009_2018 <- sf_sp_au_wide_nb_equip %>%
  select(AU2010:tailles_2016) %>%
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
  filter(TCAM > -50) %>%
  ggplot() +
  geom_violin(aes(x = typologie, y = TCAM, fill = typologie)) +
  scale_fill_manual(values = ma_palette_2009_2018) +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("taux de croissance annuel moyen du nombre de services (sup. à -50%)") +
  facet_grid(annees ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Services publics des aires urbaines en France métropolitaine")

write.csv(evolution2009_2018, "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_sp_nb_equipements_2009-2018.csv", row.names = FALSE)

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
  scale_fill_brewer(name = "TCAM", palette = "RdYlBu", drop = FALSE, direction = -1) +
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

# ---------------------- TCAM densité d'équipement services publics -------------------------------
# transformation du tableau long en tableau wide pour calculer les TCAM:
sf_sp_au_wide_densite <- sf_services_publics_aires_urbaines %>%
  select(-ID, -RGPP, -regalien, -nb_equip, -validite_temporelle) %>%
  mutate(densite_equip = as.double(densite_equip)) %>%
  pivot_wider(names_from = typologie, values_from = densite_equip)

# fonction aux 3 dates : transformer les NA en 0 quand c'est nécessaire afin de voir les disparitions
sf_sp_au_wide_densite <- sf_sp_au_wide_densite %>%
  mutate(`police et gendarmerie nationales` = na_en_zero_2009_a_2018(x = `police et gendarmerie nationales`, annee = annee),
         `bureau de poste` = na_en_zero_2009_a_2018(x = `bureau de poste`, annee = annee),
         `équipements juridictionnels\nd'ordre judiciaire` = na_en_zero_2009_a_2018(x = `équipements juridictionnels\nd'ordre judiciaire`, annee = annee),
         `établissement public d'éducation secondaire` = na_en_zero_2009_a_2018(x = `établissement public d'éducation secondaire`, annee = annee),
         `établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements publics ou privés)`, annee = annee),
         `établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure\n(filières de formation dans\ndes établissements exclusivement publics)`, annee = annee),
         `service de l'emploi avec\nconseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi avec\nconseiller spécialisé`, annee = annee),
         `direction des finances publiques` = na_en_zero_2013_a_2018(x = `direction des finances publiques`, annee = annee),
         `service postal de remplacement\ndes bureaux de poste` = na_en_zero_2013_a_2018(x = `service postal de remplacement\ndes bureaux de poste`, annee = annee),
         `établissement public ou privé de santé` = na_en_zero_2009_a_2018(x = `établissement public ou privé de santé`, annee = annee))


# maintenant : calcul des TCAM 2009-2013
sf_sp_au_wide_densite <- sf_sp_au_wide_densite %>%
  select(-pop2016:-pop1968, -TAU2016, -NB_COM, -tailles_1999) %>%
  pivot_longer(cols = `police et gendarmerie nationales`:`direction des finances publiques`, 
               names_to = "typologie", values_to = "densite_equip") %>%
  mutate(typologie = str_c(annee, typologie, sep = "_")) %>%
  select(-annee) %>%
  pivot_wider(names_from = "typologie", values_from = "densite_equip")


# flemme d'écrire tous les TCAM en ligne de code...
# mais je là ne vois pas d'autres solutions (il y en a forcément.. groupmf) :
sf_sp_au_wide_densite <- sf_sp_au_wide_densite %>%
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
  mutate(`direction des finances publiques 2013-2018` = TCAM(datefin = `2018_direction des finances publiques`, 
                                                             datedebut = `2013_direction des finances publiques`, 
                                                             nbannee = 5)) %>%
  mutate(`service postal de remplacement\ndes bureaux de poste 2013-2018` = TCAM(datefin = `2018_service postal de remplacement\ndes bureaux de poste`, 
                                                                                 datedebut = `2013_service postal de remplacement\ndes bureaux de poste`, 
                                                                                 nbannee = 5))

# ----------------> analyse des 7 sp étudiables sur toute la durée de l'étude : 2009-2018
evolution2009_2018_densite <- sf_sp_au_wide_densite[,35:48]
evolution2009_2018_densite <- sf_sp_au_wide_densite %>%
  select(AU2010:tailles_2016) %>%
  bind_cols(evolution2009_2018_densite)

evolution2009_2018_densite <- evolution2009_2018_densite %>%
  pivot_longer(cols = `police et gendarmerie nationales 2009-2013`:`établissement public ou privé de santé 2013-2018`,
               names_to = "typologie", values_to = "TCAM") %>%
  mutate(annees = str_extract_all(string = typologie, pattern = "2009-2013", simplify = TRUE)) %>% # évite le NA ici pour ligne suivante
  mutate(annees = if_else(annees != "2009-2013", "2013-2018", "2009-2013")) %>%
  mutate(typologie = str_remove_all(string = typologie, pattern = "2009-2013")) %>%
  mutate(typologie = str_remove_all(string = typologie, pattern = "2013-2018"))

evolution2009_2018_densite %>% filter(TCAM == "NaN") # ceux avec en début et en fin de période 0 équipement

evolution2009_2018_densite %>%
  filter(TCAM > -50) %>%
  ggplot() +
  geom_violin(aes(x = typologie, y = TCAM, fill = typologie)) +
  scale_fill_manual(values = ma_palette_2009_2018) +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("taux de croissance annuel moyen de la densité de services pour 10.000 hab. (sup. à -50%)") +
  facet_grid(annees ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Services publics des aires urbaines en France métropolitaine")

write.csv(evolution2009_2018_densite, "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_sp_densite_equip_2009-2018.csv", row.names = FALSE)

# ---------------------- TCAM nb equipements RGPP -------------------------------
# il est nécessaire de virer la poste pour voir ce que cela donne (qui a énormément évoluée à la baisse)
# transformation du tableau long en tableau wide pour calculer les TCAM:
sf_sp_au_wide_nb_RGPP <- sf_services_publics_aires_urbaines %>%
  filter(ID %ni% c("2", "3", "4", "5", "6")) %>% # on supprime les équipements non connus en 2009
  # "2" : les bureaux de Poste ; à garder ou à virer selon l'analyse
  select(AU2010, LIBAU2010, geometry, tailles_2016, RGPP, annee, nb_equip) %>%
  group_by(AU2010, LIBAU2010, geometry, tailles_2016, RGPP, annee) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  mutate(RGPP = str_c(annee, RGPP, sep = "_")) %>%
  select(-annee) %>%
  pivot_wider(names_from = RGPP, values_from = nb_equip)

# calcul TCAM
sf_sp_au_wide_nb_RGPP <- sf_sp_au_wide_nb_RGPP %>%
  mutate(`non 2009-2013` = TCAM(datefin = `2013_non`, datedebut = `2009_non`, nbannee = 4)) %>%
  mutate(`non 2013-2018` = TCAM(datefin = `2018_non`, datedebut = `2013_non`, nbannee = 5)) %>%
  mutate(`oui 2009-2013` = TCAM(datefin = `2013_oui`, datedebut = `2009_oui`, nbannee = 4)) %>%
  mutate(`oui 2013-2018` = TCAM(datefin = `2018_oui`, datedebut = `2013_oui`, nbannee = 5))


# ----------------> analyse des sp étudiables sur toute la durée de l'étude : 2009-2018
evolution2009_2018_nb_RGPP <- sf_sp_au_wide_nb_RGPP[,11:14]
evolution2009_2018_nb_RGPP <- sf_sp_au_wide_nb_RGPP %>%
  select(AU2010:tailles_2016) %>%
  bind_cols(evolution2009_2018_nb_RGPP)

evolution2009_2018_nb_RGPP <- evolution2009_2018_nb_RGPP %>%
  pivot_longer(cols = `non 2009-2013`:`oui 2013-2018`,
               names_to = "RGPP", values_to = "TCAM") %>%
  mutate(annees = str_extract_all(string = RGPP, pattern = "2009-2013", simplify = TRUE)) %>% # évite le NA ici pour ligne suivante
  mutate(annees = if_else(annees != "2009-2013", "2013-2018", "2009-2013")) %>%
  mutate(RGPP = str_remove_all(string = RGPP, pattern = "2009-2013")) %>%
  mutate(RGPP = str_remove_all(string = RGPP, pattern = "2013-2018"))

evolution2009_2018_nb_RGPP %>% filter(TCAM == "NaN") # ceux avec en début et en fin de période 0 équipement

evolution2009_2018_nb_RGPP %>%
  ggplot() +
  geom_violin(aes(x = RGPP, y = TCAM, fill = RGPP)) +
  scale_fill_manual(values = c("#00a2b3", "#cf3e53"), name = "Services visés par la RGPP") +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("taux de croissance annuel moyen du nombre de services") +
  facet_grid(annees ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Services publics des aires urbaines en France métropolitaine (hors Poste)")

write.csv(evolution2009_2018_nb_RGPP, "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_RGPP_nb_equip_2009-2018_hors_Poste.csv", row.names = FALSE)


# ---------------------- TCAM densité RGPP -------------------------------
# transformation du tableau long en tableau wide pour calculer les TCAM:
sf_sp_au_wide_densite_RGPP <- sf_services_publics_aires_urbaines %>%
  filter(ID %ni% c("2", "3", "4", "5", "6")) %>% # on supprime les équipements non connus en 2009
  # "2" : les bureaux de Poste ; à garder ou à virer selon l'analyse
  select(AU2010, LIBAU2010, geometry, tailles_2016, RGPP, annee, densite_equip) %>%
  group_by(AU2010, LIBAU2010, geometry, tailles_2016, RGPP, annee) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  mutate(RGPP = str_c(annee, RGPP, sep = "_")) %>%
  select(-annee) %>%
  pivot_wider(names_from = RGPP, values_from = densite_equip)

# calcul TCAM
sf_sp_au_wide_densite_RGPP <- sf_sp_au_wide_densite_RGPP %>%
  mutate(`non 2009-2013` = TCAM(datefin = `2013_non`, datedebut = `2009_non`, nbannee = 4)) %>%
  mutate(`non 2013-2018` = TCAM(datefin = `2018_non`, datedebut = `2013_non`, nbannee = 5)) %>%
  mutate(`oui 2009-2013` = TCAM(datefin = `2013_oui`, datedebut = `2009_oui`, nbannee = 4)) %>%
  mutate(`oui 2013-2018` = TCAM(datefin = `2018_oui`, datedebut = `2013_oui`, nbannee = 5))


# ----------------> analyse des sp étudiables sur toute la durée de l'étude : 2009-2018
evolution2009_2018_densite_RGPP <- sf_sp_au_wide_densite_RGPP[,11:14]
evolution2009_2018_densite_RGPP <- sf_sp_au_wide_densite_RGPP %>%
  select(AU2010:tailles_2016) %>%
  bind_cols(evolution2009_2018_densite_RGPP)

evolution2009_2018_densite_RGPP <- evolution2009_2018_densite_RGPP %>%
  pivot_longer(cols = `non 2009-2013`:`oui 2013-2018`,
               names_to = "RGPP", values_to = "TCAM") %>%
  mutate(annees = str_extract_all(string = RGPP, pattern = "2009-2013", simplify = TRUE)) %>% # évite le NA ici pour ligne suivante
  mutate(annees = if_else(annees != "2009-2013", "2013-2018", "2009-2013")) %>%
  mutate(RGPP = str_remove_all(string = RGPP, pattern = "2009-2013")) %>%
  mutate(RGPP = str_remove_all(string = RGPP, pattern = "2013-2018"))

evolution2009_2018_densite_RGPP %>% filter(TCAM == "NaN") # ceux avec en début et en fin de période 0 équipement

evolution2009_2018_densite_RGPP %>%
  ggplot() +
  geom_violin(aes(x = RGPP, y = TCAM, fill = RGPP)) +
  scale_fill_manual(values = c("#00a2b3", "#cf3e53"), name = "Services visés par la RGPP") +
  coord_flip() +
  theme_julie() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  ylab("taux de croissance annuel moyen de la densité de services pour 10.000 hab.") +
  facet_grid(annees ~ tailles_2016) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Services publics des aires urbaines en France métropolitaine (hors Poste)")

write.csv(evolution2009_2018_densite_RGPP, "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_RGPP_densite_equip_2009-2018_hors_Poste.csv", row.names = FALSE)


# ---------------------------- evo population -------------------------------------------------------
evo_pop_aire_urbaine <- sf_services_publics_aires_urbaines %>%
  select(AU2010:LIBAU2010, tailles_2016, geometry) %>%
  unique()

evo_pop_aire_urbaine <- evo_pop_aire_urbaine %>%
  mutate(pop_2013_2016 = TCAM(datefin = pop2016, datedebut = pop2013, nbannee = 3)) %>%
  mutate(pop_2009_2013 = TCAM(datefin = pop2013, datedebut = pop2009, nbannee = 4)) %>%
  mutate(pop_2006_2009 = TCAM(datefin = pop2009, datedebut = pop2006, nbannee = 3)) %>%
  mutate(pop_1999_2006 = TCAM(datefin = pop2006, datedebut = pop1999, nbannee = 7))

geometry_1 <- st_as_sfc(evo_pop_aire_urbaine$geometry, crs = 2154)
evo_pop_aire_urbaine <- evo_pop_aire_urbaine %>%
  mutate(geometry = geometry_1) %>%
  st_as_sf()
rm(geometry_1)

# petite carte pour avoir une idée perso de l'évolution
evo_pop_aire_urbaine_carto <- evo_pop_aire_urbaine %>%
  st_drop_geometry() %>%
  pivot_longer(cols = starts_with(match = "pop_"), names_to = "annees", values_to = "tcam") %>%
  mutate(annees = str_sub(string = annees, start = 5, end = 13), # récupération des années
         annees = str_replace_all(string = annees, pattern = "_", replacement = "-")) %>%
  left_join(., y = evo_pop_aire_urbaine %>% select(AU2010, geometry), by = "AU2010") %>%
  st_as_sf()

write.csv(evo_pop_aire_urbaine %>% st_drop_geometry(), "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_demographie.csv", row.names = FALSE)

# cartographie
# préalbalement, découpage en classes
classes <- evo_pop_aire_urbaine_carto %>%
  filter(tcam < 7 & tcam > -9) # ce sont toutes les AU qui ont vu de gros changements de délimitation géo. des 
  # communes infra-AU
classes <- classIntervals(var = classes$tcam, n = 7, style = "jenks") # discrétisation de Jenks
classes
# revoir manuellement
classes$brks[3] <- -0.2
classes$brks[4] <- 0.2
ma_palette_evo_pop <- c("#4575b4", "#91bfdb", "#f7f7f7", "#ffffbf", "#fee090", "#fc8d59", "#d73027")

ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50") +
  geom_sf(data = evo_pop_aire_urbaine, fill = "grey80", color = "grey70") +
  geom_sf(data = evo_pop_aire_urbaine_carto %>%
            filter(tcam < 7 & tcam > -9),
          aes(fill = cut(tcam, classes$brks, include.lowest = TRUE)), show.legend = TRUE) +
  scale_fill_manual(values = ma_palette_evo_pop, name = "Taux de croissance\nannuel moyen") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_julie() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\nSources: pop. et délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)") +
  ggtitle("Évolution démographique des aires urbaines de France métropolitaine") +
  facet_wrap(~annees)

ggsave(filename = "evo_pop_aires_urbaines.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures", device = "png",
       width = 21, height = 29.7, units = "cm")


# ---------- quelle part de services représente chaque groupe ? --------------
# se pose en fait la question de l'importance de La Poste dans ces données ?
volume_sp_typo_par_annee_validite_2009_2018 <- sf_sp_au_wide_nb_equip %>%
  group_by(tailles_2016) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  select(starts_with(match = "2009"), starts_with("2013"), starts_with("2018")) %>%
  rowid_to_column() %>%
  pivot_longer(cols = -rowid, names_to = "typologie", values_to = "nombre_total") %>%
  separate(col = typologie, into = c("annee", "typologie"), sep = "_") %>%
  filter(typologie %ni% c("service de l'emploi avec\nconseiller spécialisé", 
                          "service de l'emploi sans\nconseiller spécialisé",
                          "direction des finances publiques",
                          "service postal de remplacement\ndes bureaux de poste")) %>%
  mutate(taille = if_else(condition = rowid == 1, "Grande", 
                          if_else(rowid == 2, "Moyenne", "Petite"))) %>%
  group_by(annee) %>%
  mutate(pourc_par_an = nombre_total/sum(nombre_total)*100) %>%
  ungroup()

write.csv2(volume_sp_typo_par_annee_validite_2009_2018, 
           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/volume_sp_typo_par_annee_2009-2018.csv", row.names = FALSE)

# visualisation
volume_sp_typo_par_annee_validite_2009_2018 %>%
  ggplot(aes(x = pourc_par_an, y = typologie, fill = annee)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_tableau(palette = "Classic 10") +
  theme_julie() +
  theme(axis.title.y = element_blank()) +
  xlab("Part du service dans l'ensemble des services (par an)") +
  facet_wrap(~taille) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Taille de villes")

ggsave(filename = "part_services_validite_2009-2018.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.3.part_service_dans_sp", device = "png",
       width = 20, height = 15, units = "cm")

# idem mais sur la période 2013-2018 (services que l'on peut suivre sur ces dates)
volume_sp_typo_par_annee_validite_2013_2018 <- sf_sp_au_wide_nb_equip %>%
  group_by(tailles_2016) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  select(starts_with("2013"), starts_with("2018")) %>%
  rowid_to_column() %>%
  pivot_longer(cols = -rowid, names_to = "typologie", values_to = "nombre_total") %>%
  separate(col = typologie, into = c("annee", "typologie"), sep = "_") %>%
  mutate(taille = if_else(condition = rowid == 1, "Grande", 
                          if_else(rowid == 2, "Moyenne", "Petite"))) %>%
  group_by(annee) %>%
  mutate(pourc_par_an = nombre_total/sum(nombre_total)*100) %>%
  ungroup()

write.csv2(volume_sp_typo_par_annee_validite_2013_2018, 
           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/volume_sp_typo_par_annee_2013-2018.csv", row.names = FALSE)

# visualisation
volume_sp_typo_par_annee_validite_2013_2018 %>%
  ggplot(aes(x = pourc_par_an, y = typologie, fill = annee)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_tableau(palette = "Classic 10") +
  theme_julie() +
  theme(axis.title.y = element_blank()) +
  xlab("Part du service dans l'ensemble des services par an") +
  facet_wrap(~taille) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Taille de villes")

ggsave(filename = "part_services_validite_2013-2018.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.3.part_service_dans_sp", device = "png",
       width = 25, height = 21, units = "cm")


# de manière générale, sans prendre en considération les tailles des villes
volume_sp_typo_par_annee_validite_2009_2018 <- sf_sp_au_wide_nb_equip %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  select(starts_with(match = "2009"), starts_with("2013"), starts_with("2018")) %>%
  rowid_to_column() %>%
  pivot_longer(cols = -rowid, names_to = "typologie", values_to = "nombre_total") %>%
  separate(col = typologie, into = c("annee", "typologie"), sep = "_") %>%
  filter(typologie %ni% c("service de l'emploi avec\nconseiller spécialisé", 
                          "service de l'emploi sans\nconseiller spécialisé",
                          "direction des finances publiques",
                          "service postal de remplacement\ndes bureaux de poste")) %>%
  group_by(annee) %>%
  mutate(pourc_par_an = nombre_total/sum(nombre_total)*100) %>%
  ungroup()

write.csv2(volume_sp_typo_par_annee_validite_2009_2018, 
           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/volume_sp_typo_par_annee_2009-2018_hors_tailles.csv", row.names = FALSE)

# visualisation
volume_sp_typo_par_annee_validite_2009_2018 %>%
  ggplot(aes(x = pourc_par_an, y = typologie, fill = annee)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_tableau(palette = "Classic 10") +
  theme_julie() +
  theme(axis.title.y = element_blank()) +
  xlab("Part du service dans l'ensemble des services par an") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Services étudiables entre 2009 et 2018")

ggsave(filename = "part_services_validite_2009-2018_general.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.3.part_service_dans_sp", device = "png",
       width = 20, height = 15, units = "cm")

# idem mais sur la période 2013-2018 (services que l'on peut suivre sur ces dates)
volume_sp_typo_par_annee_validite_2013_2018 <- sf_sp_au_wide_nb_equip %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  select(starts_with("2013"), starts_with("2018")) %>%
  rowid_to_column() %>%
  pivot_longer(cols = -rowid, names_to = "typologie", values_to = "nombre_total") %>%
  separate(col = typologie, into = c("annee", "typologie"), sep = "_") %>%
  group_by(annee) %>%
  mutate(pourc_par_an = nombre_total/sum(nombre_total)*100) %>%
  ungroup()

write.csv2(volume_sp_typo_par_annee_validite_2013_2018, 
           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/volume_sp_typo_par_annee_2013-2018_hors_tailles.csv", row.names = FALSE)

# visualisation
volume_sp_typo_par_annee_validite_2013_2018 %>%
  ggplot(aes(x = pourc_par_an, y = typologie, fill = annee)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_tableau(palette = "Classic 10") +
  theme_julie() +
  theme(axis.title.y = element_blank()) +
  xlab("Part du service dans l'ensemble des services par an") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Services étudiables entre 2013 et 2018")

ggsave(filename = "part_services_validite_2013-2018_general.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.3.part_service_dans_sp", device = "png",
       width = 25, height = 21, units = "cm")


# ---------------------- TCAM nb d'équipements tous services confondus -------------------------------
# transformation du tableau long en tableau wide pour calculer les TCAM :
sf_all_sp_au_wide_nb_equip <- sf_services_publics_aires_urbaines %>%
  group_by(AU2010, geometry, validite_temporelle, tailles_2016, annee) %>%
  select(-pop2016:-pop1968, -NB_COM, -densite_equip) %>%
  summarise_if(is.numeric, sum, na.rm = FALSE) %>%
  pivot_wider(names_from = annee, values_from = nb_equip)

# existe-t-il des villes qui ont perdu la totalité de leurs services publics ?
# période 2009-2013-2018
sf_all_sp_au_wide_nb_equip %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  view() # oui, il y en a

# transformation des NA sur les périodes 2013 et 2018 en 0 (implique une disparition) :
sf_all_sp_au_wide_nb_equip <- sf_all_sp_au_wide_nb_equip %>%
  mutate(`2009` = if_else(condition = is.na(`2009`), true = 0, false = as.double(`2009`)),
         `2013` = if_else(condition = is.na(`2013`), true = 0, false = as.double(`2013`)),
         `2018` = if_else(condition = is.na(`2018`), true = 0, false = as.double(`2018`)))

# tableau général (revu au final plus bas)
# tab_all_nb_equip_tcam <- sf_all_sp_au_wide_nb_equip %>%
#  ungroup() %>%
#  group_by(validite_temporelle, tailles_2016) %>%
#  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
#  mutate(`2009-2013` = TCAM(datefin = `2013`, datedebut = `2009`, nbannee = 4),
#         `2013-2018` = TCAM(datefin = `2018`, datedebut = `2013`, nbannee = 5))

# write.csv2(tab_all_nb_equip_tcam, 
#           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_generale_all_sp_tailles.csv", row.names = FALSE)

tab_all_nb_equip_tcam <- sf_all_sp_au_wide_nb_equip %>%
  ungroup() %>%
  group_by(validite_temporelle, tailles_2016) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(`2009-2018` = TCAM(datefin = `2018`, datedebut = `2009`, nbannee = 9)) %>%
  ungroup()

tab_all_nb_equip_tcam_2 <- sf_all_sp_au_wide_nb_equip %>%
  ungroup() %>%
  group_by(tailles_2016) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(`all_2013-2018` = TCAM(datefin = `2018`, datedebut = `2013`, nbannee = 5)) %>%
  ungroup()

write.csv2(tab_all_nb_equip_tcam, 
           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_generale_all_sp_tailles_V2.csv",
           row.names = FALSE)
write.csv2(tab_all_nb_equip_tcam_2, 
           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_generale_all_sp_tailles_V3.csv",
           row.names = FALSE)  

rm(tab_all_nb_equip_tcam, tab_all_nb_equip_tcam_2)

# calcul des TCAM
sf_all_sp_au_wide_nb_equip <- sf_all_sp_au_wide_nb_equip %>%
  mutate(`2009-2013` = TCAM(datefin = `2013`, datedebut = `2009`, nbannee = 4),
         `2013-2018` = TCAM(datefin = `2018`, datedebut = `2013`, nbannee = 5))

# visualisation par tailles de villes : 2009-2018
sf_all_sp_au_wide_nb_equip %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  pivot_longer(cols = `2009-2013`:`2013-2018`, names_to = "annee", values_to = "tcam") %>%
  filter(tcam > -100 & tcam < 20) %>% # sinon on lit très mal à cause de quelques exceptions
  ggplot(aes(x = tailles_2016, y = tcam, fill = tailles_2016, colour = tailles_2016)) +
  geom_violin(show.legend = FALSE, alpha = 0.2) +
  geom_jitter(show.legend = FALSE, cex = 1.2, alpha = 0.5) +
  scale_fill_tableau(palette = "Tableau 10") +
  scale_colour_tableau(palette = "Tableau 10") +
  theme_julie() +
  theme(axis.title.y = element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution globale des services publics dans les villes de France métropolitaine") +
  facet_wrap(~annee) +
  coord_flip()

ggsave(filename = "evo_all_sp_tailles_villes.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM", device = "png",
       width = 25, height = 22, units = "cm")

# Important d'étudier l'évolution de chaque service afin d'avoir une vision plus claire
# 2009-2018
tab_2009_2018_tcam_services <- sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  select(-pop2016:-pop1968, -NB_COM, -densite_equip) %>%
  group_by(tailles_2016, annee, typologie) %>%
  summarise_if(is.numeric, sum, na.rm = FALSE) %>%
  pivot_wider(names_from = annee, values_from = nb_equip) %>%
  ungroup()

# transformation des NA sur les périodes 2013 et 2018 en 0 (implique une disparition) :
tab_2009_2018_tcam_services <- tab_2009_2018_tcam_services %>%
  mutate(`2009` = if_else(condition = is.na(`2009`), true = 0, false = as.double(`2009`)),
         `2013` = if_else(condition = is.na(`2013`), true = 0, false = as.double(`2013`)),
         `2018` = if_else(condition = is.na(`2018`), true = 0, false = as.double(`2018`))) %>%
  mutate(`2009-2018` = TCAM(datefin = `2018`, datedebut = `2009`, nbannee = 9),
         `2009-2013` = TCAM(datefin = `2013`, datedebut = `2009`, nbannee = 4),
         `2013-2018` = TCAM(datefin = `2018`, datedebut = `2013`, nbannee = 5))

write.csv2(tab_2009_2018_tcam_services, 
           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_generale_all_sp_tailles_V4.csv",
           row.names = FALSE)  


# visualisation par tailles de villes : 2009-2018
tab_2009_2018_tcam_services %>%
  pivot_longer(cols = `2009-2013`:`2013-2018`, names_to = "periode", values_to = "tcam") %>%
  ggplot(aes(x = typologie, y = tcam, fill = periode)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_tableau(palette = "Traffic") +
  theme_julie() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution globale des services publics dans les villes de France métropolitaine (2009-2018)") +
  facet_wrap(~fct_rev(tailles_2016))

ggsave(filename = "evo_sp_tailles_villes_2009-2018.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM", device = "png",
       width = 35, height = 22, units = "cm")

rm(tab_2009_2018_tcam_services)

# t2013-2018
tab_2013_2018_tcam_services <- sf_services_publics_aires_urbaines %>%
  select(-pop2016:-pop1968, -NB_COM, -densite_equip) %>%
  filter(annee != "2009") %>%
  group_by(tailles_2016, annee, typologie) %>%
  summarise_if(is.numeric, sum, na.rm = FALSE) %>%
  pivot_wider(names_from = annee, values_from = nb_equip) %>%
  ungroup()

# transformation des NA sur les périodes 2013 et 2018 en 0 (implique une disparition) :
tab_2013_2018_tcam_services <- tab_2013_2018_tcam_services %>%
  mutate(`2013` = if_else(condition = is.na(`2013`), true = 0, false = as.double(`2013`)),
         `2018` = if_else(condition = is.na(`2018`), true = 0, false = as.double(`2018`))) %>%
  mutate(`2013-2018` = TCAM(datefin = `2018`, datedebut = `2013`, nbannee = 5))

write.csv2(tab_2013_2018_tcam_services, 
           file = "BPE/sorties/tailles_villes_places_services_publics/sorties_data/evo_generale_all_sp_tailles_V5.csv",
           row.names = FALSE)  


# visualisation par tailles de villes : 2009-2018
tab_2013_2018_tcam_services %>%
  ggplot(aes(x = typologie, y = `2013-2018`, fill = fct_rev(tailles_2016))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_tableau(palette = "Color Blind", name = "Tailles de villes") +
  theme_julie() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank()) +
  ylab("Taux de croissance annuel moyen") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Évolution globale des services publics dans les villes de France métropolitaine (2013-2018)")

ggsave(filename = "evo_sp_tailles_villes_2013-2018.png", plot = last_plot(), 
       path = "BPE/sorties/tailles_villes_places_services_publics/figures/1.2.TCAM", device = "png",
       width = 25, height = 25, units = "cm")

rm(tab_2013_2018_tcam_services)
