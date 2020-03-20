library(tidyverse)
library(ggthemes)
library(sf)
library(tmap)

source("fonctions_bases.R")

# ----------------------------- tailles AU -----------------------------------
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


# ---------------------- Exploration typologie services publics -------------------------------
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
  mutate(typologie = if_else(typologie == "ervice de l'emploi sans conseiller spécialisé",
                             true = "service de l'emploi sans\nconseiller spécialisé",
                             false = typologie))



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
  ylab("nombre de service") +
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

