library(tidyverse)
library(ggthemes)
library(sf)
library(patchwork)
library(ade4)
library(explor)
library(Cairo)
library(GGally)
library(areal) # interpolation


source("fonctions_bases.R")

# -------------------- data ---------------------
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


# ------------- construction tableau pour simple exploration ACP-CAH exploratoires ---------------
# période 2009-2018
tab_acp_test <- sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018" & annee %in% c("2009", "2018")) %>%
  select(AU2010, LIBAU2010, pop2009, pop1999, pop1975, annee, typologie, nb_equip, geometry) %>%
  group_by(AU2010, LIBAU2010, pop2009, pop1999, pop1975, geometry, annee) %>%
  summarise_if(is.numeric, sum) %>%
  pivot_wider(id_cols = AU2010:geometry, names_from = annee, values_from = nb_equip) %>%
  rename(nb_equip_2009 = `2009`,
         nb_equip_2018 = `2018`) %>%
  mutate(densite_2009 = nb_equip_2009/pop2009*10000) %>%
  mutate(pop_75_99 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop_99_09 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 20),
         sp_09_18 = TCAM(datefin = nb_equip_2018, datedebut = nb_equip_2009, nbannee = 9)) %>%
  ungroup()


# il y a deux AU avec NA dans les services publics (suppression)
tab_acp_test <- tab_acp_test %>%
  filter(!is.na(densite_2009) & !is.na(sp_09_18))

acp_tableau <- tab_acp_test %>% select(densite_2009:sp_09_18)

GGally::ggpairs(acp_tableau)
# corrélation dans les évolution de population au fil du temps malgré tout, meme si pas gigantesque
# par ailleurs, beaucoup de villes où il n'y a aucune évolution des sp
# on ne s'intéresse pas au maintien ici mais aux changements donc possibilité de les virer

acp <- dudi.pca(acp_tableau, scannf = FALSE, nf = ncol(acp_tableau))
explor(acp)

# en s'intéressant seulement aux changements de sp :
acp_tableau_2 <- acp_tableau %>% filter(sp_09_18 != 0)
acp_2 <- dudi.pca(acp_tableau_2, scannf = FALSE, nf = ncol(acp_tableau_2))
explor(acp_2)


# ------------------ cah : tableau général ------------------------------------
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
# donc pour le présent cas, on peut découper en 6 classes (une classe qui contiendra 2 aires urbaines fufu)

typo_tab_general <- cutree(cah_acp, k = 6)  # k = nombre de classes

acp_tableau_enrichi <- tab_acp_test %>%
  select(densite_2009:sp_09_18) %>%
  Standar() %>% # nécessaire de standardiser les valeurs du tableau initial
  bind_cols(tab_acp_test %>% select(-densite_2009:-sp_09_18)) %>%
  as_tibble() %>%
  mutate(cluster = factor(typo_tab_general, levels = 1:6))

acp_tableau_visu <- acp_tableau_enrichi %>% # nouvelle colonne avec appartenance des lignes i par classe
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)

a <- acp_tableau_visu %>%
  select(-pop2009:-nb_equip_2018) %>%
  rename(`densité en 2009` = densite_2009,
         `evo. services` = sp_09_18,
         `evo. pop. 1975-1999` = pop_75_99,
         `evo. pop. 1999-2009` = pop_99_09) %>%
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
acp_tableau_enrichi <- st_as_sf(x = acp_tableau_enrichi, wkt = "geometry", crs = 2154)
france <- read_sf("BPE/data_communes_au/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse

b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = acp_tableau_enrichi, aes(fill = cluster), show.legend = FALSE, size = 0.2) +
  scale_fill_tableau(palette = "Tableau 10") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Caractérisation de l'évolution des services publics des aires urbaines")

b / a + plot_layout(heights = c(2, 1))

ggsave(filename = "aires_urbaines_acp_cah_carto.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/toutes_au", dpi = 300,  width = 25, height = 30, units = "cm")

# médiane par classe
acp_tableau_enrichi <- tab_acp_test %>%
  select(densite_2009:sp_09_18) %>%
  Standar() %>% # nécessaire de standardiser les valeurs du tableau initial
  bind_cols(tab_acp_test %>% select(-densite_2009:-sp_09_18)) %>%
  as_tibble() %>%
  mutate(cluster = factor(typo_tab_general, levels = 1:6))

acp_tableau_visu <- acp_tableau_enrichi %>% # nouvelle colonne avec appartenance des lignes i par classe
  group_by(cluster) %>%
  summarise_if(is.numeric, median)

a <- acp_tableau_visu %>%
  select(-pop2009:-nb_equip_2018) %>%
  rename(`densité en 2009` = densite_2009,
         `evo. services` = sp_09_18,
         `evo. pop. 1975-1999` = pop_75_99,
         `evo. pop. 1999-2009` = pop_99_09) %>%
  pivot_longer(cols = -cluster, names_to = "variables", values_to = "moyenne_cluster") %>%
  ggplot() +
  geom_bar(aes(x = moyenne_cluster, y = variables, fill = cluster), stat = "identity", show.legend = FALSE) +
  scale_fill_tableau(palette = "Tableau 10") +
  xlab("Médiane des valeurs standardisées par classe") +
  theme_julie() +
  theme(axis.title.y = element_blank()) +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités\nSources: BPE 2009, 2013, 2018 (Insee), délim. AU 2010 géo. 2019 (Insee), ADMIN EXPRESS géo. 2019 (IGN)",
       subtitle = "Classe de périodes : CAH (distance euclidienne)") +
  facet_wrap(~cluster)

b / a + plot_layout(heights = c(2, 1))

ggsave(filename = "aires_urbaines_acp_cah_carto_median.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/toutes_au", dpi = 300,  width = 25, height = 30, units = "cm")


# ------------------ cah : changements des services publics ------------------------------------
cah_acp <- acp_tableau_2 %>%
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
# donc pour le présent cas, on peut découper en 6 classes

typo_tab_general <- cutree(cah_acp, k = 6)  # k = nombre de classes

acp_tableau_enrichi <- tab_acp_test %>%
  filter(sp_09_18 != 0) %>% 
  select(densite_2009:sp_09_18) %>%
  Standar() %>% # nécessaire de standardiser les valeurs du tableau initial
  bind_cols(tab_acp_test %>% filter(sp_09_18 != 0) %>% select(-densite_2009:-sp_09_18)) %>%
  as_tibble() %>%
  mutate(cluster = factor(typo_tab_general, levels = 1:6))

acp_tableau_visu <- acp_tableau_enrichi %>% # nouvelle colonne avec appartenance des lignes i par classe
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)

a <- acp_tableau_visu %>%
  select(-pop2009:-nb_equip_2018) %>%
  rename(`densité en 2009` = densite_2009,
         `evo. services` = sp_09_18,
         `evo. pop. 1975-1999` = pop_75_99,
         `evo. pop. 1999-2009` = pop_99_09) %>%
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
acp_tableau_enrichi <- st_as_sf(x = acp_tableau_enrichi, wkt = "geometry", crs = 2154)

b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = tab_acp_test %>% st_as_sf(wkt = "geometry", crs = 2154), fill = "grey80", color = "grey70", size = 0.2) +
  geom_sf(data = acp_tableau_enrichi, aes(fill = cluster), show.legend = FALSE, size = 0.2) +
  scale_fill_tableau(palette = "Tableau 10") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Caractérisation de l'évolution des services publics des aires urbaines")

b / a + plot_layout(heights = c(2, 1))

ggsave(filename = "aires_urbaines_acp_cah_carto.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/changement_au", dpi = 300,  width = 25, height = 30, units = "cm")

# résultats plus intéressants quand on travaille sur le changement à proprement parlé que sur l'évolution
# mais ici les densités jouent pour beaucoup alors que l'on a un différentiel initial entre les tailles
# des villes qui est grandement dû au fait que les tailles des établissements (pas connus) sont forcément différenciés

# carto avec cercle proportionnel de taille en 2009
b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_point(data = acp_tableau_enrichi %>% arrange(desc(pop2009)),
    aes(color = cluster, size = pop2009, geometry = geometry),
    stat = "sf_coordinates", alpha = 0.8, show.legend = FALSE
  ) +
  scale_color_tableau(palette = "Tableau 10") +
  scale_size_area(max_size = 55) +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Caractérisation de l'évolution des services publics des aires urbaines")

b / a + plot_layout(heights = c(2, 1))

ggsave(filename = "aires_urbaines_acp_cah_carto_taille.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/changement_au", dpi = 300,  width = 25, height = 30, units = "cm")

ggsave(filename = "aires_urbaines_acp_cah_carto_taille.pdf", plot = last_plot(), device = cairo_pdf,
       path = "BPE/sorties/acp_cah/figures/changement_au", dpi = 300,  width = 25, height = 30, units = "cm")


# ------------- tableau avec 2 périodes d'évolution sp ---------------
# période 2009-2018
tab_acp_test <- sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  select(AU2010, LIBAU2010, pop2009, pop1999, pop1975, annee, typologie, nb_equip, geometry) %>%
  group_by(AU2010, LIBAU2010, pop2009, pop1999, pop1975, geometry, annee) %>%
  summarise_if(is.numeric, sum) %>%
  pivot_wider(id_cols = AU2010:geometry, names_from = annee, values_from = nb_equip) %>%
  rename(nb_equip_2009 = `2009`,
         nb_equip_2013 = `2013`,
         nb_equip_2018 = `2018`) %>%
  mutate(densite_2009 = nb_equip_2009/pop2009*10000) %>%
  mutate(pop_75_99 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop_99_09 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 20),
         sp_09_13 = TCAM(datefin = nb_equip_2013, datedebut = nb_equip_2009, nbannee = 4),
         sp_13_18 = TCAM(datefin = nb_equip_2018, datedebut = nb_equip_2013, nbannee = 5)) %>%
  ungroup()


# il y a deux AU avec NA dans les services publics (suppression)
tab_acp_test <- tab_acp_test %>%
  filter(!is.na(densite_2009) & !is.na(sp_09_13) & !is.na(sp_13_18))

acp_tableau <- tab_acp_test %>% select(densite_2009:sp_13_18)

GGally::ggpairs(acp_tableau %>% filter(sp_09_13 < 30 & sp_13_18 < 30))

acp <- dudi.pca(acp_tableau, scannf = FALSE, nf = ncol(acp_tableau))
explor(acp)

acp_tableau <- acp_tableau %>% filter(sp_09_13 < 30 & sp_13_18 < 30)
acp <- dudi.pca(acp_tableau, scannf = FALSE, nf = ncol(acp_tableau))
explor(acp)


# en s'intéressant seulement aux changements de sp :
acp_tableau_2 <- acp_tableau %>%
  filter(sp_09_13 != 0 | sp_13_18 != 0)
acp_2 <- dudi.pca(acp_tableau_2, scannf = FALSE, nf = ncol(acp_tableau_2))
explor(acp_2)


# ------------------ cah : tab général ------------------------------------
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
# donc pour le présent cas, on peut découper en 6 classes (une classe qui contiendra 2 aires urbaines fufu)

typo_tab_general <- cutree(cah_acp, k = 6)  # k = nombre de classes

acp_tableau_enrichi <- tab_acp_test %>%
  filter(sp_09_13 < 30 & sp_13_18 < 30) %>%
  select(densite_2009:sp_13_18) %>%
  Standar() %>% # nécessaire de standardiser les valeurs du tableau initial
  bind_cols(tab_acp_test %>% filter(sp_09_13 < 30 & sp_13_18 < 30) %>% select(-densite_2009:-sp_13_18)) %>%
  as_tibble() %>%
  mutate(cluster = factor(typo_tab_general, levels = 1:6))

acp_tableau_visu <- acp_tableau_enrichi %>% # nouvelle colonne avec appartenance des lignes i par classe
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)

a <- acp_tableau_visu %>%
  select(-pop2009:-nb_equip_2018) %>%
  rename(`densité en 2009` = densite_2009,
         `evo. sp 2009-2013` = sp_09_13,
         `evo. sp 2013-2018` = sp_13_18,
         `evo. pop. 1975-1999` = pop_75_99,
         `evo. pop. 1999-2009` = pop_99_09) %>%
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
acp_tableau_enrichi <- st_as_sf(x = acp_tableau_enrichi, wkt = "geometry", crs = 2154)

b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = acp_tableau_enrichi, aes(fill = cluster), show.legend = FALSE, size = 0.2) +
  scale_fill_tableau(palette = "Tableau 10") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Caractérisation de l'évolution des services publics des aires urbaines")

b / a + plot_layout(heights = c(2, 1))

ggsave(filename = "aires_urbaines_acp_cah_carto_tcam_2009-2013-2018.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/toutes_au", dpi = 300,  width = 25, height = 30, units = "cm")



# ------------------ cah : tab. changements des services publics ------------------------------------
cah_acp <- acp_tableau_2 %>%
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
# donc pour le présent cas, on peut découper en 6 classes (une classe qui contiendra 2 aires urbaines fufu)

typo_tab_general <- cutree(cah_acp, k = 6)  # k = nombre de classes

acp_tableau_enrichi <- tab_acp_test %>%
  filter(sp_09_13 != 0 | sp_13_18 != 0) %>% 
  filter(sp_09_13 < 30 & sp_13_18 < 30) %>%
  select(densite_2009:sp_13_18) %>%
  Standar() %>% # nécessaire de standardiser les valeurs du tableau initial
  bind_cols(tab_acp_test %>% filter(sp_09_13 != 0 | sp_13_18 != 0) %>% filter(sp_09_13 < 30 & sp_13_18 < 30) %>% select(-densite_2009:-sp_13_18)) %>%
  as_tibble() %>%
  mutate(cluster = factor(typo_tab_general, levels = 1:6))

acp_tableau_visu <- acp_tableau_enrichi %>% # nouvelle colonne avec appartenance des lignes i par classe
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)

a <- acp_tableau_visu %>%
  select(-pop2009:-nb_equip_2018) %>%
  rename(`densité en 2009` = densite_2009,
         `evo. sp 2009-2013` = sp_09_13,
         `evo. sp 2013-2018` = sp_13_18,
         `evo. pop. 1975-1999` = pop_75_99,
         `evo. pop. 1999-2009` = pop_99_09) %>%
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
acp_tableau_enrichi <- st_as_sf(x = acp_tableau_enrichi, wkt = "geometry", crs = 2154)

b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = tab_acp_test %>% st_as_sf(wkt = "geometry", crs = 2154), fill = "grey80", color = "grey70", size = 0.2) +
  geom_sf(data = acp_tableau_enrichi, aes(fill = cluster), show.legend = FALSE, size = 0.2) +
  scale_fill_tableau(palette = "Tableau 10") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Caractérisation de l'évolution des services publics des aires urbaines")

b / a + plot_layout(heights = c(2, 1))


ggsave(filename = "aires_urbaines_acp_cah_carto_tcam_2009-2013-2018.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/changement_au", dpi = 300,  width = 25, height = 30, units = "cm")

# ------------- tableau avec chacun des sp ---------------
# période 2009-2018
tab_densite <- sf_services_publics_aires_urbaines %>%
  filter(annee == "2009") %>%
  select(AU2010, pop2009, annee, nb_equip) %>%
  group_by(AU2010, annee) %>%
  summarise_if(is.numeric, sum) %>%
  pivot_wider(id_cols = c("AU2010", "pop2009"), names_from = annee, values_from = nb_equip) %>%
  rename(nb_equip_2009 = `2009`) %>%
  select(AU2010, pop2009, nb_equip_2009) %>%
  mutate(densite_2009 = nb_equip_2009/pop2009*10000) %>%
  select(-pop2009)

tab_acp_test_2 <- sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  select(AU2010, LIBAU2010, pop2009, pop1999, pop1975, annee, ID, nb_equip, geometry) %>%
  group_by(AU2010, LIBAU2010, pop2009, pop1999, pop1975, geometry, ID, annee) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(annee_typo = paste(annee, ID, sep = "_")) %>%
  pivot_wider(id_cols = AU2010:geometry, names_from = annee_typo, values_from = nb_equip) %>%
  mutate(pop_75_99 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop_99_09 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 20)) %>%
  mutate(police = TCAM(datefin = `2018_1`, datedebut = `2009_1`, nbannee = 9),
         poste = TCAM(datefin = `2018_2`, datedebut = `2009_2`, nbannee = 9),
         secondary_teaching = TCAM(datefin = `2018_7`, datedebut = `2009_7`, nbannee = 9),
         sup_public_only = TCAM(datefin = `2018_8`, datedebut = `2009_8`, nbannee = 9),
         sup_public_or_private = TCAM(datefin = `2018_9`, datedebut = `2009_9`, nbannee = 9),
         justice = TCAM(datefin = `2018_10`, datedebut = `2009_10`, nbannee = 9),
         health = TCAM(datefin = `2018_11`, datedebut = `2009_11`, nbannee = 9)) %>%
  ungroup() %>%
  left_join(., y = tab_densite, by = "AU2010")
# too many NA

# ggpairs
tab_acp_test_2 %>% select(pop_75_99:densite_2009) %>% ggpairs()

rm(tab_densite, tab_acp_test_2)

# ------ avec interpolation ------------
# période 2009-2018
tab_acp_test <- sf_services_publics_aires_urbaines %>%
  filter(validite_temporelle == "2009-2013-2018") %>%
  select(AU2010, LIBAU2010, pop2009, pop1999, pop1975, annee, typologie, nb_equip, geometry) %>%
  group_by(AU2010, LIBAU2010, pop2009, pop1999, pop1975, geometry, annee) %>%
  summarise_if(is.numeric, sum) %>%
  pivot_wider(id_cols = AU2010:geometry, names_from = annee, values_from = nb_equip) %>%
  rename(nb_equip_2009 = `2009`,
         nb_equip_2013 = `2013`,
         nb_equip_2018 = `2018`) %>%
  mutate(densite_2009 = nb_equip_2009/pop2009*10000) %>%
  mutate(pop_75_99 = TCAM(datefin = pop1999, datedebut = pop1975, nbannee = 24),
         pop_99_09 = TCAM(datefin = pop2009, datedebut = pop1999, nbannee = 20),
         sp_09_18 = TCAM(datefin = nb_equip_2013, datedebut = nb_equip_2009, nbannee = 9)) %>%
  ungroup()


# il y a deux AU avec NA dans les services publics (suppression)
tab_acp_test <- tab_acp_test %>%
  filter(!is.na(densite_2009) & !is.na(sp_09_18))

tab_acp_test_sf <- tab_acp_test %>%
  st_as_sf(wkt = "geometry", crs = 2154)
  
grid_hexa <- st_make_grid(x = tab_acp_test_sf, 
                          cellsize = 100*100, # mesures selon le système de projection
                          square = FALSE, # if FALSE : create hexagonal
                          what = "centers")
ggplot() +
  geom_sf(data = tab_acp_test_sf, fill = "grey90", color = "grey85", size = 0.2) +
  geom_sf(data = grid_hexa) +
  ggspatial::annotation_scale(location = "bl",  width_hint = 0.3)

grid_hexa <- grid_hexa %>%
  st_as_sf() %>% # transfo en sf (avant en sfc)
  mutate(id = seq(from = 1, to = length(grid_hexa), 1)) %>% # ajout d'une colonne d'id nécessaire pour la fonction aw_interpolate
  st_set_precision(1000000) %>% # probleme de géometrie initialement : voir https://github.com/r-spatial/sf/issues/603
  lwgeom::st_make_valid()

interpo_grid_2 <- aw_interpolate(.data = grid_hexa, tid = id, # la grille consituée
                                         source = tab_acp_test_sf, sid = AU2010, # les données à interpoler
                                         weight = "sum", # défaut en internsive (not extensive) interpolation
                                         output = "sf", intensive = c("densite_2009", "pop_75_99", "pop_99_09", "sp_09_18"))



acp_tableau <- interpo_grid_2 %>% st_drop_geometry() %>% select(densite_2009:sp_09_18)

GGally::ggpairs(acp_tableau %>% filter(sp_09_18 < 10))

acp <- dudi.pca(acp_tableau, scannf = FALSE, nf = ncol(acp_tableau))
explor(acp)

# ------------------ cah : tab général ------------------------------------
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
# donc pour le présent cas, on peut découper en 6 classes (une classe qui contiendra 2 aires urbaines fufu)

typo_tab_general <- cutree(cah_acp, k = 6)  # k = nombre de classes

acp_tableau_enrichi <- acp_tableau %>%
  Standar() %>% # nécessaire de standardiser les valeurs du tableau initial
  as_tibble() %>%
  mutate(cluster = factor(typo_tab_general, levels = 1:6))

acp_tableau_visu <- acp_tableau_enrichi %>% # nouvelle colonne avec appartenance des lignes i par classe
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)

a <- acp_tableau_visu %>%
  rename(`densité en 2009` = densite_2009,
         `evo. sp 2009-2018` = sp_09_18,
         `evo. pop. 1975-1999` = pop_75_99,
         `evo. pop. 1999-2009` = pop_99_09) %>%
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
  bind_cols(interpo_grid_2 %>% 
              mutate(geom = x) %>%
              st_drop_geometry()) %>%
  st_as_sf(crs = 2154)

b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = acp_tableau_enrichi, aes(fill = cluster), show.legend = FALSE, size = 0.2) +
  scale_fill_tableau(palette = "Tableau 10") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Caractérisation de l'évolution des services publics des aires urbaines")

b / a + plot_layout(heights = c(2, 1))

ggsave(filename = "aires_urbaines_acp_interpolation.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/toutes_au", dpi = 300,  width = 25, height = 30, units = "cm")


#--------- changements ------------
acp_tableau <- interpo_grid_2 %>% 
  st_drop_geometry() %>% 
  select(densite_2009:sp_09_18) %>%
  filter(sp_09_18 != 0)

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
# donc pour le présent cas, on peut découper en 6 classes (une classe qui contiendra 2 aires urbaines fufu)

typo_tab_general <- cutree(cah_acp, k = 6)  # k = nombre de classes

acp_tableau_enrichi <- acp_tableau %>%
  Standar() %>% # nécessaire de standardiser les valeurs du tableau initial
  as_tibble() %>%
  mutate(cluster = factor(typo_tab_general, levels = 1:6))

acp_tableau_visu <- acp_tableau_enrichi %>% # nouvelle colonne avec appartenance des lignes i par classe
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)

a <- acp_tableau_visu %>%
  rename(`densité en 2009` = densite_2009,
         `evo. sp 2009-2018` = sp_09_18,
         `evo. pop. 1975-1999` = pop_75_99,
         `evo. pop. 1999-2009` = pop_99_09) %>%
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
  bind_cols(interpo_grid_2 %>% 
              filter(sp_09_18 != 0) %>%
              mutate(geom = x) %>%
              st_drop_geometry()) %>%
  st_as_sf(crs = 2154)

b <- ggplot() +
  geom_sf(data = france, fill = "grey98", color = "grey50", size = 0.3) +
  geom_sf(data = acp_tableau_enrichi, aes(fill = cluster), show.legend = FALSE, size = 0.2) +
  scale_fill_tableau(palette = "Tableau 10") +
  ggspatial::annotation_scale(location = "tr",  width_hint = 0.2) +
  theme_igray() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Caractérisation de l'évolution des services publics des aires urbaines")

b / a + plot_layout(heights = c(2, 1))

ggsave(filename = "aires_urbaines_acp_interpolation.png", plot = last_plot(), type = "cairo",
       path = "BPE/sorties/acp_cah/figures/changement_au", dpi = 300,  width = 25, height = 30, units = "cm")

