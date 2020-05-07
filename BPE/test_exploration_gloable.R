library(tidyverse)
library(ggthemes)
library(sf)
library(patchwork)
library(ade4)
library(explor)
library(Cairo)


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


# construction tableau pour simple exploration ACP-CAH exploratoires
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
# on ne s'intéresse pas au maintien ici mais aux changements donc on les vire

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