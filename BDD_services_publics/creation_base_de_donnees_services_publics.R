library(tidyverse)
library(foreign)
library(readxl)
library(sf)
library(ggthemes)

source("fonctions_bases.R")

# ------------------------------ éducation ----------------------------------
table_services_publics <- read.csv("BDD_services_publics/data_entrees/services_publics.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
table_services_publics$ID <- as.character(table_services_publics$ID)
table_relation_sp_equipement <- read.csv("BDD_services_publics/data_entrees/table_relation_services_publics_BPE.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
table_relation_sp_equipement$ID_sp <- as.character(table_relation_sp_equipement$ID_sp)

table_services_publics <- left_join(x = table_services_publics, y = table_relation_sp_equipement, by = c("ID" = "ID_sp")) %>%
  as_tibble()

rm(table_relation_sp_equipement)

enseignement_2009 <- read.dta(file = "BDD_services_publics/data_entrees/bpe09_enseignement.dta")
enseignement_2009 <- enseignement_2009 %>%
  select(-cant:-CL_PELEM)
enseignement_2009 <- left_join(enseignement_2009, table_services_publics, by = c("typequ" = "ID_BPE"))

enseignement_2013 <- read.dta(file = "BDD_services_publics/data_entrees/bpe13_enseignement.dta")
enseignement_2013 <- enseignement_2013 %>%
  select(-cant:-ep) %>%
  rename(NB_EQUIP = "nb_equip") %>%
  mutate(NB_EQUIP = as.integer(NB_EQUIP))
enseignement_2013 <- left_join(enseignement_2013, table_services_publics, by = c("typequ" = "ID_BPE"))

enseignement_2018 <- read.csv2("BDD_services_publics/data_entrees/bpe18_enseignement.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
enseignement_2018 <- enseignement_2018 %>%
  select(-CANT:-EP)
enseignement_2018 <- left_join(enseignement_2018, table_services_publics, by = c("TYPEQU" = "ID_BPE")) %>%
  mutate(REG = as.character(REG),
         AN = as.character(AN)) %>%
  as_tibble()
colnames(enseignement_2018) <- c("reg", "dep", "depcom", "dciris", "an", "typequ", "sect", "NB_EQUIP", "ID",
                               "nom_general", "typologie", "validite_temporelle", "thesaurus", "RGPP", "regalien",
                               "date", "nom_bpe", "date_validation_thesaurus")

# faire un tableau unique pour les trois dates
enseignement <- bind_rows(enseignement_2009, enseignement_2013, enseignement_2018) %>%
  as_tibble()
rm(enseignement_2009, enseignement_2013, enseignement_2018)


# récupération des services publics étudiés, comptage des équipements par commune
enseignement <- enseignement %>%
  filter(!is.na(ID)) %>%
  group_by(reg, dep, depcom, an, typequ, sect) %>%
  summarise_at(vars("NB_EQUIP"), sum, na.rm = TRUE) %>%
  left_join(., table_services_publics, by = c("typequ" = "ID_BPE"))

# supprimer les communes appartenant aux collectivités d'outre mer et la Corse
enseignement <- enseignement %>%
  filter(dep %ni% c("971", "972", "973", "974", "976", "2A", "2B"))

# sortie intermédiaire
write.csv(x = enseignement, "BDD_services_publics/data_sorties/intermediaires/communes_services_denseignement_France_PU_PR.csv", row.names = FALSE)

# sélection des services d'enseignement publics uniquement
enseignement <- enseignement %>%
  filter(sect == "PU") %>%
  select(-sect)


# --------------------------------- ensemble --------------------------------------
ensemble_2009 <- read.dta(file = "BDD_services_publics/data_entrees/bpe09_ensemble.dta")
ensemble_2009 <- left_join(ensemble_2009, table_services_publics %>% filter(ID %ni% c("7", "8", "9")), # jointure à l'exception des services de l'enseignement
                           by = c("typequ" = "ID_BPE"))

ensemble_2013 <- read.dta(file = "BDD_services_publics/data_entrees/bpe13_ensemble.dta")
ensemble_2013 <- left_join(ensemble_2013, table_services_publics %>% filter(ID %ni% c("7", "8", "9")),
                           by = c("typequ" = "ID_BPE"))

ensemble_2018 <- read.csv2("BDD_services_publics/data_entrees/bpe18_ensemble.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
ensemble_2018 <- left_join(ensemble_2018,  table_services_publics %>% filter(ID %ni% c("7", "8", "9")),
                           by = c("TYPEQU" = "ID_BPE"))
ensemble_2018 <- ensemble_2018 %>%
  mutate(REG = as.character(REG),
         AN = as.character(AN))

bdd_noe_2009 <- read_excel("BDD_services_publics/data_entrees/2009_bdd_noe_guiraud.xls")
bdd_reprise_noe <- data.frame(
  reg = "inconnu",
  dep = as.character(str_sub(string = bdd_noe_2009$INSEECOM, start = 1, end = 2)),
  depcom = bdd_noe_2009$INSEECOM,
  dciris = "inconnu",
  an = "2009"
)

bdd_reprise_noe <- left_join(x = bdd_reprise_noe, y = bdd_noe_2009, by = c("depcom" = "INSEECOM")) %>%
  as_tibble()
bdd_reprise_noe <- bdd_reprise_noe %>%
  select(-Commune) %>%
  pivot_longer(cols = A105:A109, names_to = "typequ", values_to = "nb_equip") %>%
  filter(typequ != "greffe") %>%
  filter(!is.na(nb_equip))

bdd_reprise_noe <- left_join(bdd_reprise_noe, table_services_publics %>% filter(ID %ni% c("7", "8", "9")),
                             by = c("typequ" = "ID_BPE"))

# faire un tableau unique pour les trois dates
colnames(ensemble_2009) <- c("reg", "dep", "depcom", "dciris", "annee", "type_equip", "nb_equip", "ID",
                             "nom_general", "typologie", "validite_temporelle", "thesaurus", "RGPP", "regalien",
                             "date", "nom_bpe", "date_validation_thesaurus")
colnames(ensemble_2013) <- c("reg", "dep", "depcom", "dciris", "annee", "type_equip", "nb_equip", "ID",
                             "nom_general", "typologie", "validite_temporelle", "thesaurus", "RGPP", "regalien",
                             "date", "nom_bpe", "date_validation_thesaurus")
colnames(ensemble_2018) <- c("reg", "dep", "depcom", "dciris", "annee", "type_equip", "nb_equip", "ID",
                             "nom_general", "typologie", "validite_temporelle", "thesaurus","RGPP", "regalien", 
                             "date", "nom_bpe", "date_validation_thesaurus")
colnames(bdd_reprise_noe) <- c("reg", "dep", "depcom", "dciris", "annee", "type_equip", "nb_equip", "ID",
                             "nom_general", "typologie", "validite_temporelle", "thesaurus","RGPP", "regalien", 
                             "date", "nom_bpe", "date_validation_thesaurus")

ensemble <- bind_rows(ensemble_2009, bdd_reprise_noe, ensemble_2013, ensemble_2018) %>%
  as_tibble()
rm(ensemble_2009, ensemble_2013, ensemble_2018, bdd_reprise_noe, bdd_noe_2009)

# type d'équipement précis par année
ensemble <- ensemble %>%
  filter(!is.na(ID)) %>% # virer les équipements non étudiés
  group_by(dep, depcom, annee, type_equip) %>%
  summarise_at(vars(nb_equip), sum, na.rm = TRUE) %>% # somme des équipements à échelle communales
  left_join(., table_services_publics, by = c("type_equip" = "ID_BPE"))

colnames(enseignement) <- c("reg", "dep", "depcom", "annee", "type_equip", "nb_equip", "ID",
                               "nom_general", "typologie", "validite_temporelle", "thesaurus", "RGPP", "regalien",
                            "date", "nom_bpe", "date_validation_thesaurus")

# création du tableau contenant les types d'équipements étudiés de la BPE par année
ensemble <- bind_rows(ensemble, enseignement %>% ungroup() %>% select(-reg))
ensemble <- ensemble %>%
  filter(dep %ni% c("971", "972", "973", "974", "976", "2A", "2B")) # retirer Corse et outre mer

rm(enseignement)

# sortie intermédiaire : 
write.csv(ensemble, "BDD_services_publics/data_sorties/intermediaires/ensemble_communes_equipements_bpe.csv", row.names = FALSE)


# calcul nombre d'équipement pas type de service (thésaurus fonctionnel)
ensemble_thesaurus_fonctionnel <- ensemble %>%
  select(-type_equip, -date:-nom_bpe) %>%
  group_by(dep, depcom, annee, ID) %>%
  summarise_at(vars(nb_equip), sum, na.rm = FALSE) %>%
  left_join(., table_services_publics %>% select(-ID_BPE:-nom_bpe) %>% unique(), by = "ID")

# sortie intermédiaire : tableau des services publics étudiés
write.csv(ensemble_thesaurus_fonctionnel, "BDD_services_publics/data_sorties/intermediaires/services_thesaurus_fonctionnel.csv", row.names = FALSE)


# ------------------------------ données spatiales ---------------------------
## fond France
france <- read_sf("BDD_services_publics/data_spatiales/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=Latin1") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse

## communes : population 2016 dans la délimitation 2019
communes_2019 <- read_sf("BDD_services_publics/data_spatiales/COMMUNES.shp", stringsAsFactors = FALSE, options = "ENCODING=Latin1") %>%
  filter(INSEE_DEP %ni% c("2A", "2B")) %>% # sans la Corse
  st_set_crs(2154)

# fichier des données communales spatialisées et liées aux services analysés (aires urbaines ou non)
# utile pour effectuer un export intermédiaire en fait, mais relativement peu utile pour mes analyses persos
sf_ensemble_thes_fonctionnel <- left_join(ensemble_thesaurus_fonctionnel,
                                          communes_2019 %>%
                                            mutate(geom = geometry) %>%
                                            select(INSEE_COM, INSEE_REG, POPULATION, NOM_COM, geom) %>%
                                            st_drop_geometry(), 
                                          by = c("depcom" = "INSEE_COM")) %>%
  st_as_sf()

# quelle part de perte due à l'évolution de la géographie communale ?
# ici : on parle du nombre de services différents (selon notre typologie), pour toutes les dates,
# mais pas du nombre de services tout court
st_is_empty(sf_ensemble_thes_fonctionnel) %>%
  as_tibble(name = NULL) %>%
  filter(value == TRUE) %>%
  nrow()
# soit 745 unités sans géométrie, i.e :
745/nrow(sf_ensemble_thes_fonctionnel)*100
# 1.00 % "d'erreurs", ce qui concerne cb de communes ?
a <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  select(depcom) %>%
  unique() %>%
  st_drop_geometry() %>%
  nrow() %>%
  as.numeric()
# il y a 14553 communes différentes où l'on a présence des services publics étudiés
d <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  select(depcom) %>%
  unique() %>%
  st_is_empty() %>%
  as_tibble(name = NULL) %>%
  filter(value == TRUE) %>%
  nrow()
# 392 sans géométrie
d/a*100
# soit, 2.69 % des communes où l'on aura un manque dans les représentations spatiales
# et donc en nombre d'équipement, cela concerne ?
b <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  st_drop_geometry() %>%
  summarise_at(vars(nb_equip), sum) %>%
  as.numeric()
# soit 113179 équipements pour les 3 dates dans toutes les communes de France métropolitaine
c <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  mutate(vide = st_is_empty(geom)) %>%
  filter(vide == TRUE) %>%
  st_drop_geometry() %>%
  summarise_at(vars(nb_equip), sum) %>%
  as.numeric()
# soit 812 équipements, donc au final
c/b*100
# cela ne concerne que 0.71 % d'équipements liés à des services publics sur les 3 dates

# sortie de ces éléments :
data_precision_geographie <- data.frame(
  "communes" = c(a, d, d/a*100),
  "équipement" = c(b, c, c/b*100)
)

data_precision_geographie <- mapply(round, data_precision_geographie, 2) %>%
  as.data.frame()
rownames(data_precision_geographie) <- c("Nb général", "Nb sans géométrie", "% sans géométrie")

write.csv(data_precision_geographie, "BDD_services_publics/data_sorties/perte_d_information_geographique.csv", row.names = TRUE)
rm(data_precision_geographie, a, b, c, d)


# nb d'équipement par année
deuxmilleneuf <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  filter(annee == "2009") %>%
  st_drop_geometry() %>%
  summarise_at(vars(nb_equip), sum) %>%
  as.numeric()

deuxmille13 <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  filter(annee == "2013") %>%
  st_drop_geometry() %>%
  summarise_at(vars(nb_equip), sum) %>%
  as.numeric()

deuxmille18 <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  filter(annee == "2018") %>%
  st_drop_geometry() %>%
  summarise_at(vars(nb_equip), sum) %>%
  as.numeric()

# sortie de ces éléments :
nb_equip_annee <- c(deuxmilleneuf, deuxmille13, deuxmille18) %>%
  as.data.frame() %>%
  as_tibble() %>%
  rename("Nb équipement" = ".") %>%
  as.data.frame()

rownames(nb_equip_annee) <- c("2009", "2013", "2018")

write.csv(nb_equip_annee, "BDD_services_publics/data_sorties/nb_equipements_etudies_par_annee.csv", row.names = TRUE)
rm(nb_equip_annee, deuxmilleneuf, deuxmille13, deuxmille18)

# -----------------------> création du tableau des services à échelle communale : toutes les communes
population <- read_excel("BDD_services_publics/data_entrees/populations_communales_arrondissements_donnees_temporelles_geographie_2017.xlsx") %>%
  filter(REG %ni% c("01", "02", "03", "04", "06", "94")) %>% # sans outre-mer & Corse
  select(CODGEO, PMUN13, PMUN09, PSDC99, PSDC90, PSDC82, PSDC75, PSDC68) %>%
  rename(depcom = CODGEO,
         pop2013 = PMUN13,
         pop2009 = PMUN09, 
         pop1999 = PSDC99,
         pop1990 = PSDC90,
         pop1982 = PSDC82,
         pop1975 = PSDC75,
         pop1968 = PSDC68)

sf_ensemble_thes_fonctionnel <- left_join(x = sf_ensemble_thes_fonctionnel, y = population, by = "depcom")

# sortie dans un format long
sf_ensemble_thes_fonctionnel_long <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  mutate(vide = st_is_empty(geom)) %>% # nouvelle colonne : est-ce que TRUE/FALSE les géométries sont vides
  filter(vide != TRUE) %>% # on supprime les lignes sans géométrie, soit les services liés aux communes dont la
  # géographie a évoluée entre 2009 et 2019
  mutate(pop2016 = POPULATION) %>%
  mutate(geometry = geom) %>%
  st_drop_geometry()

geometry <- lwgeom::st_astext(sf_ensemble_thes_fonctionnel_long$geometry)

sf_ensemble_thes_fonctionnel_long <- sf_ensemble_thes_fonctionnel_long %>%
  select(-geometry) %>% # on vire la géométrie
  bind_cols(as_tibble(geometry) %>%  # on adjoint le WKT
              rename(geometry = value))

write.csv(sf_ensemble_thes_fonctionnel_long, "BDD_services_publics/data_sorties/services_publics_communes_long.csv", row.names = FALSE)
rm(geometry, sf_ensemble_thes_fonctionnel_long)

# 1) les données en .csv avec information spatiale en WKT, de toutes les communes de France métropolitaine
# sortie intermédiaire : les communes avec leur géométries (en supprimant celles pour lesquelles on ne les as pas)
# on va transformer en format wide habituel en géographie : soit un tableau d'info géographique dans le temps
# traditionnel du Pumain, Saint-Julien (analyse spatiale), soit :
# une ligne correspond à unité spatiale à une date donnée (répétition des US selon les dates)
sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_thes_fonctionnel %>%
  ungroup() %>%
  mutate(vide = st_is_empty(geom)) %>% # nouvelle colonne : est-ce que TRUE/FALSE les géométries sont vides
  filter(vide != TRUE) %>% # on supprime les lignes sans géométrie, soit les services liés aux communes dont la
  # géographie a évoluée entre 2009 et 2019
  mutate(pop2016 = POPULATION) %>%
  select(-ID, -nom_general, -validite_temporelle:-date_validation_thesaurus, -vide, -POPULATION) %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = typologie, values_from = nb_equip) %>%
  left_join(., communes_2019 %>% 
              mutate(geom = geometry) %>% 
              st_drop_geometry() %>% 
              select(INSEE_COM, geom), 
            by = c("depcom" = "INSEE_COM"))

# extraire les géométries en un WKT en character que l'on adjoint ensuite au dataframe
geometry_2 <- lwgeom::st_astext(sf_ensemble_thes_fonctionnel_wide$geom) # beaucoup plus rapide comme fonction
# voir le benchmark : https://github.com/r-spatial/sf/pull/800

sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_thes_fonctionnel_wide %>%
  select(-geom) %>% # on vire la géométrie
  bind_cols(as_tibble(geometry_2) %>%  # on adjoint le WKT
              rename(geometry = value)) %>% # on le renomme
  select(depcom, NOM_COM, dep, INSEE_REG, annee, pop2016, pop2013:geometry) # réorganisation des colonnes
  # pour que ce soit plus lisibles

# changer les NA en 0 dans les cas où l'on a des connaissances temporelles (selon la validité)

# fonction aux 3 dates
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

sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_thes_fonctionnel_wide %>%
  mutate(`police et gendarmerie nationales` = na_en_zero_2009_a_2018(x = `police et gendarmerie nationales`, annee = annee),
         `bureau de poste` = na_en_zero_2009_a_2018(x = `bureau de poste`, annee = annee),
         `équipements juridictionnels d'ordre judiciaire` = na_en_zero_2009_a_2018(x = `équipements juridictionnels d'ordre judiciaire`, annee = annee),
         `établissement public d'éducation secondaire` = na_en_zero_2009_a_2018(x = `établissement public d'éducation secondaire`, annee = annee),
         `établissement public d'éducation supérieure (filières de formation dans des établissements publics ou privés)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure (filières de formation dans des établissements publics ou privés)`, annee = annee),
         `établissement public d'éducation supérieure (filières de formation dans des établissements exclusivement publics)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure (filières de formation dans des établissements exclusivement publics)`, annee = annee),
         `service de l'emploi avec conseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi avec conseiller spécialisé`, annee = annee),
         `direction des finances publiques` = na_en_zero_2013_a_2018(x = `direction des finances publiques`, annee = annee),
         `service postal de remplacement des bureaux de poste` = na_en_zero_2013_a_2018(x = `service postal de remplacement des bureaux de poste`, annee = annee),
         `service de l'emploi sans conseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi sans conseiller spécialisé`, annee = annee),
         `établissement public ou privé de santé` = na_en_zero_2009_a_2018(x = `établissement public ou privé de santé`, annee = annee))


write.csv(sf_ensemble_thes_fonctionnel_wide, "BDD_services_publics/data_sorties/services_publics_communes.csv", row.names = FALSE)
rm(geometry_2, sf_ensemble_thes_fonctionnel_wide)


# ------------------------------------- communes dans AU ------------------------------------------
communes_2019_au_2010 <- read_excel("BDD_services_publics/data_spatiales/AU2010_au_01-01-2019.xls", sheet = "data_composition_communale")
# Paris, Lyon et Marseille sont ici 3 communes ; pour faire le lien avec les arrondissements, on supprime ces trois communesa
# puis ajout des lignes relatives aux arrondissements (création d'abord d'un tableau arrondissements)
paris <- tibble(
  dep = "751",
  com = c(1:20)
) %>%
  mutate(com = if_else(nchar(as.character(com)) == 1, true = paste("0", com, sep = ""), false = as.character(com))) %>%
  mutate(CODGEO = str_c(dep, com, sep = "")) %>%
  mutate(LIBGEO = "Paris",
         AU2010 = "001",
         LIBAU2010 = "Paris",
         CATAEU2010 = "111",
         DEP = "75",
         REG = "11") %>%
  select(-dep, -com)

marseille <- tibble(
  dep = "132",
  com = c(1:16)
) %>%
  mutate(com = if_else(nchar(as.character(com)) == 1, true = paste("0", com, sep = ""), false = as.character(com))) %>%
  mutate(CODGEO = str_c(dep, com, sep = "")) %>%
  mutate(LIBGEO = "Marseille",
         AU2010 = "003",
         LIBAU2010 = "Marseille",
         CATAEU2010 = "111",
         DEP = "13",
         REG = "93") %>%
  select(-dep, -com)

lyon <- tibble(
  dep = "693",
  com = c(81:89)
) %>%
  mutate(com = if_else(nchar(as.character(com)) == 1, true = paste("0", com, sep = ""), false = as.character(com))) %>%
  mutate(CODGEO = str_c(dep, com, sep = "")) %>%
  mutate(LIBGEO = "Lyon",
         AU2010 = "002",
         LIBAU2010 = "Lyon",
         CATAEU2010 = "111",
         DEP = "69",
         REG = "11") %>%
  select(-dep, -com)

arrondissements <- bind_rows(paris, marseille, lyon)

# lien avec le tableau initial
communes_2019_au_2010 <- communes_2019_au_2010 %>%
  filter(LIBGEO %ni% c("Paris", "Lyon", "Marseille")) %>%
  bind_rows(., arrondissements) %>%
  filter(REG %ni% c("01", "02", "03", "04", "06", "94"))
rm(paris, marseille, lyon, arrondissements)


# lier les informations des communes et celles des services
sf_ensemble_thes_fonctionnel <- sf_ensemble_thes_fonctionnel %>%
  left_join(., y = communes_2019_au_2010 %>% select(-DEP, -REG), by = c("depcom" = "CODGEO"))

# sélection des communes appartenant aux aires urbaines :
sf_ensemble_communes_au <- sf_ensemble_thes_fonctionnel %>%
  filter(CATAEU2010 %ni% c("300", "400", "120")) %>%# soit celle n'étant ni 300 = "Autre commune multipolarisée", 
  # ni 400 = "Commune isolée hors influence des pôles"
  # ni les 120 = "multipolarisées des grands pôles"
  filter(!is.na(NOM_COM)) # cela revient à supprimer les 730 services publics appartenant à des communes ayant
# évoluées dans leur géographie

#------------------------->  sortie dans un format long
sf_ensemble_thes_fonctionnel_long <- sf_ensemble_communes_au %>%
  ungroup() %>%
  mutate(pop2016 = POPULATION) %>%
  mutate(geometry = geom) %>%
  st_drop_geometry()

geometry <- lwgeom::st_astext(sf_ensemble_thes_fonctionnel_long$geometry)

sf_ensemble_thes_fonctionnel_long <- sf_ensemble_thes_fonctionnel_long %>%
  select(-geometry) %>% # on vire la géométrie
  bind_cols(as_tibble(geometry) %>%  # on adjoint le WKT
              rename(geometry = value))

write.csv(sf_ensemble_thes_fonctionnel_long, "BDD_services_publics/data_sorties/services_publics_communes_des_aires_urbaines_long.csv", row.names = FALSE)
rm(geometry, sf_ensemble_thes_fonctionnel_long)


# ---------------------> sorties des communes des AU
# 2) les données en .csv avec information spatiale en WKT des communes des aires urbaines
# sortie intermédiaire : les communes avec leur géométries
# on va transformer en format wide : soit un tableau d'info géographique dans le temps
# traditionnel une ligne correspond à unité spatiale à une date donnée (répétition des US selon les dates)
sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_communes_au %>%
  ungroup() %>%
  mutate(vide = st_is_empty(geom)) %>% # nouvelle colonne : est-ce que TRUE/FALSE les géométries sont vides
  filter(vide != TRUE) %>% # on supprime les lignes sans géométrie, soit les services liés aux communes dont la
  # géographie a évoluée entre 2009 et 2019
  mutate(pop2016 = POPULATION) %>%
  select(-ID, -nom_general, -validite_temporelle:-date_validation_thesaurus, -vide, -POPULATION) %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = typologie, values_from = nb_equip) %>%
  left_join(., communes_2019 %>% 
              mutate(geom = geometry) %>% 
              st_drop_geometry() %>% 
              select(INSEE_COM, geom), 
            by = c("depcom" = "INSEE_COM"))

# extraire les géométries en un WKT en character que l'on adjoint ensuite au dataframe
geometry_2 <- lwgeom::st_astext(sf_ensemble_thes_fonctionnel_wide$geom) # beaucoup plus rapide comme fonction
# voir le benchmark : https://github.com/r-spatial/sf/pull/800

sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_thes_fonctionnel_wide %>%
  select(-geom, -LIBGEO) %>% # on vire la géométrie et le nom (déjà présent)
  bind_cols(as_tibble(geometry_2) %>%  # on adjoint le WKT
              rename(geometry = value)) %>% # on le renomme
  select(depcom, NOM_COM, dep, INSEE_REG, annee, pop2016, pop2013:geometry) # réorganisation des colonnes
# pour que ce soit plus lisibles

# changer les NA en 0 dans les cas où l'on a des connaissances temporelles (selon la validité)
# selon les fonctions plus haut
sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_thes_fonctionnel_wide %>%
  mutate(`police et gendarmerie nationales` = na_en_zero_2009_a_2018(x = `police et gendarmerie nationales`, annee = annee),
         `bureau de poste` = na_en_zero_2009_a_2018(x = `bureau de poste`, annee = annee),
         `équipements juridictionnels d'ordre judiciaire` = na_en_zero_2009_a_2018(x = `équipements juridictionnels d'ordre judiciaire`, annee = annee),
         `établissement public d'éducation secondaire` = na_en_zero_2009_a_2018(x = `établissement public d'éducation secondaire`, annee = annee),
         `établissement public d'éducation supérieure (filières de formation dans des établissements publics ou privés)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure (filières de formation dans des établissements publics ou privés)`, annee = annee),
         `établissement public d'éducation supérieure (filières de formation dans des établissements exclusivement publics)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure (filières de formation dans des établissements exclusivement publics)`, annee = annee),
         `service de l'emploi avec conseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi avec conseiller spécialisé`, annee = annee),
         `direction des finances publiques` = na_en_zero_2013_a_2018(x = `direction des finances publiques`, annee = annee),
         `service postal de remplacement des bureaux de poste` = na_en_zero_2013_a_2018(x = `service postal de remplacement des bureaux de poste`, annee = annee),
         `service de l'emploi sans conseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi sans conseiller spécialisé`, annee = annee),
         `établissement public ou privé de santé` = na_en_zero_2009_a_2018(x = `établissement public ou privé de santé`, annee = annee))


write.csv(sf_ensemble_thes_fonctionnel_wide, "BDD_services_publics/data_sorties/services_publics_communes_des_aires_urbaines.csv", row.names = FALSE)
rm(geometry_2, sf_ensemble_thes_fonctionnel_wide)


# ------------------------- AU fond spatial -----------------------
au_2010_2019 <- read_excel("BDD_services_publics/data_spatiales/AU2010_au_01-01-2019.xls", sheet = "data_AU2010")

# lier les informations sur les AU à la composition communale, en l'occurrence "TAU2016", soit la tranche d'aire urbaine 2016
au_2010_2019_v2 <- au_2010_2019 %>%
  select(AU2010, TAU2016)

communes_2019_au_2010 <- left_join(x = communes_2019_au_2010, y = au_2010_2019_v2, by = "AU2010")
rm(au_2010_2019_v2)

# création d'un objet sf des AU en France métropolitaine
communes_2019_au_2010 <- left_join(x = communes_2019_au_2010, y = communes_2019, by =  c("CODGEO" = "INSEE_COM")) %>%
  st_sf()

au_2010_pop <- communes_2019_au_2010 %>%
  left_join(., y = population, by = c("CODGEO" = "depcom")) %>%
  filter(CATAEU2010 %ni% c("300", "400", "120")) %>% # soit celle n'étant ni 300 = "Autre commune multipolarisée", 
  # ni 400 = "Commune isolée hors influence des pôles"
  # ni les 120 = "multipolarisées des grands pôles"
  st_set_precision(1000000) %>% 
  lwgeom::st_make_valid() %>% # il y a un pb dans les données de l'IGN, en gros une topologie qui n'est pas
  # bien enregistrée (avec une auto-intersection) ; solution trouvée sur https://github.com/r-spatial/sf/issues/603
  # l'idée étant d'amoindrir la précision spatiale pour que le group_by puisse fonctionner (sachant qu'on a ici une précision très très fine,
  # de l'odre du "subsubsubsub-centimetric if we are speaking of a metric projection" pour reprendre la phrase Lorenzo Busetto)
  group_by(AU2010) %>%
  summarise_at(vars(starts_with(match = "pop")), sum) %>%
  rename(pop2016 = POPULATION) %>%
  left_join(x = ., y = au_2010_2019, by = "AU2010") %>%
  st_cast(x = ., to = "MULTIPOLYGON") # ce qui est bien pour faire de l'export en shp si besoin

# --------------------- AU équipements en services publics -----------------------
sf_ensemble_au <- au_2010_pop %>%
  left_join(., y = sf_ensemble_communes_au %>% 
              ungroup() %>% 
              st_drop_geometry() %>% 
              select(-dep, -INSEE_REG, -depcom, -nom_general, -validite_temporelle:-date_validation_thesaurus,
                     -POPULATION:-NOM_COM, -pop2013:-LIBGEO, -LIBAU2010, -CATAEU2010) %>%
              group_by(annee, ID, typologie, AU2010) %>%
              summarise_at(vars(nb_equip), sum),
            by = "AU2010") # mise en relation du sf avec les données relatives aux communes des AU


# --------------------> sortie en format long
sf_ensemble_thes_fonctionnel_long <- sf_ensemble_au %>%
  ungroup() %>%
  mutate(geom = geometry) %>%
  st_drop_geometry()

geometry_2 <- lwgeom::st_astext(sf_ensemble_thes_fonctionnel_long$geom)

sf_ensemble_thes_fonctionnel_long <- sf_ensemble_thes_fonctionnel_long %>%
  select(-geom) %>% # on vire la géométrie
  bind_cols(as_tibble(geometry_2)) %>%  # on adjoint le WKT
  rename(geometry = value)

write.csv(sf_ensemble_thes_fonctionnel_long, "BDD_services_publics/data_sorties/services_publics_aires_urbaines_long.csv", row.names = FALSE)
rm(geometry_2, sf_ensemble_thes_fonctionnel_long)

# -----------> sortie définitive : 
# 3) les données en .csv avec information spatiale en WKT des aires urbaines
# sortie des aires urbaines avec leur géométries
# on va transformer en format wide : soit un tableau d'info géographique dans le temps
# traditionnel une ligne correspond à unité spatiale à une date donnée (répétition des US selon les dates)
sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_au %>%
  ungroup() %>%
  select(-ID) %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = typologie, values_from = nb_equip) %>%
  left_join(., au_2010_pop %>% 
              mutate(geom = geometry) %>% 
              st_drop_geometry() %>% 
              select(AU2010, geom), 
            by = "AU2010")

# extraire les géométries en un WKT en character que l'on adjoint ensuite au dataframe
geometry_2 <- lwgeom::st_astext(sf_ensemble_thes_fonctionnel_wide$geom)

sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_thes_fonctionnel_wide %>%
  select(-geom) %>% # on vire la géométrie
  bind_cols(as_tibble(geometry_2) %>%  # on adjoint le WKT
              rename(geometry = value)) %>% # on le renomme
  select(AU2010, LIBAU2010, TAU2016, NB_COM, annee, pop2016:geometry) # réorganisation des colonnes
# pour que ce soit plus lisibles

# changer les NA en 0 dans les cas où l'on a des connaissances temporelles (selon la validité)
# selon les fonctions plus haut
sf_ensemble_thes_fonctionnel_wide <- sf_ensemble_thes_fonctionnel_wide %>%
  mutate(`police et gendarmerie nationales` = na_en_zero_2009_a_2018(x = `police et gendarmerie nationales`, annee = annee),
         `bureau de poste` = na_en_zero_2009_a_2018(x = `bureau de poste`, annee = annee),
         `équipements juridictionnels d'ordre judiciaire` = na_en_zero_2009_a_2018(x = `équipements juridictionnels d'ordre judiciaire`, annee = annee),
         `établissement public d'éducation secondaire` = na_en_zero_2009_a_2018(x = `établissement public d'éducation secondaire`, annee = annee),
         `établissement public d'éducation supérieure (filières de formation dans des établissements publics ou privés)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure (filières de formation dans des établissements publics ou privés)`, annee = annee),
         `établissement public d'éducation supérieure (filières de formation dans des établissements exclusivement publics)` = na_en_zero_2009_a_2018(x = `établissement public d'éducation supérieure (filières de formation dans des établissements exclusivement publics)`, annee = annee),
         `service de l'emploi avec conseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi avec conseiller spécialisé`, annee = annee),
         `direction des finances publiques` = na_en_zero_2013_a_2018(x = `direction des finances publiques`, annee = annee),
         `service postal de remplacement des bureaux de poste` = na_en_zero_2013_a_2018(x = `service postal de remplacement des bureaux de poste`, annee = annee),
         `service de l'emploi sans conseiller spécialisé` = na_en_zero_2013_a_2018(x = `service de l'emploi sans conseiller spécialisé`, annee = annee),
         `établissement public ou privé de santé` = na_en_zero_2009_a_2018(x = `établissement public ou privé de santé`, annee = annee))


write.csv(sf_ensemble_thes_fonctionnel_wide, "BDD_services_publics/data_sorties/services_publics_aires_urbaines.csv", row.names = FALSE)
rm(geometry_2, sf_ensemble_thes_fonctionnel_wide)
rm(au_2010_2019, communes_2019, communes_2019_au_2010)
