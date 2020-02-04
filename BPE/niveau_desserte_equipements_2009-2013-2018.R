library(tidyverse)
library(sf)
library(tmap)
library(readxl)
library(ggthemes)
library(GGally)
library(classInt)

source("fonctions_bases.R")

# -------------- import et réorganisation des données BPE ------------------
## fond France
france <- read_sf("BPE/data_communes_au/REGION.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154) %>% # epsg : 2154, Lambert 93
  filter(NOM_REG != "Corse") # sans la Corse

## communes : population 2016 dans la délimitation 2019
communes_2019 <- read_sf("BPE/data_communes_au/COMMUNES.shp", stringsAsFactors = FALSE, options = "ENCODING=UTF-8") %>%
  st_set_crs(2154)

## BPE 2018 : ensemble et éducation
bpe_ensemble <- read.csv2("BPE/data/bpe18_ensemble.csv", stringsAsFactors = FALSE)
metadonnees_ensemble <- read.csv2("BPE/data/varmod_bpe18_ensemble.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# grouper les IRIS et lier avec les métadonnées pour sélectionner les ensembles qui nous intéressent
bpe_ensemble <- bpe_ensemble %>%
  group_by(DEPCOM, REG, TYPEQU) %>%
  summarise(NB_EQUIP = sum(NB_EQUIP))

bpe_ensemble <- left_join(x = bpe_ensemble, y = metadonnees_ensemble %>% select(COD_MOD, LIB_MOD), by = c("TYPEQU" = "COD_MOD"))

bpe_2018 <- left_join(x = bpe_ensemble, y = communes_2019 %>% select(INSEE_COM, POPULATION, TYPE, NOM_COM), by = c("DEPCOM" = "INSEE_COM")) %>%
  st_as_sf()

rm(bpe_ensemble)


# ---------------- traitements façon T. Saint-Julien : part des communes équipées sur part de la pop desservie -------
bpe_2018_wide <- bpe_2018 %>%
  mutate(equip_1 = 1) %>%
  st_drop_geometry() %>%
  filter(TYPEQU %in% c("A101", "A104", "A105", "A106", "A107", "A108", "A109", "A115", "A199", "A120", 
                       "A121", "A122", "A123", "A124", "A125", "A126", "A206", "A207", "A208", "C201", "C302", 
                       "C303", "C401", "D101", "D102", "D103", "D106", "D107")) %>%
  select(-TYPEQU, -NB_EQUIP) %>%
  pivot_wider(names_from = LIB_MOD, values_from = equip_1)

fonction_part_equip_recup <- function(x){
  part <- sum(x, na.rm = TRUE)/33506*100
}

fonction_part_pop_recup <- function(x){ # avec première colonne étant ID = "id" et 2e population = "pop"
  sum_pop_equipement <- x %>% pivot_longer(-id:-pop, names_to = "equip", values_to = "pres_abs") %>% # données en format long
    mutate(pres_abs = ifelse(pres_abs == 1, pop, pres_abs)) %>% # sachant que si présence, alors population et sinon NA
    pivot_wider(names_from = "equip", values_from = "pres_abs", values_fn = list(pres_abs = sum)) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  somme_pop <- sum(x$pop, na.rm = TRUE)
  part <- sum_pop_equipement/somme_pop*100
  part <- part %>% select(-pop)
}

part_com_equipees_2018 <- bpe_2018_wide %>%
  ungroup() %>%
  select(-DEPCOM:-NOM_COM) %>% 
  summarise_all(list(fonction_part_equip_recup))

part_com_pop_equipees_2018 <- bpe_2018_wide %>%
  ungroup() %>%
  select(DEPCOM, POPULATION, Gendarmerie:`Cour d’appel (CA)`) %>%
  rename("id" = DEPCOM, "pop" = POPULATION) %>%
  fonction_part_pop_recup(.)

tableau_global_2018 <- bind_rows(part_com_equipees_2018, part_com_pop_equipees_2018) %>%
  t() %>%
  as.data.frame() %>%
  rename(part_com_equipee = "V1", part_pop_equipee = "V2") %>%
  rownames_to_column(var = "type") %>%
  as.tibble()

rm(part_com_equipees_2018, part_com_pop_equipees_2018)



# ------------------ graphique ----------------------
tableau_global_2018 %>%
  ggplot(aes(part_com_equipee, part_pop_equipee)) +
  geom_point() +
  xlab("Part des communes équipées (%)") +
  ylab("Part de la population directement desservie (%)") +
  labs(caption = "J. Gravier 2020 | LabEx DynamiTe, UMR Géographie-cités.\n Sources : BPE 2018, Insee") +
  ggrepel::geom_text_repel(
    mapping = aes(part_com_equipee, part_pop_equipee, label = type), size = 3,
    box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50',
  )
  





