library(tidyverse)
library(ggthemes)
library(sf)
library(classInt)
library(patchwork)

source("fonctions_bases.R")

# --------------- data ------------------
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

# select usefull data :
sf_services_publics_aires_urbaines <- sf_services_publics_aires_urbaines %>%
  select(AU2010, pop2016, pop2009, annee:tailles_2016)


# -------- usefull function for pannelling -----------
panel.cor <- function(x, y, digits = 2, cex.cor, ...) # à partir de http://www.r-bloggers.com/scatter-plot-matrices-in-r/
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  # calcul reg :
  reg <- lm(formula = y ~ x)
  # Ou intercept = b(eta), et autre = a(lpha), sachant que sur linéaire y = ax + b
  # en général ln(y) = aln(x) + b, soit y = e^b * x^a
  # sachant que l'inverse de log10(x) = 10^x [en norme ISO 80000-2 on écrit log10 = lg]
  # lg(y) = alg(x) + b, alors y = 10^b * x^a
  
  # calcul p-value
  p <- cor.test(x, y)$p.value
  txt1 <- format(c(p, 0.123456789), digits = digits)[1]
  txt1 <- paste("p = ", txt1, sep = "")
  if(p < 0.01) txt1 <- paste("p = ", "< 0.01", sep = "")
  text(0.5, 0.7, txt1)
  
  # calcul r²
  rdeux <- summary(reg)$r.squared
  txt2 <- format(c(rdeux, 0.123456789), digits = digits)[1]
  txt2 <- paste("r² = ", txt2, sep = "")
  text(0.5, 0.5, txt2)
  
  # calcul beta (nom générique correspondant plus haut à "a")
  beta <- reg$coefficients[2]
  txt3 <- format(c(beta, 0.123456789), digits = digits)[1]
  txt3 <- paste("beta = ", txt3, sep = "")
  text(0.5, 0.3, txt3)
}

# test général (sachant qu'il y aura probablement des pb quand sp = 1)
sp_au_2009 <- sf_services_publics_aires_urbaines %>%
  filter(annee == "2009") %>%
  select(AU2010, pop2009, typologie, nb_equip, annee) %>%
  mutate(pop2009 = log10(pop2009),
         nb_equip = log10(nb_equip)) %>%
  pivot_wider(id_cols = c("AU2010", "annee", "pop2009"), names_from = typologie, values_from = nb_equip)

sp_au_2009 %>% view()

# beaucoup trop de NA ou de 0 donc clairement inutile selon moi d'y appliquer un modele de ce type,
# en transformant les données > que ce soit par un constante ou non


