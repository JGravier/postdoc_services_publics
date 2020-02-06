#################################################################-
##          filtrages et enrichissements de données           ##
#################################################################-

### Filtrage

#' fonction pour identifier des éléments que l'on ne veut pas dans un vecteur
#' @return l'inverse de l'opérateur %in% 

'%ni%' <- Negate('%in%')


#' fonction pour extraire les n derniers characters d'un string dans une version vectorisée
#' fondée à partir de http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
#' @param xx le vecteur à traiter, type : c("12345","ABCDE")
#' @param n le nombre de characters que l'on souhaite récupérer
#' @return les n derniers characters

str_sub_left <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}


### Enrichissement

#' fonction pour de calculer des taux de croissance annuels moyens
#' 
#' format de données initial : tableau d'information géographique qui doit contenir : en ligne les unités spatiales étudiées, en colonne les variables datées
#' 
#' @param datefin variable : la date de fin de la période étudiée
#' @param datedebut variable : la date de début de la période étudiée
#' @param nbannee le nombre d'années effectives entre date de début et date de fin (pas le plus judicieux)
#' @return le taux de croissance annuel moyen

TCAM <- function(datefin, datedebut, nbannee){
  x <- ((datefin/datedebut)^(1/nbannee)-1)*100
  return(x)
}



#################################################################-
##          transformations de tableaux de données             ##
#################################################################-


### Nouveau tableau avec la même structure que tableau initial

#' création d'un tableau standardisé
#' @param x le tableau de données initial
#' @return le tableau standardisé avec une variance exprimée en 1/n*somme(xi - mean(x))^2

Standar <- function(x){
  ACP <- dudi.pca(x, scannf = FALSE, nf = ncol(x))
  # pas fonction scale, mais dudi.pca car ici la variance est exprimée en 1/n (et non 1/n-1)
  Stand <- ACP$tab
}


#' calcul des résidus de Pearson
#' @param x tableau de contingence
#' @return tableau des résidus de Pearson

tab_ecart_pearson_residus <- function(x){
  x <- as.matrix(x) # transformation en matrice
  x <- chisq.test(x) # test du khi-deux
  x <- x$residuals #  écarts de Pearson = écarts standardisés
  # <=> (tab observé - tab d'indépendance) / sqrt(d'indépendance)
  as.data.frame(x) # transformation en data.frame
}


### Nouveau tableau avec une structure différente que le tableau initial

#' calcul de la part des unités spatiales (US) ayant tel ou tel élément par rapport à l'ensemble des US
#' @exemple La part des communes équipées d'une gendarmerie ou d'un collège dans les communes en France métropolitaine
#' 
#' voir Saint-Julien, Thérèse (coord.), Services et commerces, Montpellier et Paris, Reclus et La Documentation Française,
#' Atlas de France, vol. 10, 1999 p. 71
#' 
#' @param x tableau d'information géographique en présence/absence
#' Il doit contenir : en ligne les unités spatiales étudiées, en colonne les éléments étudiés
#' @param y tableau des unités spatiales étudiées globalement contenant une colonne d'identifiants uniques
#' La colonne des identifiants doit être dénommée "id"
#' @exemple les codes INSEE des communes françaises doivent avoir en intitulé de variable "id"
#' @return liste (nombre et part) des US contenant tel ou tel éléments

fonction_part_equip_recup <- function(x, y){
  y <- y %>% select(id) %>% # sélection des identifiants
    unique() %>% # sélection des identifiants uniques
    nrow() # nombre de lignes (unités spatiales)
  somme <- summarise_all(x, sum, na.rm = TRUE) # somme des présences de chaque éléments
  pourcentage <- somme/y*100 # pourcentage des présences de chaque éléments par rapport à la totalités des unités spatiales
  return(list(somme, pourcentage))
}


#' calcul de la part de la population des unités spatiales (US) contenant tel ou tel élément par rapport à l'ensemble de la population
#' @exemple La part de la population des communes desservies par une gendarmerie ou un collège dans la pop en France métropolitaine
#' 
#' voir Saint-Julien, Thérèse (coord.), Services et commerces, Montpellier et Paris, Reclus et La Documentation Française,
#' Atlas de France, vol. 10, 1999 p. 71
#' 
#' @param x tableau de données en présence/absence contenant en ligne les unités spatiales étudiées
#' colonne 1 : les identifiants des unités spatiales étudiées ; intitulé = "id"
#' colonne 2:n : les éléments en présence/basence
#' @param y tableau des unités spatiales étudiées globalement contenant une colonne d'identifiants uniques
#' colonne 1 : les identifiants des unités spatiales étudiées ; intitulé = "id"
#' colonne 2 : les populations des unités spatiales étudiées ; intitulé = "pop"
#' @return liste (nombre et part) de la population concernées par la présence de tel ou tel élément 

fonction_part_pop_recup <- function(x, y){
  sum_pop_equipement <- y %>%
    select(id, pop) %>% # sélection des identifiants et de la population
    left_join(x = ., y = x, by = "id") %>% # jointure entre les deux tableaux
    pivot_longer(-id:-pop, names_to = "equip", values_to = "pres_abs") %>% # données en format long uniquement sur les éléments
    mutate(pres_abs = ifelse(pres_abs == 1, pop, pres_abs)) %>% # nlle colonne : sachant que si présence, alors population et sinon NA
    pivot_wider(names_from = "equip", values_from = "pres_abs", values_fn = list(pres_abs = sum)) %>% # format wide
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% # calcul des sommes des pop des communes ayant tel ou tel élément
    select(-pop)
  somme_pop <- sum(y$pop, na.rm = TRUE) # population totale étudiée
  pourcentage <- sum_pop_equipement/somme_pop*100 # calcul du pourcentage pop desservie par élément
  return(list(sum_pop_equipement, pourcentage))
}



#################################################################-
##      création de CAH à partir de coordonnées (ACP et AFC)   ##
#################################################################-


#' fonction pour obtenir une CAH à partir des coordonnées d'une ACP
#' @param x le tableau de données initial
#' @return Hierarchical cluster analysis selon la méthode de Ward à partir des coordonnées d'une ACP

CAH_sur_coord_ACP <- function(x){
  pca <- dudi.pca(x, scannf = FALSE, nf = ncol(x), center = TRUE, scale = TRUE)  # ACP
  dist <- dist.dudi(pca)  # distance sur les coordonnées de l'ACP
  hclust(dist, method = "ward.D2")  # CAH sur ces distances, méthode Ward
}


#' fonction pour obtenir une CAH pondérée à partir d'une matrice de distance khi-deux
#' @param x le tableau de données initial
#' @return Hierarchical cluster analysis selon la méthode de Ward à partir des coordonnées d'une AFC
CAH_DistKhi2 <- function(x){
  coa <- dudi.coa(x, scannf = FALSE, nf = ncol(x))  # AFC
  dist <- dist.dudi(coa)  # distance sur les coordonnées de l'AFC
  hclust(dist, method = "ward.D2", 
         members = apply(x, MARGIN = 1, FUN = sum))  # CAH sur ces distances, méthode Ward
  # on a pondéré la CAH selon les poids des lignes (d'après la méthode française mise en place par Benzécri)
}



#################################################################-
##                Thèmes & visualisations de données           ##
#################################################################-


#' fonctions pour obtenir des thèmes visuels "divergent" (color and fill)
#' @rdname scale_divergent
#' @export
#' @import ggplot2 scales
scale_color_divergent <- function(..., low = scales::muted("blue"), mid = "white",
                                  high = scales::muted("red"), midpoint = 0, space = "Lab",
                                  na.value = "grey50", guide = "colourbar") {
  ggplot2::scale_color_gradient2(..., low = low, high = high, mid = mid, midpoint = midpoint,
                                 space = space, na.value = na.value, guide = guide)
}

scale_fill_divergent <- function(..., low = scales::muted("blue"),
                                 mid = "white",
                                 high = scales::muted("red"),
                                 midpoint = 0,
                                 space = "Lab",
                                 na.value = "grey50",
                                 guide = "colourbar") {
  ggplot2::scale_fill_gradient2(..., low = low, high = high, mid = mid,
                                midpoint = midpoint, space = space,
                                na.value = na.value, guide = guide)
}



#' fonction du thème général
theme_julie <- function(){
  julie <- ggthemes::theme_igray() +
    theme(plot.subtitle = element_text(face = "italic"), 
          plot.caption = element_text(size = 8), 
          axis.title = element_text(size = 12), 
          axis.text = element_text(size = 9.5), 
          plot.title = element_text(size = 15, 
                                    face = "bold", hjust = 0.5), legend.text = element_text(size = 11.5), 
          legend.title = element_text(size = 11.7), 
          plot.background = element_rect(fill = "gray97"), 
          legend.key = element_rect(fill = "gray97"), 
          legend.background = element_rect(fill = "gray97"), 
          legend.position = "bottom", legend.direction = "horizontal")
  julie
}
