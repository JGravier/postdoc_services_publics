library(ggthemes)


theme_julie <- function(){
  julie <- theme_igray() +
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


'%ni%' <- Negate('%in%')

TCAM <- function(datefin, datedebut, nbannee){
  x <- ((datefin/datedebut)^(1/nbannee)-1)*100
  return(x)
}

scale_color_divergent <- function(..., low = scales::muted("blue"), mid = "white",
                                  high = scales::muted("red"), midpoint = 0, space = "Lab",
                                  na.value = "grey50", guide = "colourbar") {
  ggplot2::scale_color_gradient2(..., low = low, high = high, mid = mid, midpoint = midpoint,
                                 space = space, na.value = na.value, guide = guide)
}

#' @rdname scale_divergent
#' @export
#' @import ggplot2 scales
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

palette_wesanderson1 <- wesanderson::wes_palette(name = "Zissou1", n = 50, type = "continuous")

str_sub_left <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}

# tableau standardisé
Standar <- function(x){
  ACP <- dudi.pca(x, scannf = FALSE, nf = ncol(x))
  # pas fonction scale, mais dudi.pca car ici la variance est exprimée en 1/n (et non 1/n-1)
  Stand <- ACP$tab
}

# Fonction pour abtenir une CAH à partir des coordonnés d'une ACP
CAH_sur_coord_ACP <- function(x){
  pca <- dudi.pca(x, scannf = FALSE, nf = ncol(x), center = TRUE, scale = TRUE)  # ACP
  dist <- dist.dudi(pca)  # distance sur les coordonnées de l'ACP
  hclust(dist, method = "ward.D2")  # CAH sur ces distances, méthode Ward
}


# Fonction pour abtenir une CAH pondérée à partir d'une matrice de distance khi-deux
CAH_DistKhi2 <- function(x){
  coa <- dudi.coa(x, scannf = FALSE, nf = ncol(x))  # AFC
  dist <- dist.dudi(coa)  # distance sur les coordonnées de l'AFC
  hclust(dist, method = "ward.D2", 
         members = apply(x, MARGIN = 1, FUN = sum))  # CAH sur ces distances, méthode Ward
  # on a pondéré la CAH selon les poids des lignes (d'après la méthode française mise en place par JP Benzécri)
}


# calcul des résidus de Pearson
tab_ecart_pearson_residus <- function(x){
  x <- as.matrix(x)
  x <- chisq.test(x)
  x <- x$residuals #  écarts de Pearson = écarts standardisés
  # <=> (tab observé - tab d'indépendance) / sqrt(d'indépendance)
  as.data.frame(x)
}