#' @title Calcul des indices de mobilite dans le temps
#' 
#' @description 
#' \code{ind.mob.tps} calcule differents indices de mobilite pour des donnes longitudinales
#' selon des periodes d'evaluation a definir
#' 
#' @details 
#' La variable de la periode t est toujours compare a celle de la periode t plus
#' l'intervalle k, l'intervalle comparatif k est donc defini par les donnees. Le parametre
#' "intervalle" determine a des intervalles de combien de periode la fonction doit calculer 
#' les indices.
#' 
#' @param debut indique la premiere periode a laquelle les indices doivent etre calcules
#' @param fin indique la periode a laquelle les calcul doivent s'arreter
#' @param intervalle indique a des intervalles de combien de periode la fonction doit calculer 
#' les indices.
#' @param var.t une variable discrete correspondant aux etats possibles a la periode t
#' @param var.tpk une variable discrete correspondant aux etats possibles a la periode t 
#' plus l'intervalle k
#' @param data un dataframe dans lequel sont contenus var.t et var.tpk
#' @param periode une variable definissant les periodes
#' @param poids une variable contenant les poids a considerer
#' @param mobilite un parametre logique indiquant si les indices de mobilite ou d'immobilite
#' doivent etre retournes
#' @export
#' @return une liste de 6 elements
#' \item{CALL}{Appel de la fonction}
#' \item{data}{Les donnees utilisees pour effectuer le calcul des indices}
#' \item{cat.var.t}{Categories que prend la variable var.t}
#' \item{cat.var.tpk}{Categories que prend la variable var.tpk}
#' \item{Poids}{Nom de la variable "poids" utilisee}
#' \item{indices}{Le nom des indices calcules}
#' \item{ind}{La valeur des indices calculees pour chaque periode definie}
#' 
#' @author Nicolas Corneau-Tremblay
#' 
#' @example 
#' library(dplyr)
#' data <- data.frame(
#' id = c(rep(1, 16), rep(2, 16), rep(3, 16), rep(4, 16)),
#'    age = rnorm(64, 40, 10),
#'    male = rbinom(64, 1, 0.5),
#'    rev.t = c(10000, 15000, 20000, 25000),
#'    rev.tp5 = c(rep(10000, 16), rep(15000, 16), rep(20000, 16), rep(25000, 16))
#' )
#'
#' data <- data %>%
#'    mutate(qu.t = cut(rev.t, breaks = quantile(rev.t, seq(0, 1, by = 0.25)),
#'                      include.lowest = TRUE, labels = FALSE)) %>%
#'    mutate(qu.tp5 = cut(rev.tp5, breaks = quantile(rev.tp5, seq(0, 1, by = 0.25)),
#'                        include.lowest = TRUE, labels = FALSE))
#'
#' data <- data %>%
#'    mutate(ordre = runif(nrow(data))) %>%
#'    arrange(ordre) %>%
#'    mutate(annee = c(rep(1983, 16), rep(1988, 16), rep(1993, 16), rep(1998, 16)))
#'    
#' ind.mob.tps(1983, 1998, 5, "qu.t", "qu.tp5", data)
ind.mob.tps <- function(debut, fin, intervalle, var.t, var.tpk, data, periode = "annee", poids = NULL, mobilite = TRUE){
    stopifnot(length(data[,c(var.t)]) == length(data[,c(var.tpk)]))
    stopifnot(is.data.frame(data))
    
    tour <- 1
    periode.t <- vector(, length(seq(debut, fin, intervalle)))
    n.obs <- vector(, length(seq(debut, fin, intervalle)))
    
    if (mobilite == TRUE) {
        
        ind.ratio.mob <- vector(, length(seq(debut, fin, intervalle)))
        ind.ratio.mob.aj <- vector(, length(seq(debut, fin, intervalle)))
        ind.trace.norm <- vector(, length(seq(debut, fin, intervalle)))
        ind.saut.moy <- vector(, length(seq(debut, fin, intervalle)))
        
        for (i in seq(debut, fin, intervalle)) {
            
            mat <- mat.tr(var.t, var.tpk, data[subset(data, select = periode) == i, ], poids = poids, prob = TRUE)
            ind.ratio.mob[tour] <- ratio.mob(mat)$ind
            ind.ratio.mob.aj[tour] <- ratio.mob.aj(mat)$ind
            ind.trace.norm[tour] <- trace.norm(mat)$ind
            ind.saut.moy[tour] <- saut.moy(mat)$ind
            n.obs[tour] <- mat$n.obs
            
            periode.t[tour] <- i
            tour <- tour + 1
        }
        
        indices <- c("Ratio de mobilite", "Ratio de mobilite ajuste", "Trace normalisee", "Saut moyen (Bartholomew)")
        ind <- cbind.data.frame(periode.t, n.obs, ind.ratio.mob, ind.ratio.mob.aj, ind.trace.norm, ind.saut.moy)
        
    }
    
    if (mobilite == FALSE) {
        
        ind.ratio.im <- vector(, length(seq(debut, fin, intervalle)))
        ind.ratio.im.aj <- vector(, length(seq(debut, fin, intervalle)))
        ind.trace.norm <- vector(, length(seq(debut, fin, intervalle)))
        ind.saut.moy <- vector(, length(seq(debut, fin, intervalle)))
        
        for (i in seq(debut, fin, intervalle)) {
            
            mat <- mat.tr(var.t, var.tpk, data[subset(data, select = periode) == i, ], poids = poids, prob = TRUE)
            ind.ratio.im[tour] <- ratio.im(mat)$ind
            ind.ratio.im.aj[tour] <- ratio.im.aj(mat)$ind
            ind.trace.norm[tour] <- trace.norm(mat)$ind
            ind.saut.moy[tour] <- saut.moy(mat)$ind
            n.obs[tour] <- mat$n.obs
            
            periode.t[tour] <- i
            tour <- tour + 1
        }
        
        indices <- c("Ratio d'immobilite", "Ratio d'immobilite ajuste", "Trace normalisee", "Saut moyen (Bartholomew)")
        ind <- cbind.data.frame(periode.t, n.obs, ind.ratio.im, ind.ratio.im.aj, ind.trace.norm, ind.saut.moy)
        
    }
    
    cat.var.t <- toString(levels(as.factor(unique(data[,c(var.t)]))))
    cat.var.tpk <- toString(levels(as.factor(unique(data[,c(var.tpk)]))))
    
    output <- list( CALL = match.call(),
                    data = data,
                    cat.var.t = cat.var.t,
                    cat.var.tpk = cat.var.tpk,
                    poids = poids,
                    indices = indices,
                    ind = ind
    )
    class(output) <- c("ind.mob.tps", "list")
    return(output)
}