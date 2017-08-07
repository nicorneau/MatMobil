#' @title Calcul d'une matrice de transition
#' 
#' @description 
#' \code{mat.tr} calcule une matrice de transition
#' 
#' @details 
#' La variable de la periode t est toujours compare a celle de la periode t plus
#' l'intervalle k, l'intervalle comparatif k est donc defini par les donnees.
#' 
#' @param var.t une variable discrete correspondant aux etats possibles a la periode t
#' @param var.tpk une variable discrete correspondant aux etats possibles a la periode t 
#' plus l'intervalle k
#' @param data un dataframe dans lequel sont contenus var.t et var.tpk
#' @param poids une variable contenant les poids a considerer
#' @param prob un parametre logique indiquant si les cellules de la matrice retournee 
#' doivent contenir des probabilites ou le nombre d'observations si retrouvant
#' @export
#' @return une liste de 3 elements
#' \item{CALL}{Appel de la fonction}
#' \item{Poids}{Nom de la variable "poids" utilisee}
#' \item{n.obs}{Nombre d'observations (si "poids" specifie, somme des poids individuels)}
#' \item{taille.mat}{Taille de la matrice retournee}
#' \item{matrice}{La matrice de transition calculee}
#' 
#' @author Nicolas Corneau-Tremblay
#' 
#' @example 
#' A completer
mat.tr <- function(var.t, var.tpk, data, poids = NULL, prob=FALSE) {
    if(missing(data)){
        
        stopifnot(length(var.t)!=length(var.tpk))
        
        matrice <- table(var.t, var.tpk)
        
    } else{
        
        if(!is.null(poids)) {
            matrice <- matrix(rep(0, times = length(unique(data[,c(var.t)]))*length(unique(data[,c(var.tpk)]))),
                              ncol = length(unique(data[,c(var.tpk)])))
            
            for (col in sort(unique(data[,c(var.tpk)]))){
                
                for(row in sort(unique(data[,c(var.t)]))) {
                    
                    matrice[row, col] <- sum(data[subset(data, select = var.t) == row
                                                  & subset(data, select = var.tpk) == col, poids])
                    
                }
            }
        } else {
            
            matrice <- table(data[, var.t], data[, var.tpk])
        }
        
    }
    
    n.obs <- sum(matrice)
    
    if(prob) matrice <- matrice / rowSums(matrice)
    
    
    
    if(nrow(matrice)!=ncol(matrice)){
        
        stop("matrice non-carree")
        
    } else {
        
        output <- list(CALL = match.call(),
                       poids = poids,
                       n.obs = n.obs,
                       taille.mat = paste(nrow(matrice), "x", ncol(matrice)),
                       matrice = matrice
        )
        class(output) <- c("mat.tr", "list")
        return(output)
    }
}
