#' @title Calcul d'une matrice de transition
#' 
#' @description 
#' \code{mat.tr} calcule une matrice de transition
#' 
#' @details 
#' La variable de la période t est toujours comparée à celle de la période t plus
#' l'intervalle k. L'intervalle comparatif k est donc defini par les données.
#' 
#' @param var.t une variable discrète correspondant aux états possibles à la période t
#' @param var.tpk une variable discrète correspondant aux états possibles à la période t 
#' plus l'intervalle k
#' @param data un dataframe dans lequel sont contenues var.t et var.tpk
#' @param poids une variable contenant les poids à considerer
#' @param prob un paramètre logique indiquant si les cellules de la matrice retournée 
#' doivent contenir des probabilités ou le nombre d'observations qu'elles contiennent
#' @export
#' @return une liste de 3 éléments
#' \item{CALL}{Appel de la fonction}
#' \item{Poids}{Nom de la variable "poids" utilisée}
#' \item{n.obs}{Nombre d'observations (si "poids" specifié, somme des poids individuels)}
#' \item{taille.mat}{Taille de la matrice retournée}
#' \item{matrice}{Matrice de transition calculée}
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
