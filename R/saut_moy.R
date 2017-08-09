#' @title Calcul du saut moyen
#' 
#' @description 
#' \code{trace.norm} calcule le saut moyen d'une matrice de transition
#' 
#' @details 
#' Aussi appelé indice de Bartholomew. Somme sur i des pi_i * somme sur j
#' des p_ij * valeur absolue de i moins j
#' 
#' @param mat une matrice de transition ou un objet "mat.tr"
#' @export
#' @return une liste de 4 éléments
#' \item{CALL}{Appel de la fonction}
#' \item{matrice}{Matrice de transition utilisée}
#' \item{indice}{Nom de l'indice calculé}
#' \item{ind}{Valeur de l'indice calculé}
#' 
#' @author Nicolas Corneau-Tremblay
#' 
#' @example 
#' A completer
saut.moy <- function(mat){
    if (inherits(mat, "mat.tr"))  mat <- mat$matrice
    
    if(is.matrix(mat)) {
        
        pi <- (1 / sum(mat)) * rowSums(mat)
        vect <- vector(, length = nrow(mat))
        
        for (row in 1:nrow(mat)){
            
            temp <- 0
            
            for(col in 1:ncol(mat)){
                
                temp <- temp + mat[row, col]*abs(row - col)
                
            }
            
            vect[row] <- temp
        }
        
        ind <- pi %*% vect
        
    } else {
        
        stop("'mat' n'est pas une matrice")
        
    }
    
    output <- list(CALL = match.call(),
                   matrice = mat,
                   indice = "Saut moyen (Bartholomew)",
                   ind = ind)
    class(output) <- c("saut.moy", "list")
    return(output)
    
}