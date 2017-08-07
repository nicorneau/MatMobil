#' @title Calcul le ratio d'immobilite ajuste
#' 
#' @description 
#' \code{ratio.im.aj} calcule le ratio d'immobilite ajuste d'une matrice de transition
#' 
#' @details 
#' Proportion d'individu sur la diagonale de la matrice de transition plus ceux dans les 
#' cellules adjacentes a la diagonale
#' 
#' @param mat une matrice de transition ou un objet "mat.tr"
#' @export
#' @return une liste de 4 elements
#' \item{CALL}{Appel de la fonction}
#' \item{matrice}{La matrice de transition utilisee}
#' \item{indice}{Le nom de l'indice calcule}
#' \item{ind}{La valeur de l'indice calculee}
#' 
#' @author Nicolas Corneau-Tremblay
#' 
#' @example 
#' A completer
ratio.im.aj <- function(mat){
    if (inherits(mat, "mat.tr"))  mat <- mat$matrice
    
    if(is.matrix(mat)) {
        
        #pour une matrice 4x4
        if(nrow(mat) == 4) {
            
            ind <- (sum(mat[c(1,2),c(1,2)]) +
                        sum(mat[c(3, 4),c(3, 4)]) +
                        mat[3,2] +
                        mat[2,3]) /
                sum(mat)
            
        } else if (nrow(mat) == 5) {
            #pour une matrice 5x5
            ind <- (sum(mat[c(1,2),c(1,2)]) +
                        sum(mat[c(4, 5),c(4, 5)]) +
                        sum(mat[3, c(2, 3, 4)]) +
                        mat[2,3] +
                        mat[4,3]) /
                sum(mat)
            
        } else {
            
            stop("dimensions inadequates")
            
        }
    } else {
        
        stop("'mat' n'est pas une matrice")
    }
    
    output <- list(CALL = match.call(),
                   matrice = mat,
                   indice = "Ratio d'immobilite ajuste",
                   ind = ind)
    class(output) <- c("ratio.im.aj", "list")
    return(output)
}