#' @title Calcul du ratio d'immobilité ajusté
#' 
#' @description 
#' \code{ratio.im.aj} calcule le ratio d'immobilité ajusté d'une matrice de transition
#' 
#' @details 
#' Proportion d'individu sur la diagonale de la matrice de transition et ceux contenus 
#' dans les cellules adjacentes à la diagonale
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