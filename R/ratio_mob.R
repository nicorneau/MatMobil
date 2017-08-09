#' @title Calcul du ratio de mobilité
#' 
#' @description 
#' \code{ratio.mob} calcule le ratio de mobilité d'une matrice de transition
#' 
#' @details 
#' 1 - Proportion d'individu sur la diagonale de la matrice de transition
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
ratio.mob <- function(mat){
    if (inherits(mat, "mat.tr"))  mat <- mat$matrice
    
    if(is.matrix(mat)) {
        
        ind <- 1 - (sum(diag(mat))/sum(mat))
        
    } else {
        
        stop("'mat' n'est pas une matrice")
        
    }
    
    output <- list(CALL = match.call(),
                   matrice = mat,
                   indice = "Ratio de mobilite",
                   ind = ind)
    class(output) <- c("ratio.mob", "list")
    return(output)
    
}