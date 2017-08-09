#' @title Calcul du ratio d'immobilité
#' 
#' @description 
#' \code{ratio.im} calcule le ratio d'immobilité d'une matrice de transition
#' 
#' @details 
#' A completer
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
ratio.im <- function(mat){
    if (inherits(mat, "mat.tr"))  mat <- mat$matrice
    
    if(is.matrix(mat)) {
        
        ind <- sum(diag(mat))/sum(mat)
        
    } else {
        
        stop("'mat' n'est pas une matrice")
        
    }
    
    output <- list(CALL = match.call(),
                   matrice = mat,
                   indice = "Ratio d'immobilite",
                   ind = ind)
    class(output) <- c("ratio.im", "list")
    return(output)
    
}
