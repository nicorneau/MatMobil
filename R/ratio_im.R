#' @title Calcul le ratio d'immobilite
#' 
#' @description 
#' \code{ratio.im} calcule le ratio d'immobilite d'une matrice de transition
#' 
#' @details 
#' A completer
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
