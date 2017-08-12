#' @title Calcul de la mobilité vers le haut
#' 
#' @description 
#' \code{mob.haut} calcule la proportion d'individus ayant monter de quantile
#' 
#' @details 
#' A compléter
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
mob.haut <- function(mat){
    if (inherits(mat, "mat.tr"))  mat <- mat$matrice
    
    if(is.matrix(mat)) {
        
        temp <- 0
        
        for (col in 1:ncol(mat)){
            
            for(row in 1:nrow(mat)) {
                
                if(row < col) temp <- temp + mat[row, col]
            }
        }
        
        ind <- temp / sum(mat)
        
    } else {
        
        stop("'mat' n'est pas une matrice")
        
    }
    
    output <- list(CALL = match.call(),
                   matrice = mat,
                   indice = "Mobilite vers le haut",
                   ind = ind)
    class(output) <- c("mob.haut", "list")
    return(output)
}
