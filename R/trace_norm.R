#' @title Calcul de la trace normaliseé
#' 
#' @description 
#' \code{trace.norm} calcule la trace normalisée d'une matrice de transition
#' 
#' @details 
#' Nombre d'états de transition possibles - la trace de la matrice de transition (en prob) 
#' divisé par le nombre d'états de transition possibles - 1 : ((n - trace(mat)) / (n - 1 ))
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
trace.norm <- function(mat){
    if (inherits(mat, "mat.tr"))  mat <- mat$matrice
    
    if(is.matrix(mat)) {
        
        ind <- (nrow(mat) - sum(diag(mat) / rowSums(mat))) / (nrow(mat) - 1)
        
    } else {
        
        stop("'mat' n'est pas une matrice")
        
    }
    
    output <- list(CALL = match.call(),
                   matrice = mat,
                   indice = "Trace normalisee",
                   ind = ind)
    class(output) <- c("trace.norm", "list")
    return(output)
    
}