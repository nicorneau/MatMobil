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
    
    temp <- 0
    
    for (col in 1:ncol(mat)){
      
      for(row in 1:nrow(mat)) {
        
        if(abs(col - row) <= 1) temp <- temp + mat[row, col]
      }
    }
    
    ind <- temp / sum(mat)
    
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