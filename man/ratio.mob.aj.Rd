% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratio_mob_aj.R
\name{ratio.mob.aj}
\alias{ratio.mob.aj}
\title{Calcul du ratio de mobilité ajusté}
\usage{
ratio.mob.aj(mat)
}
\arguments{
\item{mat}{une matrice de transition ou un objet "mat.tr"}
}
\value{
une liste de 4 éléments
\item{CALL}{Appel de la fonction}
\item{matrice}{Matrice de transition utilisée}
\item{indice}{Nom de l'indice calculé}
\item{ind}{Valeur de l'indice calculé}
}
\description{
\code{ratio.mob.aj} calcule le ratio de mobilite ajusté d'une matrice de transition
}
\details{
1 - Proportion d'individu sur la diagonale de la matrice de transition et ceux contenus
dans les cellules adjacentes à la diagonale
}
\author{
Nicolas Corneau-Tremblay
}
