#construire le package
setwd("/Users/nicot/Documents/github")
library(roxygen2)
roxygenize("MatMobil")
system("R CMD build MatMobil")
system("R CMD check MatMobil")
system("R CMD INSTALL MatMobil")

#desinstalle le package pour une mise a jour
detach(package:MatMobil, unload = TRUE)
remove.packages("MatMobil")
