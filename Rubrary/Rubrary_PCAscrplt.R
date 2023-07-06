library(dplyr)
library(ggplot2)

data(iris)
PCA_iris <- Rubrary::run_PCA(t(iris[,c(1:4)]), screeplot = FALSE)

obj_prcomp <- PCA_iris
npcs = ncol(obj_prcomp$x)
label = FALSE
cum_var_exp = 80
savename = NULL

debugonce(Rubrary::plot_screeplot)
Rubrary::plot_screeplot(PCA_iris)