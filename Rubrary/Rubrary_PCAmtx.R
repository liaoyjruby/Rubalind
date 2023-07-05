# Input ----

## Iris ----
data(iris)
iris$Sample = rownames(iris)
PCA_iris <- Rubrary::run_PCA(t(iris[,c(1:4)]), screeplot = F)
# Scores
Rubrary::plot_PCA(
  df_pca = PCA_iris,
  anno = iris[,c("Sample", "Species")],
  annoname = "Sample", annotype = "Species",
  title = "Iris PCA Scores by Species",
  subtitle = "Centered & scaled",
  ellipse = TRUE
)


library(GGally)
Rubrary::use_pkg("GGally")
scores <- PCA_iris$x
PCs <- c(1:4)

ggscatmat(scores)
