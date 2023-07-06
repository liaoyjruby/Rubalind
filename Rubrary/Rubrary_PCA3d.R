# Plotly ex. ----
library(plotly)

mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')))

fig

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

# Start ----
data(iris)
iris$Sample = rownames(iris)
df_pca = Rubrary::run_PCA(t(iris[,c(1:4)]), screeplot = F)
PCs = c(1:3)
anno = iris[,c("Sample", "Species")]
annoname = "Sample"
annotype = "Species"
colors = NULL
title = "Iris PCA"
type = "Scores"
# savename = "/Users/liaoyj/Dropbox/Rubrary/PCA_mtx/PCAmtx_Iris.pdf"
# width = 10
# height = 10

library(plotly)
library(dplyr)

if (is.character(df_pca)) { # PCA results as path to txt
  dfpath <- df_pca
  df_pca <- Rubrary::rread(dfpath)
  sdevpath <- gsub("_[^_]+$", "_sdev.txt", dfpath)
  sdev <- Rubrary::rread(sdevpath) %>%
    mutate(var = .^2, #
           pve = round(var / sum(var) * 100, digits = 2)) %>%
    `rownames<-`(paste0("PC", rownames(.)))
  
  if (grepl("score", dfpath, ignore.case = T)) {
    type <- "Scores"
  } else if(grepl("loading", dfpath, ignore.case = T)){
    type <- "Loadings"
  }
} else if (methods::is(df_pca, "prcomp")){ # prcomp object
  df_sc <- df_pca$x %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Scores")
  
  df_lo <- Rubrary::get_loadings(df_pca) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Loadings")
  
  if(type == "Loadings"){
    df <- df_lo
  } else { # Scores or biplot
    df <- df_sc
  }
  
  sdev <- df_pca$sdev %>%
    as.data.frame() %>%
    mutate(var = .^2,
           pve = round(var / sum(var) * 100, digits = 2)) %>%
    `rownames<-`(paste0("PC", rownames(.)))
  
} else { # PCA results directly as dataframe
  df <- df_pca
  names(df)[1] <- type
}

PCs <- paste0("PC", PCs)
# idx <- match(PCs, names(df))

if(exists("sdev")){
  PClabs <- paste0(PCs, " (", sdev$pve[match(PCs, rownames(sdev))], "%)")
} else { PClabs <- PCs }

if(is.character(anno)) { anno <- Rubrary::rread(anno) }
if(!is.null(anno)){
  df <- df %>% left_join(., anno, by = stats::setNames(nm = "Scores", annoname))
  if(is.null(colors)){ colors <- scales::hue_pal()(length(unique(df[[annotype]])))}
} else { #No anno
  df <- df %>% mutate(anno = "none")
  annotype = "anno"
  if(is.null(colors)){ colors = "black" }
}

## Plot ----
fig <- plot_ly(
  type = "scatter3d", mode = "markers",
  x = ~df[,PCs[1]],
  y = ~df[,PCs[2]],
  z = ~df[,PCs[3]],
  color = ~df[,annotype],
  colors = colors)

fig <- fig %>%
  add_trace(
    marker = list(
      size = 7,
      line = list(
        color = "black",
        opacity = 0.5,
        width = 1
      )
    ),
    showlegend = F
  ) %>%
  layout(
    title = "Iris 3D PCA",
    legend = list(title=list(text=annotype)),
    scene = list(xaxis = list(title = PClabs[1]),
                 yaxis = list(title = PClabs[2]),
                 zaxis = list(title = PClabs[3])))

fig

htmlwidgets::saveWidget(
  plotly::as_widget(fig),
  "/Users/liaoyj/Dropbox/Rubrary/PCA3d/iris.html")

# https://plotly.com/r/static-image-export/
# `orca` required: https://github.com/plotly/orca#installation
# `processx` required: https://github.com/r-lib/processx
Rubrary::use_pkg("processx")
plotly::orca(fig, "/Users/liaoyj/Dropbox/Rubrary/PCA3d/iris.png")
# DEPRECATED - use "kaleido"

# kaleido install
install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')


