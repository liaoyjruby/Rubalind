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
## OVB5 SCN ----
UCLA_OV_B12345_PCA <- Rubrary::run_PCA(
  df = "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/ovarian_ucla12345_rsem_genes_upper_norm_counts_coding_log2.txt",
  center = T, scale = F,
  # savename = "/Users/liaoyj/Library/CloudStorage/Dropbox/OV/Batch5/PCA/UCLA_OV_B12345_PCA"
)

UCLA_OV_anno <- Rubrary::rread("/Users/liaoyj/Library/CloudStorage/Dropbox/Ovarian Project/UCLA cohort information/UCLAOvarian_CombinedBatches_Annotation_v11.txt") %>%
  mutate(TreatmentStatus = factor(TreatmentStatus, levels = ts_lvls))
ts_lvls <- c("Chemonaive", "Post-NACT", "Recurrent", "NA (CellLine)", "NA (Control)")
ts_cols <- setNames(nm = ts_lvls,
                    c(scales::hue_pal()(3), "grey30", "grey60"))


df_pca = UCLA_OV_B12345_PCA
PCs = c(1:3)
anno = UCLA_OV_anno
annoname = "Sample_ID"
annolabel = annoname
annotype = "SCN_Score_Proj"
colors = c("blue", "red")
# annotype = "TreatmentStatus"
# colors = unname(ts_cols)
title = "UCLA OV 3D PCA - SCN"
type = "Scores"
label = TRUE
savename = "/Users/liaoyj/Dropbox/Rubrary/PCA3d/UCLAOV.png"
rotate = FALSE
width = 10
height = 10

## Iris ----
data(iris)
iris$Sample = rownames(iris)
data(iris)
iris$Sample = rownames(iris)
plot_PCA_3D(
  df_pca = Rubrary::run_PCA(t(iris[,c(1:4)]), screeplot = F),
  PCs = c(1:3),
  type = "Scores",
  anno = iris[,c("Sample", "Species")],
  annoname = "Sample", annotype = "Species",
  title = "Iris PCA Scores 3D"
)



# Start ----

library(plotly)
library(dplyr)

data(iris)
iris$Sample = rownames(iris)
df_pca = Rubrary::run_PCA(t(iris[,c(1:4)]), screeplot = F)
PCs = c(1:3)
anno = iris[,c("Sample", "Species")]
annoname = "Sample"
annolabel = annoname
annotype = "Species"
colors = NULL
title = "Iris 3D PCA"
type = "Scores"
label = TRUE
savename = "/Users/liaoyj/Dropbox/Rubrary/PCA3d/iris.png"
rotate = FALSE
width = 10
height = 10

#' Plot 3D PCA scores/loadings via `plotly`
#'
#' Image saving requires `kaleido` python package setup via `reticulate` R package. See `?plotly::save_image` for more details and setup instructions.
#'
#' @import dplyr
#'
#' @param df_pca string/`prcomp` obj; (path to) PCA output with sample names in first column
#' @param PCs num vector; list of numeric PCs to plot (ex. `c(1:3)` for first 3 PCs)
#' @param type `c("Score", "Loading")`
#' @param anno string/df; Annotation info for `df_pca` with `annoname`, `annotype`, and `annolabel` columns
#' @param annoname string; Colname in `anno` matching point name
#' @param annotype string; Colname in `anno` with info to color by
#' @param annolabel string; Colname in `anno` to label points by, defaults to `annoname`
#' @param label logical; T to label points
#' @param colors char vector; For discrete `annotype`, length should be number of unique `annotype`s.
#' @param title string; Plot title
#' @param savename string; File path to save plot under, `html` if not an image format
#' @param height numeric; Saved plot height if saving as image format
#' @param width numeric; Saved plot width
#' @param df_pca string or `prcomp` obj; (path to) PCA output
#' @param rotate logical; T to have the HTML widget automatically rotate when opened. Only applicable if `savename` is not `NULL`
#'
#' @return
#' @export
#'
#' @examples
#' data(iris)
#' iris$Sample = rownames(iris)
#' plot_PCA_3D(
#'   df_pca = Rubrary::run_PCA(t(iris[,c(1:4)]), screeplot = F),
#'   PCs = c(1:3),
#'   type = "Scores",
#'   anno = iris[,c("Sample", "Species")],
#'   annoname = "Sample", annotype = "Species",
#'   title = "Iris PCA Scores 3D"
#' )
#'
plot_PCA_3D <- function(
    df_pca, PCs = c(1:3), type = c("Scores", "Loadings"),
    anno = NULL, annoname = "Sample", annotype = "Type",
    annolabel = annoname, label = FALSE, colors = NULL,
    title = NULL, savename = NULL, rotate = FALSE,
    width = 10, height = 10){
  type <- match.arg(type)
  annolabel <- ifelse(annolabel == annoname, type, annolabel)

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

  if(exists("sdev")){
    PClabs <- paste0(PCs, " (", sdev$pve[match(PCs, rownames(sdev))], "%)")
  } else { PClabs <- PCs }

  if(is.character(anno)) { anno <- Rubrary::rread(anno) }
  if(!is.null(anno) && type == "Scores"){
    df <- df %>% left_join(., anno, by = stats::setNames(nm = "Scores", annoname))
    if(is.null(colors)){ colors <- scales::hue_pal()(length(unique(df[[annotype]])))}
    showleg = TRUE
  } else { #No anno
    df <- df %>% mutate(anno = "none")
    annotype = "anno"
    showleg = FALSE
    if(is.null(colors)){ colors = "black" }
  }

  ## Plot ----
  Rubrary::use_pkg("plotly", strict = TRUE)
  fig <- plotly::plot_ly(
    type = "scatter3d", mode = "markers",
    x = ~df[,PCs[1]],
    y = ~df[,PCs[2]],
    z = ~df[,PCs[3]],
    color = ~df[,annotype],
    colors = colors,
    showlegend = showleg)

  fig <- fig %>%
    plotly::add_trace(
      marker = list(
        size = 7,
        line = list(
          color = "black",
          opacity = 0.5,
          width = 1)),
      showlegend = F
    )

  if(label){ fig <- fig %>%
    plotly::add_text(text = ~df[, annolabel], showlegend = F)}
  fig <- fig %>%
    plotly::layout(
      title = title,
      legend = list(title=list(text=annotype)),
      scene = list(
        xaxis = list(title = PClabs[1]),
        yaxis = list(title = PClabs[2]),
        zaxis = list(title = PClabs[3])))

  if(!is.null(savename)){
    ## Rotation ----
    # @ismirsehregal on StackOverflow
    # https://stackoverflow.com/questions/71042818/plot-ly-rotate-animation-in-r
    if(rotate){
      Rubrary::use_pkg("htmlwidgets")
      fig <- fig %>%
        plotly::layout(
          camera = list(
            eye = list(
              x = 1.25,
              y = 1.25,
              z = 1.25
            ),
            center = list(x = 0, y = 0, z = 0)
          )) %>%
        htmlwidgets::onRender("
        function(el, x){
    var id = el.getAttribute('id');
    var gd = document.getElementById(id);
    Plotly.update(id).then(attach);
    function attach() {
      var cnt = 0;

      function run() {
        rotate('scene', Math.PI / 720); # speed
        requestAnimationFrame(run);
      }
      run();

      function rotate(id, angle) {
        var eye0 = gd.layout[id].camera.eye
        var rtz = xyz2rtz(eye0);
        rtz.t += angle;

        var eye1 = rtz2xyz(rtz);
        Plotly.relayout(gd, id + '.camera.eye', eye1)
      }

      function xyz2rtz(xyz) {
        return {
          r: Math.sqrt(xyz.x * xyz.x + xyz.y * xyz.y),
          t: Math.atan2(xyz.y, xyz.x),
          z: xyz.z
        };
      }

      function rtz2xyz(rtz) {
        return {
          x: rtz.r * Math.cos(rtz.t),
          y: rtz.r * Math.sin(rtz.t),
          z: rtz.z
        };
      }
    };
  }
      ")
    }

    ## Save as HTML ----
    htmlname <- paste0(tools::file_path_sans_ext(savename), ".html")
    Rubrary::use_pkg("htmlwidgets")
    htmlwidgets::saveWidget(
      plotly::partial_bundle(fig),
      file = htmlname,
      selfcontained = TRUE)
    utils::browseURL(htmlname)

    ## Save as image ----
    img_fmt <- c("png", "jpeg", "jpg", "webp", "svg", "pdf")
    if(tools::file_ext(savename) %in% img_fmt){
      message("** Image saving requires `kaleido` python package setup via `reticulate` R package. See `?plotly::save_image` for more details.", immediate. = TRUE)
      Rubrary::use_pkg("reticulate")
      if(requireNamespace("reticulate", quietly = TRUE)){
        plotly::save_image(
          p = fig, file = savename,
          scale = 4)
      }
    }
  }
  return(fig)
}



# https://plotly.com/r/static-image-export/
# `orca` required: https://github.com/plotly/orca#installation
# `processx` required: https://github.com/r-lib/processx
Rubrary::use_pkg("processx")
plotly::orca(fig, "/Users/liaoyj/Dropbox/Rubrary/PCA3d/iris.png")
# DEPRECATED - use "kaleido"

# kaleido install ----
install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')

reticulate::py_install('python-kaleido')
reticulate::py_install('plotly', channel = 'plotly')

# Beltran ----
debugonce(Rubrary::plot_PCA_3D)
Rubrary::plot_PCA_3D(
  df_pca = Beltran_PCA,
  PCs = c(1:5),
  anno = Beltran_info,
  annoname = "sample",
  annotype = "type",
  rotate = TRUE,
  savename = "/Users/liaoyj/Library/CloudStorage/Dropbox/Rubrary/PCA3d/Beltran_PCA.png"
)

df_pca = Beltran_PCA
PCs = c(1:5)
anno = Beltran_info
annoname = "sample"
annotype = "type"
rotate = TRUE
savename = "/Users/liaoyj/Library/CloudStorage/Dropbox/Rubrary/PCA3d/Beltran_PCA.png"
type = "Scores"
annolabel = annoname
label = FALSE
colors = NULL

