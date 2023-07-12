# Input ----
## UCLAOV SCN
## OVB5 SCN ----
UCLA_OV_B12345_PCA <- Rubrary::run_PCA(
  df = "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/ovarian_ucla12345_rsem_genes_upper_norm_counts_coding_log2.txt",
  center = T, scale = F,
  # savename = "/Users/liaoyj/Library/CloudStorage/Dropbox/OV/Batch5/PCA/UCLA_OV_B12345_PCA"
)

ts_lvls <- c("Chemonaive", "Post-NACT", "Recurrent", "NA (CellLine)", "NA (Control)")
ts_cols <- setNames(nm = ts_lvls,
                    c(scales::hue_pal()(3), "grey30", "grey60"))
UCLA_OV_anno <- Rubrary::rread("/Users/liaoyj/Library/CloudStorage/Dropbox/Ovarian Project/UCLA cohort information/UCLAOvarian_CombinedBatches_Annotation_v11.txt") %>%
  mutate(TreatmentStatus = factor(TreatmentStatus, levels = ts_lvls))


df_pca = UCLA_OV_B12345_PCA
PCs = c(1:3)
anno = UCLA_OV_anno
annoname = "Sample_ID"
annotype = "SCN_Score_Proj"
colors = c("blue", "red")
# annotype = "TreatmentStatus"
# colors = unname(ts_cols)
title = "UCLA OV 3D PCA - SCN"
savename = "/Users/liaoyj/Dropbox/Rubrary/PCA_mtx/UCLAOV.png"
width = 10
height = 10

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
anno = iris[,c("Sample", "Species")]
annoname = "Sample"
annotype = "Species"
title = "Iris PCA"
colors = Seurat::DiscretePalette(n = 3, palette = "alphabet")
scales::show_col(colors)

PCs <- paste0("PC", PCs)
scores_anno <- PCA_iris$x %>%
  as.data.frame() %>%
  select(all_of(PCs)) %>%
  tibble::rownames_to_column("Sample") %>%
  left_join(., anno, by = join_by(Sample))

idx <- match(PCs, names(scores_anno))

diag_dens <- function(data, mapping){
  ggplot(data = data, mapping = mapping) +
    geom_density(alpha = 0.5)
}

ggpairs(
  data = scores_anno, aes(color = .data[[annotype]], fill = .data[[annotype]]),
  columns = idx,
  title = title,
  upper = list(continuous = "points"),
  diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
  # switch = "both",
  legend = c(length(idx), 1)
) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_classic() +
  theme(legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

# Function ----
data(iris)
iris$Sample = rownames(iris)

df_pca = Rubrary::run_PCA(t(iris[,c(1:4)]), screeplot = F)
PCs = c(1:3)
anno = iris[,c("Sample", "Species")]
annoname = "Sample"
annotype = "Species"
colors = NULL
title = "Iris PCA"
savename = "/Users/liaoyj/Dropbox/Rubrary/PCA_mtx/PCAmtx_Iris.pdf"
width = 10
height = 10

data(iris)
iris$Sample = rownames(iris)

Rubrary::plot_PCA_matrix(
  df_pca = Rubrary::run_PCA(t(iris[,c(1:4)]), screeplot = F),
  PCs = c(1:3),
  anno = iris[,c("Sample", "Species")],
  annoname = "Sample",
  annotype = "Species",
  title = "Iris PCA"
)

debugonce(plot_screeplot)
plot_screeplot(
  obj_prcomp = Beltran_PCA
)

# Screeplot inputs
obj_prcomp = Beltran_PCA
npcs = ncol(obj_prcomp$x)
label = FALSE
cum_var_exp = 80
savename = NULL

debugonce(plot_PCA_matrix)
plot_PCA_matrix(
  df_pca = Beltran_PCA,
  PCs = c(1:3),
  anno = Beltran_info,
  annoname = "sample",
  annotype = "type",
  title = "Beltran PCA"
)

plot_PCA_matrix <- function(
    df_pca, PCs = c(1:3), anno = NULL, annoname = "Sample", annotype = "Type",
    colors = NULL, title = NULL, savename = NULL, width = 8, height = 8) {

  if (is.character(df_pca)) { # PCA results as path to txt
    dfpath <- df_pca
    df_pca <- Rubrary::rread(dfpath)
    sdevpath <- gsub("_[^_]+$", "_sdev.txt", dfpath)
    sdev <- Rubrary::rread(sdevpath) %>%
      mutate(var = .^2, #
             pve = round(var / sum(var) * 100, digits = 2)) %>%
      `rownames<-`(paste0("PC", rownames(.)))
  } else if (methods::is(df_pca, "prcomp")){ # prcomp object
    df <- df_pca$x %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Scores")

    sdev <- df_pca$sdev %>%
      as.data.frame() %>%
      mutate(var = .^2,
             pve = round(var / sum(var) * 100, digits = 1)) %>%
      `rownames<-`(paste0("PC", rownames(.)))

  } else { # PCA results directly as dataframe, no sdev
    df <- df_pca
    names(df)[1] <- "Scores"
  }

  PCs <- paste0("PC", PCs)
  idx <- match(PCs, names(df))

  if(exists("sdev")){
    PClabs <- paste0(PCs, " (", sdev$pve[match(PCs, rownames(sdev))], "%)")
  } else { PClabs <- PCs }

  if (is.character(anno)) { anno <- Rubrary::rread(anno) }

  if(!is.null(anno)){
    df <- df %>% left_join(., anno, by = stats::setNames(nm = "Scores", annoname))
    axlab = "show"
    leg = c(length(idx), 1)
    legpos = "bottom"
    if(is.null(colors)){ colors <- scales::hue_pal()(length(unique(df[[annotype]])))}
  } else { #No anno
    df <- df %>% mutate(anno = "none")
    annotype = "anno"
    axlab = "internal"
    leg = NULL
    legpos = "none"
    if(is.null(colors)){ colors = "black" } else { colors = colors[1] }
  }

  plt <- ggpairs(
    data = df, aes(color = .data[[annotype]], fill = .data[[annotype]]),
    columns = idx,
    title = title,
    columnLabels = PClabs,
    upper = list(continuous = "points"),
    diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
    axisLabels = axlab,
    legend = leg
  ) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme_classic() +
    theme(legend.position = legpos,
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

  if(!is.null(savename)){
    ggsave(
      plot = plt,
      filename = savename,
      width = width, height = height
    )
  }
  return(plt)
}


