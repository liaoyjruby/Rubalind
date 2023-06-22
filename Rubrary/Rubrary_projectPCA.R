library(dplyr)
library(ggplot2)
setwd("~/Documents/Rubrary")

# Flip test ----
Balanis_log2 <- "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/PRAD.norm_Beltran_LUAD.norm_SCLC_LUAD.subset_rsem_genes_upper_norm_counts_coding_log2.txt"
human.info = Rubrary::rread("/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/human.info.rsem.expression.txt")

OVB5_log2 <- "/Users/liaoyj/Library/CloudStorage/Dropbox/OV/Batch5/OV_Batch5_rsem_genes_upper_norm_coding_log2.txt"
OVB5_anno <- Rubrary::rread("/Users/liaoyj/Library/CloudStorage/Dropbox/OV/Batch5/UCLAOvarian_Batch5_Annotation.txt")
OVB5_anno <- OVB5_anno %>% filter(Batch == "5")

train = Balanis_log2;
test = OVB5_log2;
scale = F; varimax = T;
train_name = "SCN"; train_anno = human.info; train_colors = NULL
train_annoname = "sample"; train_annotype = "type";
test_name = "OVB5"; test_anno = OVB5_anno;
test_annoname = "Sample_ID"; test_annotype = "Cohort_Specific";
test_colors = scales::hue_pal()(3);
label = c(F, T); flip = c(T, F); ellipse = F;
savedir = "/Users/liaoyj/Library/CloudStorage/Dropbox/OV/Batch5/Rubrary/";
test_only_plt = T; rank = 3
height = 8; width = 8; fmt = "png"

debugonce(predict_PCA)
predict_PCA(
  train = Balanis_log2,
  test = OVB5_log2,
  scale = F, varimax = T,
  train_name = "SCN", train_anno = human.info,
  train_annoname = "sample", train_annotype = "type",
  test_name = "OVB5", test_anno = OVB5_anno,
  test_annoname = "Sample_ID", test_annotype = "Cohort_Specific",
  test_colors = scales::hue_pal()(3),
  label = c(F, T), flip = c(T, F),
  savedir = "/Users/liaoyj/Library/CloudStorage/Dropbox/OV/Batch5/Rubrary/",
  test_only_plt = T
)

# Test input (maximal) ----
train <- "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/PRAD.norm_Beltran_LUAD.norm_SCLC_LUAD.subset_rsem_genes_upper_norm_counts_coding_log2.txt"
test <- "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_log2UQ_OV.txt"
scale = FALSE
varimax = T
train_name <- "TCGA SCN"
train_anno <- data.table::fread("https://raw.githubusercontent.com/graeberlab-ucla/glab.library/master/vignettes/PCA_tutorial/human.info.rsem.expression.txt") %>%
  as.data.frame() %>%
  filter(type %in% c("LUAD.norm", "LUAD", "SCLC", "PRAD.norm", "CRPC", "NEPC")) %>%
  mutate(type = factor(type, levels = c("LUAD.norm", "LUAD", "SCLC", "PRAD.norm", "CRPC", "NEPC")))
train_annoname = "sample"; train_annotype = "type"
train_colors = NULL
# train_colors <- c("pink", "red", "darkred",
#                   "lightskyblue", "blue", "navy")
test_name <- "DepMap OV"
test_anno <- read.delim("/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_anno.txt") %>%
  mutate(StrippedCellLineName = make.names(StrippedCellLineName)) %>%
  filter(grepl("Ovarian", OncotreePrimaryDisease)) %>%
  mutate(OncotreeCode = factor(OncotreeCode))
test_annoname = "StrippedCellLineName"; test_annotype = "OncotreeCode"
test_colors = Seurat::DiscretePalette(11, "alphabet")
test_only_plt <- T

ellipse = c(T, F)
label = c(F, T)
title = "Title"
savedir = "/Users/liaoyj/Dropbox/Rubrary/project_PCA/"
height = 8; width = 8
fmt = "pdf"

# Minimal args ----
train = "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/PRAD.norm_Beltran_LUAD.norm_SCLC_LUAD.subset_rsem_genes_upper_norm_counts_coding_log2.txt"
test = "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_log2UQ_OV.txt"
scale = FALSE; varimax = TRUE
train_name = "Train"; train_anno = NULL; train_annoname = NULL; train_annotype = NULL; train_colors = NULL;
test_name = "Test"; test_anno = NULL; test_annoname = NULL; test_annotype = NULL; test_colors = NULL;
ellipse = FALSE; label = FALSE; savedir = NULL; height = 8; width = 8; fmt = "png"; test_only_plt = FALSE

rread <- function(file, row.names = 0){
  Rubrary::use_pkg("data.table")
  df <- data.table::fread(file)
  if(row.names != 0){
    rn_name <-
      df <- df %>%
      tibble::column_to_rownames(var = names(.)[row.names])
  }
  return(as.data.frame(df))
}

# Function ----

# Minimal call

train_anno = NULL
train_annotype
debugonce(project_PCA)
plt <- project_PCA(
  train = "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/PRAD.norm_Beltran_LUAD.norm_SCLC_LUAD.subset_rsem_genes_upper_norm_counts_coding_log2.txt",
  test = "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_log2UQ_OV.txt",
  test_anno = "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_anno.txt",
  test_annoname = "StrippedCellLineName", test_annotype = "OncotreeCode",
  test_colors = Seurat::DiscretePalette(11, "alphabet"),
  label = T,
  varimax = T, test_only_plt = T,
  savedir = "/Users/liaoyj/Dropbox/Rubrary/project_PCA/TestAnnoOnly_"
)

plt <- project_PCA(
  train = "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/PRAD.norm_Beltran_LUAD.norm_SCLC_LUAD.subset_rsem_genes_upper_norm_counts_coding_log2.txt",
  test = "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_log2UQ_OV.txt",
  train_anno = "https://raw.githubusercontent.com/graeberlab-ucla/glab.library/master/vignettes/PCA_tutorial/human.info.rsem.expression.txt",
  train_annoname = "sample", train_annotype = "type",
  train_colors = c("pink", "red", "darkred","lightskyblue", "blue", "navy"),
  test_colors = "gray",
  label = F, ellipse = T,
  varimax = T, test_only_plt = F,
  savedir = "/Users/liaoyj/Dropbox/Rubrary/project_PCA/TrainAnnoOnly_"
)

# Maximal call
train <- "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/PRAD.norm_Beltran_LUAD.norm_SCLC_LUAD.subset_rsem_genes_upper_norm_counts_coding_log2.txt"
train_anno <- "https://raw.githubusercontent.com/graeberlab-ucla/glab.library/master/vignettes/PCA_tutorial/human.info.rsem.expression.txt"
test <- "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_log2UQ_OV.txt"
test_anno <- "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_anno.txt"

debugonce(project_PCA)
plt <- project_PCA(
  train = train,
  test = test,
  scale = FALSE, varimax = TRUE,
  train_name = "TCGA SCN", train_anno = train_anno, train_annoname = "sample", train_annotype = "type",
  train_colors = c("pink", "red", "darkred", "lightskyblue", "blue", "navy"),
  test_name = "DepMap OV", test_anno = test_anno, test_annoname = "StrippedCellLineName", test_annotype = "OncotreeCode",
  test_colors = Seurat::DiscretePalette(11, "alphabet"),
  ellipse = T, label = T, test_only_plt = T,
  savedir = "/Users/liaoyj/Dropbox/Rubrary/project_PCA/Max_",
  width = 10, height = 10
)

plt

# Call w/o train_colors - training pts should be gray
train <- "/Users/liaoyj/Dropbox/Ovarian Project/log2_coding_expression_datasets/PRAD.norm_Beltran_LUAD.norm_SCLC_LUAD.subset_rsem_genes_upper_norm_counts_coding_log2.txt"
train_anno <- "https://raw.githubusercontent.com/graeberlab-ucla/glab.library/master/vignettes/PCA_tutorial/human.info.rsem.expression.txt"
test <- "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_log2UQ_OV.txt"
test_anno <- "/Users/liaoyj/Dropbox/OV/DepMap/DepMap22Q4_anno.txt"
debugonce(project_PCA)
plt <- project_PCA(
  train = train,
  test = test,
  scale = FALSE, varimax = TRUE,
  train_name = "TCGA SCN", train_anno = train_anno, train_annoname = "sample", train_annotype = "type",
  train_colors = "pink",
  test_name = "DepMap OV", test_anno = test_anno, test_annoname = "StrippedCellLineName", test_annotype = "OncotreeCode",
  test_colors = Seurat::DiscretePalette(11, "alphabet"),
  ellipse = c(T, F), label = c(F, T), test_only_plt = T,
  savedir = "/Users/liaoyj/Dropbox/Rubrary/project_PCA/TrainPink_"
)

plt + scale_x_reverse()

#' Project query/test samples onto PCA of reference/train samples
#'
#' Plot titles are automatically constructed based on parameters. Adding a `savedir` argument will result in intermediate plots and final projected scores to be output.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @param train string/df; filepath to/df of reference/train samples data, genes/features as rownames, samples/observations as colnames
#' @param test string/df; filepath to/df of query/test samples data, genes/features as rownames, samples/observations as colnames
#' @param scale logical; T to scale variables to unit variance
#' @param varimax logical; T to perform varimax rotation on train + test
#' @param train_name string; descriptor for train samples
#' @param train_anno df; annotation info for train samples
#' @param train_annoname string; colname in `train_anno` matching point name
#' @param train_annotype string; colname in `train_anno` with info to color by
#' @param train_colors char vector; list of colors, length = # of unique `train_annotype`
#' @param test_name string; descriptor for test samples
#' @param test_anno df; annotation info for test samples
#' @param test_annoname string; colname in `test_anno` matching point name
#' @param test_annotype string; colname in `test_anno` with info to color by
#' @param test_colors char vector; list of colors, length = # of unique `test_annotype`
#' @param ellipse logical (vector); can do length 2, `ellipse[1]` for train data ellipse, `ellipse[2]` for test data ellipse
#' @param label logical (vector); can do length 2, `label[1]` for train data label, `label[2]` for test data label
#' @param savedir string; directory (+ prefix) to save output under; if directory, end string with "/"
#' @param height numeric; plot height
#' @param width numeric; plot width
#' @param fmt string; plot output format (ex. "png", "pdf")
#' @param test_only_plt logical; save scatter of projected test samples only
#'
#' @return
#' @export
#' @seealso [Rubrary::run_PCA()], [Rubrary::plot_PCA()], [Rubrary::rotate_varimax()]
#' @examples
#' data(iris)
#' iris$Sample <- rownames(iris)
#' set.seed(13)
#' samp <- sample(nrow(iris), nrow(iris)*0.75) # Train data is 75% of iris
#' iris_train <- iris[samp,] %>%
#'   mutate(Batch = "Train",
#'          Species = paste0(Species, "_Train"))
#' iris_test <- iris[-samp,] %>%
#'   mutate(Batch = "Test",
#'          Species = paste0(Species, "_Test"))
#' project_PCA(
#'   train = t(iris_train[,1:4]),
#'   test = t(iris_test[,1:4]),
#'   scale = TRUE,
#'   train_name = "Iris Train", train_anno = iris_train[,c("Sample", "Species")],
#'   train_annoname = "Sample", train_annotype = "Species",
#'   train_colors = c("pink", "red", "darkred"),
#'   test_name = "Iris Test", test_anno = iris_test[,c("Sample", "Species")],
#'   test_annoname = "Sample", test_annotype = "Species",
#'   test_colors = c("lightskyblue", "blue", "navy"),
#'   ellipse = c(FALSE, TRUE), label = FALSE
#' )
#'
project_PCA <- function(
    train, test, scale = FALSE, varimax = FALSE,
    train_name = "Train", train_anno = NULL, train_annoname = NULL, train_annotype = NULL, train_colors = NULL,
    test_name = "Test", test_anno = NULL, test_annoname = NULL, test_annotype = NULL, test_colors = NULL,
    ellipse = FALSE, label = FALSE, savedir = NULL, height = 8, width = 8, fmt = "png", test_only_plt = FALSE){
  # Manage plot params
  PCx = "PC1"; PCy = "PC2" # Set as function arg?
  # Expand args to L2 if applicable
  if(length(ellipse) == 1){ ellipse = c(ellipse, ellipse)}
  ellipse_train <- ellipse[1]
  if(length(label) == 1){ label = c(label, label)}
  # Manage dataframes
  if(is(train, "data.frame") || is(train, "matrix")){ train_df <- train } else { train_df <- Rubrary::rread(train, row.names = 1)}
  if(is(test, "data.frame") || is(test, "matrix")){ test_df <- test } else { test_df <- Rubrary::rread(test, row.names = 1)}

  # Subset and order both train + test to common/intersect genes/features
  common_feats <- dplyr::intersect(rownames(train_df), rownames(test_df)) %>% sort()
  message(paste0("Common features: ", length(common_feats)))
  # Report unique features
  train_only_feats <- dplyr::setdiff(rownames(train_df), common_feats)
  test_only_feats <- dplyr::setdiff(rownames(test_df), common_feats)
  if(length(train_only_feats) != 0) {
    message(paste0("** ", train_name, " (train) unique features: ", length(train_only_feats),
                   " (", round((length(train_only_feats)/nrow(train_df)) * 100, 1),"%)"))
  }
  if(length(test_only_feats) != 0) {
    message(paste0("** ", test_name, " (test) unique features: ", length(test_only_feats),
                   " (", round((length(test_only_feats)/nrow(test_df)) * 100, 1),"%)"))
  }
  if(length(train_only_feats) != 0 || length(test_only_feats) != 0){
    message("All unique features excluded from train PCA & test projection!")
  }
  train_df <- train_df[common_feats,]
  test_df <- test_df[common_feats,]
  # Run train PCA
  train_pca <- Rubrary::run_PCA(df = train_df, center = T, scale = scale, screeplot = F)

  # Train annotation provided
  if(!is.null(train_anno)){
    if(is.character(train_anno)){ train_anno <- Rubrary::rread(train_anno) }
    # Filter anno to samples present
    train_anno <- train_anno %>%
      filter(!!as.symbol(train_annoname) %in% names(as.data.frame(train_df)))
    # No colors given or 1 color given
    if(is.null(train_colors) || length(train_colors) == 1){
      if(is.null(train_colors)){ train_colors <- "gray" }
      ellipse[1] <- FALSE # Use `ellipse_train` for train ellipse instead
      train_anno$ellipse <- train_anno[,train_annotype]
      train_anno[,train_annotype] <- train_name
    }
    train_pc_anno <- dplyr::left_join(
      x = tibble::rownames_to_column(as.data.frame(train_pca$x)[,c(PCx, PCy)], "Scores"),
      y = train_anno, by = stats::setNames(nm = "Scores", train_annoname))
  } else { # No train annotation
    label[1] <- FALSE
    ellipse[1] <- FALSE
    ellipse_train <- FALSE
    train_colors <- "gray"
    train_pc_anno <- data.frame(
      Samples = colnames(train_df),
      Type = train_name
    )
    train_annotype <- "Type"
  }

  train_plt <- Rubrary::plot_PCA(
    df_pca = train_pca,
    PCx = PCx, PCy = PCy,
    anno = train_anno,
    annoname = train_annoname,
    annotype = train_annotype,
    colors = train_colors,
    ellipse = ellipse[1],
    label = label[1],
    title = paste0("PCA - ", train_name)
  ) +
    {if(!is.null(train_anno) && !ellipse[1] && ellipse_train) stat_ellipse(
      data = train_pc_anno,
      mapping = aes(x = .data[[PCx]], y = .data[[PCy]], group = ellipse),
      color = train_colors
    )}

  if(!is.null(savedir)){
    ggsave(
      plot = train_plt,
      filename = paste0(savedir,"PCA_", gsub(" |\\/", "", train_name), ".", fmt),
      height = height, width = width
    )
  }
  # Rotate test data with train rotation mtx
  test_proj <- stats::predict(train_pca, newdata = t(test_df)) %>% as.data.frame()

  # Varimax section
  if(varimax){
    ncomp <- 2 # Set as function arg?
    PCx = sub("PC", "V", PCx); PCy = sub("PC", "V", PCy)
    train_pca_vm <- Rubrary::rotate_varimax(train_pca, ncomp = ncomp, normalize = F)

    # Train annotation provided
    if(!is.null(train_anno)){
      train_pc_vm_anno <- dplyr::left_join(
        x = train_pca_vm$scores[,c("Scores", PCx, PCy)],
        y = train_anno, by = stats::setNames(nm = "Scores", train_annoname))
    } else {
      # No ellipse possible
      ellipse[1] <- FALSE
      ellipse_train <- FALSE
      train_colors <- "gray"
      train_pc_vm_anno <- data.frame(
        Samples = colnames(train_df),
        Type = train_name
      )
      train_annotype <- "Type"
    }

    train_plt_vm <- Rubrary::plot_PCA(
      df_pca = train_pca_vm$scores,
      PCx = PCx, PCy = PCy,
      anno = train_anno,
      annoname = train_annoname,
      annotype = train_annotype,
      colors = train_colors,
      ellipse = ellipse[1],
      label = label[1],
      title = paste0("PCA Varimax - ", train_name)
    ) +
      {if(!ellipse[1] && ellipse_train) stat_ellipse(
        data = train_pc_vm_anno,
        mapping = aes(x = .data[[PCx]], y = .data[[PCy]], group = ellipse),
        color = train_colors
      )}
    if(!is.null(savedir)){
      ggsave(
        plot = train_plt_vm,
        filename = paste0(savedir,"PCA_", gsub(" |\\/", "", train_name), "_varimax.", fmt),
        height = height, width = width
      )
    }
    train_plt <- train_plt_vm # Overwrite orig. plt w/ varimax version as base for projection
    # Test data varimax rotation
    test_proj_vm <- as.matrix(test_proj[,1:ncomp]) %*% train_pca_vm$rotation %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Scores")
    test_proj <- test_proj_vm
    train_pca <- train_pca_vm$scores
  } else {
    test_proj <- test_proj %>%
      tibble::rownames_to_column(var = "Scores")
  }

  if(!is.null(test_anno)){ # Test sample annotation present
    if(is.character(test_anno)){ test_anno <- Rubrary::rread(test_anno) } # Read if path
    # Filter to samples present
    test_anno <- test_anno %>%
      filter(!!as.symbol(test_annoname) %in% names(test_df))
    # Join
    test_proj_anno <- dplyr::left_join(
      test_proj[,c("Scores", PCx, PCy)], test_anno, by = stats::setNames(nm = "Scores", test_annoname))
    if(is.null(test_colors)){
      test_colors <- scales::hue_pal()(length(unique(test_proj_anno[,test_annotype])))
    }
  } else { # No test sample annotation
    # Set colors
    if(is.null(test_colors)){test_colors <- "red" } else { test_colors <- test_colors[1]}
    # Make fake merged test_anno
    test_annotype <- "Type"
    test_proj_anno <- test_proj %>%
      select(all_of(c("Scores", PCx, PCy))) %>%
      mutate(Type = test_name)
    label[2] <- FALSE
    ellipse[2] <- FALSE
  }
  colors <- stats::setNames(
    nm = c(as.character(sort(unique(train_pc_anno[,train_annotype]))),
           as.character(sort(unique(test_proj_anno[,test_annotype])))),
    c(train_colors, test_colors))

  vm_title <- ifelse(varimax, " Varimax", "")

  suppressMessages(
  test_plt <- train_plt +
    geom_point(data = test_proj_anno,
      mapping = aes(x = .data[[PCx]], y = .data[[PCy]], color = .data[[test_annotype]])) +
    {if(ellipse[2]) stat_ellipse(data = test_proj_anno, aes(color = .data[[test_annotype]]))} +
    {if(label[2]) ggrepel::geom_text_repel(test_proj_anno, mapping = aes(x = .data[[PCx]], y = .data[[PCy]]),
      label = test_proj_anno$Scores, max.overlaps = Inf)} +
    scale_color_manual(values = colors, limits = names(colors), name = "Type") +
    guides(color = guide_legend(title = "Type")) + # Fix legend title
    {if(varimax) xlab(PCx)} + # Overwrite w/ "V1"
    {if(varimax) ylab(PCy)} + # Overwrite w/ "V2"
    labs(title = paste0("PCA", vm_title, " - ", test_name, " Projected on ", train_name))
  )

  if(!is.null(savedir)){
    vm_name <- ifelse(varimax, "_varimax", "")
    # Save scores
    write.table(
      x = test_proj,
      file = paste0(savedir, "PCA_proj_",
                    gsub(" |\\/", "", test_name), "_on_",
                    gsub(" |\\/", "", train_name), vm_name, ".txt"),
      quote = FALSE, row.names = FALSE, sep = "\t"
    )
    # Save plot
    test_savename <- paste0(savedir, "PCA_proj_",
                            gsub(" |\\/", "", test_name), "_on_",
                            gsub(" |\\/", "", train_name), vm_name, ".", fmt)
    ggplot2::ggsave(
      filename = test_savename,
      plot = test_plt,
      height = height, width = width
    )
  }

  if(test_only_plt){
    if(label[2]){
      label_test <- "Scores"
    } else {
      label_test <- NULL
    }
    if(!is.null(savedir)){
      test_only_savename <- paste0(
        savedir, "PCA_proj_", gsub(" |\\/", "", test_name), "_on_",
        gsub(" |\\/", "", train_name), vm_name, "_testonly.", fmt)
    } else {
      test_only_savename <- NULL
    }
    test_plt_only <- Rubrary::plot_scatter(
      df = test_proj_anno, xval = PCx, yval = PCy,
      label = label_test, group = test_annotype, colors = test_colors,
      cormethod = "none", guides = FALSE,
      title = paste0("PCA", vm_title, " - ", test_name, " Projected on ", train_name),
      subtitle = paste0(test_name, " samples only"),
      savename = test_only_savename,
      height = height, width = width
    )
  }
  return(test_plt)
}
