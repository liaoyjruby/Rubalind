library(ggplot2)
library(dplyr)

# DESeq sig optional?
sig_DESeq <- Rubrary::rread("/Users/liaoyj/Library/CloudStorage/Dropbox/Ovarian Project/signatures/DESeq_Recurr_UCLAPatch/UCLAPatch_highNE/OV_UCLA&Patch_recur_prevpost_slogp_DESeq_pconly_13bno9no1no27.txt")
exp <- Rubrary::rread("/Users/liaoyj/Library/CloudStorage/Dropbox/Ovarian Project/log2_coding_expression_datasets/OV_UCLA&Patch_recur_log2UQcounts.tsv") %>%
  select(!c(X13c))
anno <- Rubrary::rread("/Users/liaoyj/Library/CloudStorage/Dropbox/OV/OV_UCLA&Patch_recur_anno.txt")
data <- "/Users/liaoyj/Library/CloudStorage/Dropbox/Ovarian Project/log2_coding_expression_datasets/OV_UCLA&Patch_recur_log2UQcounts.tsv"
sig_DESeq <- "/Users/liaoyj/Library/CloudStorage/Dropbox/Ovarian Project/signatures/DESeq_Recurr_UCLAPatch/UCLAPatch_highNE/OV_UCLA&Patch_recur_prevpost_slogp_DESeq_pconly_13bno9no1no27.txt"
metric <- "sign_log_p"
metric <- "log2FoldChange"
sig_name <- "UCLA High NE & Patch OV Recurrent"
anno_sig <- "Cohort_Specific" # Column in annotation corresponding to DESeq conditions
anno_type <- "Batch" # Optional anno column to color points by
colors <- c("goldenrod2", "navyblue")
title <- "UCLA High NE & Patch OV Recurrent DE Genes Expression"
genes <- NULL # Optional list of genes to pull out, by default top / bot 6 genes
# genes <- rev(exp[1:10, 1])
genes_n <- 6
savename = NULL


data <- "/Users/liaoyj/Library/CloudStorage/Dropbox/Ovarian Project/log2_coding_expression_datasets/OV_UCLA&Patch_recur_log2UQcounts.tsv"
anno <- "/Users/liaoyj/Library/CloudStorage/Dropbox/OV/OV_UCLA&Patch_recur_anno.txt"
set.seed(13)
Rubrary::plot_DEgene_boxplot(
  data = data,
  anno = anno,
  anno_sig = "Cohort_Specific",
  anno_type = "Batch",
  colors = c("navyblue", "goldenrod2"),
  sig_DE = "/Users/liaoyj/Library/CloudStorage/Dropbox/Ovarian Project/signatures/DESeq_Recurr_UCLAPatch/UCLAPatch_highNE/OV_UCLA&Patch_recur_prevpost_slogp_DESeq_pconly_13bno9no1no27.txt"
)

genes_n = 6
anno_sig = "Cohort_Specific"
anno_type = "Batch"
title = NULL
savename = "/Users/liaoyj/Library/CloudStorage/Dropbox/Rubrary/plot_DEgene_boxplot/test.pdf"
height = 6; width = 6

metric = "sign_log_p"



#' Plot gene expression boxplots of selected genes (from DESeq signature)
#'
#' Great for sanity checking, to make sure that differentially expressed genes as reported through DE gene analysis metrics (such as DESeq2) are reflected in gene expression data per sample.
#'
#' Providing DE(seq) gene signature (`sig_DE`) is optional; if not provided, `genes` list must be defined. If DE gene info *is* provided, each subplot's title will have the `metric` value appended in parenthesis.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @param data df/string; (path to) numeric gene expression matrix, rownames = genes, colnames = samples
#' @param anno df/string; (path to) annotation table, rownames = samples, columns = metadata
#' @param anno_sig string; colname in `anno` to be used as breaks for x-axis
#' @param anno_type string; colname in `anno` to be used to color jitter points by
#' @param sig_DE df/string, optional; (path to) DE(seq) gene results table
#' @param genes char vector; list of genes to plot, must be present in `data`
#' @param metric string; colname in `sig_DE` to rank genes by for auto gene selection, only used if `genes` not provided
#' @param genes_n integer; if `genes` not provided, get top/bottom `genes_n`
#' @param colors char vector; jitter point colors, length == # of unique `anno_type`s
#' @param title string; plot title
#' @param savename string; file path to save plot under
#' @param height numeric; plot height
#' @param width numeric; plot width
#'
#' @return ggplot object, with facet-wrapped boxplot
#' @export
#'
plot_DEgene_boxplot <- function(data, anno, anno_sig, anno_type = NULL, sig_DE, genes = NULL, metric = "sign_log_p", genes_n = 6, colors = NULL, title = NULL, savename = NULL, height = 8, width = 4){
  # DESeq dataframe
  if(!missing(sig_DE)){ # DESeq signature provided
    if(is.character(sig_DE)){ sig_DE <- Rubrary::rread(sig_DE, row.names = 1)}
    de_df <- sig_DE %>%
      select(all_of(metric)) %>%
      arrange(desc(metric)) %>%
      tibble::rownames_to_column("gene")
    # Get genes first (if null)
    if(is.null(genes)){
      de_df <- de_df %>%
        {rbind(utils::head(., genes_n), utils::tail(., genes_n))}
      genes <- de_df$gene
    } else {
      na_genes <- data.frame(
        gene = setdiff(genes, de_df$gene),
        metric = rep(NA, length(setdiff(genes, de_df$gene)))
      ) %>%
        rename_at(vars(colnames(.)), ~colnames(de_df))

      if(nrow(na_genes) != 0){
        message(paste0("Genes not in DE signature: ", paste0(na_genes[,1], collapse = ", ")))
      }

      de_df <- de_df %>%
        filter(gene %in% genes) %>%
        bind_rows(na_genes)
    }
    de_df <- de_df %>%
      mutate(sign = sign(!!sym(metric)))
  } else { # DESeq signature NOT provided
    if(is.null(genes)){ stop("Must provide `genes` if no DE genes signature provided!")}
  }

  # Gene expression & annotation dataframes
  if(is.character(data)){ exp <- Rubrary::rread(data, row.names = 1) } else { exp <- data }
  if(is.character(anno)){ anno <- Rubrary::rread(anno, row.names = 1)}
  # Get expression values for selected genes
  exp_df <- exp %>%
    tibble::rownames_to_column("gene") %>%
    filter(gene %in% genes) %>%
    arrange(match(gene, genes)) # All genes must be in exp_df

  # Simplify annotation
  anno_df <- anno %>%
    tibble::rownames_to_column("sample") %>%
    select(1, all_of(c(anno_sig, anno_type)))
  if(is.null(anno_type)){ # One color
   anno_type <- "type"
   anno_df$type <- anno_type
   if(is.null(colors)){
     colors = "black"
   } else {
     colors = colors[1]
   }
  }

  exp_anno <- exp_df %>%
    tidyr::pivot_longer(!gene, names_to = "sample", values_to = "exp") %>%
    left_join(., anno_df, by = "sample") %>% # Merge anno data
    filter(!is.na(!!sym(anno_sig))) %>% # No NA for x-axis allowed
    as.data.frame()

  if(!missing(sig_DE)){ # Merge in DE info/table
    exp_anno <- exp_anno %>%
      left_join(., de_df, by = "gene") %>% # Merge DESeq data (conditional?)
      mutate(gene = factor(gene, levels = genes)) # Control order of plotting

    # Make facet labels
    gene_labs <- c(paste0(de_df[!is.na(de_df[,metric]), "gene"], " (", round(de_df[!is.na(de_df[,metric]), metric], 2), ")"),
                   de_df[is.na(de_df[,metric]), "gene"])
    names(gene_labs) <- genes
  } else {
    gene_labs <- stats::setNames(nm = genes, genes)
  }

  # Manage colors
  if(is.null(colors)){colors <- scales::hue_pal()(length(unique(exp_anno[,anno_type])))}

  plt <- ggplot(exp_anno, aes(x = .data[[anno_sig]], y = exp)) +
    # geom_boxplot(aes(fill = .data[[metric]]), outlier.shape = NA) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color = .data[[anno_type]]), width = 0.05) +
    # scale_fill_gradient(low = "blue", high = "red") +
    scale_color_manual(values = colors) +
    facet_wrap(vars(gene), ncol = 3, scales = "free_y", labeller = labeller(gene = gene_labs)) +
    ylab("Gene Expression") +
    labs(title = title) +
    theme_classic() +
    theme(strip.background = element_blank()) +
    {if(length(colors) == 1) theme(legend.position = "none")}

  if(!is.null(savename)){
    ggsave(
      plot = plt,
      filename = savename,
      height = height, width = width
    )
  }
  return(plt)
}
