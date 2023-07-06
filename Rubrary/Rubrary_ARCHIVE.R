
# plot_waterfall_hl ----
#' Get number of TF MRA signature genes to highlight
#'
#' @param signature df; Has "pvalue" and "NES" columns
#' @param pos logical; "T" for positive NES
#' @param sig logical; "T" for p < 0.05
#'
#' @return integer with n number of genes to highlight
get_n <- function(signature, pos = T, sig = F){
  if (sig) {
    signature <- signature[signature$pvalue < 0.05,]
  }
  if (pos){
    signature <- signature[signature$NES > 0,]
  } else {
    signature <- signature[signature$NES < 0,]
  }
  return(length(nrow(signature)))
}

#' Plot ranked waterfall with genes highlighted
#'
#' Revamp of Favour's original function
#'
#' @param sig df; Ranked signature with "rankcol" and "gene" columns
#' @param rankcol string; column to rank sig df by
#' @param sig_hl df; ranked signature with genes in column 1
#' @param topn integer; number of genes from sig_hl to highlight
#' @param rev logical; reverse to bottom n genes from sig_hl instead of top
#' @param toplabel string; name of highlight group
#' @param title string; plot title
#' @param subtitle string; plot subtitle
#' @param savename string; name to save plot under
#' @param label logical or char vector; if vector, should match length of nrow(sig_hl)
#' @param ylab string; rankcol axis label
#' @param wf_pos string; label for positive side of waterfall
#' @param wf_neg string; label for negative side of waterfall
#' @param pval logical; include KS p value?
#' @param density logcal; include density plot on right?
#'
#' @importFrom ggplot2 geom_text layer_scales
#' @importFrom ggpubr ggbarplot ggdensity rremove rotate
#'
#' @return Waterfall plot with n genes from sig_hl highlighted, ks p value
#' @export
plot_waterfall_hl <- function(sig, rankcol = "sign_log_p", sig_hl, topn = 200, rev = F, toplabel = "Top SCN",
                              label = F, ylab = "DESeq Signed log p-values",
                              wf_pos = "Post-Therapy", wf_neg = "Pre-Therapy",
                              title = "DE Genes", subtitle = NA, pval = T, density = T,
                              savename = NA) {
  
  # Rank DF by rankcol values
  sig <- sig[order(sig[,rankcol], decreasing = T),]
  sig$rank <- nrow(sig):1
  
  colors <- c("#120632", "#999999")
  if(!rev) {
    hlrange <- 1:topn
  } else {
    hlrange <- (nrow(sig_hl) - topn):nrow(sig_hl)
    # colors <- rev(colors)
  }
  sig$type <- ifelse(sig$gene %in% sig_hl[hlrange, 1], toplabel, "Other")
  sig$type <- factor(sig$type, levels = c(toplabel, "Other"))
  
  if(pval){
    ks_pval = stats::ks.test(
      sig[sig$type == toplabel, "rank"],
      sig[!sig$type == toplabel, "rank"]
    )$p.value
  }
  
  a = ggbarplot(
    data = sig,
    x = "rank",
    y = rankcol,
    xlab = "Gene Rank",
    ylab = ylab,
    title = title,
    rotate = T,
    palette = colors,
    fill = "type",
    color = "type",
    sort.val = "asc", # Ascending
    sort.by.groups = FALSE,
    legend.title = "Type",
    label = label, lab.hjust = -0.1
  )
  
  # Figure out dimensions and scale for label position
  layer <- layer_scales(a)
  yrange <- layer$y$range$range # slogp range
  ypos <- (yrange[which.max(abs(yrange))] / 2)
  xrange <- layer$x$range_c$range # number of genes
  xpos_bot <- max(xrange) / 4
  xpos_mid <- max(xrange) / 2
  xpos_top <- (max(xrange) / 4) * 3
  
  a <- a +
    rremove("y.ticks") +
    rremove("y.text") +
    theme(legend.position=c(0,1), legend.justification=c(0,1)) +
    geom_text(x = xpos_top, y = ypos, label = wf_pos) +
    {if(pval) geom_text(x = xpos_mid, y = ypos, label = paste0("KS enrich. p-value = ", signif(ks_pval, digits = 4)))} +
    geom_text(x = xpos_bot, y = ypos, label = wf_neg) +
    {if(!is.na(subtitle)) labs(subtitle = subtitle)}
  
  if(density){
    x = ggdensity(
      data = sig,
      x = "rank",
      xlab = "",
      ylab = "Density",
      fill = "type",
      alpha = 0.85,
      palette = colors
    ) +
      rotate() +
      rremove("y.ticks") +
      rremove("y.text") +
      rremove("x.text") +
      rremove("legend")
    
    pg = cowplot::plot_grid(
      a,
      x,
      align = "h", axis = "bt",
      ncol = 2,
      rel_widths = c(2, 0.7), #increase 0.3 to 0.5 (or more) if density plot is too squished
      rel_heights = c(0.7, 2) #increase 0.3 to 0.5 (or more) if density plot is too squished
    )
    
    if(!is.na(savename)){
      cowplot::ggsave2(
        filename = savename,
        plot = pg,
        width = 10, height = 8
      )
    }
  } else {
    pg <- a
    if(!is.na(savename)){
      ggplot2::ggsave(
        filename = savename,
        plot = pg,
        width = 8, height = 8
      )
    }
  }
  
  return(pg)
}

# DESeq old ----

utils::globalVariables(c(
  "Type", "Batch",
  "gene1", "gene2", "gene3", "gene4", "gene5"
))

#' Run DESeq
#'
#' Not generalizable to non OV project related datasets yet...
#'
#' @param mtx_rawcts matrix; numeric matrix of raw counts
#' @param mtx_annotation matrix; samples as rownames, cols "Subject" & "Condition" (& "Batch" if merged)
#' @param savename string; filename to save outputs under (no ext.)
#' @param merged logical; T if dataset has two batches merged
#' @param shrink logical; apply lfcShrink "ashr" to results (NOT IMPLEMENTED)
#'
#' @return DESeq results in dataframe
#' @export
#'
run_DESeq <- function(mtx_rawcts, mtx_annotation, savename = NULL,
                      merged = F, shrink = F) {
  # Paired
  dds_simple <- DESeq2::DESeqDataSetFromMatrix(
    countData = mtx_rawcts,
    colData = mtx_annotation,
    design = ~ Subject + Condition
  )
  
  if (merged) {
    num_batches <- length(unique(mtx_annotation$Batch))
    subj_n <- c()
    for (i in 1:num_batches) {
      batch_pairs <- sum(mtx_annotation$Batch == unique(mtx_annotation$Batch)[i]) / 2
      subj_n <- c(subj_n, rep(1:batch_pairs, each = 2)) # list of 1-n_batch, together
    }
    mtx_annotation$Subject.n <- subj_n
    mtx_annotation$Subject.n <- factor(mtx_annotation$Subject.n)
    # Single DS DESeq should already have Condition / Subject columns
    
    design <- stats::model.matrix(~ Batch + Batch:Subject.n + Batch:Condition, mtx_annotation)
    design <- design[, colSums(design != 0) > 0]
    
    seq <- DESeq2::DESeq(dds_simple, full = design, betaPrior = FALSE)
    contrast_list <- list(c(DESeq2::resultsNames(seq)[length(DESeq2::resultsNames(seq)) - 1],
                            DESeq2::resultsNames(seq)[length(DESeq2::resultsNames(seq))]))
  } else {
    seq <- DESeq2::DESeq(dds_simple)
    contrast_list <- c("Condition", "Recurrent", "Primary")
  }
  DESeq2::resultsNames(seq)
  res <- DESeq2::results(seq, contrast = contrast_list)
  DESeq2::plotMA(res, ylim = c(-2, 2), main = "DESeq")
  if (shrink) {
    res_lfc <- DESeq2::lfcShrink(seq, res = res, contrast = contrast_list, type = "ashr")
    DESeq2::plotMA(res_lfc, ylim = c(-2, 2), main = "DESeq log2 Shrink")
    res_lfc_df <- as.data.frame(res_lfc)
  }
  res_df <- as.data.frame(res)
  res_df$sign_log_p <- sign(res_df$stat) * -log2(res_df$pvalue)
  resOrdered <- res_df[order(res_df$sign_log_p, decreasing = T), ]
  resOrdered <- resOrdered[stats::complete.cases(resOrdered), ] # excludes rows with NA
  resOrdered <- tibble::rownames_to_column(resOrdered, "gene")
  if(!is.null(savename)){
    Rubrary::rwrite(x = resOrdered, file = paste0(savename, ".txt"))
    rank_file <- resOrdered[c("gene", "sign_log_p")]
    Rubrary::rwrite(rank_file, paste0(savename, ".rnk"))
  }
  return(resOrdered)
}

#' plot_DESeq_scatter
#'
#' @param sig1path Path to 1st signature (x-axis)
#' @param sig2path Path to 2nd signature (y-axis)
#' @param batch Descriptive word/phrase appended to output path
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param title Plot title
#' @param desc Plot subtitle
#' @param rank Plot rank scatter instead of slogp/raw values
#'
#' @return Plot comparing 1 sig to another
#' @export
#'
plot_DESeq_scatter <- function(sig1path, sig2path, batch = "",
                               xlab = "Old DESeq signed log p", ylab = "Updated DESeq signed log p",
                               title = "", desc = "", rank = F) {
  pathsplit <- strsplit(sig2path, "[_|.]")[[1]]
  set <- pathsplit[2] # UCLA or Patch or UCLA/Patch
  if (set == "UCLA&Patch") {
    set2 <- "UCLA & Patch"
  } else {
    set2 <- set
  }
  
  if (title == "") {
    title <- paste0(set2, " DESeq Signature Old vs Updated")
  }
  
  sig1 <- utils::read.delim(sig1path, header = T)
  # if (set == "Patch"){
  #   batch <- ""
  #   colnames(sig1) <- c("gene", "sign_log_p")
  # } else if (batch == ""){
  #   batch <- paste0("_",pathsplit[length(pathsplit)-1])
  # }
  
  sig2 <- utils::read.delim(sig2path, header = T)
  if (rank) {
    sig1 <- sig1[order(sig1$sign_log_p, decreasing = T), ]
    sig2 <- sig2[order(sig2$sign_log_p, decreasing = T), ]
    sig1$sign_log_p <- 1:nrow(sig1)
    sig2$sign_log_p <- 1:nrow(sig2)
  }
  sig_merged <- base::merge(sig1[, c("gene", "sign_log_p")], sig2[, c("gene", "sign_log_p")], by = "gene", all = T)
  colnames(sig_merged) <- c("gene", "sign_log_p.1", "sign_log_p.2")
  sig1_range <- c(min(sig_merged$sign_log_p.1, na.rm = T), max(sig_merged$sign_log_p.1, na.rm = T))
  sig2_range <- c(min(sig_merged$sign_log_p.2, na.rm = T), max(sig_merged$sign_log_p.2, na.rm = T))
  limits <- c(min(sig1_range, sig2_range) * 1.1, max(sig1_range, sig2_range) * 1.1)
  plt <- ggplot2::ggplot(sig_merged, ggplot2::aes_string(x = "sign_log_p.1", y = "sign_log_p.2")) +
    ggplot2::geom_point(alpha = 0.2, size = 0.5) +
    # geom_abline(linetype="dashed", ggplot2::aes(intercept=0, slope=1), size = 1) +
    # geom_smooth(method = "lm", se=FALSE, color = 'red') +
    ggpubr::stat_cor(
      method = "pearson",
      label.x = limits[1] + (limits[2] * 0.05),
      label.y = limits[2]
    ) +
    # stat_regline_equation(label.x = -40, label.y = 28) +
    # coord_fixed(xlim = limits, ylim = limits) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title, # include comp % on axis
      subtitle = desc,
      x = xlab,
      y = ylab
    )
  print(plt)
  ggplot2::ggsave(
    filename = paste0(dirname(sig2path), "/OV_", set, "_recur_DESeq_slogp_oldvnew", batch, ".png"),
    plot = plt,
    height = 8,
    width = 8
  )
}

#' Plot scatter, either metric or rank
#'
#' Rubrary::plot_scatter is written to be a bit more general.
#'
#' @import ggplot2
#'
#' @param set1path Path to 1st set (x axis)
#' @param set2path Path to 2nd set (y axis)
#' @param set1lab Axis label for 1st set (x)
#' @param set2lab Axis label for 2nd set (y)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param rank Convert sign log p values to rank
#' @param savename string; File output full name
#' @param guides logical; Show m=1 and linear fit?
#'
#' @return Plot comparing DESeq signatures
#' @export
#'
plot_scatter_compare <- function(set1path, set2path, set1lab = "Set 1", set2lab = "Set 2", rank = F, guides = F,
                                 title = paste0(set1lab, " vs. ", set2lab), subtitle = "",
                                 savename = NULL) {
  
  sig1 <- utils::read.delim(set1path, header = T)
  sig2 <- utils::read.delim(set2path, header = T)
  
  sig_merged <- base::merge(sig1[, c("gene", "sign_log_p")], sig2[, c("gene", "sign_log_p")], by = "gene")
  colnames(sig_merged) <- c("gene", "sign_log_p.1", "sign_log_p.2")
  if (rank) {
    sig_merged <- sig_merged[order(sig_merged$sign_log_p.1, decreasing = T), ]
    sig_merged$sign_log_p.1 <- 1:nrow(sig_merged)
    sig_merged <- sig_merged[order(sig_merged$sign_log_p.2, decreasing = T), ]
    sig_merged$sign_log_p.2 <- 1:nrow(sig_merged)
    cormethod = "spearman"
  } else {
    cormethod = "pearson"
  }
  sig1_range <- c(min(sig_merged$sign_log_p.1, na.rm = T), max(sig_merged$sign_log_p.1, na.rm = T))
  sig2_range <- c(min(sig_merged$sign_log_p.2, na.rm = T), max(sig_merged$sign_log_p.2, na.rm = T))
  limits <- c(min(sig1_range, sig2_range) * 1.1, max(sig1_range, sig2_range) * 1.1)
  plt <- ggplot(sig_merged, aes_string(x = "sign_log_p.1", y = "sign_log_p.2")) +
    geom_point(alpha = 0.2, size = 0.5) +
    {if (!rank) geom_abline(linetype="dashed", aes(intercept=0, slope=1), size = 1)} +
    {if (!rank) geom_smooth(method = "lm", se=FALSE, color = 'red')} +
    ggpubr::stat_cor(method = cormethod) +
    theme_classic() +
    labs(
      title = title, # include comp % on axis
      subtitle = paste0(subtitle, "; ", tools::toTitleCase(cormethod), " correlation"),
      x = set1lab,
      y = set2lab
    )
  
  if (!is.null(savename)) {
    ggsave(
      filename = savename,
      plot = plt,
      height = 8,
      width = 8
    )
  }
  
  return(plt)
}


# Kaplan Meier ----
utils::globalVariables(c(
  "time", "estimate",
  "conf.low", "strata"
))

#' Plot SCN Kaplan Meier Survival Analysis
#'
#' NOT GENERALIZED
#'
#' @import ggplot2
#'
#' @param df dataframe; cols "OS" (overall survival censor), "OS.time" (overall survival time)
#' @param xlab string; x-axis label
#' @param title plot title
#' @param SCN SCN threshold to designate SCN-like status if not already in DF
#' @param savename string; filename to save as
#'
#' @return Kaplan-Meier overall survival analysis plot comparing SCN-like status
#' @export
#'
plot_SCN_KaplanMeier <- function(df, xlab = "Time", title = "Survival Analysis",
                                 SCN = NULL, savename = NULL) {
  Rubrary::use_pkg("survival", "ggsurvfit")
  
  if (!is.null(SCN)) {
    df$SCN.like <- "Non-SCN"
    df[df$Zscored_SCN_score > SCN, ]$SCN.like <- "SCN-like"
  } else {
    SCN <- 3
  }
  
  sf <- ggsurvfit::survfit2(survival::Surv(OS.time, OS) ~ SCN.like, data = df)
  pval <- ggsurvfit::survfit2_p(sf)
  km_plot <- sf %>%
    ggsurvfit::tidy_survfit() %>%
    ggplot(aes(
      x = time, y = estimate,
      ymin = conf.low, ymax = conf.low,
      linetype = strata
    )) +
    geom_step() +
    labs(
      x = xlab,
      y = "Overall Survival",
      title = title,
      subtitle = paste0("SCN-like: Z-scored SCN score > ", SCN)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    scale_x_continuous(breaks = seq(0, 8000, by = 2000), expand = c(0.02, 0)) +
    theme_classic() +
    scale_linetype_manual(
      values = c("solid", "longdash"),
      labels = c(
        paste0(
          "Non-SCN (",
          length(which(df$SCN.like == "Non-SCN")), ")"
        ),
        paste0(
          "SCN-like (",
          length(which(df$SCN.like == "SCN-like")), ")"
        )
      )
    ) +
    theme(
      legend.position = c(0.7, 0.8),
      legend.title = element_blank()
    ) +
    annotate("text", x = 4000, y = 0.7, label = paste0("Log-rank ", pval))
  
  if (!is.null(savename)) {
    ggsave(savename, km_plot, width = 8, height = 8)
  }
  
  message(survival::coxph(survival::Surv(OS.time, OS) ~ SCN.like, data = df))
  return(km_plot)
}

# Paired boxplot ----
#' plot_boxplot_paired
#'
#' @param sig Column name of specific signature
#' @param sig_paired_df Dataframe with all signatures
#' @param batch Plot subtitle
#'
#' @return Paired boxplot separated by type
#' @export
#'
plot_boxplot_paired <- function(sig, sig_paired_df, batch){
  sig_paired_df <- sig_paired_df[,c("Sample","Type", sig)]
  # order <- c(rbind(1:(nrow(sig_paired_df)/2), ((nrow(sig_paired_df)/2)+1):nrow(sig_paired_df)))
  # sig_paired_df <- sig_paired_df[order,] # Put pairs next to each other
  colnames(sig_paired_df) <- c("Sample", "Type", "Sum.Z.Score")
  ggpubr::ggpar(
    ggpubr::ggpaired(
      sig_paired_df,
      x = "Type",
      y = "Sum.Z.Score",
      xlab = F,
      ylab = "Sum Z Score",
      title = sig,
      subtitle = batch,
      color = "black",
      fill = "Type",
      line.color = "gray",
      line.size = 0.4,
      palette = "jco",
      label = sig_paired_df$Sample,
      font.label = list(size = 8, color = "black"),
      repel = T
    ),
    legend = "none"
  ) +
    ggpubr::stat_compare_means(paired = TRUE, label = "p.format", label.x = 1.3)
}
