library(dplyr)
library(ggplot2)

airway_deseq = Rubrary::airway_deseq_res %>% relocate(hgnc_symbol)
genes = Rubrary::GSEA_pathways$GOBP_REGULATION_OF_GLUCOSE_IMPORT
# Inputs ----
sig = airway_deseq
geneset = genes
genecol = "hgnc_symbol"
rankcol = "sign_log_p"
rankcol_name = "Sign log p value"
hightolow = FALSE
label = FALSE
legendpos = "none"
hl_color = "firebrick3"
lab_high = NULL
lab_low = NULL
lab_high = "\U2191 in treated\n\U2193 in untreated"
lab_low = "\U2191 in untreated\n\U2193 in treated"
hllab = "Highlight genes"
title = "Airway Treated vs. Untreated DESeq - Glucose Import Regulation Genes"
subtitle = NULL
savename = NULL

Rubrary::plot_GSEA_pathway(
  sig = airway_deseq,
  geneset = genes,
  genecol = "hgnc_symbol",
  rankcol = "sign_log_p",
  rankcol_name = "Sign log p value",
  lab_high = "\U2191 in treated\n\U2193 in untreated",
  lab_low = "\U2191 in untreated\n\U2193 in treated",
  hllab = "Highlight genes",
  title = "Airway Treated vs. Untreated DESeq - Glucose Import Regulation Genes"
)

airway_deseq = Rubrary::airway_deseq_res %>% relocate(hgnc_symbol)
pathways <- Rubrary::GSEA_pathways
pws_plot <- c("HALLMARK_ADIPOGENESIS", "GOCC_POSTSYNAPTIC_MEMBRANE")

lapply(
  pws_plot,
  Rubrary::plot_GSEA_pathway_batch,
  pthwys = pathways,
  sig = airway_deseq,
  genecol = "hgnc_symbol",
  rankcol = "sign_log_p",
  rankcol_name = "Sign log p value",
  lab_high = "\U2191 in treated\n\U2193 in untreated",
  lab_low = "\U2191 in untreated\n\U2193 in treated",
)

