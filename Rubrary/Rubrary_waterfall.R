library(dplyr)
library(ggplot2)
library(patchwork)
# Inputs ----
airway_deseq = Rubrary::airway_deseq_res %>% relocate(hgnc_symbol)
genes = Rubrary::GSEA_pathways$GOBP_REGULATION_OF_GLUCOSE_IMPORT[1:20]

rankcol = "sign_log_p"
rankcol_name = "Sign log p value"
hightolow = FALSE
vert = FALSE
label = TRUE
replab_size = 3.5
density = FALSE
hllab = "Highlight"
otherlab = "Other"
pval = TRUE
# lab_high = "\U2191 in treated\n\U2193 in untreated"
# lab_low = "\U2191 in untreated\n\U2193 in treated"
lab_high = NULL
lab_low = NULL
lab_size = 6
legendpos = "none"
title = NULL
colors = c("firebrick3", "gray")
width = 10
height = 5
savename = NULL

Rubrary::plot_waterfall(
  sig = airway_deseq,
  highlight = genes,
  rankcol = "sign_log_p",
  rankcol_name = "Sign log p value",
  lab_high = "\U2191 in treated\n\U2193 in untreated",
  lab_low = "\U2191 in untreated\n\U2193 in treated",
  title = "Airway Treated vs. Untreated DESeq - Glucose Import Regulation Genes",
  density = TRUE
)

