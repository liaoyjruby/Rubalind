GSEA_HNECell <- read.delim("/Users/liaoyj/Library/CloudStorage/Dropbox/OV/scRNA_UCLA/noPDX/noPDX_HNECell_fGSEA.txt")
GSEA_HNECluster <- read.delim("/Users/liaoyj/Library/CloudStorage/Dropbox/OV/scRNA_UCLA/noPDX/HighNECluster/SRPCAnoPDX_HighNE_fGSEA.txt")

gsea_res = GSEA_HNECell
gsea_name = "High NE Cells"
pw_format = TRUE
pw_ignore = c("G2M", "E2F", "MHC")
gsea2_res = GSEA_HNECluster
gsea2_name = "High NE Clusters"
ptrn2 = "crosshatch"
title = "GSEA - High NE Cells vs. High NE Clusters"

GSEA_RNA <- read.delim("/Users/liaoyj/Library/CloudStorage/Dropbox/Misc/Anna/GSEA_RNA_seq_top_bottom_pathways_for_barplot_only_ER.txt") %>%
  arrange(pathway)
GSEA_prot <- read.delim("/Users/liaoyj/Library/CloudStorage/Dropbox/Misc/Anna/protein_P_vs_FA_GSEA_for_barplot_only_ER.txt") %>%
  arrange(pathway)

gsea_res = GSEA_RNA
gsea_name = "RNA"
pw_format = TRUE
pw_ignore = c("EIF2AK1", "HRI", "ERAD", " N ")
gsea2_res = GSEA_prot
gsea2_name = "Protein"

sig_cutoff = c("pval", 0.05)
title = NULL
gsea_pws = NULL
n_pws = 5
pw_split = FALSE
pw_source = TRUE
pw_size = 5
order2 = F
ptrn2 = "stripe"
NES_cutoff = NULL
colors = c("firebrick", "darkblue")
savename = NULL
width = 8
height = NULL

library(dplyr)
library(ggplot2)

Rubrary::plot_GSEA_barplot(
  gsea_res = GSEA_HNECell,
  gsea_name = "High NE Cells",
  pw_format = TRUE,
  pw_ignore = c("G2M", "E2F", "MHC"),
  gsea2_res = GSEA_HNECluster,
  gsea2_name = "High NE Clusters",
  title = "GSEA - High NE Cells vs. High NE Clusters",
  width = 10,
  savename = "/Users/liaoyj/Library/CloudStorage/Dropbox/Rubrary/plot_GSEA/HNE.pdf"
)
