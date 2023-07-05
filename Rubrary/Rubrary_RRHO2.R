library(dplyr)
library(ggplot2)

# Inputs
# sig1 = DE_PDXB3N_recur_PC
sig1 = "/Users/liaoyj/Library/CloudStorage/Dropbox/OV/PDX_CarboCycling/Batch3/PDXB3_N/DESeq/PDXB3N_DESeq_PreRecur_results_PC.txt"
sig2 = "/Users/liaoyj/Dropbox/Ovarian Project/signatures/DESeq_Recurr_UCLAPatch/UCLAPatch_highNE/OV_UCLA&Patch_recur_prevpost_slogp_DESeq_pconly_13bno9no1no27.txt"
sig1_name = "PDXB3N_Recur"
sig1_low = "Pre-therapy"
sig1_high = "Recurrent"
# sig2 = UCLAHNEPatch
sig2_name = "UCLAHNEPatch_Recur"
sig2_low = "Chemonaive"
sig2_high = "Recurrent"
key = "gene"
metric1 = "sign_log_p"
metric2 = metric1
savename = "/Users/liaoyj/Library/CloudStorage/Dropbox/Rubrary/RRHO2/RRHO_PDXB3NRecurDE_UCLAHNEPatchDE"
steps = NULL
webtool = TRUE
BY = FALSE
hm_method = "ggplot"
palette = "Spectral"
waterfall = TRUE
scatter = TRUE
plot_fmt = "png"

# Function call
# Compare PreRecur to UCLAHNEPatch
debugonce(Rubrary::run_RRHO)
RRHO_recur <- Rubrary::run_RRHO(
  sig1 = "/Users/liaoyj/Library/CloudStorage/Dropbox/OV/PDX_CarboCycling/Batch3/PDXB3_N/DESeq/PDXB3N_DESeq_PreRecur_results_PC.txt",
  sig1_name = "PDXB3N_Recur",
  sig1_low = "Pre-therapy", sig1_high = "Recurrent",
  sig2 = "/Users/liaoyj/Dropbox/Ovarian Project/signatures/DESeq_Recurr_UCLAPatch/UCLAPatch_highNE/OV_UCLA&Patch_recur_prevpost_slogp_DESeq_pconly_13bno9no1no27.txt",
  sig2_name = "UCLAHNEPatch_Recur",
  sig2_low = "Chemonaive", sig2_high = "Recurrent",
  key = "gene", metric1 = "sign_log_p",
  savename = "/Users/liaoyj/Library/CloudStorage/Dropbox/Rubrary/RRHO2/RRHO_PDXB3NRecurDE_UCLAHNEPatchDE"
)
