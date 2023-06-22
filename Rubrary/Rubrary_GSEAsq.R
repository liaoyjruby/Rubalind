# Normal call ----
library(msigdbr)
GSEA_pathways <- rbind(
  msigdbr(category = "C2", subcategory = "CP"),
  msigdbr(category = "C5", subcategory = "GO:BP"),
  msigdbr(category = "C5", subcategory = "GO:CC"),
  msigdbr(category = "C5", subcategory = "GO:MF"),
  msigdbr(category = "H")
) %>% split(x = .$gene_symbol, f = .$gs_name) # Named list of pathways

pws <- Rubrary::GSEA_pathways

source("/Users/liaoyj/Dropbox/Ovarian Project/functions/gsea_squared.v2.R")
# Load GSEA squared function for OV project (Favour's version, slightly modified by Ruby?)
setwd("/Users/liaoyj/Dropbox/Misc/Anna/GSEAsq/") # Directory w/ everything
source("gsea_squared.v2.R")
# ex. keywords that we use for OV GSEA
keywords <- c("INFLAM|IMMUN|INTERLEUKIN|LEUKOCYTE|TNF|MHC|CYTOKINE_|CHEMOKINE|ANTIGEN|LYMPH",
              "_COA_|LIPID|STEROL|FATTY|FAT",
              "NEURON|NEUROTRANSMITTER|SYNAP|VOLTAGE|AXON|CEREBRAL|CORTEX|NEURAL",
              "DIFFERENTIATION",
              "CELL_CYCLE|MITOTIC|DNA_REPLICATION|CHROMOSOME_SEGREGATION|SPINDLE|CELL_DIVISION|REPLICATION|E2F|STRAND|G2",
              "REPAIR|STRESS|HDR|HRR|DAMAGE|FANCONI",
              "HISTONE|METHYL|UBIQUITIN|ACETYL|SUMOYLATION")
keywords.labels <- c("immune", "lipid", "neuro", "differentiation", "cycle", "repair", "histone_modification")
# ex. UCLA high NE + Patch (f)GSEA output
gsea_squared.v2(
  file1 = "fGSEA_UCLAhighNEPatch_deseq_recur.txt",
  get.keywords = T, #can set to false after keyword file + plot have already been generated
  keywords = keywords,
  keyword.labels = keywords.labels,
  title = "UCLAhighNEPatch_deseq_recur",
  h = 8,
  w = 11,
  pos.nes_high.num.rnk = F,
  exact = T
)

df_GSEA <- "/Users/liaoyj/Dropbox/Misc/Anna/GSEAsq/fGSEA_UCLAhighNEPatch_deseq_recur.txt"
if(is.character(df_GSEA)){ df_GSEA <- Rubrary::rread(df_GSEA)}
dfm <- df_GSEA

# Run GSEAsq ----
library(dplyr)

if(is.character(df_GSEA)){ df_GSEA <- Rubrary::rread(df_GSEA)}
df_GSEA <- df_GSEA %>%
  arrange(NES) %>% # descending or ascending?
  mutate(rank = 1:nrow(.))

## Get GSEA terms ----
filt_freq <- c(5, 500)
term_freq_df <- df_GSEA$pathway %>%
  tolower() %>%
  strsplit(., "_") %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  rename(Term = ".") %>%
  arrange(desc(Freq)) %>%
  filter(Freq > filt_freq[1],
         Freq < filt_freq[2]) %>%
  mutate(Term = as.character(Term))

# set.seed(13)
terms_ksp <- function(term){
  message(paste0(term_freq_df[],]))
}


## GSEAsq calc ----

## GSEAsq viz ----
