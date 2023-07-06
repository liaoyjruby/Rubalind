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
setwd("/Users/liaoyj/Dropbox/Rubrary/GSEAsq/GSEAsq/") # Directory w/ everything
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

# glab GSEAsq inputs ----
file1 = "/Users/liaoyj/Dropbox/Rubrary/GSEAsq/GSEAsq/fGSEA_UCLAhighNEPatch_deseq_recur.txt"
file2 = NULL
keywords <- c("INFLAM|IMMUN|INTERLEUKIN|LEUKOCYTE|TNF|MHC|CYTOKINE_|CHEMOKINE|ANTIGEN|LYMPH",
              "_COA_|LIPID|STEROL|FATTY|FAT",
              "NEURON|NEUROTRANSMITTER|SYNAP|VOLTAGE|AXON|CEREBRAL|CORTEX|NEURAL",
              "DIFFERENTIATION",
              "CELL_CYCLE|MITOTIC|DNA_REPLICATION|CHROMOSOME_SEGREGATION|SPINDLE|CELL_DIVISION|REPLICATION|E2F|STRAND|G2",
              "REPAIR|STRESS|HDR|HRR|DAMAGE|FANCONI",
              "HISTONE|METHYL|UBIQUITIN|ACETYL|SUMOYLATION")
keywords.pval = 1e-05
keyword.labels <- c("immune", "lipid", "neuro", "differentiation", "cycle", "repair", "histone_modification")
keyword_plot_method = 2
get.keywords = F
dir = "/Users/liaoyj/Dropbox/Rubrary/GSEAsq/GSEAsq/"
title = "UCLAhighNEPatch_deseq_recur"
h = 5.5; w = 9.5
pos.nes_high.num.rnk = F; exact = NULL
cat_colors <- NULL

# Rubrary ----
terms_df <- get_GSEAsq_keywords(df_GSEA = df_GSEA,
                                savename = "/Users/liaoyj/Dropbox/Rubrary/GSEAsq/test",
                                seed = 1)

# Keywords ----

kws_df <- data.frame(categories = keyword.labels,
                     terms = keywords)

# Main GSEAsq run ----
## Inputs ----
library(dplyr)
setwd("/Users/liaoyj/Documents/Rubrary/")
df_GSEA <- "/Users/liaoyj/Dropbox/Rubrary/GSEAsq/GSEAsq/fGSEA_UCLAhighNEPatch_deseq_recur.txt"
savename = "/Users/liaoyj/Dropbox/Rubrary/GSEAsq/test"
get_terms <- FALSE
plot_type = "jitter"
title = "GSEA Squared - UCLA HNE & Patch"
height = 6; width = 8
signlogp_base = 10
rep0 = 2.2e-16
plot_pval = TRUE
seed = 13
cat_colors <- NULL
verbose = TRUE

cats <- data.frame(
  category = c("Immune", "Lipid", "Neuro", "Differentiation", "Cycle", "Repair", "Histone Modification"),
  terms = c("INFLAM|IMMUN|INTERLEUKIN|LEUKOCYTE|TNF|MHC|CYTOKINE_|CHEMOKINE|ANTIGEN|LYMPH",
            "_COA_|LIPID|STEROL|FATTY|FAT",
            "NEURON|NEUROTRANSMITTER|SYNAP|VOLTAGE|AXON|CEREBRAL|CORTEX|NEURAL",
            "DIFFERENTIATION",
            "CELL_CYCLE|MITOTIC|DNA_REPLICATION|CHROMOSOME_SEGREGATION|SPINDLE|CELL_DIVISION|REPLICATION|E2F|STRAND|G2",
            "REPAIR|STRESS|HDR|HRR|DAMAGE|FANCONI",
            "HISTONE|METHYL|UBIQUITIN|ACETYL|SUMOYLATION")
)
categories = cats$category
cat_terms = cats$terms

GSEAsq <- Rubrary::run_GSEA_squared(
  df_GSEA = "/Users/liaoyj/Dropbox/Rubrary/GSEAsq/GSEAsq/fGSEA_UCLAhighNEPatch_deseq_recur.txt",
  categories = cats$category,
  cat_terms = cats$terms,
  get_terms = T, plot_pval = F, plot_type = "density",
  savename = "/Users/liaoyj/Dropbox/Rubrary/GSEAsq/fulltest"
)

# airway example
library(dplyr)
# Load data
deseq_stats <- setNames(
  airway_deseq_res[,"sign_log_p"], 
  airway_deseq_res[,"hgnc_symbol"]
)
pthwys <- GSEA_pathways
# Run (f)gsea
gsea_results <- fgsea::fgsea(
  pathways = pthwys,
  stats = deseq_stats,
  eps = 0.0,
  minSize = 15,
  maxSize  = 500) %>%
  arrange(NES)
# Get terms
terms <- Rubrary::get_GSEAsq_terms(gsea_results, verbose = FALSE)
GSEAsq_terms <- terms$Term[c(1, 2)] # "METABOLIC" "DNA"
# Run GSEA squared
GSEAsq <- Rubrary::run_GSEA_squared(
  df_GSEA = gsea_results,
  categories = GSEAsq_terms,
  cat_terms = GSEAsq_terms,
  plot_pval = TRUE,
  plot_type = "jitter"
)
names(GSEAsq) # Various outputs as list
GSEAsq$plot

## Start ----
#' Run GSEA Squared
#' 
#' @import dplyr
#' @import ggplot2
#' 
#' @param df_GSEA df/string; (path to) GSEA results with `pathway` and `NES` columns
#' @param categories char vector; list of category names
#' @param cat_terms char vector; list of category terms with each element being keywords separated by "`|`", ex. "CELL_CYCLE|MITOTIC|DNA_REPLICATION"
#' @param rep0 numeric; value to replace `pval == 0` results with, use `rep0 = 2.2e-16` (rounded `.Machine$double.eps`) to be same as original function
#' @param signlogp_base integer; log base when calculating signed log_base p metric
#' @param get_terms logical; `TRUE` to run `Rubrary::get_GSEAsq_terms` to output filtered table of terms and associated statistics
#' @param terms_filt_freq num vector length 2; `get_GSEAsq_terms` arg, `filt_freq[1]` is minimum frequency, `filt_freq[2]` is maximum frequency
#' @param plot_type `c("jitter", "density")`; GSEA squared output plot type
#' @param plot_pval logical; `TRUE` to append p-value to category name. If `ggtext` package is installed, can do fancy markdown formatting
#' @param title string; plot title
#' @param cat_colors char vector; list of colors corresponding to category
#' @param savename string; path to save outputs under (no extension)
#' @param height numeric; output plot height
#' @param width numeric; output plot width
#' @param seed integer; randomization seed
#'
#' @return `ggplot` object of GSEA squared plot
#' @export
run_GSEA_squared <- function(
    df_GSEA, categories, cat_terms, rep0 = 2.2e-16, signlogp_base = 10,
    get_terms = FALSE, terms_filt_freq = c(5, 500),
    plot_type = c("jitter", "density"), plot_pval = TRUE, title = NULL, cat_colors = NULL,
    savename = NULL, height = 6, width = 8, seed = 13){
  
  plot_type = match.arg(plot_type)
  if(is.null(cat_colors)){ cat_colors <- scales::hue_pal()(length(categories))}
  
  if(is.character(df_GSEA)){ df_GSEA <- Rubrary::rread(df_GSEA)}
  df_GSEA <- df_GSEA %>%
    filter(!is.na(NES)) %>%
    arrange(NES) %>% # default descending
    mutate(rank = 1:nrow(.)) # rank 1 == lowest value
  
  # Get terms - can be run indendently of function but orig. GSEAsq has it included
  if(get_terms){
    Rubrary::get_GSEAsq_terms(
      df_GSEA, savename,
      filt_freq = terms_filt_freq, seed = seed, signlogp_base = signlogp_base)
  }
  cat_df <- data.frame(Category = categories, Term = cat_terms)
  
  # "contains" mode - per category GSEA df
  cats_df <- lapply(
    cat_df$Category,
    function(cat){
      df_GSEA %>%
        filter(grepl(cat_df[cat_df$Category == cat, "Term"], pathway)) %>%
        mutate(Category = cat)
    }) %>%
    bind_rows() %>%
    arrange(rank)
  
  # Remaining pathways not captured by categories
  others_df <- df_GSEA %>%
    filter(!(pathway %in% unique(cats_df$pathway))) %>%
    mutate(Category = "Other")
  
  # Merge category + non-category pathways
  GSEAsq_df <- bind_rows(cats_df, others_df) %>%
    arrange(rank) %>%
    mutate(Category = factor(Category, levels = c(cat_df$Category, "Other")))
  
  # Category ks-pvalues
  cat_ksp <- function(cat){ # per term KS test & ES
    set.seed(seed)
    freq <- nrow(GSEAsq_df[GSEAsq_df$Category == cat,])
    if(verbose){ message(paste0("** ",cat, " (", freq, ")"))}
    # Filter to relevant category - original function doesn't do this? leaves duplicates in background
    cat_other_df <- GSEAsq_df %>% filter(Category == cat | Category == "Other")
    # Use `rep0 = 2.2e-16` (rounded `.Machine$double.eps`) to be same as original function
    ksp <- Rubrary::get_kspval(
      df = cat_other_df, value = "rank", group = "Category", goi = cat, rep0 = rep0, signed = TRUE, viz = F)
    # ES: see orig. GSEA squared function that uses https://github.com/franapoli/signed-ks-test/blob/master/signed-ks-test.R
    return(data.frame(Category = cat, Freq = freq, pval = ksp$pval, ES = ksp$ES))
  }
  
  if(verbose){ message("Calculated category KS p-values...")}
  cat_ksp_df <- lapply(categories, cat_ksp) %>% # List w/ each element being a term
    bind_rows() %>% # Turn list of rows into df
    mutate(signedlogp = sign(ES) * abs(log(pval, signlogp_base)),
           sign = case_when( # ifelse(sign(ES) != 1, "\u002b", "\u2212"))
             # Sign symbols opposite of actual sign but better for intuitive understanding?
             sign(ES) == 1 ~ "\u2212", # minus sign == towards left side
             sign(ES) == 0 ~ "0",
             sign(ES) == -1 ~ "\u002b" # plus sign == towards right side
           )
    )
  
  # Plot
  if(plot_pval){ # Include pval calculations in plot
    Rubrary::use_pkg("ggtext")
    if((requireNamespace("ggtext", quietly = TRUE))){ # ggtext available - fancy!!
      cat_text_obj <- ggtext::element_markdown(
        size = 12, angle = 0, hjust = 1, color = "black", lineheight = 1.2)
      cat_text <- paste0(
        "**", cat_ksp_df$Category, "**<br>",
        "<span style = 'font-size:10pt;color:gray40;'>*p = ", signif(cat_ksp_df$pval, digits = 2), "* (", cat_ksp_df$sign, ")</span>")
    } else { # No ggtext
      cat_text_obj <- element_text(size = 10, angle = 0, hjust = 1, color = "black")
      cat_text <- paste0(
        cat_ksp_df$Category, "\n",
        "p = ", signif(cat_ksp_df$pval, digits = 2), " (", cat_ksp_df$sign, ")")
    }
  } else {
    cat_text <- cat_ksp_df$Category
  }
  
  cat_lab <- stats::setNames(nm = cat_ksp_df$Category, cat_text)
  
  if(plot_type == "density"){ # density plot type
    theme_type = theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = cat_text_obj,
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none")
  } else { # jitter plot type
    theme_type = theme(
      legend.position = "none",
      axis.text.y = cat_text_obj
    )
  }
  GSEAsq_plt <- ggplot(GSEAsq_df[GSEAsq_df$Category != "Other",], aes(x = rank, color = Category)) +
    {if(plot_type == "jitter") geom_point(aes(y = Category), size = 0.75,
                                          position = position_jitter(seed = seed))} +
    {if(plot_type == "density") geom_density(aes(fill = Category), alpha = 0.5)} +
    {if(plot_type == "density") geom_rug(aes(color = Category), alpha = 0.5)} +
    {if(plot_type == "density") facet_wrap(
      vars(Category), ncol = 1, scales = "free_y", strip.position = "left",
      labeller = labeller(Category = cat_lab))} +
    xlab("Rank") +
    labs(title = title) +
    scale_color_manual(values = cat_colors) +
    {if(plot_type == "jitter") scale_y_discrete(name = NULL, labels = cat_lab)} +
    theme_classic() +
    theme_type
  
  if(!is.null(savename)){
    Rubrary::rwrite(
      x = cat_ksp_df,
      file = paste0(savename, "_GSEAsq_category_kspvals.txt")
    )
    ggsave(
      plot = GSEAsq_plt,
      filename = paste0(savename, "_GSEAsq_category_", plot_type, "plot.png"),
      height = height, width = width
    )
  }
  return(GSEAsq_plt)
}
