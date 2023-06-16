library(msigdbr)
GSEA_pathways <- rbind(
  msigdbr(category = "C2", subcategory = "CP"),
  msigdbr(category = "C5", subcategory = "GO:BP"),
  msigdbr(category = "C5", subcategory = "GO:CC"),
  msigdbr(category = "C5", subcategory = "GO:MF"),
  msigdbr(category = "H")
) %>% split(x = .$gene_symbol, f = .$gs_name) # Named list of pathways


source("/Users/liaoyj/Dropbox/Ovarian Project/functions/gsea_squared.v2.R")

file1 <- 

run_GSEAsquared <- function(){
  
}

