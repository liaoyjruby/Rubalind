
setup <- function(ID, sample = NA){
  setwd("~/Documents/Rubalind/Rosalind")
  dir.create(paste0("./", ID), showWarnings = F)
  file.copy("template.R", paste0("./", ID))
  file.rename(paste0("./", ID, "/template.R"), paste0("./", ID, "/", ID, ".R"))
  
  if(!is.na(sample)){
    samplefile <- file(paste0("./", ID, "/", ID, "_sample.txt"))
    writeLines(sample, con = samplefile)
    close(samplefile)
  }
}
