path <- dirname(rstudioapi::getSourceEditorContext()$path)
problem <- tail(strsplit(path, "/")[[1]], n=1)
setwd(paste0("~/Documents/Rosalind/", problem))

# SPLC - RNA Splicing
samplepath <- paste0(problem, "_sample.txt")
inputpath <- tolower(paste0("rosalind_", problem, ".txt"))

# library(Biostrings)

dna <- "GATGGAACTTGACTACGTAAATT"

DNA2RNA <- function(dna){
  dna_spl <- strsplit(dna, "")[[1]]
  rna = ""
  for (nuc in dna_spl){
    rna <- paste0(rna, ifelse(nuc == 'T', 'U', nuc))
  }
  return(rna)
}

SPLC <- function(a, b){
  set <- Biostrings::readDNAStringSet(inputFile)
  str_DNA <- as.character(set[1])
}
inputFile <- samplepath


SPLCWrap <- function(inputFile){
  # ds <- readLines(inputFile)
  # ds <- unlist(strsplit(ds, split = " ")[[1]])
  # a <- as.numeric(ds[1])
  # b <- as.numeric(ds[2])
  out <- as.character(SPLC(inputFile))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}

SPLCWrap(samplepath)
SPLCWrap(inputpath)
