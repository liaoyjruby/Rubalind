path <- dirname(rstudioapi::getSourceEditorContext()$path)
problem <- tail(strsplit(path, "/")[[1]], n=1)
setwd(paste0("~/Documents/Rosalind/", problem))

# SPLC - RNA Splicing
samplepath <- paste0(problem, "_sample.txt")
inputpath <- tolower(paste0("rosalind_", problem, ".txt"))

library(Biostrings)

SPLC <- function(inputFile){
  set <- Biostrings::readDNAStringSet(inputFile)
  str_DNA <- as.character(set[1])
  str_introns <- as.character(set[2:length(set)])
  # Splice / remove introns
  for (intron in str_introns){
    str_DNA <- gsub(intron, "", str_DNA)
  }
  # Transcribe (with codon table from Biostrings)
  AAs <- ""
  codons <- ""
  for (idx in 1:(nchar(str_DNA)-1)){
    if (((idx+2) %% 3) == 0){
      codon <- substring(str_DNA, idx, idx+2)
      codons <- paste0(codons, codon)
      AAs <- paste0(AAs, GENETIC_CODE[codon])
    }
  }
  return(gsub("\\*", "", AAs))
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
