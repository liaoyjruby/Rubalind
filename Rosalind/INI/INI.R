path <- dirname(rstudioapi::getSourceEditorContext()$path)
problem <- tail(strsplit(path, "/")[[1]], n=1)
setwd(paste0("~/Documents/Rubalind/Rosalind/", problem))

# INI
# Given: A DNA string s of length at most 1000 bp.
# Return: Four integers (separated by spaces) representing the respective number of times that the symbols 'A', 'C', 'G', and 'T' occur in s
# . Note: You must provide your answer in the format shown in the sample output below.
# 20 12 17 21

samplepath <- paste0(problem, "_sample.txt")
inputpath <- tolower(paste0("rosalind_", problem, ".txt"))

FUNC <- function(s){
  library(dplyr)
  splt <- strsplit(s, "")[[1]] %>%
    table() %>%
    unname() %>%
    paste0(collapse = " ")
  return(splt)
}

inputFile <- samplepath

FUNCwrap <- function(inputFile){
  s <- readLines(inputFile)
  # ds <- unlist(strsplit(ds, split = " ")[[1]])
  # a <- as.numeric(ds[1])
  out <- as.character(FUNC(s))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}

FUNCwrap(samplepath)
FUNCwrap(inputpath)
