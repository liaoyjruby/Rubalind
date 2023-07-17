path <- dirname(rstudioapi::getSourceEditorContext()$path)
problem <- tail(strsplit(path, "/")[[1]], n=1)
setwd(paste0("~/Documents/Rosalind/", problem))

# Problem Name
samplepath <- paste0(problem, "_sample.txt")
inputpath <- tolower(paste0("rosalind_", problem, ".txt"))

is_transition <- function(nuc1, nuc2) {
  if(nuc_1)
}


TRAN <- function(inputFile){
  set <- Biostrings::readDNAStringSet(inputFile)
  str_1 <- as.character(set[1])
  str_2 <- as.character(set[2])
  for (idx in 1:nchar(str_1)){
    nuc_1 <- substr(str_1, idx, idx)
    nuc_2 <- substr(str_2, idx, idx)
    if(nuc_1 != nuc_2){
      check
    }
  }
}

inputFile <- samplepath

TRANwrap <- function(inputFile){
  out <- as.character(TRAN(inputFile))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}

TRANwrap(samplepath)
TRANwrap(inputpath)
