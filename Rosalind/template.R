path <- dirname(rstudioapi::getSourceEditorContext()$path)
problem <- tail(strsplit(path, "/")[[1]], n=1)
setwd(paste0("~/Documents/Rubalind/Rosalind/", problem))

# Problem Name
samplepath <- paste0(problem, "_sample.txt")
inputpath <- tolower(paste0("rosalind_", problem, ".txt"))

FUNC <- function(a, b){
  # BODY
}

inputFile <- samplepath

FUNCwrap <- function(inputFile){
  ds <- readLines(inputFile)
  ds <- unlist(strsplit(ds, split = " ")[[1]])
  a <- as.numeric(ds[1])
  b <- as.numeric(ds[2])
  out <- as.character(FUNC(a, b))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}

FUNCwrap(samplepath)
FUNCwrap(inputpath)
