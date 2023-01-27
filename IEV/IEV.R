path <- dirname(rstudioapi::getSourceEditorContext()$path)
problem <- tail(strsplit(path, "/")[[1]], n=1)
setwd(paste0("~/Documents/Rosalind/", problem))

# Problem Name
samplepath <- paste0(problem, "_sample.txt")
inputpath <- tolower(paste0("rosalind_", problem, ".txt"))

# 1 AA-AA 100
# 2 AA-Aa 100
# 3 AA-aa 100
#   Aa Aa \\ Aa Aa
# 4 Aa-Aa 75
#   AA Aa \\ Aa aa
# 5 Aa-aa 50
#   Aa aa \\ Aa aa
# 6 aa-aa 0
#   aa aa \\ aa aa
IEV <- function(ints){
  prs <- c(1, 1, 1, 0.75, 0.5, 0)
  exp <- ints
  for (i in 1:length(ints)){
    exp[i] <- prs[i] * ints[i] * 2
  }
  exp_sum <- sum(exp)
}

inputFile <- samplepath
IEVWrap <- function(inputFile){
  ds <- readLines(inputFile)
  ds <- unlist(strsplit(ds, split = " ")[[1]])
  a <- as.numeric(ds)
  # b <- as.numeric(ds[2])
  out <- as.character(IEV(a))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}

IEVWrap(samplepath)
IEVWrap(inputpath)
