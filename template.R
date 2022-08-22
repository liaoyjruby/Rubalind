problem <- "CODE"
setwd(paste0("~/Documents/Rosalind/", problem))

# Problem Name
sample_path <- paste0(problem, "_ds_sample.txt")

funcName <- function(a, b){
  # BODY
}

funcWrap <- function(inputFile){
  ds <- readLines(inputFile)
  ds <- unlist(strsplit(ds, split = " ")[[1]])
  n <- as.numeric(ds[1])
  k <- as.numeric(ds[2])
  out <- as.character(fib(n, k))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}



