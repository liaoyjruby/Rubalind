problem <- "FIB"
setwd(paste0("~/Documents/Rosalind/", problem))

# Rabbits and Recurrence Relations
input_sample <- paste0(problem, "_ds_sample.txt")

fib <- function(n, k){
  if (n == 1 | n == 2){
    F_n <- 1
  } else {
    F_n <- fib(n-1, k) + (fib(n-2, k) * k)
  }
  F_n
}

fib_wrap <- function(inputFile){
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




