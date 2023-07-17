path <- dirname(rstudioapi::getSourceEditorContext()$path)
problem <- tail(strsplit(path, "/")[[1]], n=1)
setwd(paste0("~/Documents/Rosalind/", problem))

# Problem Name
samplepath <- paste0(problem, "_sample.txt")
inputpath <- paste0("rosalind_", problem,".txt")

# Assuming all rabbits produce a single pair of offspring
fibd <- function(n, m){
  month <- 2
  if (n <= 2){
    F_n <- 1
  } else if (month = 1){
    
  }
    F_n <- fibd(n-m, m) + fibd(n-(m-1), m)
  }
  F_n
}

fibdWrap <- function(inputFile){
  ds <- readLines(inputFile)
  ds <- unlist(strsplit(ds, split = " ")[[1]])
  n <- as.numeric(ds[1])
  m <- as.numeric(ds[2])
  out <- as.character(fibd(n, m))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}

fibdWrap(samplepath)
fibdWrap(inputpath)
