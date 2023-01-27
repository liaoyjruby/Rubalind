path <- dirname(rstudioapi::getSourceEditorContext()$path)
problem <- tail(strsplit(path, "/")[[1]], n=1)
setwd(paste0("~/Documents/Rosalind/", problem))

# Problem Name
samplepath <- paste0(problem, "_sample.txt")
inputpath <- paste0(problem, "_ds.txt")

subs <- function(str, sub){
  n_str <- nchar(str)
  n_sub <- nchar(sub)
  idxs <- c()
  for (i in 1:(n_str - n_sub + 1)) {
    str_window <- substr(str, i, i + n_sub - 1)
    # print(str_window)
    if (sub == str_window){
      idxs <- append(idxs, i)
    }
  }
  # print(idxs)
  return(idxs)
}

subsWrap <- function(inputFile){
  ds <- readLines(inputFile)
  # ds <- unlist(strsplit(ds, split = " ")[[1]])
  a <- ds[1]
  b <- ds[2]
  out <- as.character(subs(a, b))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}

subsWrap(samplepath)
subsWrap(inputpath)
