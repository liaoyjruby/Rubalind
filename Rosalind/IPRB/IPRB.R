path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
problem <- tail(strsplit(path, "/")[[1]], n=1)

# IPRB - Mendel's First Law
samplepath <- paste0(problem, "_sample.txt")
inputpath <- paste0(problem, "_ds.txt")

iprb <- function(k, m, n){
  p <- k + m + n
  # First pick
  pr_k <- k/p
  pr_m <- m/p
  pr_n <- n/p
  # Second pick
  # 1st pick is k
  pr_kk <- pr_k * ((k-1) / (p-1)) # 100%
  pr_km <- pr_k * (m / (p-1)) # 100%
  pr_kn <- pr_k * (n / (p-1)) # 100%
  # 1st pick is m
  pr_mk <- pr_m * (k / (p-1)) # 100%
  pr_mm <- pr_m * ((m-1) / (p-1)) # 75%
  pr_mn <- pr_m * (n / (p-1)) # 50%
  # 1st pick is n
  pr_nk <- pr_n * (k / (p-1)) # 100%
  pr_nm <- pr_n * (m / (p-1)) # 50%
  pr_nn <- pr_n * ((n-1) / (p-1)) # Never
  
  # Pairs dom calc
  all_k <- pr_kk + pr_km + pr_kn
  all_m <- pr_mk + (pr_mm * 0.75) + (pr_mn * 0.5)
  all_n <- pr_nk + (pr_nm * 0.50) + (pr_nn * 0)
  
  total <- all_k + all_m + all_n
}

iprbWrap <- function(inputFile){
  ds <- readLines(inputFile)
  ds <- unlist(strsplit(ds, split = " ")[[1]])
  k <- as.numeric(ds[1])
  m <- as.numeric(ds[2])
  n <- as.numeric(ds[3])
  out <- as.character(iprb(k, m, n))
  print(out)
  
  outfile <- file(paste0(problem, "_submission.txt"))
  writeLines(out, con = outfile, sep = " ")
  close(outfile)
}

iprbWrap(samplepath)

