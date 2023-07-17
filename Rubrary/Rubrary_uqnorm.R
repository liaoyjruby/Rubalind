library(dplyr)

OVB5_cts <- Rubrary::rread(
  "/Users/liaoyj/Dropbox/Rubrary/UQnorm/OV_Batch5_rsem_genes_raw_counts.txt", row.names = 1)
OVB5_UQ_perl <- Rubrary::rread(
  "/Users/liaoyj/Dropbox/Rubrary/UQnorm/OVB5_UQ_perl.txt", row.names = 1)

vals <- OVB5_cts$X31
q = 75
target = 1000
min = 1
m1 = TRUE

quant = q/100
vals <- vals_filt
# Upper quartile - size factor = divide each read count by 75th quantile of read counts in its sample
quantile_norm <- function(
    vals, q = 75, target = 1000, min = 1, perl = FALSE){
  if(is.data.frame(vals)){ # Do entire dataframe at once
    vals <- vals %>%
      mutate(across(all_of(names(.)), quantile_norm, perl = perl))
    vals_uq <- vals
  } else {
    vals_filt <- vals[vals >= min] %>% sort()
    if(perl){
      options(digits = 9)
      # Compatibility with Graeber Lab perl quantile_norm.pl - truncates
      quant_perl <- function(vals, quant) {
        len <- length(vals)
        idx <- ((len-1) * quant) + 1 # Perl arrays start idx at 0, R arrays start idx at 1
        qval <- vals[trunc(idx)] # `trunc` replicates Perl's `int` function
        if (idx > trunc(idx)) { # Index not integer value
          qval <- qval + quant * (vals[trunc(idx) + 1] - qval) # Add difference scaled by quant
        }
        return(qval)
      }
      uquant <- quant_perl(vals_filt, q/100)
      vals_uq <- as.numeric(sprintf("%.4f", (vals * (target / uquant))))
    } else {
      uquant <- stats::quantile(vals_filt)[[paste0(q, "%")]]
      vals_uq <- round(vals * (target / uquant), 4)
    }
  }
  return(vals_uq)
}

samp <- "X31"
smp_UQ <- quantile_norm(OVB5_cts_UQ_RL[,samp], perl = TRUE)
smp_UQ == OVB5_UQ[,samp]

# Apply to whole dataframe
# debugonce(quantile_norm)
OVB5_UQ_RL <- OVB5_cts %>%
  mutate(across(all_of(names(.)), quantile_norm, perl = TRUE))

OVB5_UQ_RL_df <- quantile_norm(OVB5_cts, perl = TRUE)

# OVB5_cts_UQ_RL == OVB5_UQ

samp <- "X10_5"
df <- data.frame(RL = OVB5_UQ_RL[,samp], OG = OVB5_UQ_perl[,samp]) %>%
  mutate(div = RL/OG, diff = RL - OG) %>%
  filter(RL != OG)

Rubrary::plot_scatter(
  xval = cts_uq,
  yval = OVB5_UQ$X31,
)

# Bard?

library(dplyr)

normalize_data <- function(vals, q, target, min) {
  vals <- vals[vals > 1]
  vals <- quantile(vals, q/100) * target / quantile(vals, min)
  return(data)
}

# Example
debugonce(normalize_data)
normalize_data(
  vals = cts_no0,
  q = 75, target = 1000, min = 1)

