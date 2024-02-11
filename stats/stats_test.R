
# Plot density function
x<-seq(from=-3,to=+3,length.out=1000) 
x
# Normal
plot(x, dnorm(x))
# Beta
x2 <-seq(from=0,to=1,length.out=100) 
plot(x2, dbeta(x2, 0.5, 0.5))

# ICC ----
library(irr)
data(anxiety)
icc(anxiety, model="twoway", type="agreement")

r1 <- round(rnorm(20, 10, 4))
r2 <- round(r1 + 10 + rnorm(20, 0, 2))
r3 <- round(r1 + 20 + rnorm(20, 0, 2))
icc(cbind(r1, r2, r3), "twoway")              # High consistency
icc(cbind(r1, r2, r3), "twoway", "agreement") # Low agreement
