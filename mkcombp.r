#!/usr/bin/Rscript

# functions to combine p values
fisherP <- function(x) pchisq(-2 * sum(log(x)), df=2*length(x), lower=FALSE)
binomP  <- function(x) pbinom(sum(x<0.05),size=length(x),prob=0.05,lower=FALSE)
sumPuni <- function(x,n) 1/factorial(n) * sum(sapply(0:n, function(k) (-1)^k * choose(n,k) * ifelse(x>k,x-k,0)^(n)))
sumP    <- function(x) {
  n=length(x)
  if(n<10) {sumPuni(sum(x),n)}
  else {pnorm(sum(x),n/2,sqrt(n/12),lower=TRUE)}}

# read p-values & combine
pp <- read.csv("pvals.csv")
x  <- pp[[1]]
x[x<0.0001] <- 0.00009 
pf <- round(fisherP(x),digits=4)
pb <- round(binomP(x),digits=4)
ps <- round(sumP(x),digits=4)

# print combined p-values
message(names(pp)[[1]]," ",pf," ",pb," ",ps," ",mean(x)," ",min(x)," ",max(x))

