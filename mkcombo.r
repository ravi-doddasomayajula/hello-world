#!/usr/bin/Rscript

# functions to combine p values
fisherP <- function(x) pchisq(-2 * sum(log(x)), df=2*length(x), lower=FALSE)
binomP  <- function(x) pbinom(sum(x<0.05),size=length(x),prob=0.05,lower=FALSE)
sumPuni <- function(x,n) 1/factorial(n) * sum(sapply(0:n, function(k) (-1)^k * choose(n,k) * ifelse(x>k,x-k,0)^(n)))
sumP    <- function(x) {
  n=length(x)
  if(n<10) {sumPuni(sum(x),n)}
  else {pnorm(sum(x),n/2,sqrt(n/12),lower=TRUE)}}

# read ratios & pvalues
rat <- read.csv("ratios.csv")
pvl <- read.csv("pvalues.csv")
num <- ncol(pvl)

# calc combined pvalues & mean/stdev of ratios
message("COMBINED p-values num=",num)
message("var pFisher pBinomial pSum pMean pStdev pMin pMax")
nam <- c()
rts <- c()
dev <- c()
for(i in 2:num) {
  x  <- pvl[[i]]
  x[x<0.0001] <- 0.00009 
  pf <- round(fisherP(x),digits=4)
  pb <- round(binomP(x),digits=4)
  ps <- round(sumP(x),digits=4)
  pm <- round(mean(x),digits=4)
  pd <- round(sd(x),digits=4)
  p0 <- round(min(x),digits=4)
  p1 <- round(max(x),digits=4)
  if( pm<0.05 ) s <- "*" else s <- " "
  x  <- rat[[i]]
  rm <- mean(x)
  rd <- sd(x)
  vr <- sprintf("%s%s",names(pvl)[[i]],s)
  nam <- c(nam,vr)
  rts <- c(rts,rm)
  dev <- c(dev,rd)
  tx <- sprintf("%s %g %g %g %g %g %g %g",vr,pf,pb,ps,pm,pd,p0,p1)
  message(tx)
}

# plot ratios & stdevs
png(filename="ratios.png",width=1000,height=800,pointsize=24)
ytxt <- "ratio(Ruptured/Unruptured)"
mtxt <- ""
bplt <- barplot(rts,names.arg=nam,las=2,ylab=ytxt,main=mtxt,ylim=c(0,6))

top <- rts + dev
btm <- rts - dev
segments(bplt,top,bplt,btm,lwd=1)

abline(h=1.0,lwd=4)

dev.off()
