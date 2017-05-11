#!/usr/bin/Rscript

options(width=200)  # nr columns in terminal

dat <- read.csv("table2.csv")
dat$aid <- NULL
dat$size.group <- NULL
mat <- cor(dat)

# label variables that are correlated
thd <- 0.8
message("threshold=",thd)
n   <- ncol(mat)
lab <- rep(0,n)
nid <- 1
cid <- 1
for(i in 1:n) {
  if( lab[i]==0 ) {
    nid <- nid+1
    cid <- nid
  }
  else cid <- lab[i]
  lab[i] <- cid

  if( i<n ) {
    for(j in (i+1):n) {
      if( abs(mat[[i,j]])>thd ) {
        if( lab[j]==0 ) lab[j] <- cid
        else {
          oid <- lab[j]
          for(k in 1:n) if( lab[k]==oid ) lab[k]=cid
        }
      }
    }
  }
}

# print groups of correlated variables
k <- 1
for(i in 1:nid) {
  fst <- 1
  for(j in 1:n) {
    if( lab[j]==i ) {
      if( fst==1 ) {
        txt <- sprintf("group[%d]=",k)
        fst <- 0
      }
      txt <- sprintf("%s %s",txt,colnames(mat)[j])
    }
  }
  if( fst==0 ) {
    print(txt)
    k <- k+1
  }
}


