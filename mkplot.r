#!/usr/bin/Rscript

# args
n1 <- "R"
n2 <- "U"
n3 <- "all"
n4 <- "xxx"
args <- commandArgs(trailingOnly=TRUE)
if( length(args)>0 ) {
  n1 <- sprintf("%s",args[1])
  n2 <- sprintf("%s",args[2])
  n3 <- sprintf("%s",args[3])
  n4 <- sprintf("%s",args[4])
}

# data
dat <- read.csv("data.csv")
num <- (ncol(dat)-1)/2
mat <- dat[,-((num+2):ncol(dat))]
mat <- mat[,-(1)]

# plot to file
file <- sprintf("plot.png")
ylb<-sprintf("ratio(%s/%s)",n1,n2)
tit<-sprintf("%s vs %s - %s",n1,n2,n3)
png(filename=file,width=3600,height=2400,pointsize=40)
plt <- barplot(t(as.matrix(mat)),beside=TRUE,names=dat[[1]],las=2,col=2:(num+1),legend=names(dat)[2:(num+1)],main=tit,sub=n4,xlab=NULL,ylab=ylb,ylim=c(0,round(0.49+max(mat[1:num]))))
abline(h=1.0,lwd=3)
for(i in 1:nrow(dat)) {
  for(j in 1:num ) {
    if( dat[[i,num+1+j]]=="*" ) {
      text(x=(i-1)*(num+1)+(j-1)+1.5,y=dat[[i,j+1]],labels="*",pos=3)
    }
  }
}
dev.off()

