#!/usr/bin/Rscript

options(width=800)  # nr columns in terminal

dat <- read.csv("counts.csv")

file <- sprintf("counts.png")
png(filename=file,width=3600,height=2400,pointsize=40)

barplot(t(as.matrix(dat[2:ncol(dat)])),beside=TRUE,legend=c("Ruptured","Unruptured"),names=dat[[1]],col=c("red","green","red3","green3"),args.legend=list(x="topleft"),main="Ruptured Status By Size and Location",sub="Size",ylim=c(0,round(3.9+max(t(as.matrix(dat[2:ncol(dat)]))))),ylab="Count")

text(x=2,y=max(dat[[1,2]],dat[[1,3]]),labels="BAtip",pos=3)
text(x=4,y=max(dat[[1,4]],dat[[1,5]]),labels="ICAbif",pos=3)

text(x=7,y=max(dat[[2,2]],dat[[2,3]]),labels="BAtip",pos=3)
text(x=9,y=max(dat[[2,4]],dat[[2,5]]),labels="ICAbif",pos=3)

text(x=12,y=max(dat[[3,2]],dat[[3,3]]),labels="BAtip",pos=3)
text(x=14,y=max(dat[[3,4]],dat[[3,5]]),labels="ICAbif",pos=3)

dev.off()
