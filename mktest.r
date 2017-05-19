#!/usr/bin/Rscript

options(width=200)  # nr columns in terminal

# args
args <- commandArgs(trailingOnly=TRUE)
if( length(args)==0 ) {
  print("usage:   mktest.r title group1_name group2_name file_name")
  print("example: mktest.r title stable ruptured S_vs_R.1")
  quit(save="no",status=1)
} else {
  ti <- sprintf("%s",args[1])
  n1 <- sprintf("%s",args[2])
  n2 <- sprintf("%s",args[3])
  fn <- sprintf("%s",args[4])
}

  
# read ids of groups 1 and 2 & table
id1 <- read.csv("id.ctrls")
id2 <- read.csv("id.cases")
dat <- read.csv("table1.csv", sep = ",")
ign <- read.csv("ignore.csv", sep = ",")
dat$X <- NULL

# get subtables for groups 1 and 2
d1 <- merge(id1,dat,by="aid",sort=FALSE)
d2 <- merge(id2,dat,by="aid",sort=FALSE)

# empty vectors for results
VAR   <- c()
MEAN1 <- c()
MED1  <- c()
STDV1 <- c()
MIN1  <- c()
MAX1  <- c()
MEAN2 <- c()
MED2  <- c()
STDV2 <- c()
MIN2  <- c()
MAX2  <- c()
RAT21 <- c()
PVAL  <- c()
SIGNF <- c()
SLESS <- c()
PLESS <- c()
SMORE <- c()
PMORE <- c()
MULTI <- c()

# compare each column & store ratios and variable names
rts <- c()
lbs <- c()
i=2
while( i<=ncol(d1) ) {
  # ignore variable?
  vr <- names(d1)[i]
  if( ! (any(ign==vr)) ) {
    # wilcoxon test
    ts <- wilcox.test(d1[[i]],d2[[i]],paired=TRUE,alternative="two.sided")
    tl <- wilcox.test(d1[[i]],d2[[i]],paired=TRUE,alternative="less")
    tg <- wilcox.test(d1[[i]],d2[[i]],paired=TRUE,alternative="greater")
    # gather results
    m1  <- round(mean(d1[[i]]),digits=4)
    md1 <- round(median(d1[[i]]),digits=4)
    s1  <- round(sd(d1[[i]]),digits=4)
    mn1 <- round(min(d1[[i]]),digits=4)
    mx1 <- round(max(d1[[i]]),digits=4)
    m2  <- round(mean(d2[[i]]),digits=4)
    md2 <- round(median(d2[[i]]),digits=4)
    s2  <- round(sd(d2[[i]]),digits=4)
    mn2 <- round(min(d2[[i]]),digits=4)
    mx2 <- round(max(d2[[i]]),digits=4)
    r   <- round(m2/m1,digits=4)
    p   <- round(ts$p.value,digits=4)
    if( p<0.05 ) s <- "*" else s <- " "
    if( p<0.05 ) mm <- "&" else mm <- " "
    pl  <- round(tl$p.value,digits=4)
    if( pl<0.05 ) sl <- "<" else sl <- " "
    pg  <- round(tg$p.value,digits=4)
    if( pg<0.05 ) sg <- ">" else sg <- " "
    lab <- sprintf("%s%s%s%s",names(d1)[i],s,sl,sg)
    rts <- c(rts,r)
    lbs <- c(lbs,lab)
    # add results to vectors
    VAR   <- c(VAR,vr)
    MEAN1 <- c(MEAN1,m1)
    MED1  <- c(MED1,md1)
    STDV1 <- c(STDV1,s1)
    MIN1  <- c(MIN1,mn1)
    MAX1  <- c(MAX1,mx1)
    MEAN2 <- c(MEAN2,m2)
    MED2  <- c(MED2,md2)
    STDV2 <- c(STDV2,s2)
    MIN2  <- c(MIN2,mn2)
    MAX2  <- c(MAX2,mx2)
    RAT21 <- c(RAT21,r)
    PVAL  <- c(PVAL,p)
    SIGNF <- c(SIGNF,s)
    PLESS <- c(PLESS,pl)
    SLESS <- c(SLESS,sl)
    PMORE <- c(PMORE,pg)
    SMORE <- c(SMORE,sg)
    MULTI <- c(MULTI,mm)
  }
  i<-i+1
}

# output
message("comparing ",n1,"(1) vs. ",n2,"(2) -> nr pairs=",nrow(d1)-1)
df <- data.frame(VAR,MEAN1,MED1,STDV1,MIN1,MAX1,MEAN2,MED2,STDV2,MIN2,MAX2,RAT21,PVAL,SIGNF,PLESS,SLESS,PMORE,SMORE,MULTI)
print(df)
file <- sprintf("%s.csv",fn)
message("saving ",file)
write.csv(df,file)

# plot
file <- sprintf("%s.png",fn)
message("saving ",file)
ylb<-sprintf("ratio(%s/%s)",n2,n1)
png(filename=file,width=800,height=600,pointsize=22)
barplot(rts,names.arg=lbs,ylab=ylb,las=2,main=ti)
abline(h=1.0,lwd=3)
dev.off()
