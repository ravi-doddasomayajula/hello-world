#!/usr/bin/Rscript

library(ROCR)
library(aod)
library(ggplot2)

options(width=200)  # nr columns in terminal

# args
args <- commandArgs(trailingOnly=TRUE)
if( length(args)!=3 ) {
  print("usage:   mkglm.r group1_name group2_name file_name")
  print("example: mkglm.r stable ruptured SR.csv")
  quit(save="no",status=1)
} else {
  n1 <- sprintf("%s",args[1])
  n2 <- sprintf("%s",args[2])
  fn <- sprintf("%s",args[3])
}
  
# read ids of groups 1 and 2 & table
id1 <- read.csv("id.ctrls")
id2 <- read.csv("id.cases")
dat <- read.csv("table1.csv")
ign <- read.csv("ignore.csv")

# create class=0 for group 1 & class=1 for group 2
cls1 <- id1
cls1$class <- 0
cls2 <- id2
cls2$class <- 1

# create table for groups 1 & 2 including class
ids <- rbind(cls1,cls2)
tab <- merge(ids,dat,by="aid",sort=FALSE)

# empty vectors to hold results
VAR   <- c()
AUC   <- c()
PVAL  <- c()
SIGNF <- c()
OR    <- c()
ORmin <- c()
ORmax <- c()
ORout <- c()
THSLD <- c()
MULTI <- c()

# loop over variables (columns) 
i=3
while( i<=ncol(tab) ) {
  var <- names(tab)[i]
  if( ! (any(ign==var)) ) {
    # fit a univariate logistic regression model & calc ORs
    attach(tab)
    txt  <- sprintf("%s ~ + %s","class",var)
    fit  <- glm(as.formula(txt), family="binomial")
    odds <- exp(cbind(OR=coef(fit),confint(fit)))
    detach(tab)
    # calc ROC curve & AUC
    prob <- predict(fit,type="response")
    pred <- prediction(prob,tab$class)
    perf <- performance(pred,measure="tpr",x.measure="fpr")
    auc  <- performance(pred,measure="auc")
    auc  <- auc@y.values[[1]]
    roc.data <- data.frame(fpr=unlist(perf@x.values),tpr=unlist(perf@y.values),model="GLM")
    # calc optimal threshold (min dist to up-left corner)
    a <- unlist(perf@alpha.values)
    x <- unlist(perf@x.values)
    y <- unlist(perf@y.values)
    d2 <- x^2 + (1-y)^2 
    im <- which.min(d2)
    xi <- a[[im]]
    x0 <- min(tab[[i]])
    x1 <- max(tab[[i]])
    th <- round((1-xi)*x0 + xi*x1,digits=4)
    # gather results
    ar <- round(auc,digits=4)
    p  <- round(summary(fit)$coeff[[8]],digits=4)
    or <- round(odds[[2]],digits=4)
    o0 <- round(odds[[4]],digits=4)
    o1 <- round(odds[[6]],digits=4)
    if( p<0.05 ) s <- "*" else s <- " "
    if( o0<1.0 && o1>1.0 ) g <- " " else g <- "#"
    if( p<0.10 && g=="#" ) mm <- "&" else mm <- " "
    # plot ROC
    file <- sprintf("%s.%s.png",fn,var)
    message("plotting ROC --> ",file)
    tit <- sprintf("ROC(%s) AUC=%0.4f",var,ar)
    ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
         geom_ribbon(alpha=0.2) +
         geom_line(aes(y=tpr)) +
         geom_abline(intercept=0,slope=1,colour="blue",linetype="dashed") +
         ggtitle(tit) +
         theme(text=element_text(size=8))
    ggsave(file,width=8,height=6,units="cm")
    # add results to vectors
    VAR   <- c(VAR,var)
    AUC   <- c(AUC,ar)
    PVAL  <- c(PVAL,p)
    SIGNF <- c(SIGNF,s)
    OR    <- c(OR,or)
    ORmin <- c(ORmin,o0)
    ORmax <- c(ORmax,o1)
    ORout <- c(ORout,g)
    THSLD <- c(THSLD,th)
    MULTI <- c(MULTI,mm)
  }
  i<-i+1
}

# results data frame
df <- data.frame(VAR,AUC,PVAL,SIGNF,OR,ORmin,ORmax,ORout,THSLD,MULTI)
message("comparing ",n1," vs. ",n2)
print(df)
file <- sprintf("%s.csv",fn)
message("saving ",file)
write.csv(df,file)

