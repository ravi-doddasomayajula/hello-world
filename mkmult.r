#!/usr/bin/Rscript

library(QuantPsyc)
library(leaps)
library(DAAG)
library(MASS)
library(ROCR)
library(boot)
library(aod)
library(ggplot2)

options(width=200)  # nr columns in terminal

# read: table, groups, signif variables
tab <- read.csv("table1.csv")
id1 <- read.csv("id.ctrls")
id2 <- read.csv("id.cases")
vrs <- read.csv("vars.csv")

# create classes for each group
cls1 <- id1
cls1$class <- 0
cls2 <- id2
cls2$class <- 1

# create new data table with classes
ids <- rbind(cls1,cls2)
dat <- merge(ids,tab,by="aid",sort=FALSE)

# create text with signif variables (model equation)
eqn <- sprintf("%s",names(vrs)[1])
i=2
while( i<=ncol(vrs) ) {
  var <- names(vrs)[i]
  eqn <- sprintf("%s + %s",eqn,var)
  i <- i+1
}
eqn <- sprintf("class ~ %s",eqn)

# multivariate logistic regression model
mdl <- glm(as.formula(eqn), data=dat, family="binomial")

message("\n======FULL INITIAL MODEL=========\n")
message("EQUATION: ",eqn)
summary(mdl)
message("MODEL:")
print(mdl)
message("\nODD RATIOS AND CONFIDENCE INTERVALS:\n")
exp(cbind(OR = coef(mdl),confint(mdl)))
message("\nLINEARIZED COEFFICIENTS (IMPORTANCE):\n")
lm.beta(mdl)

message("\nCROSS VALIDATION:\n")
err <- cv.glm(dat,mdl)$delta
message("prediction error=")
print(err)

message("\nROC CURVE:\n")
prob <- predict(mdl,type="response")
pred <- prediction(prob,dat$class)
perf <- performance(pred,measure="tpr",x.measure="fpr")
auc  <- performance(pred,measure="auc")
auc  <- auc@y.values[[1]]
ar   <- round(auc,digits=4)
roc.data <- data.frame(fpr=unlist(perf@x.values),tpr=unlist(perf@y.values),model="GLM")

file <- "roc_initial.png"
message("plotting ROC --> ",file)
tit <- sprintf("ROC(%s) AUC=%0.4f",eqn,ar)
print(tit)
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  geom_abline(intercept=0,slope=1,colour="blue",linetype="dashed") +
  ggtitle(tit) +
  theme(text=element_text(size=8))
ggsave(file,width=8,height=6,units="cm")

# stepwise regression
message("\n======STEPWISE VARIABLE SELECTION=========\n")

stp <- stepAIC(mdl,direction="both")
#stp <- step(mdl,direction="backward",test="F")

message("\nANOVA:\n")
stp$anova
print(anova(mdl,stp,test="Chisq"))

message("\n======FINAL MODEL:========\n")
message("EQUATION: ",paste((stp$call)$formula,collapse=" "))
summary(stp)
message("MODEL:")
print(stp)
message("\nODD RATIOS AND CONFIDENCE INTERVALS:\n")
exp(cbind(OR = coef(stp),confint(stp)))
message("\nLINEARIZED COEFFICIENTS (IMPORTANCE):\n")
lm.beta(stp)

message("\nCROSS-VALIDATION:\n")
err <- cv.glm(dat,stp)$delta
message("prediction error=")
print(err)


message("\nROC CURVE:\n")
prob <- predict(stp,type="response")
pred <- prediction(prob,dat$class)
perf <- performance(pred,measure="tpr",x.measure="fpr")
auc  <- performance(pred,measure="auc")
auc  <- auc@y.values[[1]]
ar   <- round(auc,digits=4)
roc.data <- data.frame(fpr=unlist(perf@x.values),tpr=unlist(perf@y.values),model="GLM")

file <- "roc_final.png"
message("plotting ROC --> ",file)
tit <- sprintf("ROC(%s) AUC=%0.4f",paste((stp$call)$formula,collapse=" "),ar)
print(tit)
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  geom_abline(intercept=0,slope=1,colour="blue",linetype="dashed") +
  ggtitle(tit) +
  theme(text=element_text(size=8))
ggsave(file,width=8,height=6,units="cm")

# variable independece analysis
# correlation matrix
message("\nVARIABLE CORRELATIONS:\n")
dvar       <- dat
dvar$aid   <- NULL
dvar$class <- NULL
dvar$location <- NULL
dvar$X <- NULL
cmat       <- cor(dvar)

# label variables that are correlated
thd <- 0.8
message("threshold=",thd)
n   <- ncol(cmat)
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
      if( abs(cmat[[i,j]])>thd ) {
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
      txt <- sprintf("%s %s",txt,colnames(cmat)[j])
    }
  }
  if( fst==0 ) {
    print(txt)
    k <- k+1
  }
}


message("\n=====SUBSET REGRESSION=========\n")
leaps <- regsubsets(as.formula(eqn),dat,nbest=5)
summary(leaps)
message("bic=")
summary(leaps)$bic
message("min=")
min(summary(leaps)$bic)

