#!/usr/bin/Rscript

# read ids, clinical & hemo tables
ids <- read.csv("list.ids")
cli <- read.csv("clinical.csv")
tb0 <- read.csv("table0.csv")
# insert aid & location into dataframe
loc <- data.frame(cli$aid,cli$position)
names(loc)[names(loc)=="cli.aid"]      <- "aid"
names(loc)[names(loc)=="cli.position"] <- "location"
# convert locations to coordinates
# anterior
loc$coord[loc$location=="ICA-CAV"]   <- 0.0
loc$coord[loc$location=="ICA-OPH"]   <- 0.1
loc$coord[loc$location=="ICA-SHYP"]  <- 0.2
loc$coord[loc$location=="PCOM"]      <- 0.3
loc$coord[loc$location=="ICA-ACHOR"] <- 0.4
loc$coord[loc$location=="ICA-BIF"]   <- 0.5
loc$coord[loc$location=="MCA-PROX"]  <- 0.6
loc$coord[loc$location=="ACA-PROX"]  <- 0.7
loc$coord[loc$location=="MCA-BIF"]   <- 0.8
loc$coord[loc$location=="ACOM"]      <- 0.9
loc$coord[loc$location=="MCA-DIST"]  <- 1.0
loc$coord[loc$location=="ACA-DIST"]  <- 1.0
# posterior
loc$coord[loc$location=="VA-VERT"]   <- 0.0
loc$coord[loc$location=="VA-PICA"]   <- 0.1
loc$coord[loc$location=="BA-PROX"]   <- 0.2
loc$coord[loc$location=="BA-TIP"]    <- 0.5
loc$coord[loc$location=="BA-DIST"]   <- 0.6
# add to table & save
tb1 <- merge(tb0,loc,by="aid",sort=FALSE)
write.csv(tb1,"table1.csv")


