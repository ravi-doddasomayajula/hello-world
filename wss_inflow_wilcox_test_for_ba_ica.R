#for batip vs icabif

tabl <- read.csv("inflow_and_wss_near.csv")
all.inflows <- tabl[[2]]
all.wssnear <- tabl[[3]]
batip.inflows <- all.inflows[1:62]
icabif.inflows <- all.inflows[63:119]
batip.wssnear <- all.wssnear[1:62]
icabif.wssnear <- all.wssnear[63:119]
wilcox.test(batip.inflows, icabif.inflows, paired=FALSE, "two.sided")
wilcox.test(batip.wssnear, icabif.wssnear, paired=FALSE, "two.sided")


