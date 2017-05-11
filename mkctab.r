#!/usr/bin/Rscript

options(width=200)  # nr columns in terminal

# read counts
cnts <- read.csv("tableC.csv")

# create contingency table
ctab <- xtabs(number ~ group + status, cnts)
print(ctab)
pvals <- "p-values:"

# perform Fisher test
if( ncol(ctab)==2 && nrow(ctab)==2 ) {
  ftst <- fisher.test(ctab)
  print(ftst)
  pf <- round(ftst$p.value,digits=4)
  if( pf<0.05 ) sf <- "*" else sf <- " "
  message("Fisher p=",pf,sf)
  pvals <- paste0(pvals," Fisher=",pf,sf)
}

# perform Chi^2 test
ctst <- chisq.test(ctab)
print(ctst)
pc <- round(ctst$p.value,digits=4)
if( pc<0.05 ) sc <- "*" else sc <- " "
message("ChiSquare p=",pc,sc)
pvals <- paste0(pvals," Chi^2=",pc,sc)

# plot
file <- sprintf("%s.png","contingency")
message("saving ",file)
png(filename=file,width=3200,height=2400,pointsize=40)
plt <- barplot(t(ctab),beside=TRUE,col=c("red","green"),legend=c("Ruptured","Unruptured"),args.legend=list(title="status",x="topleft"),xlab="location",ylab="count",main="Rupture Status by Location",sub=pvals)
text(plt, 0,t(ctab),cex=1,pos=3)
dev.off()



