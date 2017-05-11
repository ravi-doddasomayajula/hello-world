setwd("/odin1/rdoddaso/aneuDB/locations/")
clin <- read.csv("clinical2.csv", stringsAsFactors = FALSE, sep = "")
# View(clin)
# location <- factor(clin$artery, exclude = "xxx")
# location
# locs <- levels(location)
# locs2 <- list(locs)
# 
# for (i in locs){
#   assign(paste(i,'dat',sep = "."), subset.data.frame(clin,clin$artery == paste0(i) &clin$morpho !="FUS"&clin$morpho =="BIL"&clin$morpho !="xxx"))
#   #assign(paste(i,'gender',sep = "."),table(paste(i,'dat',sep=".")[["sex"]], paste(i,'dat',sep=".")[["rupture"]], exclude = "xxx"))
# }
# 
# View(ACA.dat)

# ACA
ACA.dat <- subset(clin, clin$artery == "ACA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(ACA.dat)
ACA.male.U <- subset(ACA.dat, ACA.dat$sex == "M" & ACA.dat$rupture == "U")
tab.ACA.gender <- table(ACA.dat$sex, ACA.dat$rupture, exclude = "xxx")
tab.ACA.position <- table(ACA.dat$morpho, ACA.dat$rupture, exclude = "xxx")
#tab1 <- table(ACA.dat$morpho, ACA.dat$rupture, exclude = "xxx")
#addmargins(tab1)
#prop.table(tab1, 1)
#prop.table(tab1, 2)
ACA.male.R <- subset(ACA.dat, ACA.dat$sex == "M" & ACA.dat$rupture == "R")
ACA.female.U <- subset(ACA.dat, ACA.dat$sex == "F" & ACA.dat$rupture == "U")
ACA.female.R <- subset(ACA.dat, ACA.dat$sex == "F" & ACA.dat$rupture == "R")
ACA.lat.U <- subset(ACA.dat, ACA.dat$morpho == "LAT" & ACA.dat$rupture == "U")
ACA.lat.R <- subset(ACA.dat, ACA.dat$morpho == "LAT" & ACA.dat$rupture == "R")
ACA.bif.U <- subset(ACA.dat, ACA.dat$morpho == "BIF" & ACA.dat$rupture == "U")
ACA.bif.R <- subset(ACA.dat, ACA.dat$morpho == "BIF" & ACA.dat$rupture == "R")
ftable(tab.ACA.gender)
ftable(tab.ACA.position)
write.table(tab.ACA.gender, "gender.csv")
write.table(tab.ACA.position, "position.csv")

# ACOM
ACOM.dat <- subset(clin, clin$artery == "ACOM" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(ACOM.dat)
ACOM.male.U <- subset(ACOM.dat, ACOM.dat$sex == "M" & ACOM.dat$rupture == "U")
tab.ACOM.gender <- table(ACOM.dat$sex, ACOM.dat$rupture, exclude = "xxx")
tab.ACOM.position <- table(ACOM.dat$morpho, ACOM.dat$rupture, exclude = "xxx")
ACOM.male.R <- subset(ACOM.dat, ACOM.dat$sex == "M" & ACOM.dat$rupture == "R")
ACOM.female.U <- subset(ACOM.dat, ACOM.dat$sex == "F" & ACOM.dat$rupture == "U")
ACOM.female.R <- subset(ACOM.dat, ACOM.dat$sex == "F" & ACOM.dat$rupture == "R")
ACOM.lat.U <- subset(ACOM.dat, ACOM.dat$morpho == "LAT" & ACOM.dat$rupture == "U")
ACOM.lat.R <- subset(ACOM.dat, ACOM.dat$morpho == "LAT" & ACOM.dat$rupture == "R")
ACOM.bif.U <- subset(ACOM.dat, ACOM.dat$morpho == "BIF" & ACOM.dat$rupture == "U")
ACOM.bif.R <- subset(ACOM.dat, ACOM.dat$morpho == "BIF" & ACOM.dat$rupture == "R")
#View(ACOM.male.U)
ftable(tab.ACOM.position)
ftable(tab.ACOM.gender)
write.table(tab.ACOM.gender, "gender.csv", append = T)
write.table(tab.ACOM.position, "position.csv", append = T)

# AICA - discard not enough cases
AICA.dat <- subset(clin, clin$artery == "AICA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(AICA.dat)
AICA.male.U <- subset(AICA.dat, AICA.dat$sex == "M" & AICA.dat$rupture == "U")
tab.AICA.gender <- table(AICA.dat$sex, AICA.dat$rupture, exclude = "xxx")
tab.AICA.position <- table(AICA.dat$morpho, AICA.dat$rupture, exclude = "xxx")
AICA.male.R <- subset(AICA.dat, AICA.dat$sex == "M" & AICA.dat$rupture == "R")
AICA.female.U <- subset(AICA.dat, AICA.dat$sex == "F" & AICA.dat$rupture == "U")
AICA.female.R <- subset(AICA.dat, AICA.dat$sex == "F" & AICA.dat$rupture == "R")
AICA.lat.U <- subset(AICA.dat, AICA.dat$morpho == "LAT" & AICA.dat$rupture == "U")
AICA.lat.R <- subset(AICA.dat, AICA.dat$morpho == "LAT" & AICA.dat$rupture == "R")
AICA.bif.U <- subset(AICA.dat, AICA.dat$morpho == "BIF" & AICA.dat$rupture == "U")
AICA.bif.R <- subset(AICA.dat, AICA.dat$morpho == "BIF" & AICA.dat$rupture == "R")
#View(AICA.male.U)
ftable(tab.AICA.position)
ftable(tab.AICA.gender)
#write.table(tab.AICA.gender, "gender.csv", append = T)
#write.table(tab.AICA.position, "position.csv", append = T)

# ATEMP  discard not enough cases
ATEMP.dat <- subset(clin, clin$artery == "ATEMP" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(ATEMP.dat)
ATEMP.male.U <- subset(ATEMP.dat, ATEMP.dat$sex == "M" & ATEMP.dat$rupture == "U")
tab.ATEMP.gender <- table(ATEMP.dat$sex, ATEMP.dat$rupture, exclude = "xxx")
tab.ATEMP.position <- table(ATEMP.dat$morpho, ATEMP.dat$rupture, exclude = "xxx")
ATEMP.male.R <- subset(ATEMP.dat, ATEMP.dat$sex == "M" & ATEMP.dat$rupture == "R")
ATEMP.female.U <- subset(ATEMP.dat, ATEMP.dat$sex == "F" & ATEMP.dat$rupture == "U")
ATEMP.female.R <- subset(ATEMP.dat, ATEMP.dat$sex == "F" & ATEMP.dat$rupture == "R")
ATEMP.lat.U <- subset(ATEMP.dat, ATEMP.dat$morpho == "LAT" & ATEMP.dat$rupture == "U")
ATEMP.lat.R <- subset(ATEMP.dat, ATEMP.dat$morpho == "LAT" & ATEMP.dat$rupture == "R")
ATEMP.bif.U <- subset(ATEMP.dat, ATEMP.dat$morpho == "BIF" & ATEMP.dat$rupture == "U")
ATEMP.bif.R <- subset(ATEMP.dat, ATEMP.dat$morpho == "BIF" & ATEMP.dat$rupture == "R")
#View(ATEMP.male.U)
ftable(tab.ATEMP.position)
ftable(tab.ATEMP.gender)
#write.table(tab.ATEMP.gender, "gender.csv", append = T)
#write.table(tab.ATEMP.position, "position.csv", append = T)

# BA  
BA.dat <- subset(clin, clin$artery == "BA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(BA.dat)
BA.male.U <- subset(BA.dat, BA.dat$sex == "M" & BA.dat$rupture == "U")
tab.BA.gender <- table(BA.dat$sex, BA.dat$rupture, exclude = "xxx")
tab.BA.position <- table(BA.dat$morpho, BA.dat$rupture, exclude = "xxx")
BA.male.R <- subset(BA.dat, BA.dat$sex == "M" & BA.dat$rupture == "R")
BA.female.U <- subset(BA.dat, BA.dat$sex == "F" & BA.dat$rupture == "U")
BA.female.R <- subset(BA.dat, BA.dat$sex == "F" & BA.dat$rupture == "R")
BA.lat.U <- subset(BA.dat, BA.dat$morpho == "LAT" & BA.dat$rupture == "U")
BA.lat.R <- subset(BA.dat, BA.dat$morpho == "LAT" & BA.dat$rupture == "R")
BA.bif.U <- subset(BA.dat, BA.dat$morpho == "BIF" & BA.dat$rupture == "U")
BA.bif.R <- subset(BA.dat, BA.dat$morpho == "BIF" & BA.dat$rupture == "R")
#View(BA.male.U)
ftable(tab.BA.position)
ftable(tab.BA.gender)
write.table(tab.BA.gender, "gender.csv", append = T)
write.table(tab.BA.position, "position.csv", append = T)

# ICA  
ICA.dat <- subset(clin, clin$artery == "ICA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(ICA.dat)
ICA.male.U <- subset(ICA.dat, ICA.dat$sex == "M" & ICA.dat$rupture == "U")
tab.ICA.gender <- table(ICA.dat$sex, ICA.dat$rupture, exclude = "xxx")
tab.ICA.position <- table(ICA.dat$morpho, ICA.dat$rupture, exclude = "xxx")
ICA.male.R <- subset(ICA.dat, ICA.dat$sex == "M" & ICA.dat$rupture == "R")
ICA.female.U <- subset(ICA.dat, ICA.dat$sex == "F" & ICA.dat$rupture == "U")
ICA.female.R <- subset(ICA.dat, ICA.dat$sex == "F" & ICA.dat$rupture == "R")
ICA.lat.U <- subset(ICA.dat, ICA.dat$morpho == "LAT" & ICA.dat$rupture == "U")
ICA.lat.R <- subset(ICA.dat, ICA.dat$morpho == "LAT" & ICA.dat$rupture == "R")
ICA.bif.U <- subset(ICA.dat, ICA.dat$morpho == "BIF" & ICA.dat$rupture == "U")
ICA.bif.R <- subset(ICA.dat, ICA.dat$morpho == "BIF" & ICA.dat$rupture == "R")
#View(ICA.male.U)
ftable(tab.ICA.position)
ftable(tab.ICA.gender)
write.table(tab.ICA.gender, "gender.csv", append = T)
write.table(tab.ICA.position, "position.csv", append = T)

# MCA  
MCA.dat <- subset(clin, clin$artery == "MCA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(MCA.dat)
MCA.male.U <- subset(MCA.dat, MCA.dat$sex == "M" & MCA.dat$rupture == "U")
tab.MCA.gender <- table(MCA.dat$sex, MCA.dat$rupture, exclude = "xxx")
tab.MCA.position <- table(MCA.dat$morpho, MCA.dat$rupture, exclude = "xxx")
MCA.male.R <- subset(MCA.dat, MCA.dat$sex == "M" & MCA.dat$rupture == "R")
MCA.female.U <- subset(MCA.dat, MCA.dat$sex == "F" & MCA.dat$rupture == "U")
MCA.female.R <- subset(MCA.dat, MCA.dat$sex == "F" & MCA.dat$rupture == "R")
MCA.lat.U <- subset(MCA.dat, MCA.dat$morpho == "LAT" & MCA.dat$rupture == "U")
MCA.lat.R <- subset(MCA.dat, MCA.dat$morpho == "LAT" & MCA.dat$rupture == "R")
MCA.bif.U <- subset(MCA.dat, MCA.dat$morpho == "BIF" & MCA.dat$rupture == "U")
MCA.bif.R <- subset(MCA.dat, MCA.dat$morpho == "BIF" & MCA.dat$rupture == "R")
#View(MCA.male.U)
ftable(tab.MCA.position)
ftable(tab.MCA.gender)
write.table(tab.MCA.gender, "gender.csv", append = T)
write.table(tab.MCA.position, "position.csv", append = T)

# PCA  
PCA.dat <- subset(clin, clin$artery == "PCA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(PCA.dat)
PCA.male.U <- subset(PCA.dat, PCA.dat$sex == "M" & PCA.dat$rupture == "U")
tab.PCA.gender <- table(PCA.dat$sex, PCA.dat$rupture, exclude = "xxx")
tab.PCA.position <- table(PCA.dat$morpho, PCA.dat$rupture, exclude = "xxx")
PCA.male.R <- subset(PCA.dat, PCA.dat$sex == "M" & PCA.dat$rupture == "R")
PCA.female.U <- subset(PCA.dat, PCA.dat$sex == "F" & PCA.dat$rupture == "U")
PCA.female.R <- subset(PCA.dat, PCA.dat$sex == "F" & PCA.dat$rupture == "R")
PCA.lat.U <- subset(PCA.dat, PCA.dat$morpho == "LAT" & PCA.dat$rupture == "U")
PCA.lat.R <- subset(PCA.dat, PCA.dat$morpho == "LAT" & PCA.dat$rupture == "R")
PCA.bif.U <- subset(PCA.dat, PCA.dat$morpho == "BIF" & PCA.dat$rupture == "U")
PCA.bif.R <- subset(PCA.dat, PCA.dat$morpho == "BIF" & PCA.dat$rupture == "R")
#View(PCA.male.U)
ftable(tab.PCA.position)
ftable(tab.PCA.gender)
#write.table(tab.PCA.gender, "gender.csv", append = T)
#write.table(tab.PCA.position, "position.csv", append = T)

# PCOM  
PCOM.dat <- subset(clin, clin$artery == "PCOM" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(PCOM.dat)
PCOM.male.U <- subset(PCOM.dat, PCOM.dat$sex == "M" & PCOM.dat$rupture == "U")
tab.PCOM.gender <- table(PCOM.dat$sex, PCOM.dat$rupture, exclude = "xxx")
tab.PCOM.position <- table(PCOM.dat$morpho, PCOM.dat$rupture, exclude = "xxx")
PCOM.male.R <- subset(PCOM.dat, PCOM.dat$sex == "M" & PCOM.dat$rupture == "R")
PCOM.female.U <- subset(PCOM.dat, PCOM.dat$sex == "F" & PCOM.dat$rupture == "U")
PCOM.female.R <- subset(PCOM.dat, PCOM.dat$sex == "F" & PCOM.dat$rupture == "R")
PCOM.lat.U <- subset(PCOM.dat, PCOM.dat$morpho == "LAT" & PCOM.dat$rupture == "U")
PCOM.lat.R <- subset(PCOM.dat, PCOM.dat$morpho == "LAT" & PCOM.dat$rupture == "R")
PCOM.bif.U <- subset(PCOM.dat, PCOM.dat$morpho == "BIF" & PCOM.dat$rupture == "U")
PCOM.bif.R <- subset(PCOM.dat, PCOM.dat$morpho == "BIF" & PCOM.dat$rupture == "R")
#View(PCOM.male.U)
ftable(tab.PCOM.position)
ftable(tab.PCOM.gender)
write.table(tab.PCOM.gender, "gender.csv", append = T)
write.table(tab.PCOM.position, "position.csv", append = T)    

# PICA  
PICA.dat <- subset(clin, clin$artery == "PICA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(PICA.dat)
PICA.male.U <- subset(PICA.dat, PICA.dat$sex == "M" & PICA.dat$rupture == "U")
tab.PICA.gender <- table(PICA.dat$sex, PICA.dat$rupture, exclude = "xxx")
tab.PICA.position <- table(PICA.dat$morpho, PICA.dat$rupture, exclude = "xxx")
PICA.male.R <- subset(PICA.dat, PICA.dat$sex == "M" & PICA.dat$rupture == "R")
PICA.female.U <- subset(PICA.dat, PICA.dat$sex == "F" & PICA.dat$rupture == "U")
PICA.female.R <- subset(PICA.dat, PICA.dat$sex == "F" & PICA.dat$rupture == "R")
PICA.lat.U <- subset(PICA.dat, PICA.dat$morpho == "LAT" & PICA.dat$rupture == "U")
PICA.lat.R <- subset(PICA.dat, PICA.dat$morpho == "LAT" & PICA.dat$rupture == "R")
PICA.bif.U <- subset(PICA.dat, PICA.dat$morpho == "BIF" & PICA.dat$rupture == "U")
PICA.bif.R <- subset(PICA.dat, PICA.dat$morpho == "BIF" & PICA.dat$rupture == "R")
#View(PICA.male.U)
ftable(tab.PICA.position)
ftable(tab.PICA.gender)
#write.table(tab.PICA.gender, "gender.csv", append = T)
#write.table(tab.PICA.position, "position.csv", append = T)

# Pontine  
Pontine.dat <- subset(clin, clin$artery == "Pontine" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(Pontine.dat)
Pontine.male.U <- subset(Pontine.dat, Pontine.dat$sex == "M" & Pontine.dat$rupture == "U")
tab.Pontine.gender <- table(Pontine.dat$sex, Pontine.dat$rupture, exclude = "xxx")
tab.Pontine.position <- table(Pontine.dat$morpho, Pontine.dat$rupture, exclude = "xxx")
Pontine.male.R <- subset(Pontine.dat, Pontine.dat$sex == "M" & Pontine.dat$rupture == "R")
Pontine.female.U <- subset(Pontine.dat, Pontine.dat$sex == "F" & Pontine.dat$rupture == "U")
Pontine.female.R <- subset(Pontine.dat, Pontine.dat$sex == "F" & Pontine.dat$rupture == "R")
Pontine.lat.U <- subset(Pontine.dat, Pontine.dat$morpho == "LAT" & Pontine.dat$rupture == "U")
Pontine.lat.R <- subset(Pontine.dat, Pontine.dat$morpho == "LAT" & Pontine.dat$rupture == "R")
Pontine.bif.U <- subset(Pontine.dat, Pontine.dat$morpho == "BIF" & Pontine.dat$rupture == "U")
Pontine.bif.R <- subset(Pontine.dat, Pontine.dat$morpho == "BIF" & Pontine.dat$rupture == "R")
#View(Pontine.male.U)
ftable(tab.Pontine.position)
ftable(tab.Pontine.gender)
#write.table(tab.Pontine.gender, "gender.csv", append = T)
#write.table(tab.Pontine.position, "position.csv", append = T)

# SCA  
SCA.dat <- subset(clin, clin$artery == "SCA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(SCA.dat)
SCA.male.U <- subset(SCA.dat, SCA.dat$sex == "M" & SCA.dat$rupture == "U")
tab.SCA.gender <- table(SCA.dat$sex, SCA.dat$rupture, exclude = "xxx")
tab.SCA.position <- table(SCA.dat$morpho, SCA.dat$rupture, exclude = "xxx")
SCA.male.R <- subset(SCA.dat, SCA.dat$sex == "M" & SCA.dat$rupture == "R")
SCA.female.U <- subset(SCA.dat, SCA.dat$sex == "F" & SCA.dat$rupture == "U")
SCA.female.R <- subset(SCA.dat, SCA.dat$sex == "F" & SCA.dat$rupture == "R")
SCA.lat.U <- subset(SCA.dat, SCA.dat$morpho == "LAT" & SCA.dat$rupture == "U")
SCA.lat.R <- subset(SCA.dat, SCA.dat$morpho == "LAT" & SCA.dat$rupture == "R")
SCA.bif.U <- subset(SCA.dat, SCA.dat$morpho == "BIF" & SCA.dat$rupture == "U")
SCA.bif.R <- subset(SCA.dat, SCA.dat$morpho == "BIF" & SCA.dat$rupture == "R")
#View(SCA.male.U)
ftable(tab.SCA.position)
ftable(tab.SCA.gender)
#write.table(tab.SCA.gender, "gender.csv", append = T)
#write.table(tab.SCA.position, "position.csv", append = T)

# VA  
VA.dat <- subset(clin, clin$artery == "VA" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(VA.dat)
VA.male.U <- subset(VA.dat, VA.dat$sex == "M" & VA.dat$rupture == "U")
tab.VA.gender <- table(VA.dat$sex, VA.dat$rupture, exclude = "xxx")
tab.VA.position <- table(VA.dat$morpho, VA.dat$rupture, exclude = "xxx")
VA.male.R <- subset(VA.dat, VA.dat$sex == "M" & VA.dat$rupture == "R")
VA.female.U <- subset(VA.dat, VA.dat$sex == "F" & VA.dat$rupture == "U")
VA.female.R <- subset(VA.dat, VA.dat$sex == "F" & VA.dat$rupture == "R")
VA.lat.U <- subset(VA.dat, VA.dat$morpho == "LAT" & VA.dat$rupture == "U")
VA.lat.R <- subset(VA.dat, VA.dat$morpho == "LAT" & VA.dat$rupture == "R")
VA.bif.U <- subset(VA.dat, VA.dat$morpho == "BIF" & VA.dat$rupture == "U")
VA.bif.R <- subset(VA.dat, VA.dat$morpho == "BIF" & VA.dat$rupture == "R")
#View(VA.male.U)
ftable(tab.VA.position)
ftable(tab.VA.gender)
#write.table(tab.VA.gender, "gender.csv", append = T)
#write.table(tab.VA.position, "position.csv", append = T)

# VV  
VV.dat <- subset(clin, clin$artery == "VV" & clin$morpho !="FUS" & clin$morpho !="xxx")
#View(VV.dat)
VV.male.U <- subset(VV.dat, VV.dat$sex == "M" & VV.dat$rupture == "U")
tab.VV.gender <- table(VV.dat$sex, VV.dat$rupture, exclude = "xxx")
tab.VV.position <- table(VV.dat$morpho, VV.dat$rupture, exclude = "xxx")
VV.male.R <- subset(VV.dat, VV.dat$sex == "M" & VV.dat$rupture == "R")
VV.female.U <- subset(VV.dat, VV.dat$sex == "F" & VV.dat$rupture == "U")
VV.female.R <- subset(VV.dat, VV.dat$sex == "F" & VV.dat$rupture == "R")
VV.lat.U <- subset(VV.dat, VV.dat$morpho == "LAT" & VV.dat$rupture == "U")
VV.lat.R <- subset(VV.dat, VV.dat$morpho == "LAT" & VV.dat$rupture == "R")
VV.bif.U <- subset(VV.dat, VV.dat$morpho == "BIF" & VV.dat$rupture == "U")
VV.bif.R <- subset(VV.dat, VV.dat$morpho == "BIF" & VV.dat$rupture == "R")
#View(VV.male.U)
ftable(tab.VV.position)
ftable(tab.VV.gender)
#write.table(tab.VV.gender, "gender.csv", append = T)
#write.table(tab.VV.position, "position.csv", append = T)

#ALL
ALL.dat <- subset(clin, clin$morpho !="FUS" & clin$morpho !="xxx")
tab.ALL.position <- table(ALL.dat$morpho, ALL.dat$rupture, exclude = "xxx")
tab.ALL.gender <- table(ALL.dat$sex, ALL.dat$rupture, exclude = "xxx")
ftable(tab.ALL.gender)
ftable(tab.ALL.position)
write.table(tab.ALL.gender, "gender.csv", append = T)
write.table(tab.ALL.position, "position.csv", append = T)
