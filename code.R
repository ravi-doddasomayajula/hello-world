########dplyr lessons###########

dat <- read.csv("femaleMiceWeights.csv")
library(dplyr)
controls <- filter(dat, Diet == "chow")
controls <- select(controls, Bodyweight)
controls2 <- unlist(controls)
View(controls2)

dat2 <- read.csv("msleep_ggplot2.csv")
class(dat2)
controls2 <- filter(dat2, order == "Primates")
View(controls2)
nrow(controls2)
class(controls2)
controls22 <- select(controls2, sleep_total)
View(controls22)
sum(controls22)
class(controls22)
controls222 <- unlist(controls22)
View(controls222)
summarize(controls22, mean(sleep_total))
filter(dat, order=="Primates") %>% summarise( mean( sleep_total) )

####################### for lesson 2
dat <- read.csv("femaleMiceWeights.csv")
control <- filter(dat, Diet == "chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet == "hf") %>% select(Bodyweight) %>% unlist
population <- read.csv("femaleMiceWeights.csv")
population <- unlist(population)
mean(sample(population, 12))
################# hw on random variables
dat <- read.csv("femaleControlsPopulation.csv")
View(dat)
dat <- unlist(dat)
mean(dat)
set.seed(1)
sample <- sample(dat,5)
abs(mean(sample) - mean(dat))
set.seed(5)
sample5 <- sample(dat,5)
abs(mean(sample5) - mean(dat))
####################################### null hypo
obs <- mean(treatment) - mean(control)
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)
control <- sample(population, 12)
treatment <- sample(population, 12)
mean(treatment) - mean(control)
n <- 10000
nulls <- vector("numeric",n)
for (i in 1:n){
  control <- sample(population, 12)
  treatment <- sample(population, 12)
  nulls[i] <- mean(treatment) - mean(control)
}
max(nulls)
hist(nulls)
mean(nulls > obs)
mean(abs(nulls) > obs)
############ null distro hw
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

xavg <- mean(x)
set.seed(1)
n <-1000
nulls <- vector("numeric",n)
for (i in 1:n){
  rand5 <- sample(x, 5)
  rand5avg[i] <- mean(rand5)
  nulls[i] <- mean(rand5) - mean(x)
}
mean(abs(nulls)>1)
hist(rand5avg)
hist(nulls)

#######same problem, n = 10000

set.seed(1)
n <-10000
nulls <- vector("numeric",n)
for (i in 1:n){
  rand5 <- sample(x, 5)
  rand5avg[i] <- mean(rand5)
  nulls[i] <- mean(rand5) - mean(x)
}
mean(abs(nulls)>1)
###hist(rand5avg)
###hist(nulls)
#######same problem, n = 1000, random size = 50
set.seed(1)
n <-1000
nulls <- vector("numeric",n)
#rand50avg <- vector("numeric",n)
for (i in 1:n){
  rand50[i] <- sample(x, 50)
  #rand50avg[i] <- mean(rand50)
  nulls[i] <- mean(rand50) - mean(x)
}
mean(abs(nulls)>1)
############## next hw

install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
a <- filter(gapminder, year == "1952")
#a <- filter(gapminder, year == "1952")
b <- select(a, lifeExp)
x <- unlist(b)
hist(x)
mean(x<=40) # proportion of life exp <= 40
# proportion of life exp between 40 and 60
q <- mean(x<=60)
w <- abs(q-mean(x<=40))
w
######sapply
prop = function(qq) {
  mean(x<=qq)
}
prop(40)
qs <- seq(from = min(x),to = max(x), length = 20)
qs
props = sapply(qs, prop)
props
plot(qs,props)
####use of inline fn: props = sapply(qs, function(qq) mean(x <= qq))
plot(ecdf(x))
############central limit theorem hw
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
View(x)

set.seed(1)
n <- 1000
#nulls1 <- vector("numeric",n)
rand5avg <- vector("numeric",n)
#rand5 <- vector("numeric",n)
for (i in 1:n) {
  #rand5[i] <- sample(x, 5)
  rand5avg[i] <- mean(sample(x, 5))
#  nulls1[i] <- mean(rand5) - mean(x)
}


set.seed(1)
n <- 1000
#nulls2 <- vector("numeric",n)
rand50avg <- vector("numeric",n)
#rand50 <- vector("numeric",n)
for (i in 1:n) {
  #rand50[i] <- sample(x, 50)
  rand50avg[i] <- mean(sample(x, 50))
  #nulls2[i] <- mean(rand50) - mean(x)
}
hist(rand5avg)
hist(rand50avg)

## question #2
a <- mean(rand50avg <= 23)
a
b <- mean(rand50avg <= 25)
b
abs(a-b)

###question 3
pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43) 
############# central limit theorem assignment
dat <- read.csv("mice_pheno.csv")
dat <- na.omit(dat) #removing all na fileds from dat
head(dat)
a <- filter(dat, Diet == "chow", Sex == "M")
b <- select(a, Bodyweight)
head(b)
x <- unlist(b)
mean(x)
library(rafalib)
popsd(x)
set.seed(1)
X <- sample(x,25)
mean(X)
c <- filter(dat, Diet == "hf", Sex == "M")
d <- select(c, Bodyweight)
head(c)
head(d)
y <- unlist(d)
mean(y)
popsd(y)
set.seed(1)
Y <- sample(y,25)
mean(Y)
abs(mean(x) - mean(y))
#(mean(x) - mean(y))
abs(mean(X) - mean(Y))
abs(mean(x) - mean(y)) - abs(mean(X) - mean(Y))
######## for female mice
e <- filter(dat, Diet == "chow", Sex == "F")
f <- select(e, Bodyweight)
#head(b)
xx <- unlist(f)
mean(xx)
library(rafalib)
popsd(xx)
set.seed(1)
XX <- sample(xx,25)
mean(XX)
g <- filter(dat, Diet == "hf", Sex == "F")
h <- select(g, Bodyweight)
#head(c)
#head(d)
yy <- unlist(h)
mean(yy)
popsd(yy)
set.seed(1)
YY <- sample(yy,25)
mean(YY)
#abs(mean(x) - mean(y))
#(mean(x) - mean(y))
#abs(mean(X) - mean(Y))
abs(mean(xx) - mean(yy)) - abs(mean(XX) - mean(YY))

##### central limit theorem exercise

head(dat)
pnorm(3) - pnorm(-3)
### ex. no. 4
dat <- read.csv("mice_pheno.csv")
dat <- na.omit(dat) #removing all na fileds from dat
View(dat)
head(dat)
a <- filter(dat, Diet == "hf", Sex == "M")
b <- select(a, Bodyweight)
head(b)
x <- unlist(b)
mean(x)
library(rafalib)
popsd(x)
z <- (x-mean(x))/popsd(x) #z-scores
z
mean(abs(z<=1)) #gives the proportion of values within 1 std deviation of mean.
#### proportion of values within 2 std deviations of mean:
##zz <- (x-(mean(x)))/(2*popsd(x))
#zz
mean(abs(z<=2))
mean(abs(z<=3))#for 3 SDs
################8th q
mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
# 9th 
set.seed(1)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
sd(avgs)
#################
#####coursera code stuff here###########
###############

###########jan 27th 2016 edx t-test
#######get from home computer########

########edx question # 6
install.packages("dplyr")
install.packages("rafalib")
library(dplyr)
library(rafalib)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)
popsd(X)
sd(X)
#Use the CLT to approximate the probability that our estimate X¯ is off by more than 2 grams from μX.
2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )
#############
popsd(X)
popsd(Y)
se <- sqrt(((popsd(X)**2) + (popsd(Y)**2))/12)
se2 <- sqrt(((sd(X)**2) + (sd(Y)**2))/12)
se
se2 # correct answer

t.test(X,Y)

