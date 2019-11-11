

library(olsrr)
library(plyr)
library(MASS)
library(GGally)
library(gmodels)
library(nnet)
library(tidyverse)

g <- function(X,y,B){
  Q <- matrix(0, nrow = ncol(B), ncol = nrow(B))
  for (i in 1:nrow(Q)) {
    for (j in 1:ncol(Q)) {
      Q[i,j] <- sum((y==i)*X[,j])
    }
  }
  machana <- (rowSums(exp(X%*%B)))
  A <- matrix(0, nrow = nrow(B), ncol = ncol(B))
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      A[i,j] <- sum((exp((X%*%B)[,j])*X[,i])/machana)
    }
  }
  return((t(Q)-A))
}

K=4
p=5
p1=p+1
row.ix = NULL
col.ix = NULL
for (j in 0:p) {
  for (k in 1:(K-1)) {
    row.ix = c(row.ix,j)
    col.ix = c(col.ix,k)
  }
}

row.ix <- rep(0:5,3)
col.ix <- rep(1:3,each=6)
index <- cbind((1:(6*(4-1))),row.ix,col.ix)

dg <- function(X,y,B) {
  machane <- (rowSums(exp(X%*%B)))^2
  C <- matrix(0, nrow = nrow(B)*ncol(B), ncol = ncol(B)*nrow(B))
  row.ix <- rep(0:5,3)
  col.ix <- rep(1:3,each=6)
  index <- cbind((1:(6*(4-1))),row.ix,col.ix)
  for (i in 1:nrow(C)){
    for (j in 1:ncol(C)){
      r <- index[i, "row.ix"]
      a <- index[i, "col.ix"]
      s <- index[j,"row.ix"]
      t <- index[j, "col.ix"]
      C[i,j] <- sum((X[,r]*exp(B[r,a]*X[,r])*X[,s]*rowSums(exp(X%*%B)) - exp(B[r,a]*X[r,a])*X[,r]*exp(B[s,t]*X[,s])*X[,s])/machane)
    }
  }
  return(-C)
}

newton <- function(g,dg,b_start){
  b_1 <-   b_start - solve(dg(b_start))%*%g(b_start)
  while (abs(b_1-b_start)<0.001) {
    b_start <- b_1
    b_1 <- b_start - solve(dg(b_start))%*%g(b_start)
  }
  return(b_1)
}

#b
occupation <- read.csv("C:/R/r/bochan 2 reg/.occupation.csv")

# farm-1, unskilled-2, skilled-3 proffesional-4
occup <- data.frame(occupation$n)
names(occup) <- "n"
occup$y <- as.numeric(occupation$sonOccup=="farm")
occup$y[occupation$sonOccup=="unskilled"] <- 2
occup$y[occupation$sonOccup=="skilled"] <- 3
occup$y[occupation$sonOccup=="professional"] <- 4
occup$x_farm <- as.numeric(occupation$fatherOccup=="farm")
occup$x_skilled <- as.numeric(occupation$fatherOccup=="skilled")
occup$x_unskilled <- as.numeric(occupation$fatherOccup=="unskilled")
occup$x_black <- as.numeric(occupation$black=="yes")
occup$x_nonintact <- as.numeric(occupation$nonintact=="yes")

occ2 <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("y", "x_farm", "x_skilled", "x_unskilled", "x_black", "x_nonintact")
colnames(occ2) <- x
for(i in 1:nrow(occup)){
  for(j in 1:occup$n[i]){
    occ2 = occ2 %>% add_row()
    occ2[nrow(occ2),] <- occup[i,2:7]
  }
}

y <- occ2$y
X <- matrix(c(rep(1, nrow(occ2)), occ2$x_farm,occ2$x_skilled,occ2$x_unskilled,occ2$x_black,occ2$x_nonintact),nrow = sum(occup$n),ncol = 6)

B <- matrix(0,6,3)

newton(g = g(X = X, y = y, B = B),dg = dg(X = X, y = y, B = B), b_start = B )

multinom(data =occ2 )

################################################################################################

#Question 2
#a
setwd("C:\R\r\bochan 2 reg")
insurance.dat = read.csv("insurance.csv", header=T)
attach(insurance.dat)
#> names(insurance.dat)
#[1] "X" "age" "sex" "bmi" "children" "smoker" "region" "family_size" "expenses"   

insurance.dat$X <- NULL
contin.dat = insurance.dat[,c(1,3,4,7,8)]

insurance.dat$sex <- as.numeric(insurance.dat$sex=="female")
insurance.dat$smoker <- as.numeric(insurance.dat$smoker=="yes")
#northeast-1 northwest-2 southeast-3 southwest-4
reg <- insurance.dat$region
insurance.dat$region <- as.numeric(reg=="northeast")
insurance.dat$region[reg=="northwest"] <- 2
insurance.dat$region[reg=="southeast"] <- 3
insurance.dat$region[reg=="southwest"] <- 4

#1)
desc = function(inpvar, vname) {
  n = sum(!is.na(inpvar))
  sumry = summary(inpvar)
  std =  sd(inpvar, na.rm=T)
  range = max(inpvar) - min(inpvar)
  iqr = IQR(inpvar, na.rm=T)
  prbs = seq(0.05, 0.95, 0.05)
  prbs = c(0.01, prbs, 0.99)
  quant = quantile(inpvar, probs=prbs, na.rm=T)
  ans = list(n=n, sumry=sumry, std=std, range=range, iqr=iqr, quant=quant)
  print(ans)
  hist(inpvar, breaks="Scott", freq=F, main = paste("Histogram of" , vname), xlab=vname)
  stem(inpvar,scale=2)
  return(ans)
} 

desc(age,'age')
desc(bmi,'bmi')
desc(children,'children')
desc(family_size,'family_size')
desc(expenses,'expenses')

#2-4)
ggscatmat(insurance.dat)

#5)
CrossTable(insurance.dat$sex)
CrossTable(insurance.dat$smoker)
CrossTable(insurance.dat$region)

#6)
boxplot(insurance.dat$age ~ insurance.dat$sex, xlab='sex', ylab='age')
boxplot(insurance.dat$bmi ~ insurance.dat$sex, xlab='sex', ylab='bmi')
boxplot(insurance.dat$children ~ insurance.dat$sex, xlab='sex', ylab='children')
boxplot(insurance.dat$family_size ~ insurance.dat$sex, xlab='sex', ylab='family_size')
boxplot(insurance.dat$expenses ~ insurance.dat$sex, xlab='sex', ylab='expenses')

boxplot(insurance.dat$age ~ insurance.dat$smoker, xlab='smoker', ylab='age')
boxplot(insurance.dat$bmi ~ insurance.dat$smoker, xlab='smoker', ylab='bmi')
boxplot(insurance.dat$children ~ insurance.dat$smoker, xlab='smoker', ylab='children')
boxplot(insurance.dat$family_size ~ insurance.dat$smoker, xlab='smoker', ylab='family_size')
boxplot(insurance.dat$expenses ~ insurance.dat$smoker, xlab='smoker', ylab='expenses',main = "smoker V.S expenses" )

boxplot(insurance.dat$age ~ insurance.dat$region, xlab='region', ylab='age')
boxplot(insurance.dat$bmi ~ insurance.dat$region, xlab='region', ylab='bmi', main = "region V.S bmi")
boxplot(insurance.dat$children ~ insurance.dat$region, xlab='region', ylab='children')
boxplot(insurance.dat$family_size ~ insurance.dat$region, xlab='region', ylab='family_size')
boxplot(insurance.dat$expenses ~ insurance.dat$region, xlab='region', ylab='expenses')

#7)
CrossTable(insurance.dat$sex,insurance.dat$smoker)
CrossTable(insurance.dat$sex,insurance.dat$region)
CrossTable(insurance.dat$region,insurance.dat$smoker)

#b
model <- lm(expenses ~ age + smoker, data = insurance.dat)

#c
full_model <- lm(expenses ~ age + sex + bmi + children + smoker + region + family_size, data = insurance.dat)
r_sqrt <- summary(model)$r.squared
adj_r_aqrt <- summary(model)$adj.r.squared
mallows_cp <- ols_mallows_cp(model, full_model)
aic <- AIC(model)
bic <- BIC(model)

#d
#backwards
min_model <- lm(expenses ~ smoker, data = insurance.dat)
reg.bw = stepAIC(full_model, scope=list(lower=formula(min_model), 
                                      upper=formula(full_model)), direction='backward', k=2)
summary(reg.bw)

#forward
reg.fw = stepAIC(min_model, scope=list(lower=formula(min_model), 
                                     upper=formula(full_model)), direction='forward', k=2)
summary(reg.fw)

model_d <- lm(expenses ~ smoker + age + bmi + family_size + region + children, data=insurance.dat)
r_sqrt <- summary(model_d)$r.squared
adj_r_aqrt <- summary(model_d)$adj.r.squared
mallows_cp <- ols_mallows_cp(model_d, full_model)
aic <- AIC(model_d)
bic <- BIC(model_d)
#e

#f
#expenses ~ smoker + age + bmi + family_size + region + children
model_d <- lm(expenses ~ smoker + age + bmi + family_size + region + children, data=insurance.dat)
e <- model_d$residuals
s <- sqrt(sum((e)^2)/(nrow(insurance.dat)-7))
x<- matrix(c(rep(1,nrow(insurance.dat)),insurance.dat$smoker,insurance.dat$age, insurance.dat$bmi,
             insurance.dat$family_size,insurance.dat$region,insurance.dat$children),nrow = nrow(insurance.dat),ncol = 7)
p <- x%*%solve(t(x)%*%x)%*%t(x)
I <- diag(nrow(insurance.dat))
Q <- (I - p)
r <- e/(s*sqrt(diag(Q)))
plot(insurance.dat$expenses,r, xlab = "expenses", main = "expenses V.S r")
abline(h = c(-2,2), col = "red")
mean(e>2)

#g
qqnorm(r)
qqline(r)
hist(r)
#age
plot(insurance.dat$age, r, xlab = "age", main = "age V.S r")
abline(h = 0, col = "red")
#bmi
plot(insurance.dat$bmi, r, xlab = "bmi", main = "bmi V.S r")
abline(lm(r~insurance.dat$bmi),col="red")
abline(h = 0, col = "red")
#family
plot(insurance.dat$family_size, r, xlab = "family size", main = "family_size V.S r")
abline(h = 0, col = "red")
for(i in 1:8){
  print(i)
  print(sum(x[,5]==i))
}
#region
plot(insurance.dat$region, r, xlab = "region", main = "region V.S r")
abline(h = 0, col = "red")
#children
plot(insurance.dat$children, r, xlab = "children", main = "children V.S r")
abline(h = 0, col = "red")
for(i in 1:5){
  print(i)
  print(sum(x[,7]==i))
}
#smoker
plot(insurance.dat$smoker, r, xlab = "smoker", main = "smoker V.S r")
abline(h = 0, col = "red")
boxplot(r~insurance.dat$smoker, xlab= "smoker", ylab= "r", main = "smoker V.S r")

#h
summary(insurance.dat$bmi)
insurance.dat$bmi_dummy <- as.numeric(insurance.dat$bmi > mean(insurance.dat$bmi))
insurance.dat$bmi_smoker <- insurance.dat$bmi_dummy*insurance.dat$smoker
plot(insurance.dat$bmi, insurance.dat$expenses, xlab = "bmi", ylab = "expenses", main = "bmi V.S expenses")
plot(log(insurance.dat$bmi), insurance.dat$expenses, xlab = "log(bmi)", ylab = "expenses", main = "log(bmi) V.S expenses")
plot(sqrt(insurance.dat$bmi), insurance.dat$expenses, xlab = "sqrt(bmi)", ylab = "expenses", main = "sqrt(bmi) V.S expenses")
new_model <- lm(expenses ~ smoker + age + bmi_smoker + family_size + region + children,data=insurance.dat)
r_sqrt <- summary(new_model)$r.squared
adj_r_aqrt <- summary(new_model)$adj.r.squared
mallows_cp <- ols_mallows_cp(new_model, full_model)
aic <- AIC(new_model)
bic <- BIC(new_model)
