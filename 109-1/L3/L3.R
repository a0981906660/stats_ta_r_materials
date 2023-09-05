### R Lesson 3: Data Manipulation with Tidyverse & CLT

# WLLN
n = 1000
Yn = rnorm(n)
Xn_bar = c()
for(i in 1:n){
  Xn_bar = c(Xn_bar, sum(Yn[i])/i)
}
plot(1:1000, Xn_bar, type = "l")

n = 10000
Yn = rnorm(n)
Sn_sq = c()
for(i in 1:n){
  Sn_sq = c(Sn_sq, var(Yn[1:i]))
}
plot(1:n, Sn_sq, type = "l")

# Practice: USE rbinom(n, 10, 0.2) instead

n = 10000
Yn = rbinom(n, 10, 0.2)
Sn_sq = c()
for(i in 1:n){
  Sn_sq = c(Sn_sq, var(Yn[1:i]))
}
plot(1:n, Sn_sq, type = "l")
var(Yn)


### Sampling Distribution (Practice)
rm(list = ls())
X = rnorm(100000, 5, 3)
hist(X)
# Standardized
Z = (X-5)/3
hist(Z, ylim = c(0, 0.4), xlim = c(-4,4), freq = F,
     main = '', xlab = '')
par(new = T)
plot(density(Z), ylim = c(0, 0.4), xlim = c(-4,4), col = 'red',
     main = '', xlab = '')
# Squared
Chi = Z^2
density(Chi)
hist(Chi, freq = F,
     xlim = c(0, 20), ylim = c(0, 1),
     xlab = '', main = '')
par(new = T)
plot(density(Chi),
     xlim = c(0, 20), ylim = c(0, 1), col = "red",
     xlab = '', main = '')

# Additive
X_2 = rnorm(100000, 5, 3)
Z_2 = (X_2-5)/3
Chi_2 = Z_2^2
Chi + Chi_2

hist(Chi + Chi_2, freq = F,
     xlim = c(0, 10), ylim = c(0, 0.5),
     xlab = '', main = '')
par(new = T)
plot(density(Chi + Chi_2),
     xlim = c(0, 10), ylim = c(0, 0.5), col = "red",
     xlab = '', main = '')

# t
t = Z/sqrt(rchisq(100000, 100)/100)
hist(t)
hist(rt(100000, 100))
plot(density(t))

# F
hist(t^2)
hist(rf(100000, 1, 100))
plot(density(t^2))
plot(density(rf(100000, 1, 100)))

# t & normal
X = seq(-10, 10, 0.01)

plot(X, dnorm(X), type = 'l', ylim = c(0, 0.4))
par(new = T)
plot(X, dt(X, 1), type = "l", col = 'blue', ylim = c(0, 0.4))
par(new = T)
plot(X, dt(X, 10), type = "l", col = 'red', ylim = c(0, 0.4))
graphics.off()


### CLT
rm(list = ls())

#generate pseudo data from known dist.
#X~Bin(n = 5, p = 0.1)
#E(X) = 0.5
x1 = rbinom(10000, size = 5, prob = 0.1)
mean(x1)
#this is what only one x_bar behaves. If I want to see the dist. of the r.v. x_bar
#I need a bunch of realizations of x_bar

x_bar_bin = function(n){ #input: the number of realization of x_bar
  x_bar = c()
  for(i in 1:n){
    x1 = rbinom(n, size = 5, prob = 0.1)
    x_bar = rbind(x_bar, mean(x1))
  }
  return(x_bar)
}

x_bar_bin(10) #10 realization
hist(x_bar_bin(10)) #see the distribution
mean(x_bar_bin(10)); var(x_bar_bin(10))
hist(x_bar_bin(100))
mean(x_bar_bin(100)); var(x_bar_bin(100))
hist(x_bar_bin(1000))
mean(x_bar_bin(1000)); var(x_bar_bin(1000))
plot(density(x_bar_bin(1000)))

set.seed(12345)
hist(x_bar_bin(10), freq = F); curve(dnorm(x, mean = mean(x_bar_bin(10)), sd = sd(x_bar_bin(10))), add = T, col = 'blue')
hist(x_bar_bin(100), freq = F); curve(dnorm(x, mean = mean(x_bar_bin(100)), sd = sd(x_bar_bin(100))), add = T, col = 'blue', ylim = c(0, 10))
hist(x_bar_bin(1000), freq = F); curve(dnorm(x, mean = mean(x_bar_bin(1000)), sd = sd(x_bar_bin(1000))), add = T, col = 'blue')


#See what the normalized r.v. behaves
#Z = sqrt(n)*(X_bar - mu)/sigma^(1/2)
z_bin = function(n){
  z = c()
  for(i in 1:n){
    x1 = rbinom(n, size = 5, prob = 0.1) #Bin(5, 0.1)
    #sqrt(n)*(mean(x1) - 5*0.1)/(5*0.1*0.9)^(1/2) #normalization
    z = rbind(z, sqrt(n)*(mean(x1) - 5*0.1)/(5*0.1*0.9)^0.5)
  }
  return(z)
}

z_bin(10) #10 normalized realization
hist(z_bin(10))
hist(z_bin(100))
z = z_bin(1000)
hist(z, freq = F)
mean(z); var(z) #the parameters are like the parameters of N(0, 1)
curve(dnorm(x, mean=mean(z),sd=sd(z)),add=T, col="red")




## Introduction to tidyverse
#install.packages("tidyverse")
library(tidyverse)
# What is "pipeline"?
#e.g.
x <- rnorm(10000, 0, 1)
y <- pnorm(x, 0, 1)
hist(y)

#with pipeline %>%
rnorm(10000, 0, 1) %>% 
  pnorm(0, 1) %>% 
  hist()

# Actually, in R, you can define any "operator" within "%"
# So, "%>%" is one of the self defined operators
#e.g.
"a" + "b"

`+` <- function(e1, e2) {
  if (is.character(e1) | is.character(e2)) {
    paste0(e1, e2)
  } else {
    base::`+`(e1, e2)
  }
}
"a" + "b"

`%L1norm%` <- function(e1, e2){
  if((is.numeric(e1) & is.numeric(e2)) & (length(e1)==2 & length(e2)==2)){
    abs(e1-e2)
  }
}
a = c(1,2)
b = c(3,4)
a%L1norm%b

# Pipeline help us organize the codes well and make them readable

# functions in tidyverse
# mutate, filter, select 


## Efficient Frontier - without pipeline
rm(list = ls())
path = '/Users/Andy 1/Google 雲端硬碟 (r08323004@g.ntu.edu.tw)/0 Semesters/109-1/1 三345 統計TA/3 實習課教材/3 R/L3/data/'
setwd(path)
# 5 year monthly price
data1 = read.csv('AMZN.csv')
data2 = read.csv('JPM.csv')
stock_price = cbind(data1$Close, data2$Close)
colnames(stock_price) = c('AMZN', 'JPM')
stock = data.frame(stock_price)

#setting variables
#r1 = diff(log(stock$AMZN), 1)
r1 = c(NA, diff(stock$AMZN, 1))/stock$AMZN
r1 = na.omit(r1)
hist(r1)
r2 = c(NA, diff(stock$JPM, 1))/stock$JPM
r2 = na.omit(r2)
hist(r2)



## Efficient Frontier - with pipeline
rm(list = ls())
getwd()
# 5 year monthly price
df <- read_csv('AMZN.csv') %>%
  select(AMZN = Close)
df <- read.csv('JPM.csv') %>% 
  select(JPM = Close) %>% 
  bind_cols(df)

#setting variables
df <- df %>% 
  mutate(rAMZN = c(NA, diff(AMZN, 1))/AMZN,
         rJPM = c(NA, diff(JPM, 1))/JPM) %>%
  filter(!is.na(rAMZN) & !is.na(rJPM)) %>%
  select(rAMZN, rJPM)

r1 = df$rAMZN
r2 = df$rJPM

hist(r1)
hist(r2)
# gaining parameters
# Annualized Return
# Actual: (1+mean(r1))^12-1
mean(r1)*12
mean(r2)*12
sd(r1)*12
sd(r2)*12
cov(r1, r2)

#setting function(let P consist of MSFT & JPM)
# P = omega * r1 +  (1-omega) * r2
# E(P) = omega * E(r1) +  (1-omega) * E(r2)
returnP = function(omega_r1){
  retP = omega_r1*mean(r1) + (1-omega_r1)*mean(r2)
  return(retP)
}

#X, Y
#Z = aX + (1-a)Y
#Var(Z) = a^2 Var(X) + (1-a)^2 Var(Y) + 2a(1-a) Cov(X, Y)
sigmaP = function(omega_r1){
  varP = omega_r1^2*var(r1) + (1-omega_r1)^2*var(r2)
  + 2*omega_r1*(1-omega_r1)*cov(r1, r2)
  return(sqrt(varP))
}

#plotting sigma-return scatter plot
omega = seq(0, 1, 0.01)
plot(omega, returnP(omega), type = 'l')
plot(omega, sigmaP(omega), type='l')

plot(sigmaP(omega), returnP(omega), type = 'l', 
     main="Efficient Frontier of P consisting of AMZN & JPM",
     ylab="Expected Return (E(r))", xlab = "risk (sigma)")

#find omega that minimizes the risk of rate of return
min(sigmaP(omega))
omega[which.min(sigmaP(omega))]
#3D Plot
library(scatterplot3d)
scatterplot3d(omega,returnP(omega),sigmaP(omega))

library(plotly)
plot_ly(x=sigmaP(omega), y=returnP(omega), z=omega, type="scatter3d", mode="markers", color=returnP(omega))
plot_ly(x=omega, y=returnP(omega), z=sigmaP(omega), type="scatter3d", mode="markers", color=returnP(omega))


