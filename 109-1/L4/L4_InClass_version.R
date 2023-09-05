library(tidyverse)
### R Lesson 4: MLE, Interval Estimate and Hypothesis Test
rm(list = ls())

##### MLE #####
# MLE Concept Review
# Let's Play A Game :) In order to know the concept of MLE well
# n = 100
guess_MLE_par = function(n=integer("n toss")){
  prob = round(runif(1),digits = 2)
  out = sample(c(1, 0), size = n, replace = T, prob = c(prob, 1-prob))
  print(out)
  message(sprintf("%s heads out of %s coins tossed", sum(out), n))
  
  #user input
  p = readline("enter your guess for the prob: ")
  
  #keep playing :)
  while(p != prob){
    if(p == "end"){
      print('End Game')
      break
    }
    if(p > prob){
      print('The prob. you guessed is too high')
    }else if(p < prob){
      print('The prob. you guessed is too low')
    }else{
      break
    }
    p = readline("enter your guess for the prob: ")
  }
  print('Good guess! You are right!')
}
guess_MLE_par(100)

### One Parameter
rm(list = ls())
# X~Bernoulli(mu)
# i.e. X~Binomial(m=1, mu)
n = 100
mu = 0.87
set.seed(1234)
X <- rbinom(n, size = 1, prob = mu)
hist(X)
# Write down the likelihood function with bare hand
mll <- function(mu){
  logLikelihood = sum(X)*log(mu) + (n-sum(X))*log(1-mu)
  return(-logLikelihood) #since `nlm` function is 'non-linear minimization'
}
mll(0.1)
mll(0.5)
mll(0.6)
mll(0.87)
mll(0.9)

# Plot
plot(seq(from = 0, to = 1, by = 0.01), -mll(seq(from = 0, to = 1, by = 0.01)),
     type = "l",
     xlab = "true parameter mu",
     ylab = "log-likelihood")

# Numerical Minimization
nlm(mll, 0.1, print.level = 2, hessian = T)


# Analytic Estimate is \bar{X}_n
mean(X)

# Re-Write with default density function

mll <- function(mu){
  logLikelihood = sum(log(dbinom(X, size = 1, prob = mu)))
  return(-logLikelihood)
}
nlm(mll, 0.1, print.level = 2, hessian = T)

##### Practice 1 #####
# Pr(X=x) = f(x) = p(1-p)^x, x=0,1,2,3,...
# given a set of random samples from the above pdf, find the ML estimate for E(X)
getwd()
wd_path <- "/Users/Andy 1/Google 雲端硬碟 (r08323004@g.ntu.edu.tw)/0 Semesters/109-1/1 三345 統計TA/3 實習課教材/3 R/L4/data"
setwd(wd_path)
X <- read.csv("practice1.csv")
X <- X$x
mean(X)

# Directly write down the likelihood function

mll <- function(p){
  logLikelihood = length(X)*log(p)+sum(X)*log(1-p)
  return(-logLikelihood)
}

nlm(mll, 0.1)
1/(pi+1)
# What will you use to estimate p?
# It turns out...
1/(mean(X)+1)

##### Practice 1 Ends #####

### Two Paramters
rm(list = ls())

#MLE for mu & sigma^2 of a normal dist.
#simply use the `dnorm()` function to write the log-likelihood fcn

# Method 1
# mll = function(mu, sigma){
#   logLikelihood = sum(log(dnorm(Y, mean = mu, sd = sigma)))
#   return(-logLikelihood)
# }

# Method 2
mll = function(mu, sigma){
  logLikelihood <-0
  for(y in Y){
    logLikelihood <- logLikelihood + log(dnorm(y, mean = mu, sd = sigma))
  }
  return(-logLikelihood)
}

#then we generate a series of pseudo data
n = 1000
set.seed(1234)
Y = rnorm(n, 0.9487, 9.487)
hist(Y)
View(Y)
mean(Y)
sd(Y)

# Plot
require(lattice)
axis_1 <- seq(from = -50, to = 50, length = 100)
axis_2 <- seq(from = 0.1, to = 100, length = 100)
axis_3 <- outer(axis_1, axis_2, mll)
axis_3[axis_3==Inf] <- 1000000
persp(axis_1, axis_2, axis_3,
      main="Likelihood Function of Normally Distributed Random Samples",
      zlab = "negative log-likelihood",
      xlab = "mu", ylab = "sigma",
      theta = 30, phi = 15,
      col = "grey", shade = 0.5)

library(plotly)
fig <- plot_ly(z = ~axis_3)
fig <- fig %>% add_surface()
fig

min(axis_3)
mll(c(0.9487), c(9.487)) #the "maximum" => actually, find the negative minimum
mll(0, 1)
mll(9, 2)

# Numerical Minimization
library(stats4)
MaxLikeEst = mle(mll, start = list(mu = 0, sigma = 4))
summary(MaxLikeEst)

# Heavily depends on starting point
MaxLikeEst = mle(mll, start = list(mu = 10, sigma = 2))
summary(MaxLikeEst)

# nlm
nlm(mll, 0,1, print.level=2, hessian=TRUE) #we can find mu by non-linear minimization
mean(Y) #Recall mean(Y) is the MLE estimate for mu

##### Practice 2 #####
# Given X~F(nu_1, nu_2)
# Find the ML estimate for nu_1 and nu_2
set.seed(20201223)
X <- rf(10000, df1 = 2020, df2 = 1223)






##### Practice 2 Ends #####

##### Interval Estimator #####
rm(list = ls())
# back to example 1
# X~Bernoulli(mu)
# i.e. X~Binomial(m=1, mu)
n = 100
mu = 0.87
#set.seed(1234)
X <- rbinom(n, size = 1, prob = mu)
# z_{0.975} and z_{0.025}
qnorm(0.025); qnorm(0.975)
mean(X)-qnorm(0.975)*sd(X)/sqrt(n); mean(X)+qnorm(0.975)*sd(X)/sqrt(n)

confidence_interval = function(n=100, alpha=0.05){
  mu = 0.87
  X <- rbinom(n, size = 1, prob = mu)
  # qnorm(0.025); qnorm(0.975)
  l = mean(X)-qnorm(1-alpha/2)*sd(X)/sqrt(n)
  u = mean(X)+qnorm(1-alpha/2)*sd(X)/sqrt(n)
  return(c(l, u))
}
library(tidyverse)
res <- replicate(n = 100, expr = confidence_interval(), simplify = T)
res <- as.data.frame(t(res))
res <- res %>% 
  select(L = V1,
              U = V2) %>% 
  mutate(lie_in_interval = ifelse(L<=0.87 & U>=0.87, 1, 0))
res %>% View
sum(res$lie_in_interval)/100

# Plot
ggplot(res, aes(x = 1:100, y = 0.87)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = U, ymin = L))

##### Practice 3 #####
# Consider X~N(mu, sigma^2)
# where mu, sigma^2 unknown
# construct the 95% confidence interval for \hat{sigma}
X <- rnorm(100, mean = pi, sd = sqrt(pi))
var(X)
# 95% C.I.





##### Practice 3 Ends #####

##### Hypothesis Test #####
rm(list = ls())

# Continue Example 1
# X~Bernoulli(mu)
# i.e. X~Binomial(m=1, mu)
n = 100
mu = 0.87
X <- rbinom(n, size = 1, prob = mu)
qnorm(0.975)
mean(X)-qnorm(0.975)*sd(X)/sqrt(n); mean(X)+qnorm(0.975)*sd(X)/sqrt(n)

# Aspect one: Just like interval estimate
# Consider whether the parameter is in the interval
grid <- seq(0.6, 1, length = 1000)
plot(grid, dnorm(x = grid, 
                 mean = mean(X), 
                 sd = sd(X)/sqrt(length(X))),
     type = "l",
     xlab = "X",
     ylab = "")
abline(v = 0.87, col = 'red') #the true parameter
abline(v = mean(X), col = 'royalblue') #the true parameter
abline(v = mean(X)-qnorm(0.975)*sd(X)/sqrt(n), col = 'blue')
abline(v = mean(X)+qnorm(0.975)*sd(X)/sqrt(n), col = 'blue')
# repeat the above codes, and see the movement of vertical lines
# note that the red line is not moving at all


# Aspect two: Suppose the null-hypothesis is true
# Under H_0, how unlikely it is to see the random sample?
n = 100
mu = 0.87
X <- rbinom(n, size = 1, prob = mu)
# H0: the population mean is 0.8
# Ha: the population mean is not 0.8
grid <- seq(0.6, 1, length = 1000)
plot(grid, dnorm(x = grid, 
                 mean = 0.8, #under H0 
                 sd = sd(X)/sqrt(length(X))),
     type = "l",
     xlab = "X",
     ylab = "")
abline(v = 0.8, col = 'black') #under the null hypothesis
abline(v = mean(X), col = 'royalblue') #the random sample we saw
#?polygon
polygon(c(mean(X), grid[grid>=mean(X)]),
        c(0, dnorm(grid[grid>=mean(X)], mean=0.8, sd=sd(X)/sqrt(length(X)))),
        col = "royalblue")
abline(v = qnorm(0.975, mean = 0.8, sd = sd(X)/sqrt(length(X))), col = 'red') #given the significance level: alpha = 0.05
# repeat the above codes, and see the movement of vertical lines
# note that the black line is not moving at all
# this time, the red line will move, depends on the distribution.

##### Practice 4 #####
# Calculate the test statistics under null hypothesis
# Given the same example in Practice 3, we have the D.G.P.:
X <- rnorm(10000, pi, sqrt(pi))
# Let's assume mu & sigma^2 are unknown

# H0: sigma^2 = 3
# Ha: sigma^2 != 3






##### Practice 4 Ends #####

##### Permutation Test #####
rm(list = ls())

#Chapter 3 Introduction to Hypothesis Testing: Permutation Tests
##-------------------------------------
##Section 3.3
Beerwings = read.csv("https://sites.google.com/site/chiharahesterberg/data2/Beerwings.csv")

tapply(Beerwings$Hotwings, Beerwings$Gender, mean)
#equivalent to the following approach:
mean(subset(Beerwings$Hotwings, subset = Beerwings$Gender == 'F'))
mean(subset(Beerwings$Hotwings, subset = Beerwings$Gender == 'M'))

observed = 14.5333 - 9.3333 #store observed mean differences

#Get hotwings variable
hotwings = Beerwings$Hotwings

#Alternative way:
hotwings = subset(Beerwings, select = Hotwings, drop = TRUE)
#`drop = TRUE`` to convert hotwings to a vector (without this, hotwings will be a
#30x1 data frame

#set.seed(0)
N = 10^5-1  #set number of times to repeat this process
result = numeric(N) # space to save the random differences
for(i in 1:N){
  index = sample(30, size=15, replace = FALSE) # sample of numbers from 1:30
  result[i] = mean(hotwings[index]) - mean(hotwings[-index])
}

#note that `[-i]` means that we "skip" the index i
#eg.
x = 1:10
x[2]
x[-2]
x[2:5]
x[-(2:5)]

##Plot
hist(result, xlab = "xbarM - xbarF", main = "Permutation distribution for hot wings")
abline(v = observed, col = "blue", lty=5)

#Alternative
plot(density(result))
abline(v = observed, col = "blue", lty=5)

#Compute P-value
(sum(result >= observed)+1)/(N+ 1)  #P-value

#Why we add 1 to both the numerator and the denominator?
#Somtimes the p-value (prob. of extreme events) is very small, our re-samples may not catch them.
#That would happen when the times of re-sampling is small.
set.seed(1234)
N = 100
result = numeric(N)
for(i in 1:N){
  index = sample(30, size=15, replace = FALSE) # sample of numbers from 1:30
  result[i] = mean(hotwings[index]) - mean(hotwings[-index])
}
sum(result >= observed)/(N)  #Actual relative frequency over permutations
(sum(result >= observed)+1)/(N+ 1)  #Adjusted P-value

