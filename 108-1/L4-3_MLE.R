rm(list = ls())

##Let's Play A Game :)
#In order to know the concept of MLE well
##n = 100
guess_MLE_par = function(n=integer("n toss")){
  prob = round(runif(1),digits = 2)
  out = sample(c(1, 0), size = n, replace = T, prob = c(prob, 1-prob))
  print(out)
  message(sprintf("%s heads out of %s coin tossed", sum(out), n))
  
  #user input
  p = readline("enter your guess for the prob: ")
  
  #keep playing :)
  while(p != prob){
    if(p > prob){
      print('The prob. you guessed is too high')
    }else if(p < prob){
      print('The prob. you guessed is too low')
    }else{
      break
    }
    p = readline("enter your guess for the prob: ")
  }
  print('Good guess!')
}
guess_MLE_par(100)
guess_MLE_par(1000)
guess_MLE_par(10000)

#MLE for mu & sigma^2 of normal dist.
#simply use the `dnorm()` function to write the log-likelihood fcn
mll = function(mu, sigma){
  logLikelihood = sum(log(dnorm(Y, mean = mu, sd = sigma)))
  return(-logLikelihood)
}

#then we generate a series of pseudo data
n = 1000
set.seed(1234)
Y = rnorm(n, 10, 2)
hist(Y)
View(Y)

mll(10, 2) #the "maximum" => actually, find the negative minimum
mll(11, 2)
mll(9, 2)
mll(10, 0)
mll(10, 1)#let computer find the mu & sigma2 to find the minimum of 'mll'

library(stats4)
MaxLikeEst = mle(mll, start = list(mu = 10, sigma = 2)) ##
MaxLikeEst = mle(mll, start = list(mu = 0, sigma = 4))
summary(MaxLikeEst) ##

nlm(mll, 0,1, print.level=2, hessian=TRUE) #we can find mu by non-linear minimization
mean(Y) #Recall mean(Y) is the MLE estimate for mu


#(Optional)
#writing likelihood function with bare hands...
#but in matrix form...
#beta = c(mu_1, mu_2)
n = 1000
beta = c(10,2) #intercept is 1 & slope is 2
set.seed(1234)
x1 = runif(n, 0, 25) #x1 can come from any dist.
X = cbind(rep(1,n), x1)
Y = X %*% beta + rnorm(n, mean = 0, sd = 2) #where espilon comes from normal
hist(Y)
plot(x1, Y)

mll = function(beta){
  logLikelihood = -n/2*log(2*pi) -n/2*log(beta[2]^2) - 1/(2*beta[2]^2)*t((Y-X%*%beta))%*%(Y-X%*%beta)
  return(-logLikelihood)
}
#View(t((Y-X%*%beta)))
#View(Y-X%*%beta)
(t((Y-X%*%beta))) %*% (Y-X%*%beta) #is a scalar

est_opt = optim(c(0, 1), mll)
est_opt[1]
est_nlm = nlm(mll, c(10,3), print.level=2, hessian=TRUE) #we can find mu
est_nlm[2]


#compare the result of `lm()`
reg1 = lm(Y~X-1)
summary(reg1)
coef(reg1)
vcov(reg1)



#when beta is a (k by 1) vector
n = 1000
beta = c(1,2,3)
set.seed(1234)
x1 = runif(n, 0, 100) #x1 can come from any dist.
x2 = rbinom(n, 10, 0.22)
X = cbind(rep(1,n), x1, x2)
class(beta)
beta = as.matrix(beta)
dim(X)
dim(beta)
Y = X %*% beta + rnorm(n, mean = 0, sd = 1) #where espilon comes from normal
hist(Y)
library(scatterplot3d)
scatterplot3d(x1, x2, Y, angle = 67.5)
scatterplot3d(x1, x2, Y, angle = 157.5)


mll = function(beta){
  logLikelihood = -n/2*log(2*pi) -n/2*log(beta[2]^2) - 1/(2*beta[2]^2)*t((Y-X%*%beta))%*%(Y-X%*%beta)
  return(-logLikelihood)
}
#View(t((Y-X%*%beta)))
#View(Y-X%*%beta)
(t((Y-X%*%beta))) %*% (Y-X%*%beta) #is a scalar

est_opt = optim(c(1,2,3), mll)
est_opt[1]
est_nlm = nlm(mll, c(1,1,1), print.level=2, hessian=TRUE) #we can find mu
est_nlm[2]

#what happen to the `optim()` and `nlm()`? => too non-linear
#assign randomly guess
set.seed(12345)
est_opt = optim(rnorm(3), mll)
est_opt[1]
set.seed(1234567)
est_nlm = nlm(mll, rnorm(3), print.level=2, hessian=TRUE) #we can find mu
est_nlm[2]
#Let's cheat, use `lm()` to look at the answer
lm(Y~X-1)

