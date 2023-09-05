rm(list = ls())
#BS CI for rho
#pseudo data
set.seed(1234)
X = seq(0, 5, length.out = 100)
Y = 1.2*X + rnorm(100)
plot(X, Y); abline(lm(Y~X))
lm(Y~X) #return coef #Not important now

cor(X,Y) #note that correlation represents how one vlb linearly related to the other vlb
##there is no need for beta to be the same as rho!!
cov(X, Y)/sqrt(var(X)*var(Y)) #same approach
cov(X,Y)/var(X) #beta = cov(X,Y)/var(X) i.e. regression coef #Not important now

#Another example clearly shows that when rho=1
X = runif(100, 0, 5) #why use `runif()`, because I want X to be r.v.
Y = 1.2*X #simply rule out the exogenous error term out
plot(X, Y); abline(lm(Y~X))
lm(Y~X) #return coef, which is perfectly estimated

cor(X,Y) #since they are perfectly linearly related with the same direction, their rho is 1
cov(X, Y)/sqrt(var(X)*var(Y)) #same approach
cov(X,Y)/var(X) #beta = cov(X,Y)/var(X) i.e. regression coef #Not important now

#last example for negative rho
set.seed(1234)
X = runif(100, 0, 5)
Y = -1.2*X + rnorm(100)
plot(X, Y); abline(lm(Y~X))
lm(Y~X) #return coef
cov(X,Y)/var(X) #beta = cov(X,Y)/var(X) i.e. regression coef
cor(X,Y)

#Compare to a relative big error term
set.seed(333)
X2 = runif(100, 0, 5)
Y2 = -1.2*X2 + rnorm(100, 0, 10)
plot(X2, Y2); abline(lm(Y2~X2))
lm(Y2~X2) #return coef
cov(X2,Y2)/var(X2) #beta = cov(X,Y)/var(X) i.e. regression coef #Not important now
cor(X2,Y2) #sometimes rho is very close to 0

#higer |rho| with small error term v.s. lower |rho| with bigger error term
cor(X,Y);cor(X2,Y2)

#KEY Question: How to estimate rho? How to construct the interval estimator for rho?
#Am I able to conclude that the linear relation between X2 & Y2 is 0?

#We switch to bivariate normal case, since we can predetermine rho very well
rm(list = ls())
num_samples = 50
rho = 0.87

library('MASS')
rs = mvrnorm(n = num_samples, mu=c(0, 0), Sigma=matrix(c(1, rho, rho, 1), nrow=2), empirical=TRUE)
colnames(rs) = c('X', 'Y')
X = rs[, 1]  # standard normal (mu=0, sd=1)
Y = rs[, 2]  # standard normal (mu=0, sd=1)
plot(X,Y)
cor(X, Y)  # yay! surely is 0.83
cor(X*0.01 + 42, Y*3 - 1)  # Linear transformations of X and Y won't change r.

#Remember that our population for X&Y is a bivariate r.v. which follows normal
#Now what we have is 100 pair of samples (X,Y)

##################################A BAD Version############################
##ONE Bootstrap sample from (X,Y)
Xstar = sample(X, size = num_samples, replace = T) #做一次重抽
which(X == Xstar) #因為是配對的X,Y，要找抽到的X對應的index
#但這種對應法是錯的，因為兩邊是長度一樣的vector，只會對應各自的element
#然後回傳T/F的vector

#我們希望把抽到的X一個個對回去已有的rs
count = c()
for(i in 1:num_samples){
  count = rbind(count, which(X == Xstar[i]))
}
count
Y[count] #這些Y就是被抽到的X所對應的Y
boot_rs = cbind(Xstar, Y[count]) #把抽出來的(X,Y)裝在一起
cor(boot_rs[,1], boot_rs[,2]) #就得到一組bs statistic #centered at rho=0.87

##B times of Bootstrap resampling from (X,Y)
B = 1000;
Xstar = matrix(NA, num_samples, B) #等等拿來裝bs samples from X
Ystar = Xstar #等等拿來裝對應的bs samples from Y
for(i in 1:B){
  Xstar[,i] = sample(X, size = num_samples, replace = T) #每個col是一組resampling from X
  for(j in 1:num_samples){ #row index j代表某一組resample的第j個obs
    Ystar[j,i] = Y[which(X == Xstar[j,i])] #每個element是一個被重抽出來的X，
    #找出他對應的Y，再裝起來。這樣的事要對每個row做，所以row從j=1,2,...,50
  }
}
################The End of the BAD Version#####################

####################A Relative GOOD Version####################
##ONE Bootstrap sample from (X,Y)
bootIndex = sample(num_samples, replace = T)
Xstar = X[bootIndex]
Ystar = Y[bootIndex]
cor(Xstar, Ystar) #就得到一組bs statistic #centered at rho=0.87

##B times of Bootstrap resampling from (X,Y)
B = 1000;
Xstar = matrix(NA, num_samples, B) #等等拿來裝bs samples from X
Ystar = Xstar #等等拿來裝對應的bs samples from Y

bootIndex = replicate(B, sample(num_samples, replace = T))
dim(bootIndex)
Xstar[,2] = X[bootIndex[,2]] #the 2nd BS sample for X
Ystar[,2] = Y[bootIndex[,2]] #the corresponding BS sample for Y

for(i in 1:B){
  Xstar[,i] = X[bootIndex[,i]]
  Ystar[,i] = Y[bootIndex[,i]]
}
####################A Relative GOOD Version####################

#Now we have B sets of resamples. In each set there are 50 pairs of (X*,Y*)
#We can calculate a rho from a set of 50 pairs of (X*,Y*) => We can have B rhos!
cor_star = rep(NA, B)
for(i in 1:B){
  cor_star[i] = cor(Xstar[,i], Ystar[,i])
}
cor_star #B by 1 vector
summary(cor_star)
hist(cor_star) #we have a lot of realizations of a statistic: correlation (rho)
#so we can draw the dist. of this statistic!

#Now we're constructing the Interval estimator for rho
cor_star_sorted = sort(cor_star)
#the 95% Interval Estimate (or CI) for rho is:
cor_star_sorted[25]
cor_star_sorted[975]

#verify by using `quantile()`
quantile(cor_star)
cor_star_sorted[B*0+1]; cor_star_sorted[B*0.25]; cor_star_sorted[B*0.5]; cor_star_sorted[B*0.75]; cor_star_sorted[B]


#Are we able to estimate the rho of previous linear regression model?
rm(list = ls())
set.seed(1)
X2 = runif(100, 0, 5)
Y2 = -1.2*X2 + rnorm(100, 0, 10)
plot(X2, Y2); abline(lm(Y2~X2)) #the scatter plot shows that...there might not be any patterns between X & Y
cor(X2,Y2) #Is this value different from 0?

#use same procedure to construct the BS Interval Estimate
B = 10000;
Xstar = matrix(NA, 100, B)
Ystar = Xstar 
bootIndex = replicate(B, sample(100, replace = T))
dim(bootIndex)

for(i in 1:B){
  Xstar[,i] = X2[bootIndex[,i]]
  Ystar[,i] = Y2[bootIndex[,i]]
}

cor_star = rep(NA, B)
for(i in 1:B){
  cor_star[i] = cor(Xstar[,i], Ystar[,i])
}

cor_star #B by 1 vector
summary(cor_star)
hist(cor_star)

#Now we're constructing the Interval estimate for rho
cor_star_sorted = sort(cor_star)
#the 95% Interval Estimate (or CI) for rho is:
cor_star_sorted[250]
cor_star_sorted[9750]

#With the rejection rule under 5% significant level, we can reject that X is not linearly related to Y
#Even though we generated (X,Y) with linear relation. <= the effect of error term diminish the linear relation between X&Y.
