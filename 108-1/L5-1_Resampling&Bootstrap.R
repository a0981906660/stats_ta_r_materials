rm(list = ls())
#ref: https://www.youtube.com/watch?v=_nhgHjdLE-I
#bootstrap example
set.seed(333)
x = rnorm(30) #treat this sample as population
graphics.off()
bootMean = rep(NA, 10000) #create empty container
sampledMean = rep(NA, 10000) 
for(i in 1:10000){
  bootMean[i] = mean(sample(x, replace = T))
}
for(i in 1:10000){
  sampledMean[i] = mean(rnorm(30))
}
plot(density(sampledMean)) #true sampling dist.
lines(density(bootMean), col = 'red') #boot dist.

sd(bootMean) #approximate s.e.
sd(sampledMean) #true s.e.
#########Think about why: Why sometimes the 2 dist are distant?############

########輸出gif動圖區###########
setwd('/Users/Andy 1/Google 雲端硬碟/108-1/1 三345 統計TA/3 TA課內容/0 備課/L5 Bootstrap')
dir.create("examples")
setwd("examples")

#gif for example 1
bootPlot = function(size, B){
  x = rnorm(size);bootMean = rep(NA, B);sampledMean = rep(NA, B) 
  for(i in 1:B){
    bootMean[i] = mean(sample(x, replace = T))
    sampledMean[i] = mean(rnorm(size))
  }
  return(cbind(bootMean, sampledMean))
}

png(file="example%01d.png")
for(j in 1:100){
  plot(density(bootPlot(30, 1000)[,2]), xlim = c(-1,1), ylim = c(0,2.5),
       main = 'Sampling Distribution under Normality & Bootstrap Distribution',
       xlab = 'quantile')
  lines(density(bootPlot(30, 1000)[,1]), col = 'red')
  abline(v = 0, lty = 2)
  text(0, 0, j, cex = 1)
}
dev.off()

# convert the .png files to one .gif file using ImageMagick.
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
system("convert -delay 30 *.png example_1.gif")

# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))
########動圖區結束###########

####What do we learn from the above simulation?
#for some simple statistic, such as Xbar, we can use bootstrap to describe its sampling distribution very well
#although the bootstrap dist. is centered at RS mean.

#Let's do another simulation that the population is not normal
rm(list = ls())
set.seed(333) #我故意挑了一個不好的center
x = rgamma(30, 5, 1/2) #treat this sample as population
#plot(density(x))
bootMean = rep(NA, 10000) #create empty container
sampledMean = rep(NA, 10000) 
for(i in 1:10000){
  bootMean[i] = mean(sample(x, replace = T))
}
for(i in 1:10000){
  sampledMean[i] = mean(rgamma(30,5,1/2))
}
plot(density(sampledMean), main = "Sampling Dist. & Bootstrap Dist. of X_bar 
     from Gamma(5, 1/2)"
     ,xlim = c(5,15), ylim = c(0,0.6)) #true sampling dist for Xbar.
lines(density(bootMean), col = 'red') #boot dist.
#the graph still looks like normal. BECAUSE the main character is Xbar, wich is the best statistic that applies CLT
#try to change the seed and repeat the above process

##back to first example with packages: population is normal
rm(list = ls())
library(boot)
?boot

set.seed(333)
x = rnorm(30)
sampledMean = rep(NA, 1000)
for(i in 1:1000){
  sampledMean[i] = mean(rnorm(30))
}
meanFunc = function(x, i){
  mean(x[i])
}
bootMean = boot(x, meanFunc, 1000)
bootMean #note that this vlb is a special dataframe
#its subset `t` contains the 1000 bootstrap statistics
#so we call: `$t` in order to acess this vector

plot(density(sampledMean)) 
lines(density(bootMean$t), col = 'red')


#Construct Interval Estimate by hands:
#textbook p.270. If we assume the population is Noram
#then sampling dist. of Xbar is also normal with Var(Xbar) = Var(X)/n
#if we do not know pop. variance: sigma, then we know the standardized Xbar follows t-dist.
x #x is our Random Sample (RS)
mean(x) #Xbar
sd(x)/sqrt(30) #s.e. of 
qt(p = 0.05/2, df = 30-1, lower.tail = F) #the quantile we need
?qt
#note that if df->infty, then the quantile approaches to standard normal
  qt(p = 0.05/2, df = Inf, lower.tail = F);
  qnorm(p = 0.05/2, lower.tail = F); #z0.025

#95% C.I. under Nomrality
L = mean(x)-qt(p = 0.05/2, df = 30-1, lower.tail = F)*sd(x)/sqrt(30)
U = mean(x)+qt(p = 0.05/2, df = 30-1, lower.tail = F)*sd(x)/sqrt(30)
L;U

plot(density(x))
abline(v = L, col = "blue", lty = 2)
abline(v = U, col = "blue", lty = 2)

#95% C.I. under BS dist.
UL_bs = quantile(bootMean$t, c(0.025, 0.975))
UL_bs

#Compare the two interval estimate
U-L
UL_bs[2]-UL_bs[1]
#Think about why Bootstrap Dist. constructs a narrower interval?

#Vidualization
plot(density(x), xlim = c(-3, 3), ylim = c(0,2.5),
     main = 'Dist. of Observed Sample & C.I. Under Normality 
     versus Bootstrap Dist. & C.I.',xlab = 'Quantile')
abline(v = L, lty = 2)
abline(v = U, lty = 2)

par(new = T)
plot(density(bootMean$t), xlim = c(-3, 3), ylim = c(0,2.5), 
     col = 'red', main = 'Dist. of Observed Sample & C.I. Under Normality 
     versus Bootstrap Dist. & C.I.',xlab = 'Quantile')
abline(v = UL_bs[1], col = "red", lty = 2)
abline(v = UL_bs[2], col = "red", lty = 2)

graphics.off()

#####Laura M. Chihara, Tim C. Hesterberg - Mathematical Statistics with Resampling and R#####
##p.125##
rm(list = ls())
Bangladesh = read.csv("http://sites.google.com/site/chiharahesterberg/data2/Bangladesh.csv")
Arsenic = Bangladesh$Arsenic
hist(Arsenic)
qqnorm(Arsenic) #QQ Plot gives us some evidence whether the sample is normally distributed or not
qqline(Arsenic) #If the quantile of the sample data lies on the line, then it is likely that it is normally distributed #the normality assumption may not work

n = length(Arsenic)
B = 10^4
arsenic.mean = numeric(B) 
for(i in 1:B){ 
  x = sample(Arsenic, n, replace = TRUE) 
  arsenic.mean[i] = mean(x) 
}

hist(arsenic.mean, main = "Bootstrap distribution of means") 
abline(v = mean(Arsenic), col = "blue", lty = 2) # vertical line at observed mean

mean(arsenic.mean) # bootstrap mean
mean(arsenic.mean)-mean(Arsenic) # bias
sd(arsenic.mean) # bootstrap SE

#IIIFFFF we treat the bootstrap dist as normal, then in order to get 95% CI, we need z0.025 = 1.96
#Compute the points that are 1.96 standard errors from the mean of the bootstrap distribution:
125.5375-1.96*18.25759 #mark of 1.96 SE from mean
125.5375+1.96*18.25759 #mark of 1.96SE from mean
sum(arsenic.mean > 161.3224)/B
sum(arsenic.mean < 89.75262)/B #the relative freq. of more extreme events
sort(arsenic.mean)[10000*0.025]
sort(arsenic.mean)[10000*0.975]
#the normal dist is not a good approximation
#Since we have the empirical dist.

quantile(arsenic.mean, c(0.025, 0.975)) #conf interval

#Compare the two CI
#Under Normality
125.5375-1.96*18.25759; 125.5375+1.96*18.25759
#Under Bootstrap
quantile(arsenic.mean, c(0.025, 0.975)) #a narrower interval

####################Optional####################
rm(list = ls())
#Bootstrap in non-linear statistics
set.seed(55555)
B = 10^4
x = rnorm(30)
sampledMed = rep(NA, B)
for(i in 1:B){
  sampledMed[i] = median(rnorm(30))
}
medFunc = function(x, i){
  median(x[i])
}
bootMed = boot(x, medFunc, B)
plot(density(sampledMed), lwd = 3)
lines(density(bootMed$t), col = 'red', lwd = 3)
#always be careful. Some BS statistic may not be a good approximation for original statistic

rm(list = ls())
#Things you can't bootstrap (max)
set.seed(333)
x = rnorm(30)
sampledMax = rep(NA, 1000)
for(i in 1:1000){
  sampledMax[i] = max(rnorm(30))
}
maxFunc = function(x, i){
  max(x[i])
}
bootMax = boot(x, maxFunc, 1000)

plot(density(sampledMax), lwd = 3, xlim = c(1,3), ylim = c(0, 10))
lines(density(bootMax$t), col = 'red', lwd = 3)
