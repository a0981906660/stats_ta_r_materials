rm(list = ls())
#Q1.
#BS CI for ratio of sample means

#### example 5.6
rm(list = ls())
Verizon = read.csv("http://sites.google.com/site/chiharahesterberg/data2/Verizon.csv")
#write.csv(x = Verizon, file = 'Verizon.csv')

Time.ILEC = subset(Verizon, select = Time, Group == "ILEC", drop = T) 
Time.CLEC = subset(Verizon, select = Time, Group == "CLEC", drop = T)
mean(Time.ILEC)/mean(Time.CLEC)
B = 10^4
time.ratio.mean = numeric(B) 
for(i in 1:B){
  ILEC.sample = sample(Time.ILEC, 1664, replace = TRUE)
  CLEC.sample = sample(Time.CLEC, 23, replace = TRUE)
  time.ratio.mean[i] = mean(ILEC.sample)/mean(CLEC.sample)
}
sd(time.ratio.mean)
quantile(time.ratio.mean, c(0.025, 0.975))
L = quantile(time.ratio.mean, 0.025)
U = quantile(time.ratio.mean, 0.975)
hist(time.ratio.mean, main="Bootstrap Distribution of the Ratio of Means")
abline(v=L, col = "blue", lty = 2) 
abline(v=U, col = "blue", lty = 2)

#
SingleTailUpperBound = quantile(time.ratio.mean, 0.95)
hist(time.ratio.mean, main="Bootstrap Distribution of the Ratio of Means")
abline(v=SingleTailUpperBound, col = "blue", lty = 2)
abline(v=1, col = "red", lty = 1)

mean(time.ratio.mean)
sd(time.ratio.mean)
quantile(time.ratio.mean, c(0.025, 0.975))
mean(time.ratio.mean) - mean(Time.ILEC)/mean(Time.CLEC)

##Part B: Permutation Test
repairTime = Verizon$Time
observed = mean(Time.ILEC)/mean(Time.CLEC)

N = 10000-1  #set number of times to repeat this process
result = numeric(N) # space to save the random differences
for(i in 1:N){
  index = sample(1687, size=1664, replace = FALSE) # sample of numbers from 1:1687
  result[i] = mean(repairTime[index])/mean(repairTime[-index])
}

hist(result, xlab = "mean(Time.ILEC)/mean(Time.CLEC)", main = "Permutation distribution for ratio of repair time")
abline(v = observed, col = "blue", lty=5)

#Alternative
plot(density(result), main = "Permutation distribution for ratio of repair time")
abline(v = observed, col = "blue", lty=5)

#Compute P-value
(sum(result <= observed)+1)/(N+ 1)  #P-value

