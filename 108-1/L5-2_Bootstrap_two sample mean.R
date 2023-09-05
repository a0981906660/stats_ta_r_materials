rm(list = ls())
#Two sample Bootstrap
#eg5.4 in Chihara
#Story:我們想問：Do men take more physical risks in the presence of an attractive woman?
#用18-35歲的男性Skateboarders分別在男性以及女性面前表演後的testosterone睪酮水準來看
Skateboard = read.csv("http://sites.google.com/site/chiharahesterberg/data2/Skateboard.csv")
#被randomly assigned在女性面前表演的受試者
testF = subset(Skateboard, select = Testosterone, subset = Experimenter == "Female", 
               drop = TRUE) #不加`drop = T`則是回傳dataframe type，加了回傳vector
testF
#被randomly assigned在男性面前表演的受試者
testM = subset(Skateboard, select = Testosterone, subset = Experimenter == "Male",
                drop = TRUE)
#我們在乎的是兩組受試者的testosterone level是否有差異
observed = mean(testF) - mean(testM)

#怎麼在兩組sample的情況下做Bootstrap呢？按各組佔sample size的比例來resample
#skateboard資料有71 obs，是49+22
nf = length(testF)
nm = length(testM)

N = 10^4 #B = 10000

TestMean = numeric(N) #create an empty vector to store the BS statistic

for(i in 1:N){
  sampleF = sample(testF, nf, replace = TRUE)
  sampleM = sample(testM, nm, replace = TRUE)
  TestMean[i] = mean(sampleF) - mean(sampleM) #BS statistic
}

hist(TestMean, main = "Bootstrap distribution of difference in means",
     xlab="Means")
abline(v = observed , col = "blue", lty = 2)
plot(density(TestMean)) #Dist. of BS statistic is centered at our observed sampled mean
abline(v = observed , col = "blue", lty = 2)

#QQ Plot shows how the BS dist. is like Normal
qqnorm(TestMean)
qqline(TestMean)

#It seems like we do not need to assume normality on population (because we have BS)
#But we can still find the approximate sampling dist. of our interested statistic
#Furthermore, it turns out that the dist. of the statistic is almost normally distributed
mean(testF) - mean(testM) #Our observed difference in sample means
mean(TestMean) #BS diff in sample means
sd(TestMean)#we may use this s.e. and construct 95% CI under normality

####NOTE THAT: 這是早期的做法：利用BS找出不易計算的s.e.，然後再套用大樣本情況下的sampling dist.
#以BS取代抽樣分配的s.e.，實際上還是assume Normality for population或者假設母體服從某種分配
#這種情況下的BS只是簡化計算解析解的流程而已
#Check Textbook p.445
#[Xbar - z0.025*se(Xbar), Xbar + z0.025*se(Xbar)]
observed - 1.96*sd(TestMean)
observed + 1.96*sd(TestMean)

#在BS dist跟母體很像時上述做法跟以下不會差太遠
quantile(TestMean,c(0.025,0.975))
#We are 95% confident that testosterone levels of men who skateboard in front of a female experimenter are,
#on average, between 24.1 & 139.12 ng/dl higher than that of the men who skateboard in front of a male experimenter.

#但要注意BS dist.可以是skewed的
#這時候可能就沒有像Normal的symmetry這麼棒的事情

mean(TestMean)- observed  #bias
#跟點估計的定義法相同

#Can we conduct a Hypothesis test for this conclusion?
#Sure! H0: There is no difference in the two sample means; Ha: There is difference
#If we don't know the sampling dist., we can still do the permutation test in order to conduct an approximation for p-value

#############Caution!!##############
#############Permutation Test (Actually Required)##############

testAll = Skateboard$Testosterone
testAll = subset(Skateboard, select = Testosterone, drop = TRUE) #equivalent to the above

N = 10^4 - 1  #set number of times to repeat this process
#Why `-1` here? Because we want to adjust p-value in case that the mumerator at the end of the permutation sumulation is 0

#Single Tail Permutation Test
set.seed(99)
result = numeric(N) # space to save the random differences
for(i in 1:N){
  index = sample(71, size = nf, replace = FALSE) #sample of numbers from 1:71, WITHOUT REPLACEMENT
  result[i] = mean(testAll[index]) - mean(testAll[-index])
}

(sum(result >= observed)+1)/(N + 1)  #P-value

#Plot
hist(result, xlab = "xbar1 - xbar2",
     main="Permutation distribution for testosterone levels")
abline(v = observed, col = "blue")

#the area of the RHS is p-value. Under H0 is true, the prob. of the more extreme event
plot(density(result))
abline(v = observed, col = "blue")
