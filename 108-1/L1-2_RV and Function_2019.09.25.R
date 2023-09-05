##R.V. - Review 
#Binomial
#X: r.v. of the total number of 'H' for tossing a fair coin 10 times
#X~Bin(n, p)            #f(x) = (nCx)(p^x)(1-p)^(n-x)
#X~Bin(n = 10, p = 0.5) #f(x) = (10Cx)(0.5^x)(1-0.5)^(10-x)
#eg. f(x = 6) #擲10次硬幣有6次正面朝上的機率
choose(10, 6) #C10取6
factorial(10)/(factorial(6)*factorial(4)) #10!/(5!5!)
choose(10,6)*(0.5^6)*(1-0.5)^(10-6) #value of f(x=6)
dbinom(x = 6, size = 10, prob = 0.5) #equivalent to f(x=6), give the density(or pmf)
pbinom(6, 10, 0.5) #give the CDF of X F_X(x<=6)
#算看看F(X=6) = Σ P(X=0)+P(X=1)+P(X=2)+P(X=3)+P(X=4)+P(X=5)+P(X=6)
dbinom(0,10,0.5)+dbinom(1,10,0.5)+dbinom(2,10,0.5)+dbinom(3,10,0.5)+dbinom(4,10,0.5)+dbinom(5,10,0.5)+dbinom(6,10,0.5)
#or use for loop
  cdf = 0
  for(i in 0:6){
    cdf = cdf+dbinom(i, 10, 0.5)
  }
  cdf
qbinom(0.828125, size = 10, prob = 0.5) #inverse CDF, given prob. then output quantile

#Remember the def of quantile, if CDF is not strictly increase, 
#given p, find the min x s.t. F(X<=x)>=p
qbinom(0.65, 10, 0.5) #all 3 give the same output
qbinom(0.7, 10, 0.5)
qbinom(0.8, 10, 0.5)
pbinom(6, 10, 0.5) #擲10次硬幣，正面朝上的次數小於等於6的機率
pbinom(5, 10, 0.5) #擲10次硬幣，正面朝上的次數小於等於5的機率
pbinom(6, 10, 0.5)-pbinom(5, 10, 0.5) #相減就是正面朝上恰好為6次的機率
dbinom(6, 10, 0.5)
#plot the pdf of Binomial
x = seq(from = 0, to = 10, by = 1)
y = dbinom(x, size = 10, prob = 0.5)
y
plot(x, y)
#plot the cdf of Binomial
y = pbinom(x, 10, 0.5)
plot(x, y, type ='s')

#What if I change the Prob?
y = dbinom(x, 10, prob = 0.3)
plot(x, y) #pdf of Bin(10, 0.3)
y = pbinom(x, 10, 0.3)
plot(x, y, type = 's') #cdf of Bin(10, 0.3)
y = dbinom(x, 10, prob = 0.7)
plot(x, y) #左偏
y = pbinom(x, 10, 0.7)
plot(x, y, type = 's') #比較慢才跑到1

#Uniform Dist.
dunif(x = 0.3, min = 0, max = 1) #give pdf of U(0,1) #任何在上下界內的x density都一樣
dunif(x = 0.03, min = 0, max = 1) #note that, this is NOT Prob.
dunif(x = 0.003, min = 0, max = 1)
dunif(2, 0, 1) #不在上下界的x就會是0
dunif(x = 1, min = 0, max = 100) #height=1/(upper-lower)
#plot the pdf of U(0, 1)
x = seq(0, 1, by = 0.01) #10+1 pts
y = dunif(x, min = 0, max = 1) #y = x*1/(upper-lower)
plot(x, y, type = 'p') #plot with points
plot(x, y, type = 'l') #plot with lines
#expand the range of X, not only supp(X)
x = seq(-1, 2, by = 0.1) #30+1 pts
y = dunif(x, min = 0, max = 1)
plot(x, y, type = 'l') #zoom in and see sth strange...
plot(x, y, type = 's') #plot with jumping stairs
#plot the cdf of U(0,1)
y = punif(x, 0, 1)
plot(x, y, type = 'l') #斜率是1/(upper-lower)

##R.V. - generating random numbers
set.seed(9487) #設定亂數產生器的起始值，使具有重現性
runif(n = 10, min = 0, max = 1) #give 10 random numbers from U(0, 1)
rnorm(10, mean = 0, sd = 1) #give 10 random numbers from N(0, 1)

set.seed(1234) 
rbinom(n = 1, size = 10, prob = c(0.5, 0.5))
set.seed(1234)
#sample(x = c(1, 6), size = 1, replace = T)
sum(sample(x = c(0, 1), size = 10, replace = T, prob = c(0.5, 0.5)))
set.seed(1234)
sum(replicate(expr = sample(x = c(0, 1), size = 1, replace = T, prob = c(0.5, 0.5)), n = 10))

##functions
#來寫一個幫大家調分的函數
a = 1
rm(a)
addition = function(a, b){
  c = a+b
  return(c)
}
addition(2,4)

stupid_score_modifier = function(input){
  output = input +1
  return(output)
}


stupid_score_modifier = function(score){
  score = score +1
  return(score)
}
stupid_score_modifier(score0)
#來改良一下，只調一分太沒意思
smart_score_modifier = function(score){
  score = sqrt(score)*10
  return(score)
}
smart_score_modifier(1)
#現在有全班30個人的成績：
score0 = sample(10:90, 30, replace = T) #10到100分隨便抽30個成績出來
score0
summary(score0)
score1 = smart_score_modifier(score0)
summary(score1) #可以看到分布改變，但這公平嗎？
max(score0)
#假設我想讓調分是個平移，而且平移到滿分(或特定分數)為止
score_modifier = function(score, highestGrade){
  shift = highestGrade - max(score)
  score = score + shift
  return(score)
}
score_modifier(score0, 100)
#或者我想讓低於中位數或x分的不調，其餘低於60的調到及格
#超過60的調一樣的幅度，滿分還是滿分
score0[]
score0[score0<50]
score_modifier2 = function(score, threshold_X){
  belowX = score[score<threshold_X]
  btwXto60 = score[score<60 & score>=threshold_X]
  above60 = score[score>60]
  print("低於門檻分數"); print(belowX)
  print("可以調到及格分數"); print(btwXto60)
  print("最高調到滿分"); print(above60)
}
score_modifier2(score0, 50)

score_modifier2 = function(score, X){
  shift = 60 - min(score[score<60 & score>=X])
  for(i in 1:length(score)){
    if(score[i]<X){
      score[i] = score[i]
    }
    else if(score[i]>=X & score[i]<60){
      score[i] = 60
    }
    else{
      score[i] = score[i]+shift
    }
  }
  return(score)
}
score_modifier2(score0, 50)
score0
#隨機實驗
set.seed(9487)
sample(1:6, size = 1, replace = F)
sample(1:6, size = 1, replace = F, prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
hist(replicate(n = 1000, sample(1:6, size = 1, replace = F, prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))))

#toss a coin #a binomial appoach to approximate normal
set.seed(9487)
toss = sample(c("H", "T"), size = 10, replace = T, prob = c(0.5, 0.5))
toss
toss[] == 'T'
sum(toss[] == 'T')

tossCoin = function(n){
  toss = sample(c('H', 'T'), size = n, replace = T, prob = c(0.9, 0.1))
  return(sum(toss[] == 'T')/n)
}
hist(replicate(n = 1000, tossCoin(10)))#n = 10, do not converge to norm
hist(replicate(n = 1000, tossCoin(100)))
hist(replicate(n = 1000, tossCoin(1000))) #var decrease

tossUnfairCoin = function(n, p){
  toss = sample(c('H', 'T'), size = n, replace = T, prob = c(p, 1-p))
  return(sum(toss[] == 'T'))
}
hist(replicate(n = 1000, tossUnfairCoin(200, 0.01))) #左偏，尾巴在左邊
hist(replicate(n = 1000, tossUnfairCoin(400, 0.01))) #隨著重複次數增加，越容易收斂到常態

##Shift U(0, 1) to U(a, b)
X = runif(10000, 0, 1)
hist(X)
X
Y = 3*X+2 #trivial
hist(Y) #look at the range of Y
#or
Y = punif(X) #plug in to its own cdf, note: F_X(x) = x, where X~U(0,1)
head(Y-X)
hist(Y) #still being U(0,1)

#How about U(a,b)? What happens after pluging in into its own cdf?
X = runif(1000, 2, 5)
hist(X)
Y = (X-2)/3 #先平移後伸縮
hist(Y) #become U(0,1)
Y = punif(X, min = 2, max = 5) #Or I plug it in its own cdf
hist(Y) #Also become U(0,1)

#Does it hold on other r.v.?
#X~Bin(n = 10, p = 0.3)
X = rbinom(n = 1000, size = 10, prob = 0.3)
hist(X)
Y = pbinom(X, size = 10, prob = 0.3)
hist(Y) #not that clear, but we can see some pattern
                 #the lower&upper bound of Y are (0,1)
#Can we do another direction?
#Generating Bin(n = 10, p = 0.3) from U(0, 1)
Y = runif(100000, 0, 1)
hist(Y)
X = qbinom(Y, size = 10, prob = 0.3)
hist(X)

##Optional
#機率積分轉換Probability Integral Transform
#use U(0,1) & CDF of Standard Normal Dist to generate N(0, 1)
qnorm(0.95) #input Prob., return quantile
pnorm(1.96 ,mean = 0, sd = 1) #cdf of N(0, 1)
dnorm(1) #return pdf

#By PIT, we can make all r.v. from N(0,1) to U(0,1)
X = rnorm(10000, 0, 1)
hist(X)
Y = pnorm(X) #Y = Fx(X)
hist(Y)

#Also, we can have the inverse transform
#and generate N(0,1) form U(0,1)
Y = runif(10000, 0, 1)
hist(Y)
X = qnorm(Y) #Fx^-1(Y) = X
hist(X)

#if we want N(60, 10), just modify the qnorm
X = qnorm(Y, mean = 60, sd = 10)
hist(X)

#Practice 1
#請用亂數模擬擲一顆骰子時所出現的點數
#ans:
sample(x = 1:6, size = 1)
hist(sample(x = 1:6, size = 1000000, replace = T))
#可以用runif嗎？
runif(1000000, min = 1, max = 6)
hist(round(runif(1000000, min = 1, max = 6))) #1&6的freq明顯少很多
hist(round(runif(1000000, min = 0.5, max = 6.5))) 

#Practice2
#請試著以sample()模擬一個擲出不公正硬幣的隨機事件,
#其中擲出Head的機率為0.9, 擲出Tail的機率為0.1,一次擲10枚
#並試著計算此10玫硬幣中共有幾枚為Tail
#hint: use boolean operator and sum() to count the number of Tail
#let toss = samp(...); sum(toss[] == 'T')
#ans:
sample(c('H', 'T'), size = 10, replace = T, prob = c(0.9, 0.1))
toss = sample(c('H', 'T'), size = 10, replace = T, prob = c(0.9, 0.1))
toss[] == 'T'
sum(toss[] == 'T')

#Practice 3
#請用function及亂數模擬擲n= 1,2,3,4,5顆骰子時所出現的點數和
#ans:
dice = function(n){
  a = sample(x = 1:6, size = n, replace = T)
  return(sum(a))
}
set.seed(9487945)
dice(1)
rep(dice(1), 10) #不能用rep函數，why?
replicate(n = 10, dice(1)) #replicate函數才能重新運算
hist(replicate(n = 10, dice(3)))
hist(replicate(n = 100, dice(3)))
hist(replicate(n = 1000, dice(3)))
hist(replicate(n = 10000, dice(3)))
hist(replicate(n = 100000, dice(3)))

#Practice 4
#假設台大學生共n人，其身高服從常態，且mean = 160, sd = 10
#今隨機從台大學生中抽出1人，其平均身高超過175公分的機率為何？
#以1-pnorm()來計算理論機率值
#請試著只以rnorm()及function來計算
#hint:利用邏輯判斷回傳布林值，再計算True的個數所佔的比例
#=>將相對頻率看作為機率
#ans:
1-pnorm(175, mean = 160, sd = 10)
x = rnorm(100, mean = 160, sd = 10)
length(x[x>175])
height = function(n){
  x = rnorm(n, mean = 160, sd = 10)
  freq = length(x[x>175])/length(x)
  return(freq)
}
height(100000)
