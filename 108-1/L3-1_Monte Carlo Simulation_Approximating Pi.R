##Monte Carlo Simulation
rm(list = ls())
#simulate the dice-roll game
#the expected value of the number come up after rolling a faire dice is:
sum(1:6*(1/6)) #理論值 -> 但實際值不總是理論值 ->如何模擬出來？

#roll a fair dice n times
sample(1:6, size = 1, replace = T)
sample(1:6, size = 10, replace = T)
mean(sample(1:6, size = 10, replace = T))
mean(sample(1:6, size = 1000, replace = T))
mean(sample(1:6, size = 100000, replace = T)) #getting closer and closer
mean(replicate(100000, sample(1:6, size = 1, replace = T))) #an equivalent expression, but minor difference in performance

#Some values are stationary if we try to repeat the process of getting that value for nearly infinity times.
#We have not yet talked about the theory behind it, let's just take this kind of phenomenon as granted.

#Approximating Pi
#want to know pi...
#Recall the definition of Pi: the ratio of a circle's circumference to its diameter
#We can derive Pi from some crazing things such as Polygon approximation, or some infinite series or fractions...
#These are indeed the true formula with infinite terms for calculating Pi.
#Given all the truths above, we get our formula for areas of circle: Pi*r^2

#Let's mess up this formula with probability...
#Say we have a square and a quarter circle inside it.
#Ref: https://helloacm.com/r-programming-tutorial-how-to-compute-pi-using-monte-carlo-in-r/
# both x and y contains now 100000 random numbers
x=runif(10000) #We just randomly distribute the points inside the square on X-Y plane
y=runif(10000)
z=sqrt(x^2+y^2)
# The following returns the indices vector when z is less or equal than 1
which(z<=1)
length(which(z<=1))*4/length(z) #
plot(x[which(z<=1)],y[which(z<=1)],xlab="X",ylab="Y",main="Monte Carlo Simulation for Approximating Pi", col = 'blue')
points(x[which(z>1)],y[which(z>1)],col='gray')

#Same thing for the whole circle
x = runif(10000, -1, 1)
y = runif(10000, -1, 1)
z=sqrt(x^2+y^2)
abs(z) <= 1
length(which(abs(z) <= 1))*4/length(z)
plot(x[which(abs(z)<=1)], y[which(abs(z)<=1)], main = 'Monte Carlo Simulation for Approximating Pi', type = 'p', col = 'blue')
points(x[which(abs(z)>1)], y[which(abs(z)>1)], col = 'gray')

#In Class Practices (Are Left in Optional Lecture Videos)
#Back to rolling a dice
#define a function that give us the number after rolling a n sided dice once
dice = function(sides = numeric(), times = numeric()){
  sample(1:sides, size = times, replace = T)
}

#令X為一隨機變數，表示擲一枚公正骰子十次且點數為5的次數
#How does X distribute? What is E(X)?
#In L1, we know that X~Bin(n = 10, p = 1/6), so we can response the above question theoretically
x = 0:10 #可能沒有出現半個5，也可能10次都是5
y = dbinom(x, size = 10, prob = 1/6)
plot(x, y, main = 'p.m.f. of Bin(10,1/6)', ylim = c(0,.4)) #只有一次是5的機率最高
plot(x, pbinom(x, size = 10, prob = 1/6), type = 's', main = 'CDF of Bin(10,1/6)', ylim = c(0,1))
#E(X) = n*p = 10/6 = 1.6667
#也就是平均而言，每擲十次骰子，只會有1.6667次出現5

#If we don't know the theoretical mean...Use simulation!
#那我就擲好幾組十次骰子，看看每組出現5的次數是多少，有幾組就有幾個次數
dice_num = dice(6,10)
dice_num == 5
sum(dice_num == 5) #得到一個「次數」
#重複以上三行很多次，以得到進似的「期望次數」

mat = replicate(n = 1000, dice(6,10)) #create a matrix
count = c()
for(i in 1:1000){
  count = rbind(count, sum(mat[,i] == 5))
}
count
mean(count) #給了我平均而言，擲10次骰子會有這樣多次出現5

#為了不把事情複雜化...
#令X為一隨機變數，表示擲一枚公正骰子一次的點數
#How does X distribute? What is E(X)?
#Does X~Bin anymore? No
#If we don't know the theoretical mean...Use simulation!
#那我就擲好幾次骰子，看看每次出現的點數是多少
dice_num = dice(6,1)
dice_num #重複很多次就可以告訴我E(X)：平均而言，擲1次骰子會出現`dice_num`點

#重複十萬次擲一次骰子的試驗，看看這十萬個點數有幾個1點、幾個2點，...到幾個6點
set.seed(1234)
dice_num = replicate(n = 100000, expr = dice(6,1))
#用古典機率的定義：機率為樣本空間的大小分之事件的大小
count = c()
for(i in 1:6){
  count = c(count, sum(dice_num == i))
}
View(count)
barplot(count, names.arg = c(1,2,3,4,5,6), ylab = 'Frequency', xlab = 'Outcome of dice roll')
#E(X) = ∑x*P(X = x)
p_dice = count/100000
expected_dice = 1*p_dice[1]+2*p_dice[2] +3*p_dice[3]+4*p_dice[4]+5*p_dice[5]+6*p_dice[6]

mean(dice_num) #Tells me E(X) = 3.5
#When the mechanism behind the random process is discrete uniform, then I can use `mean()`
expected_dice-mean(dice_num)==0 #still has minor difference

#Try the following...
#Let X be a r.v. represents the sum of the number of rolling a fair dice 10 times
#What is E(X)?
mean(replicate(n = 1000, sum(dice(6,10))))
#Why it equals to 10 times the above result?
