# Lesson 1: 
dog
"dog"
TRUE
FALSE
T
F
TRUE&FALSE
TRUE||FALSE

x <- c(5, 32, 56)

mdata <- c(1,2,3,4,5,6)
m <- matrix(data <- mdata, nrow = 2, ncol = 3, byrow = FALSE)
m
colnames(m) <- c("2","3","4")

m <- as.data.frame(m)

dim1 <- c('a1', 'a2', 'a3')
dim2 <- c('b1', 'b2', 'b3')
dim3 <- c('AA', 'BB')
arr <- array(1:18, c(3,3,2), dimnames = list(dim1, dim2, dim3))
arr

a <- 3
is.numeric(a)
class(a)
as.character(a)

#Section4: Calculator
Inf
NA
NaN
2<5
9>=8
101==101
7!=11
#numbers and operators
6/2*(1+2)
pi
2^4 #power
2**4
9%%5 #mod
#Practice: 二的三十次方除以六十三的餘數是多少？
2^30%%63
#Practice2:
integrand <- function(x) {(x^3*cos(x/2)+0.5)*sqrt(4-x^2)}
integrate(integrand, lower = -2, upper = 2)

# *%*: matrix multiplication
A <- matrix(1:6, nrow = 3)
B <- matrix(7:12, nrow = 2)
A*B #raise an error
A%*%B

#Section5: Function
#In calculating
sqrt(100)
log(1)
factorial(3)
exp(1)
abs(-5)
#Practice 1
#X~Bin(n=4,p=0.6)
#試用R計算Pr(X=2)
factorial(4)/(factorial(2)*factorial(2))*0.6^2*0.4^2

#sample(x, size, replace = FALSE, prob = NULL)
#隨機抽樣
?sample()
# 從一到六取後不放回地抽，直到抽完
sample(1:6)
# 藍紅兩色球取後不放回地抽兩顆球
sample(c("Blue", "Red"), 2)
# 藍紅兩色球取後放回地抽兩顆球
sample(c("Blue", "Red"), 2, replace = T)
# 藍紅兩色球取後放回地抽三顆球，並且藍紅球出現的機率分別為(0.6, 0.4)
sample(c("Blue", "Red"), 3, replace = T, prob = c(0.6, 0.4))

#為何不能取後不放回地抽超過兩顆球？
sample(c("Blue", "Red"), 3) #raising an error

# Graphing
# 假設今有一枚公正硬幣，擲10次，並用長條圖畫出出現正反面的次數
outcome <- sample(c("H", "T"), 10, replace = T)
outcome=="H"
nH <- sum(outcome=="H")
nT <- sum(outcome=="T")
barplot(c(nH,nT))
barplot(c(nH,nT), names.arg = c("Head", "Tail"))

# plot()
# f(x)=x^2
X <- -10:10
Y <- X^2
plot(X, Y) #scatter plot
plot(X, Y, type = 'p') #points
plot(X, Y, type = 'l') #line
plot(X, Y, type = 'h') #height
plot(X, Y, type = 's') #steps
plot(X, Y, type = 'b') #both point & line

# hist()
# 擲一枚公正骰子100次，紀錄出現各點數的次數
outcome <- sample(1:6, 100, replace = T)
hist(outcome)

# Self-defined functions
triple <- function(n){
  3*n
}
triple(2)
# return()的重要性
triple <- function(n){
  3*n
  return(n)
}
triple(2)

# 找出Binomial(n, p)的pmf在X=x時的高度
binomial_pmf <- function(n,p,x){
  factorial(n)/(factorial(x)*factorial(n-x))*p^x*(1-p)^(n-x)
}
# 擲三次公正骰子，點數3出現恰好2次的機率為？
binomial_pmf(3,1/6,2)
# Practice: 承上，驗證點數3出現恰好0次的機率加上恰好1到3次的機率，總和為一
binomial_pmf(3,1/6,0)+binomial_pmf(3,1/6,1)+binomial_pmf(3,1/6,2)+binomial_pmf(3,1/6,3)

# self-defined functions can be complicate
# Monty Hall Game
monty_hall_n = function(switch = logical(), n){
  #randomly arrange the door
  doors = 1:n
  names(doors) = sample(rep(c("goat", "car"), c(n-1,1)))
  prize_door = which(names(doors)=='car') #find the index of car
  #Now Guess
  guess = sample(doors, 1)
  if(guess == prize_door){
    revealed_door = sample(doors[doors != prize_door], n-2) #randomly open one of the goat door
  }else{
    revealed_door = doors[!doors %in% c(prize_door, guess)] #open the other goat door
  }
  #Show the return
  if(switch){
    switched_door = doors[!doors %in% c(guess, revealed_door)]#'%in%' is value matching
    return(prize_door == switched_door)
  } else {
    return(prize_door == guess)
  }
}
monty_hall_n(T, 3)
outcome <- replicate(monty_hall_n(T, 4), n = 100)
# The relative frequency that you win the car
mean(outcome)
# The relative frequency that you win the goat
(length(outcome)-sum(outcome))/length(outcome)

# Packages
# Plotting Again
result = replicate(monty_hall_n(T, 4), n = 1000)
barplot(c(sum(result), length(result)-sum(result)),
        main = 'The frequency of winning the car in 
        Monty Hall Problem with 4 doors in 1000 rounds',
        names.arg = c('Succeed', 'Fail'), ylim = c(0,1000))
#install.packages("ggplot2")
library(ggplot2)
ggplot(c(sum(result), length(result)-sum(result)),
       aes(x,y))

df <- data.frame(switch=c("Yes", "No"), freq=c(sum(result), length(result)-sum(result)))
ggplot(data=df, aes(x=switch, y=freq))+geom_bar(stat="identity")

# changing colors
ggplot(data=df, aes(x=switch, y=freq)) +
  geom_bar(stat="identity", fill="steelblue")
