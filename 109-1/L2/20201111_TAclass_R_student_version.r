####### Statistics
library(moments)
## Mean
x <- rnorm(100)
mean(x)

x <- rnorm(100000)
mean(x)

## Standard deviation
sd(x)

## Skewness
skewness(x)
mean(((x-mean(x))/sd(x))^3)
## Kurtosis
kurtosis(x)
mean(((x-mean(x))/sd(x))^4)

## Correlation # "線性"相關
x <- 0:100
x <- x/100
y <- 2*x
cor(x,y)
plot(x, y, type = "p")

set.seed(12345)
x <- 0:100
x <- x/100
y <- 2*x + rnorm(length(x), 0, 0.1)
cor(x,y)
plot(x, y, type = "p")

set.seed(12345)
x <- 0:100
x <- x/100
y <- 2*x + rnorm(length(x), 0, 1)
cor(x,y)
plot(x, y, type = "p")

# No LINEAR Correlation
x <- -100:100
x <- x/100
y <- 1-x^2
cor(x,y)
plot(x, y, type = "p")

x <- -100:100
x <- x/100
y <- 1-x^2 + rnorm(length(x), 0, 0.1)
cor(x,y)
plot(x, y, type = "p")

## Quantile
x <- rnorm(1000)
hist(x)
quantile(x,0.5)
quantile(x,c(0.25,0.75))

qnorm(0.25)
qnorm(0.05)
qnorm(0.025)
pnorm(1.96)
dnorm(0)

#練習查表
# z = 1.25
pnorm(1.25)-pnorm(0) #z=1.25 ; Pr(Z<=z) = 0.5+0.3944
qnorm(0.5+0.3944)

# z = 1.96, Pr(Z<=z) = ?
pnorm(1.96)

# What is the value of z s.t. Pr(Z<=z) = 95% ?
qnorm(0.95)

# What is the value of z s.t. Pr(Z<=z) = 3.5% ?
# with R
qnorm(0.035)
# with table
#50-3.5 = 46.5
#check the table, where is 0.4649 ? z=1.81
#z = -1.81

###########################################################
###################   (Practice)   ########################
###########################################################

# What is the value of z s.t. Pr(Z<=z) = 97.5% ?


# z = -1.645, Pr(Z<=z) = ?


# What is the value of z s.t. Pr(Z<=z) = 21.5% ?
# with R

# with table

###########################################################


####### for loop 
## summation
x <- 0
for(i in 1:100){
  x <- x+i
}
x

## Fibonacci
f0 <- 0
f1 <- 1
seq <- c(f0, f1)

for(i in 3:30){
  seq <- c(seq, seq[i-2]+seq[i-1])
}
seq
plot(1:30, seq, type = "l")

####### while loop 
x <- 0
i <- 1
while(i<=100){
  x <- x+i
  i <- i+1
}
x; i

## Geometric distribution
X <- 0
while(runif(1)>0.3){
  X <- X+1
}
X
rgeom(1, 0.3)

geom_generator <- function(p){
  X <- 0
  while(runif(1)>p){
    X <- X+1
  }
  return(X)
}
hist(replicate(10000, geom_generator(0.3)))
hist(rgeom(10000, 0.3))

## Newton's method -> find root
rm(list = ls())
#Requirement:
#1.function is smooth enough
#2.Good initial guess

#eg1. find the root of the following polynomial
#f(x) = x^3+2*x^2-7
f = function(x){
  y = x^3+2*x^2-7
  return(y)
}
f.prime = function(x){
  y = 3*x^2+4*x
  return(y)
}
#have a glance
x = seq(-10, 10, by = 0.1)
##overlapped view
plot(x, f(x), type = 'l', ylim = c(-500, 1000), ylab  = 'function value')
par(new = T)
plot(x, f.prime(x), type = 'l', ylim = c(-500, 1000), col = 'blue', ylab = 'function value')
legend("topright", # 表示在右上角
       pch = 1,    # pch代表點的圖案
       col = c("black", "blue"), # col代表顏色 
       legend = c("f(x)", "f '(x)") # 顏色所對應的名稱
)
lines(x = x, y=x*0, col = "red")
graphics.off()
#With Newton's Method, we know the iteration is:
# x_n+1 = x_{n} \frac{f(x_{n})}{f'(x)_{n}}
#initial / starting point
x = 2
#repeat the following 3 lines and get the root
###################
f=x^3+2*x^2-7
f.prime = 3*x^2+4*x
x = x - f/f.prime
###################

#Write the method in a for loop
#for loop
x = 2
for(i in 1:100){
  f=x^3+2*x^2-7
  f.prime = 3*x^2+4*x
  x = x - f/f.prime
}
x

#Write the method in a while loop with a given tolerance
x = 2
tolerance = 10^(-6)
f = x^3+2*x^2-7
while(abs(f) > tolerance){
  f = x^3+2*x^2-7
  f.prime = 3*x^2+4*x
  x = x - f/f.prime
}
x

rm(list = ls())
#eg.2 write a function to solve x^m = c
#i.e. c^(1/m)=?
findRoot = function(m = numeric('power'), c = numeric()){
  x = 1
  tolerance = 10^(-6)
  f = x^m - c
  f.prime = m*x^(m-1)
  
  while(abs(f) > tolerance){
    x = x - f/f.prime
    f = x^m - c
    f.prime = m*x^(m-1)
  }
  
  return(x)
}
findRoot(5, 10) #誰的五次方是10 #only find the real number root
1.584893^5
10^(1/5)
#visualization
x = seq(-3, 3, by = 0.01)
y = x^5
plot(x, y, type = 'l')
lines(x, rep(10, length(x)), col = 'blue')
points(findRoot(5, 10), 10, col = 'red')
legend("topright", # 表示在右上角
       pch = 1,    # pch代表點的圖案
       col = c("blue", "red"), # col代表顏色 
       legend = c("y = 10", "x*") # 顏色所對應的名稱
)
rm(list = ls())

## rock paper scissors
# One round
A <- sample(c("rock", "paper", "scissors"),1)
B <- sample(c("rock", "paper", "scissors"),1)

if(A==B){
  print("Tie")
} else {
  if((A=="rock" & B=="scissors")|(A=="scissors" & B=="paper")|(A=="paper" & B=="rock")){
    print("A wins")
  } else {
    print("B wins")
  }
}

###########################################################
###################   (Practice)   ########################
###########################################################
# Five wins (method 1: break)



###########################################################


####### Monte Carlo simulation
rm(list = ls())
## Estimate pi
x=runif(10000) #We just randomly distribute the points inside the square on X-Y plane
y=runif(10000)
plot(x, y)
z=sqrt(x^2+y^2)
# The following returns the indices vector when z is less or equal than 1
which(z<=1)
length(which(z<=1))*4/length(z)
plot(x[which(z<=1)], y[which(z<=1)],
     xlab = "X", ylab = "Y",
     main = "Monte Carlo Simulation for Approximating Pi", 
     col = 'blue')
points(x[which(z>1)],y[which(z>1)],col='gray')

# For a whole circle
x = runif(10000, -1, 1)
y = runif(10000, -1, 1)
z=sqrt(x^2+y^2)
abs(z) <= 1
length(which(abs(z) <= 1))*4/length(z)
sum(abs(z) <= 1)*4/length(z) #the same
plot(x[which(abs(z)<=1)], y[which(abs(z)<=1)], main = 'Monte Carlo Simulation for Approximating Pi', type = 'p', col = 'blue')
points(x[which(abs(z)>1)], y[which(abs(z)>1)], col = 'gray')


## Estimate pi
point <- 1000000

a <- matrix(NA,point,2)
a[,1] <- runif(point)
a[,2] <- runif(point)
b <- (a[,1])^2+(a[,2])^2
for(i in 1:point){
  if(b[i]<=1) b[i] <- 1 else b[i] <- 0
}
sum(b)/point*4

## Plot
converge <- function(point){
  a <- matrix(NA,point,2)
  a[,1] <- runif(point)
  a[,2] <- runif(point)
  b <- (a[,1])^2+(a[,2])^2
  for(i in 1:point){
    if(b[i]<=1) b[i] <- 1 else b[i] <- 0
  }
  c <- c(1:point)
  b <- 4*cumsum(b)/c
  data <- as.data.frame(cbind(b,c))
  g <- ggplot()+
    ggtitle("Convergence")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_hline(yintercept = pi)+
    geom_line(data=data,aes(c,b))
  cat("Pi=",b[point],"\n")
  g
}

ptm <- proc.time()
converge(100000)
proc.time()-ptm

###########################################################
###################   (Practice)   ########################
###########################################################
## Estimate square root(2)



###########################################################

## Mean of geometric distribution
rm(list = ls())
data <- c()
for(i in 1:1000){
  X <- 1
  while(runif(1)>0.5){
    X <- X+1
  }
  data <- c(data,X)
}
mean(data)
barplot(table(data)/length(data))
title("Geometric distribution")
curve(0.5^x, from = 1, to = 10, col = "red", add = TRUE)

data <- data.frame(x = data)
library(ggplot2)
ggplot(data, aes(x)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_x_continuous(breaks=c(1:10)) +
  ggtitle("Geometric distribution") +
  ylab("freq") +
  stat_function(fun = function(x) 0.5^x,colour="red")

rm(list = ls())
