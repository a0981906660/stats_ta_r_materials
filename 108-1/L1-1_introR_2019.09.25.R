##Simple Calculator
1+1
3*4
3**4 #power
sqrt(2) #square root of
pi
pi*3^2
log(exp(1))
3/4
5/0
10-Inf
log(0)
-Inf/Inf #"NaN" means not a number
NA #"NA" means Not Available
8%%4 #remainder
sin(pi/2)
round(1234.5678, 1)
round(1234.5678, 2)
round(1234.5678, -2)
help(round)
?round
floor(pi)
ceiling(pi)
trunc(pi)
x = seq(1, 10, by = 0.1)
x
plot(x, floor(x))

#assign vlb
a = 2
b = 3
a+b
print(a+b)
rm(b) #清空b
rm(list = ls()) #清空變數 #等於按掃把按鈕
a = 5
#several frequent used data structure
#value (mentioned above)
#vector
c1 = c(0,1,2,3,4)
c1
is.vector(c1) #當你想確認資料型態時
a*c1
c1 = 1:5
c1 = seq(1,5, by = 1)
c2 = c(2,4,6,8,10)
c1+c2 #元素相加
c1*c2 #兩個向量的"*乘法"
c3 = c(c(1,2,3),
       c(4,5,6)) #串起來；還是vector，永遠是n*1的向量
c3
length(c1)
c1
c1[2]
c2[4]
#logic (Boolean)
c1[2] == 2
c2[4] > 8
length(c3) != 5

mean(c1)
min(c1)
max(c1)
sum(c1) # R^n -> R^1
log(c1) # R^n -> R^n
factorial(c1)

z = c(1, 2, NA, 3, NA, 4)
z
is.na(z)
mean(z)
mean(z, na.rm = TRUE)

#character
x1 = "I love Statistics"
x1[1]
x1[2]
length(x1)
x2 = c("I", "love", "Statistics")
x2[3]
length(x2)
nchar(x2)

#matrix
A1 = matrix(1:15, nrow = 5)
A1
A2 = matrix(16:30, ncol = 3) #assign either one is enough
A2
A3 = matrix(1:15, nrow = 5, ncol = 5) #if exceed 1:15
A3
A1 = matrix(1:15, nrow = 5, dimnames = list(c('r1', 'r2', 'r3', 'r4', 'r5'), 
                                            c('c1', 'c2', 'c3')))
A1
nrow(A1)
ncol(A1)
dim(A1)
colnames(A1)
rownames(A1)
head(A1)
tail(A1)
A1
A1+A1
A1*A1
A1t = t(A1) #transpose
A1%*%A1t # "%*%矩陣乘法"

A1[3, ] #list the 3rd row
A1[3, 1:2] #list the 3rd and only the 1st to 2nd cols
A1[, 2] #list the 2nd col
A1[, 2:3]
A2
A3 = cbind(A1, A2)
colnames(A3)
rbind(A1, A2)

