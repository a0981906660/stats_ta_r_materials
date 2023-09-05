##R Programming
#Newton's Method -> find root
rm(list = ls())

#Requirement:
#1.Smooth function
#2.Good initial guess

#eg1.
#f(x) = x^3+2*x^2-7
f = function(x){
   y = x^3+2*x^2-7
   return(y)
}
f.prime = function(x){
  y = 3*x^2+4*x
  return(y)
}
x0 = 0
f(0)
f(1)
f(2)
f(6)
#have a glance
x = seq(-10, 10, by = 0.1)
f(x)
plot(x, f(x), type = 'l')
plot(x, f.prime(x), type = 'l')

##overlapped view
plot(x, f(x), type = 'l', ylim = c(-500, 1000), ylab  = 'function value')
par(new = T)
plot(x, f.prime(x), type = 'l', ylim = c(-500, 1000), col = 'blue', ylab = 'function value')
legend("topright", # 表示在右上角
       pch = 1,    # pch代表點的圖案
       col = c("black", "blue"), # col代表顏色 
       legend = c("f(x)", "f '(x)") # 顏色所對應的名稱
)
graphics.off()

#With Newton's Method, we know the iteration is:
# x_n+1 = x_{n} \frac{f(x_{n})}{f'(x)_{n}}
#initial / starting point
x = 2
#repeat the following 3 lines and get the root
f=x^3+2*x^2-7
f.prime = 3*x^2+4*x
x = x - f/f.prime

#for loop
for(i in 1:100){
  f=x^3+2*x^2-7
  f.prime = 3*x^2+4*x
  x = x - f/f.prime
}
x

#Try to write the above repeating things in a loop
x = 2
tolerance = 10^(-6)
while(abs(f) > tolerance){
  x = x - f/f.prime
  f = x^3+2*x^2-7
  f.prime = 3*x^2+4*x
}
x

#Let's write a function to solve x^m = c
#i.e. find one of the root of x^m - c = 0
#i.e. c^(1/m)
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
findRoot(5, 10) #誰的五次方是10 #only find the real root
1.584893^5
10^(1/5) #小型工程計算機就是用類似的方法寫的

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

#when Newton's Method fails => starting point is not good enough
#f(x) = x^(1/3)
#have a glance
x = seq(-10, 10, by = 0.1)
u = x^(1/3)
plot(x, u, type = 'l')

#find root #will fail
x = 1
u = x^(1/3)
u.prime = (1/3)*x^(-2/3)
##u = log(x)     ##you can try log utility
##u.prime = 1/x  ##
tolerance = 10^(-6)
while(abs(u)>tolerance){
  x = x - u/u.prime
  u = x^(1/3)
  u.prime = (1/3)*x^(-2/3)
}
x #the iteration fails


#(Optional)
#another version that you can decide your starting point ##need check
root = function(f, f.prime, guess) {
  tolerance = 10^(-6) #tol
  x = guess
  while (abs(f(x)) > tolerance) {
    x = x - f(x)/f.prime(x)
  }
  return(x)
}

f = function(x) {x^3 + 2*x^2 - 7}
f.prime = function(x) {3*x^2 + 4*x}

root(f, f.prime, 10)

#if I change the tol, I'll see different deviated points
x = rep(0, 99)
root_found = c()
for(i in 1:99){
  x[i] = -10+ 0.1*i
  root_found = cbind(root_found, root(f, f.prime, x[i]))
}
length(x)
length(root_found)
plot(x[1:99], root_found) #redo this line after changing tol

#Sin function
x = seq(-2*pi, 2*pi, by = 0.1)
y = sin(x)
plot(x, y, type = 'l')
lines(x, rep(0, length(x)), col = 'green')
#the numerical approach depends on initial guess
root(sin, cos, 3) #derivative of sin is cos #start from 3 => find pi
root(sin, cos, -3) #start from -3 => find -pi
######
#root(sin, cos, pi/2) #caution: infty loop
######
root(sin, cos, pi/2+0.001) #jump really far away

#introduce `nlm` : minimize non-linear model
nlm(sin, pi, hessian = T) #give pi => goes to the right and find 1.5*pi
4.712387/pi
