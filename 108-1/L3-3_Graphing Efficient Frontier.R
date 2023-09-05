path = '/Users/Andy 1/Google 雲端硬碟/108-1/1 三345 統計TA/3 TA課內容/0 formal/Data/efficient frontier stock price'
setwd(path)
data1 = read.csv('MSFT.csv')
data2 = read.csv('JPM.csv')
stock_price = cbind(data1$Open, data2$Open)
colnames(stock_price) = c('MSFT', 'JPM')
stock = data.frame(stock_price)

#setting variables
r1 = diff(log(stock$MSFT), 1)
hist(r1)
r2 = diff(log(stock$JPM), 1)
hist(r2)

#gaining parameters
mean(r1)*251
mean(r2)*251
sd(r1)*251
sd(r2)*251
cov(r1, r2)

#setting function(let P consisting of MSFT & JPM)
# P = omega * r1 +  (1-omega) * r2
# E(P) = omega * E(r1) +  (1-omega) * E(r2)
returnP = function(omega_r1){
  retP = omega_r1*mean(r1) + (1-omega_r1)*mean(r2)
  return(retP)
}

#X, Y
#Z = aX + (1-a)Y
#Var(Z) = a^2 Var(X) + (1-a)^2 Var(Y) + 2a(1-a) Cov(X, Y)
sigmaP = function(omega_r1){
  varP = omega_r1^2*var(r1) + (1-omega_r1)^2*var(r2)
  + 2*omega_r1*(1-omega_r1)*cov(r1, r2)
  return(sqrt(varP))
}

#plotting sigma-return scatter plot
omega = seq(0, 1, 0.01)
plot(omega, returnP(omega), type = 'l')
plot(omega, sigmaP(omega), type='l')

plot(sigmaP(omega), returnP(omega), type = 'l', 
     main="Efficient Frontier of P consisting of MSFT & JPM",
     ylab="Expected Return (r)", xlab = "risk (sigma)")

#find omega that minimizes the risk of rate of return
min(sigmaP(omega))
omega[which.min(sigmaP(omega))]
#3D Plot
library(scatterplot3d)
scatterplot3d(omega,returnP(omega),sigmaP(omega))
scatterplot3d(sigmaP(omega), returnP(omega), omega)

library(plotly)
plot_ly(x=sigmaP(omega), y=returnP(omega), z=omega, type="scatter3d", mode="markers", color=returnP(omega))

#模擬股價波動
stock_price = cbind.data.frame(data1$Date, data1$Open, data2$Open)
colnames(stock_price) = c('Date', 'MSFT', 'JPM')

#Treat vlb as 'date' type
?as.Date
stock_price$Date = as.Date(stock_price$Date)

#兩張股價的折線圖
plot(stock_price$Date, stock_price$MSFT, type = 'l')
plot(stock_price$Date, stock_price$JPM, type = 'l')

#parameters(daily frequency)
mean(r1)
mean(r2)
sd(r1)
sd(r2)

set.seed(1234)
days = 252-1
changes = rnorm(days, mean = mean(r1), sd = sd(r1))
stock_price$MSFT[1] #MSFT第一天的股價
MSFT_sim = cumprod(c(stock_price$MSFT[1], changes+1))#放入起始股價以及每日的成長率
?cumprod
length(MSFT_sim) #有252天的價格
stock_price = cbind.data.frame(stock_price, MSFT_sim)


plot(stock_price$Date, stock_price$MSFT_sim, type = 'l')

#兩張圖一起看
par(mfrow = c(1,2))
plot(stock_price$Date, stock_price$MSFT, type = 'l')
plot(stock_price$Date, stock_price$MSFT_sim, type = 'l')

#But the above is only one possible future QQ
#Though the stock price at the last day seems not too different
stock_price$MSFT[252] - stock_price$MSFT_sim[252]

#Use Simulation!
runs = 10000

#simulates future movements and returns the closing price on day 252
generate_path = function(){
  days = 252-1
  changes = rnorm(days, mean=mean(r1), sd=sd(r1))
  sample_path = cumprod(c(stock_price$MSFT[1], changes+1))
  closing_price = sample_path[days+1] #+1 because we add the opening price
  return(closing_price)
}

closing_sim = replicate(runs,generate_path())
mean(closing_sim)
#This is the first step of simulation, actually, we can change the expected rate of return over time, and we can also add disturbance randomly
#When in doubt, Monte Carlo.
#May Monte Carlo Simulation be with you!

