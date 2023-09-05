#Use packages
library(MASS)
library(ggplot2)
#if there is no packages, install it
#install.packages('ggplot2') #have to be char
#install.packages('plotly')
library(plotly)

##DataFrame & Graphing
#set work directory
path = "/Users/Andy 1/Google 雲端硬碟/108-1/1 三345 統計TA/3 TA課內容/0 formal/Data"
setwd(path)

##DataFrame
#read.csv()
#Let use the following data:
#https://udb.moe.edu.tw/StatCardList/University/000012CE619F/0003/國立臺灣大學
data = read.csv('107NTU Student.csv')
head(data) #1st row is not the real label...
data[1,]

#Let's drop the 1st row, and assign a new DataFrame
data = data[2:nrow(data),] #what is the disadvantage of doing this? Your 'data' is overlapped
head(data)
data = data[1:302,] #further drop out the last several rows
colnames(data) #Then we have another problem...the name of col is incorrect

#Modify the col names
colnames(data) = c('year', 'x1', 'x2', 'x3', 'x4', 'x5', '系所','BachMD','在學生數','Male','Female')
colnames(data)
#we further drop out other cols that are not interesting
data = data[,c('year','系所','BachMD','在學生數','Male','Female')]

#drop out the rows that we are not interested at
BachMD = data$BachMD #因為每個col是factor，所以我不能寫：
data2 = data['BachMD' == '學士班(日間)',] #這是char對char的邏輯判斷
#但我的col裡沒有char，只有factor，所以要這樣寫才能比：
data = data[BachMD == '學士班(日間)',] #才能篩選出學士班的row

data['在學生數']
numStudent = data$在學生數 #We can use chinese as a variable's name, cool!
numStudent = data$'在學生數' #or write in this way
numStudent
mean(numStudent) #error, because of "factor"
is.numeric(numStudent) #only when data type == numeric, then you can calculate
is.factor(numStudent) #What is 'factor'? 類別變數

#change factor to numeric, a WRONG way
numStudent2 = as.numeric(numStudent) #as.numeric() : only do the ranking
is.numeric(numStudent2)
numStudent2
mean(numStudent2)
#The right way
numStudent3 = as.numeric(levels(numStudent))[numStudent]
is.numeric(numStudent3)
numStudent3
mean(numStudent3) #finally we get the average number of student in NTU among all departments.

#Store the dataFrame that you've clean up
write.table(data, "data.txt", row.names=F) #write txt file
write.csv(data, "data.csv", row.names = F) #write csv file

#sort
#將整個dataFrame的特定col轉成numeric
data$在學生數 = as.numeric(levels(data$在學生數))[data$在學生數]
mean(data$在學生數) #same result as above
data = data[order(data$在學生數),] #order()會回傳排序的index
write.csv(data, "data2.csv", row.names = F) #save a sorted ver.

#plot
data2 = read.csv("data2.csv")

max(data$在學生數); min(data$在學生數)
median(data$在學生數)
summary(data$在學生數)
#boxplot
boxplot(data2$在學生數)

#RStudio does not fully support chinese...QQ
barplot(data2$在學生數, names.arg = data2$系所, xlab = 'department', main = 'number of students')
barplot(data2$在學生數, names.arg = data2$系所, xlab = '系所', ylab = '學生數', main = '各系所學生人數')

#https://blog.gtwang.org/r/how-to-use-your-favorite-fonts-in-r-charts/
#install.packages("showtext")
#if we import chinese fonts
library(showtext)
showtext.auto(enable = TRUE)
font_add("PingFang", "/System/Library/Fonts/PingFang.ttc")

#save the bar plot
png("output.png", width = 1920, height = 1080)
barplot(data2$在學生數, names.arg = data2$系所, xlab = '系所', ylab = '學生數', main = '各系所學生人數',family = "PingFang", horiz = F)
dev.off()

##Another way to read csv
#if you first make the table cleaner...
data = read.csv('107NTU Student-2.csv', header = F) #F if the first row is not the name of variables
head(data)
data = read.csv('107NTU Student-2.csv', header = T) #F if the first row is not the name of variables
head(data)

#easy to assign vlbs and do the operation
#A WRONG way to operater by vectors
male = data$'在學學生數男'
female = data$'在學學生數女'
ratio = male/female
ratio #there are a lot of NAs
sum(is.na(ratio)) #I have 16 NA values
max(ratio)
max(ratio, na.rm=TRUE) #see the max value without NA
#remove NA from vectors
ratio = ratio[!is.na(ratio)]
sum(is.na(ratio)) #no NAs anymore
#remove Inf values from vectors
sum(ratio == Inf) #I have 3 Inf
ratio = ratio[!is.infinite(ratio)]
sum(ratio == Inf)
#alternative way
ratio = ratio[is.finite(ratio)]
barplot(ratio) #no tags, not good, and hard to recover the tags
#I'll show in a more proper way

#We just look at the undergraduate students
male = data2$Male
female = data2$Female
ratio = male/female
data2 = cbind.data.frame(data2, ratio) #add a new col to the dataFrame
#plot without handeling the Inf, so we have to set limit
png("output2.png", width = 3840, height = 2160)
barplot(data2$ratio, names.arg = data2$系所, ylim = c(0,10), family = "PingFang")
dev.off()

#Use Data from Internet
#we can also import data from the internet
library(foreign)
gpa1 = read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/gpa1.dta")
