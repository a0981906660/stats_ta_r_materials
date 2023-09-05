#R HW2. 請根據「統計電腦實習課之莫名其妙問卷大調查」資料表回答以下問題：

data = read.csv('/Users/Andy 1/Google 雲端硬碟/108-1/1 三345 統計TA/3 TA課內容/出作業/R HW2/AssignmentDataFrame.csv')
#1. 資料表中是否有無法處理的欄位？請試著處理NA值，並另外儲存你的資料表為csv檔，最後附於附錄中

#Method 1: Drop the NAs row by row in each column
data2 = data[!is.na(data$DogLover),] #we first drop NA in the first vlb
View(!is.na(data$DogLover))
data2 = data2[!is.na(data2$NumFamilyMemb),] #then the second
data2 = data2[!is.na(data2$pi_millionth_digit),] #no more NAs
sum(is.na(data2))


#Method 1 Alternative way: Drop All the rows that includes NAs
df = na.omit(data) 

#Method 2: We know there are NAs in 'DogLover', 'NumFamilyMemb' and 'pi_millionth_digit'
#We'll use functions such as `sum(x, na.rm = T)` to avoid NAs that interfere the numeric calculation
#eg.
sum(data$DogLover, na.rm = T)
mean(data$DogLover, na.rm = T)
boxplot(data$pi_millionth_digit, na.rm = T)

#2. 請將資料表的row按照出生年月日排序（由時點遠到近）
#hint:先按照年排序，接著將同一年份的row(sub dataframe)宣告為另一data frame
#在其內排序後取代原先的row，如此便排序完年與月，重複此步驟即可排序年月日
#Method 1: follow the hint #Cons:產生很多中間變數
#Year
data2 = data2[order(data2$BirthYear),] #先排完年
View(order(data2$BirthYear))
#Month: 區分成兩年做
data_1999 = data2[data2$BirthYear == 1999,] #取同一年的子資料表
View(data2$BirthYear == 1999)
data_1999 = data_1999[order(data_1999$BirthMonth),] #在其內排序月
data2[data2$BirthYear == 1999,] = data_1999 #丟回去原資料表，現在是年排序好且1999年的月也排序好的狀態

data_2000 = data2[data2$BirthYear == 2000,] #取2000年的子資料表
data_2000 = data_2000[order(data_2000$BirthMonth),] #在2000年內排序月
data2[data2$BirthYear == 2000,] = data_2000 #丟回，現在是年月都排好的狀態

#Day: 區分成兩年的各兩個月做->要做四次
data_9907 = data2[data2$BirthYear == 1999 & data2$BirthMonth == 7,]
data_9907 = data_9907[order(data_9907$BirthDay),]
data2[data2$BirthYear == 1999 & data2$BirthMonth == 7,] = data_9907

data_9908 = data2[data2$BirthYear == 1999 & data2$BirthMonth == 8,]
data_9908 = data_9908[order(data_9908$BirthDay),]
data2[data2$BirthYear == 1999 & data2$BirthMonth == 8,] = data_9908

data_0007 = data2[data2$BirthYear == 2000 & data2$BirthMonth == 7,]
data_0007 = data_0007[order(data_0007$BirthDay),]
data2[data2$BirthYear == 2000 & data2$BirthMonth == 7,] = data_0007

data_0008 = data2[data2$BirthYear == 2000 & data2$BirthMonth == 8,]
data_0008 = data_0008[order(data_0008$BirthDay),]
data2[data2$BirthYear == 2000 & data2$BirthMonth == 8,] = data_0008

#Method 2: Sort by date-month-year
data = na.omit(data) 
df2 = data[order(data$BirthDay),]
df2 = df2[order(df2$BirthMonth),]
df2 = df2[order(df2$BirthYear),]

#Method 3: Actually `order()` can simply do the job...
df = df[order(df$BirthYear, df$BirthMonth, df$BirthDay),]

write.csv(df, 'df.csv')

#3. 請畫出貓派與狗派人數的barplot
barplot(c(sum(df$DogLover), length(df$DogLover) - sum(df$DogLover)),
        names.arg =c('Dog Lover','Cat Lover'), ylim = c(0,50))

#4. 畫出「家中有幾位成員」的boxplot，mean, Q1, Q3為何？
#Method 1: show the unmodified boxplot
boxplot(df$NumFamilyMemb)
#Method 2: drop the extreme values
summary(df$NumFamilyMemb)
#say the extreme values are those exceed median+1.5*IQR
upperLimit = 4+1.5*(6-4)
NumFamilyMemb = df$NumFamilyMemb[df$NumFamilyMemb < 7]
boxplot(NumFamilyMemb, main = 'Boxplot of Number of Family Members (After Dropping Extreme Values)')


#5. 在「終極密碼」題中，繪製以終極密碼答案為橫軸，智商為縱軸的scatter plot
#Method 1: show the unmodified scatter plot
plot(df$GuessNumber, df$IQ)
#Method 2: drop the extreme values
summary(df$IQ)
#say the extreme values are those exceed median+1.5*IQR
upperLimit = 150+1.5*(200-50)
df2 = df[df$IQ <= upperLimit,]
plot(df2$GuessNumber, df2$IQ, xlab = 'The Number Guessed', ylab = 'IQ',
     main = 'Scatter Plot of The Number Guessed & IQ')

#繳交格式要求：
#Q1,2：繳交r code及csv文件。請將資料表整理好儲存於同一csv文件中
#Q3-5：繳交pdf文件，內有各題的plot
#或以R Markdown同時繳交r code及plot，另外附上csv文件即可
