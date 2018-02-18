data1 <- read.csv("BP apprehensions 2010.csv", header = TRUE, stringsAsFactors = FALSE)
as.matrix(data1)
data2 <- read.csv("PB Apprehensions 2017.csv", header = TRUE, stringsAsFactors = FALSE)
#as.matrix(data2)
print(data2)
data2 <- head(data2[1:13],-1)

print(data2)
x <- subset(data1, select= -c(Sector))
print(x)
y <- subset(data2, select= -c(Sector))
print(y)
#y <- head(y[1:12],-1)
print(y)

#comparison between each month in both years 
t.test(data1$October, data2$October, paired=TRUE)
test2 = (t.test(data1$November, data2$November, paired=TRUE))$statistic
test3 = (t.test(data1$December, data2$December, paired=TRUE))$statistic
test4 = (t.test(data1$January, data2$January, paired=TRUE))$statistic
test5 = (t.test(data1$February, data2$February, paired=TRUE))$statistic
test6 = (t.test(data1$April, data2$April, paired=TRUE))$statistic
test7 = (t.test(data1$May, data2$May, paired=TRUE))$statistic
test8 = (t.test(data1$June, data2$June, paired=TRUE))$statistic
test9 = (t.test(data1$July, data2$July, paired=TRUE))$statistic
test10 = (t.test(data1$August, data2$August, paired=TRUE))$statistic
test11 = (t.test(data1$September, data2$September, paired=TRUE))$statistic

allMonths = c(test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11)
print(allMonths)
pts = seq(-2,2,length=11)
#plot(pts,dt(pts,df=12),col='red',type='l')
print(range(allMonths))
print(length(pts))
print(length((density(allMonths))))
plot(pts,allMonths, type = 'l')

#comparison between each location in both years 
print(x)
print(y)
total2010<- rowSums(x[1:12])
print(total2010)
total2017 <- rowSums(y[1:12])
print(total2017)
t.test(total2010, total2017, paired=TRUE)
ttest = t.test(total2010,total2017)
names(ttest)

