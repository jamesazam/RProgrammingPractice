#OLS Regression and Cointegration Testing

#Set Directory
setwd("Set Directory")
mydata<-read.csv("Set File to path ols_stock.csv")
attach(mydata)

#Ordinary Least Squares Regression
reg1<-lm(stock_return_scaled ~ earnings_ranking + dividend + debt_to_equity)
summary(reg1)

#Load Variables and run Breusch-Pagan Test
library(car)
library(lmtest)
Y <- cbind(stock_return_scaled)
X <- cbind(dividend, earnings_ranking, debt_to_equity)
bptest(Y ~ X)

#VIF Tests for Multicollinearity
reg1m <- lm(dividend ~ earnings_ranking + debt_to_equity)
summary(reg1m) 
reg2m <- lm(earnings_ranking ~ dividend + debt_to_equity)
summary(reg2m) 
reg3m <- lm(debt_to_equity ~ earnings_ranking + dividend)
summary(reg3m)
vif(reg1m)
vif(reg2m)
vif(reg3m)

#Cointegration Testing
mydata2<-read.csv("Set File to path cointegration_sample.csv")
attach(mydata2)

#Dickey-Fuller Tests and Regression Plots
library(tseries)
adf.test(lnseries1)
adf.test(lnseries2)
adf.test(lnseries1_firstdifference)
adf.test(lnseries2_firstdifference)
olsreg <- lm(lnseries1_firstdifference ~ lnseries2_firstdifference)
summary(olsreg)
olsreg <- lm(lnseries1_timet ~ lnseries2_timetminusone)
summary(olsreg)
plot (lnseries1)
plot (lnseries1_firstdifference)
plot (lnseries2)
plot (lnseries2_firstdifference)

##########################################################################################################################################################
#Data Cleaning Example (clear environment to run)
mydata<- read.csv("Set File to path sales.csv")
attach(mydata)

mydata2<- read.csv("Set File to path customers.csv")
attach(mydata2)

#1.Storing variables in a data frame
dataframe<-data.frame(ID,Age)

#2.Mimic VLOOKUP by using the merge function
mergeinfo<-merge(mydata[, c("ID", "Sales")],mydata2[, c("ID", "Age", "Country")])
mergeinfo

#3.Using as.date to format dates
currentdate=as.Date('2016-12-15')
dateinfile=as.Date(Date)
durationcalculation=currentdate-dateinfile
Duration=as.double.difftime(durationcalculation, units='days')
updateddataframe=data.frame(ID,Sales,Date,Duration)
updateddataframe

#4.grepl: Remove instances of a string from a variables
countryremoved<-mydata2[!grepl("Greenland", mydata2$Country),]

#5.Delete observations using head and tail functions
Sales
Salesminus30days<-head(Sales,-30)
X1=as.matrix(Salesminus30days)
X1

Salesplus30days<-tail(Sales,-30)
X2=as.matrix(Salesplus30days)
X2

#6. SUMIF Table (Clear Environment to run)
names <- c("John", "Elizabeth", "Michael", "John", "Elizabeth", "Michael")
webvisitsframe <- cbind("24","32","40","71","65","63")
webvisits=as.numeric(webvisitsframe)
averageminutesspentframe <- cbind("20", "41", "5", "6", "48", "97")
averageminutespent=as.numeric(averageminutesspentframe)

nametable<-data.frame(names,webvisits,averageminutespent)

sumiftable<-aggregate(. ~ names, data=nametable, sum)
sumiftable
