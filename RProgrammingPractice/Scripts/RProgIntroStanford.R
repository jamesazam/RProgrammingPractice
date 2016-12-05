dir <- getwd()
setwd(dir)

library("xlsx")
library("foreign")
library("foreach")
library("googleVis")
library("reshape")
library("XML")
library("parallel")


ncore = multicore:::detectCores()
#creating dataframes
data <- data.frame(Day=c(1,2,3), Name = c("Monday","Tuesday","Wednesday"), 
Activity=c("dancing","singing","walking"))

#Exporting data
write.table(data, file = "dat.csv", sep = ",")
some.data <- read.csv("dat.csv", header = TRUE, stringsAsFactors = TRUE)
some.data
str(some.data)
#subsetting dataframes
subset(data, activity=="dancing") 

#importing text files

URL <- "http://www.stanford.edu/~druau/pivot_table.csv"
pivot <- read.table(URL, sep=',', header = TRUE)
head(pivot, n=3)



#excel files
??read.xlsx


#XML/HTML (comes from the XML package)
eq <- readHTMLTable("http://www.iris.edu/seismon/last30.html")

#for more information
help.start()

#subsetting:
#with subset
dim(iris)
dim(subset(iris, Species=="setosa"))
dim(subset(iris, Species=="versicolor"))
dim(subset(iris, Species=="virginica"))


unique(iris$Species) #find the species with unique names

#with sql
install.packages("sqldf", dependencies = TRUE)
library("sqldf")

#with doBy
install.packages("doBy")
library("doBy")
head(iris)
#example with doBy. Find the means and sd of the iris data according to species
# with columns of sepal.Width, etc

summaryBy(Sepal.Length+Sepal.Width+Petal.Length+Petal.Width ~ Species, data = iris, FUNC =c(mean,sd))
##############################################################
#manipulating CSV files
dem.stats.dat <- read.csv("Demographic_Statistics_By_Zip_Code.csv", header = TRUE)
head(dat, n=3)
names(dat) #view the column names

crimes12_15.dat <- read.csv("Crimes_2012-2015.csv", header = TRUE)
head(crimes12_15.dat, n=3)
names(crimes12_15.dat)
length(names(crimes12_15.dat))==ncol(crimes12_15.dat) #check if the number of column names corresponds to the number of columns
dim(crimes12_15.dat) #check the dimensions of the data

cons.complaints.dat <- read.csv("Consumer_Complaints.csv", header = TRUE)
head(cons.complaints.dat, n=3)
names(cons.complaints.dat)
length(names(cons.complaints.dat))==ncol(cons.complaints.dat)
dim(cons.complaints.dat)


#DPLYR
library("dplyr")
Crimes <- read.csv("Crimes_2012-2015.csv", header = TRUE)
print(Crimes)
Date.Occured <- group_by(Crimes, DATE.OCC)
?count
counts <- summarise(Date.Occured, n=n(), sort=TRUE)
head(counts, 10)


dat <- read.csv("meas.csv", header = TRUE)
print(dat)


#magrittr package
#generic methods
x<- 1:11
sqrt(x)
sum(sqrt(x))

#using pipes
x <- 1:11
x %>% sqrt() %>% sum() #take x, find the square root nof x and pass the
#output to the sum function

dat <- iris

dat %>% subset(dat$Species=="setosa") %>% tail()

#googleVis
library("googleVis")
dat <- iris

par(mfrow=c(2,1))
for(i in 1:2){
dat%>%subset(Sepal.Width>i, select=c(Species,Sepal.Length))%>%plot(main="adasdasd")
}

#measles

dir <- getwd()
setwd(dir)
library("gdata")
measles.dat <- read.xls("60measles.xls", sheetName = "60measles")
cities <- read.xlsx("60measles.xls", sheetName = "60cities")

head(measles.dat)


#more on dplyr
dat <- mtcars
head(dat)
#dplyr functions
colnames(dat)
rownames(dat)

#change some column names the old way
colnames(dat)[1] <- "Miles_Per_Gallon"


#Now, using dplyr
library("dplyr")
#first reset the name to the original

colnames(dat)[1] <- "mpg"

#1. with dplyr, we use the function (?rename)
dat <- rename(dat, Miles_per_Gallon=mpg)
#you can rename multiple columns at a time
dat <- rename(dat, Miles_per_Gallon = mpg, Cylinder = cyl)

# 2. ?filter for subsetting rows
dat_mpg_over_20 <- filter(mtcars,mpg>20 )
head(dat_mpg_over_20)
#filter() does not keep the rownames. One way to cater for that
#is to copy over the data and assign the rownames as a new column

mt <- mtcars
mt$Cars <- rownames(mtcars)

#AND operator
mt_mpg_over_20_and_cyl_equal_6 <- filter(mt, mpg>20, cyl==6) #automatically joins them with an AND oprator
head(mt_mpg_over_20_and_cyl_equal_6)

#OR operator
mt_mpg_over_20_or_cyl_equal_6 <- filter(mt, mpg>20, cyl==6) #automatically joins them with an AND oprator
head(mt_mpg_over_20_or_cyl_equal_6, n=10)

#3. ?slice() grab objects by their ROW position
slice.example <- slice(mtcars, 1:5)
head(slice.example)

#4. sort ROWS with ?arrange() by columns.

mt_mpg_sort <- arrange(mt,cyl,desc(mpg))
#this example means "arrange mtcars by cylinder and mpg in
#descending order"
head(mt_mpg_sort)


#5. subset with select()

mt.carb.mpg <- select(mt, carb,mpg)
mt$Cars <- rownames(mt)


#select() can be used to grab columns with partially matched names
mt4 <- select(mt, cyl,contains("p"))
View(mt4)
#default subsetting in R
mt.carb.mpg2 <- mt[,c("carb","mpg")]
head(mt.carb.mpg2)
arrange(mt.carb.mpg, mpg)
arrange(mt.carb.mpg, desc(mpg))


#6. mutate() tables to get new columns

mt <- mutate(mt, mpg.sq = mpg^2) #create a new column with mpg squared
head(mt)
#standard procedure

mt$mpg.sq2 <- mt$mpg^2 
head(mt)

#more interesting will be for you to add more than one column

mt2 <- mutate(mt, mpg.sq = mpg^2, cyl.sq = cyl^2)
head(mt2)

#7. transmute() is used for transforming and mutating.
#If you only want to keep the new variablesdata without 
#keeping the original, use transmute():
# An example will suffice:

mt_mut <- transmute(mt,mpg.sq = mpg^2, cyl.sq = cyl^2 )
head(mt_mut)


#8. Summarise data with ?summarise()

mt.summarised <- summarise(mt, mean.mpg = mean(mpg), max.cyl= max(cyl))
mt.summarised 


#9. Group a table into various variables ?group_by()

mt.by.cyl <- group_by(mt, cyl=n(),mpg=n)
head(mt.by.cyl)
head(mt) #compare with the original table
print(mt.by.cyl)
