######################################################
#COURSERA ADVANCED R PROGRAMMING
#######################################################
library("purrr")
x <- 1:100
y <- seq(200,1000, length.out = 100)
z <- seq(200,300, length.out = 100)

#map()

mtcars %>%
    split(.$cyl) %>%
    map(~ lm(mpg ~ wt, data = .x))%>%
    map(summary) %>%
    map_dbl("r.squared")

#map2() - map a function over two vectors. %>% is a pipe
map2(x,y,paste0) %>%unlist()

#pmap() - map a function over more than 2 vectors. They should be 
#coerced as  a list of lists
pmap(list(as.list(x),as.list(y),as.list(z)),mean) %>%unlist()

#reduce() - combines or reduces a vector to a single value
reduce(x, function(n,m){
    message("x = ", n)
    message("y = ", m)
    message("mean = ")
    message("", mean(c(n,m)))
    mean(c(n,m))
})

reduce(x, function(n,m){
   n+m
})

#reduce_right() starts the opration from the right to the left
reduce_right(letters[1:7], function(x, y){
    paste0(x, y)
})



#SEARCHING THROUGH A VECTOR WITH:
#CONTAINS() returns a logical T/F if a specified element is present in
#the vector
contains(letters,"z")
contains(y,200)


#DETECT() returns the first element of the vector for which the predicate function
#returns a true
detect(x,function(n){n^2}==49)
detect_index(x,function(n){n^2/7}==7)


#compose() combines any number of functions into one function
lgl <- compose(is.na,mean)
lgl(c(2,4,5,7,89))


#Partial application
#partial() - is used to apply a function to an incomplete set of arguments and 
#then passed on to another function elsewhere. For e.g,
#you can apply partial to bind a dataset to a function before applying that
#function to another function elsewhere

some_function <- function(x,y,z1,z2){
    results <- glm(x~y+z1+z2^2)
    return(predict(results))
}

test_this <- partial(some_function,x=x[1:50],y=y[1:50],z1=z[1:50])
test_this(z2=z[51:100])

########################################################################
#UDEMY COURSE 1: R, GGPLOT, AND SIMPLE LINEAR REGRESSION
########################################################################


#GRAPHING POINTS

x <- c(2,5,1)
y <- c(6,4,9)

#it's nice to plot points in ggplot by putting it in a dataframe
df <- data.frame(x,y)

ggplot() #creates a layer
ggplot()+geom_point(data=df,aes(x=x,y=y),size=5,color="blue")


#let's control the axes
#we control the axes by adding the "scale_x_..." . the ...has many options
#we choose continuous
ggplot()+geom_point(data=df,aes(x=x,y=y),size=5,color="blue")+
scale_x_continuous(limits = c(0,15),breaks = seq(0,15,1))+
scale_y_continuous(limits = c(0,15),breaks = seq(0,15,1))

#more about color and shapes

#We change the shape of the point by adding the "shape=" argument
ggplot()+geom_point(data=df,aes(x=x,y=y),size=5,color="blue", shape=10)+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,1))+
    scale_y_continuous(limits = c(0,15),breaks = seq(0,15,1))


#GRAPHING A LINE
#we use "geom_line" 
ggplot()+geom_line(data=df,aes(x=x,y=y),size=1,color="blue")+
    scale_x_continuous(limits = c(0,15),breaks = seq(0,15,1))+
    scale_y_continuous(limits = c(0,15),breaks = seq(0,15,1))

#using the equation of a line
x1 <- c(1,2,5,6,3,7)
y1 <- 3*x1+1
df1 <- data.frame(x1,y1)
ggplot()+geom_line(data=df1,aes(x=x1,y=y1),size=1,color="blue")


#randomization
#we use the sample() function to do a sampling with replacement
?sample
sample(1:10,100,replace = TRUE)

#sampling from the normal distribution
set.seed(1) #for reproducibility
rnorm(100,50,10)

#plot a vertical sample i.e one axis has the same value and the 
#corresponding axis has values spread around this fixed value
x2 <- rep(1,times=100)
y2 <- rnorm(100,50,10)

df2 <- data.frame(x2,y2)
y3 <- 50
df3 <- data.frame(1,y3) #50 is the mean of the y values

#now plot
ggplot()+geom_point(data = df2,aes(x=x2,y=y2))+
    geom_point(data = df3,aes(x=x2[1],y=y3), size=7,color="red")

#task:plot 3 vertical samples
i1 <- c(rep(1,times=100),rep(9,100),rep(15,100))

set.seed(2)
j1 <- c(rnorm(100,50,10),rnorm(100,30,10),rnorm(100,78,10))

dfi <- data.frame(i1,j1)
i <- c(1,9,15)
j <- c(50,30,78)
dfj <- data.frame(i,j)

ggplot()+geom_point(data = dfi,aes(x=i1,y=j1))+
    geom_point(data = dfj,aes(x=i,y=j), color="red", size=5)


#' generate 100 data points in the following way:
#' The x-coordinates are drawn from a normal distribution
#' of mean 10 and standard deviation 5
#' For each x-value, one y-value is drawn from a normal 
#' populatiopn with mean 3x+1 and standard deviation 10
#' 

set.seed(3)
x <- rnorm(100,10,5)
y <- 3*x+1
means <- data.frame(x,y)

ggplot()+geom_point(data = means, aes(x=x,y=y), color="red")+
    geom_line()
xlim <- c(-5,25)
ylim <- 3*x+1
line <- data.frame(xlim,ylim)

ggplot()+geom_point(data = means, aes(x=x,y=y), color="red")+
    geom_line()+geom_line(data = means, aes(x=x,y=y))+
    scale_x_continuous(limits=c(-10,30))+
    scale_y_continuous(limits=c(-20,80))

x1 <- means$x
y1 <- sapply(x1,function(x)rnorm(1,3*x1+1,10))
df1 <- data.frame(x1,y1)

ggplot()+geom_point(data = means, aes(x=x,y=y), color="red")+
    geom_line()+geom_line(data = means, aes(x=x,y=y))+
    scale_x_continuous(limits=c(-10,30))+
    scale_y_continuous(limits=c(-20,80))+
    geom_point(data = df1, aes(x=x1,y=y1), color="blue")


#Using real data
library("UsingR")

#view data
View(father.son)

#visualize the data
ggplot() + geom_point(data = father.son,aes(x=fheight,y=sheight))

#guessing the fitting line
xlim <- c(60,75)
ylim <- c(63,78)
line.fit <- data.frame(xlim,ylim)
ggplot() + geom_point(data = father.son,aes(x=fheight,y=sheight))+
    geom_line(data = line.fit,aes(x=xlim,y=ylim))   


#residual visualization
mydat <- father.son
names(mydat) <- c("x","y")
mydat$group <- 1:dim(mydat)[1]

#we are guessing that the best fitting line is y=x+3
mydat.fit.x <- mydat$x
mydat.fit.y <- mydat$x+3
mydat.fit.group <- 1:dim(mydat)[1]
mydat.fit <- data.frame(mydat.fit.x,mydat.fit.y,mydat.fit.group)

#visualize the two data frames
ggplot()+geom_point(data = mydat,aes(x=x,y=y))+
    geom_point(data = mydat.fit,aes(x=mydat.fit.x,y=mydat.fit.y),color="red")

#add the fitting line
xlim <- c(50,80)
ylim <- xlim+3
line.fit <- data.frame(xlim,ylim)
ggplot()+geom_point(data = mydat,aes(x=x,y=y))+
    geom_point(data = mydat.fit,aes(x=mydat.fit.x,y=mydat.fit.y),color="red")+
    geom_line(data = line.fit,aes(x=xlim,y=ylim))  

#task: we want to connect the red dots with the corresponding data points
#with a line

#make sure the columns have the same names
head(mydat)
head(mydat.fit)
names(mydat.fit) <- c("x","y","group")

Dat <- rbind(mydat,mydat.fit)
head(Dat)

ggplot()+geom_point(data = mydat,aes(x=x,y=y))+
    geom_point(data = mydat.fit,aes(x=mydat.fit.x,y=mydat.fit.y),color="red")+
    geom_line(data = line.fit,aes(x=xlim,y=ylim))+
    geom_line(data = Dat,aes(x=x,y=y,group=group))
#the distance from a black point to the corresponding red point
#is a residual

SSE <- sum((mydat$y-mydat.fit$y)^2)

#Fitting the least squares line
results <- lm(y~x,data = mydat)
slope <-results$coefficients[2] 
Interc <- results$coefficients[1]
xlim <- c(57,80)
ylim <- slope*xlim+Interc
line <- data.frame(xlim,ylim)
ggplot()+geom_point(data = mydat,aes(x=x,y=y))+
    geom_point(data = mydat.fit,aes(x=mydat.fit.x,y=mydat.fit.y),color="red")+
    geom_line(data = line,aes(x=xlim,y=ylim),size=1.5,color="blue")+
    geom_line(data = Dat,aes(x=x,y=y,group=group))

#Now we know the best fit so we will do a proper fit
x <- mydat.fit$x
y <- slope*x+Interc
group <- 1:dim(mydat)[1]
means <- data.frame(x,y,group)

ggplot()+geom_point(data = mydat,aes(x=x,y=y))+
    geom_point(data = means,aes(x=x,y=y),color="red")+
    geom_line(data = line,aes(x=xlim,y=ylim),size=1.5,color="blue")+
    geom_line(data = Dat,aes(x=x,y=y,group=group))

#making predictions with the least squares line
ggplot()+geom_line(data = line,aes(x=xlim,y=ylim),size=1.5)
  
#given one point, say x(father's height) or y(son's height), we can predict the 
#other. For example, given a father is 70cm tall, how tall is his son?
x <- 70
y <- slope*x+Interc
y
df <- data.frame(x,y)
ggplot()+geom_line(data = line,aes(x=xlim,y=ylim))+
    geom_point(data = df,aes(x=x,y=y),size=5)


########################################################################
#UDEMY COURSE 2: POLYNOMIAL REGRESSION, R, AND GGPLOT
########################################################################

#plotting a function in ggplot

f <- function(x){
    return(5*x+1)
}

#define the limits of x and plot it
x <- c(0,10)
axis <- data.frame(x)

ggplot()+stat_function(data=axis,aes(x=x),fun=f)

#reading in the data
dat <- read.csv("Dat.csv")

####fitting a line
x <- c(min(dat$x),max(dat$x))
axis <- data.frame(x)

#visualize the data
ggplot()+geom_point(data = dat, aes(x=x,y=y))
results <- lm(y~x,data = dat)
slope <- results$coefficients[2]
inter <- results$coefficients[1]

f <- function(x){
    return(slope*x+inter)
}

ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=f,show.legend = TRUE)



#we want to point out the points on the fitting line that correspond
#to each data point
x <- dat$x
y <- f(x)
means <- data.frame(x,y)
ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=f,show.legend = TRUE)+
    geom_point(data = means, aes(x=x,y=y),color="red",size=3)

###visualizing the residuals by grouping the points
dat$group <- 1:dim(dat)[1]
means$group <- 1:dim(means)[1]

#put the two data points together

groups <- rbind(dat,means)
ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=f,show.legend = TRUE)+
    geom_point(data = means, aes(x=x,y=y),color="red",size=3)+
    geom_line(data=groups,aes(x=x,y=y,group=group),color="blue")

SSE <- sum((dat$y-means$y)^2)

####fitting a polynomial of degree two
results <- lm(y~x+I(x^2),data = dat)
Interc <- results$coefficients[1]
x.sq.coef <- results$coefficients[3]
x.coef <- results$coefficients[2]

polyf <- function(x){
    return(Interc + x.coef*x + x.sq.coef*x^2)
}
#plot
ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=polyf)
   
means$y <- polyf(means$x)
#plot
ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=polyf)+
    geom_point(data = means, aes(x=x,y=y),color="red",size=3)
    
groups <- rbind(dat,means)
#plot
ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=polyf,show.legend = TRUE)+
    geom_point(data = means, aes(x=x,y=y),color="red",size=3)+
    geom_line(data=groups,aes(x=x,y=y,group=group),color="blue")

SSE.polyf <- sum((dat$y-means$y)^2)
SSE.polyf



####fitting a polynomial of degree three
x <- dat$x
y <- polyf3(x)
means <- data.frame(x,y)

###visualizing the residuals by grouping the points
dat$group <- 1:dim(dat)[1]
means$group <- 1:dim(means)[1]

results <- lm(y~x+I(x^2)+I(x^3),data = dat)
Interc <- results$coefficients[1]
x.cb.coef <- results$coefficients[4]
x.sq.coef <- results$coefficients[3]
x.coef <- results$coefficients[2]

polyf3 <- function(x){
    return(Interc + x.coef*x + x.sq.coef*x^2+x.cb.coef*x^3)
}
#plot
ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=polyf3)

means$y <- polyf3(means$x)
#plot
ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=polyf3)+
    geom_point(data = means, aes(x=x,y=y),color="red",size=3)

groups <- rbind(dat,means)
#plot
ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=polyf3,show.legend = TRUE)+
    geom_point(data = means, aes(x=x,y=y),color="red",size=3)+
    geom_line(data=groups,aes(x=x,y=y,group=group),color="blue")

SSE.polyf3 <- sum((dat$y-means$y)^2)
SSE.polyf3

####################################################################
###fitting with a smoothing spline
####################################################################


means <- data.frame(x,y)

dat$group <- 1:dim(dat)[1]
means$group <- 1:dim(means)[1]

fit <- smooth.spline(dat$x,dat$y,df=101)
# NB:the smaller the df, the smoother the curve

library("splines")
#we want to make y predictions
spf <- function(x){
    return(predict(fit,x)$y)
}

means$y <- predict(fit,means$x)$y

ggplot()+geom_point(data = dat, aes(x=x,y=y))+
    stat_function(data = axis,aes(x=x),fun=spf)+
    geom_point(data = means, aes(x=x,y=y),color="red",size=3)#+
    #geom_line(data=groups,aes(x=x,y=y,group=group),color="blue")


SSE.fit <- sum((dat$y-means$y)^2)
SSE.fit

attach(iris)
plot(Sepal.Length,Sepal.Width, pch=4)
plot(Sepal.Length,Petal.Length, pch=15)
plot(Sepal.Length,Petal.Width,pch=19)

par(mfrow=c(1,3))
plot(Sepal.Length,Sepal.Width, pch=4)
plot(Sepal.Length,Petal.Length, pch=15)
plot(Sepal.Length,Petal.Width,pch=19)

par(mfrow=c(3,1))
plot(Sepal.Length,Sepal.Width, pch=4)
plot(Sepal.Length,Petal.Length, pch=15)
plot(Sepal.Length,Petal.Width,pch=19)

par(mfrow=c(2,2))
plot(Sepal.Length,Sepal.Width, pch=4)
plot(Sepal.Length,Petal.Length, pch=15)
plot(Sepal.Length,Petal.Width,pch=19)
detach(iris)
