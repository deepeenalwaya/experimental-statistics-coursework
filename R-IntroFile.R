
# make sure we are in the right directory
getwd()   # get working directory
setwd("/Users/jsguinne/Documents/teaching/ST516_2017/st516_2017")
getwd()

# reading in data
cardata <- read.table("http://www4.stat.ncsu.edu/~guinness/datasets/mpg.txt",header=TRUE)


# Everything that exists  in R is an object
# Everything that happens in R is a function call.
# - John Chambers


# see what's in the data object
cardata
class(cardata)    # "class" of the object
names(cardata)    # names of columns of data frame
str(cardata)      # more detailed info
head(cardata)     # first six rows
dim(cardata)      # number of rows and columns
summary(cardata)  # statistical summary


# variable types
typeof(TRUE)
typeof(1L)
typeof(1)
typeof(pi)
typeof("I am a string")
typeof("Integer")


# access a column of the data
cardata$gallons       # gallons variable
cardata[2,3]           # second row, third column
cardata[2,]           # second row 
cardata[ ,2 ]         # second column
cardata["gallons"]
cardata[1,2]          # first row, second column
cardata$gallons[1]    # first entry of gallons variable


# get a subset of the rows based on one of the variables
cardata$gallons > 12
cardata[cardata$gallons > 12 , ]

# a different way to get a subset of the data
subset(cardata, gallons > 12)
# use ? to get documentation for functions
?subset



# for plotting, just type plot(x,y)
plot(cardata$gallons,cardata$mpgactual)
plot(cardata$gallons,cardata$mpgactual,xlab="Gallons",ylab="Actual MPG",cex.lab=2)
# add a title 
title("Actual Miles per Gallon vs. Number of Gallons")
# or
plot(cardata$gallons,cardata$mpgactual,xlab="Gallons",ylab="Actual MPG",
     main = "Actual Miles per Gallon vs. Number of Gallons")
?plot


# can change all sorts of things
plot(cardata$gallons,cardata$mpgactual,xlab="Gallons",ylab="Actual MPG",
     pch = 13, col = "red", col.axis = "blue")
plot(cardata$gallons,cardata$mpgactual,xlab="Gallons",ylab="Actual MPG",cex=3)
plot(cardata$gallons,cardata$mpgactual,xlab="Gallons",ylab="Actual MPG",cex=(1:14)/4)
?par   # to see all the things we can change

# R uses recycling in the "cex = 3" argument (reuses 3 for all points)
# more on that


## Exercise! Make a plot of car mpg against actual mpg 
## with points from 2012 in red and 2013 in black
## label axes
?points()


# assignment can be done with either of two operators
# we prefer to use '<-' because '=' is used in function arguments
x = 4
x <- 4
# alt+-

# print out the value of a variable simply by typing it
x
x + 3
x*3
x^3
cos(x)


# R has built-in functions for manipulating vectors
c(3.4, 5.2, 98, 0)
1:10
1:10+3                    # different behavior than matlab
1:(10+3)                  
seq(1,10)
seq(1,10,by=0.1)
seq(1,10,length.out=5)
?seq
help(seq)
apropos("seq")
example(seq)
rep(0,5)
rep(1:4,5)
rep(1:4,each=5)
vec = seq(0,1,by=0.1)
vec[1]
vec[-1]
vec(1)


# and for matrices
mat <- matrix(1:6,2,3)               # column-major ordering
mat <- matrix(1:6,2,3,byrow=TRUE)    # row-major ordering
t(mat)                               # transpose
cbind( cardata$mpgcar, cardata$mpgactual)  # bind two vectors together

# * and / do elementwise multiplication and division
cardata$mpgactual*cardata$gallons
cardata$miles/cardata$gallons


# %*% is for matrix multiplication
vec <- c(1,2,3)
mat %*% vec
t(vec) %*% t(mat)


# and has support for unstructured lists
x = list( mystring = "a string" , myvec = 1:4 )
x
x$mystring
x$myvec
x[1]
class(x)


## Exercise! create these 3x3 matrices 
# [ 1 2 3 ]      [4 4 4]
# [ 1 2 3 ]      [4 4 4]
# [ 1 2 3 ]      [4 4 4]
# assign them to variables, and matrix multiply them




# many built-in functions and packages operate on data frames
# create with data.frame() function
subcardata = data.frame( gallons = cardata$gallons , mpgactual = cardata$mpgactual )
subcardata$gallons
class(subcardata)


# reading dates
fueldates = as.Date(cardata$date, format="%m/%d/%y")
plot( fueldates , cardata$mpgcar, cex.lab = 2, cex.axis = 2 )
plot( fueldates , cardata$mpgcar, ylim = c(20,32) )
plot( fueldates , cardata$mpgcar, ylim = c(20,32), pch = 8 )

# adding points to a plot
plot( cardata$gallons, cardata$mpgcar, ylim = c(20,32) )
points(cardata$gallons, cardata$mpgactual, pch = 16 )

# add text to a plot
plot( cardata$gallons, cardata$mpgcar, ylim = c(20,32) )
text(8,22," really small text")

# or use text as the plotting symbols! (type = "n" gives empty plot)
plot( fueldates, cardata$mpgcar, ylim = c(20,32), type = "n" )
text(fueldates, cardata$mpgcar, cardata$gallons )

# too many digits
plot( fueldates, cardata$mpgcar, ylim = c(20,32), type = "n" , cex.lab = 2, cex.axis = 2 )
text(fueldates, cardata$mpgcar, round( cardata$gallons, 1), cex = 2 )


# computing probabilities
?pnorm
pnorm(0)
pnorm(-3)
pnorm(0, mean = -2)
pnorm(2)
1 - pnorm(2)          # upper tail
dnorm(0)              # value of the N(0,1) density at 0
1/sqrt(2*pi)          # from the density formula
qnorm(0.975)          # familiar 0.975 upper quantile


pt(3.2,df = 7)        # t distribution probability
dbinom(0:2, 2, 1/2)   # binomial(n=2,p=1/2)
dbinom(0:3, 3, 1/2)   # binomial(n=3,p=1/2)

# simulating random varibles
rnorm(100, mean = 4)               # 100 standard normals
hist(rnorm(1000))        # histogram of normals
hist(rnorm(1000),breaks = 30)
mean(rnorm(10))          # mean of 10 standard normals
mean(rnorm(100))         # of 100
mean(rnorm(1000))        # of 1000
mean(rnorm(1000000))     # of a million

y = rnorm(1000)
max(y)
min(y)
range(y)



## Exercise! simulate 1000 Binomial(48,1/2) values
## and make a histogram with integer breakpoints






# this is really useful! 
# you can define your own functions
addtwonumbers = function(x,y){
  z = x + y + w
  return(z)
}
addtwonumbers(1,2)
addtwonumbers(pi,1)



# fit a linear model (we'll get to this next week)
beta = lm( mpgcar ~ mpgactual ,  data = cardata)


# for a summary of the regression
summary( lm( mpgcar ~ mpgactual, data = cardata) )


# plot the data and a regression line
plot(cardata$mpgactual,cardata$mpgcar)
abline(beta)


# save plots either with the menus
plot(cardata$gallons,cardata$mpgactual,xlab="Gallons",ylab="Actual MPG",
     main = "Actual Miles per Gallon vs. Number of Gallons")


# or do it automatically
pdf(file = "mpgplot.pdf", width = 4, height = 3)
plot(cardata$gallons,cardata$mpgactual,xlab="Gallons",ylab="Actual MPG",
     main = "Actual Miles per Gallon vs. Number of Gallons")
dev.off()


postscript( file = "mpgplot.eps", width = 6, height = 4)
plot(cardata$gallons,cardata$mpgactual,xlab="Gallons",ylab="Actual MPG",
     main = "Actual Miles per Gallon vs. Number of Gallons")
dev.off()



# see what variables we've created
ls()



# remove variables from the workspace
rm(list=ls())




mat = matrix(0,10,4)

write.table( mat, file = "zeromatrix.txt")

write.table( mat, file = "zeromatrix.txt",row.names=FALSE,col.names=FALSE)

write.table( mat, file = "zeromatrix.txt",row.names=FALSE,col.names=FALSE,sep=",")

zeros = rep(0,5)
ones= rep(1,5)
twos = rep(2,5)
save(list=c("zeros","ones","twos"),file = "integers.Rdata")

rm(list = ls())
load("integers.Rdata")








