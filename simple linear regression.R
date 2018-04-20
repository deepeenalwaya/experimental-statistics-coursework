rm(list = ls())

# install the textbook's R package (only need to do this once)
# install.packages("alr4")

# load the package for use in this session
library(alr4)

# load one of the alr4 datasets into the workspace
data(Htwt)
Htwt
Htwt$ht   # height variable
Htwt$wt   # weight variable

# for the lecture, we'll use the fort collins weather data
data(ftcollinstemp)

# make a time series plot of the data
plot(ftcollinstemp$year,ftcollinstemp$fall,type = "o",ylim = c(15,54),
     xlab = "Year", ylab = "Temperature (F)")
lines(ftcollinstemp$year,ftcollinstemp$winter,type="o",col="blue")
legend("bottomright", c("Fall", "Winter"), col=c("black","blue"),lty=c(1,1),pch=c(1,1))

# Can we predict the winter temperature from the fall temperature?
plot(ftcollinstemp$fall,ftcollinstemp$winter,
     xlab = "Fall Temp. (F)",ylab = "Winter Temp. (F)")

# plot with the years
plot(ftcollinstemp$fall,ftcollinstemp$winter,
     xlab = "Fall Temp. (F)",ylab = "Winter Temp. (F)",type="n")
text(ftcollinstemp$fall,ftcollinstemp$winter,ftcollinstemp$year-1900,cex=3/4)



# do the regression calculations manually first
x = ftcollinstemp$fall
y = ftcollinstemp$winter
n = length(x)
xbar = mean(x)
ybar = mean(y)
SXX = sum( (x - xbar)^2 )
SXY = sum( (x - xbar)*(y-ybar) )
beta1hat = SXY/SXX
beta0hat = ybar - beta1hat*xbar

sigmahat = sqrt( 1/(n-2)*sum( (y - beta0hat - beta1hat*x)^2 ) )

SEbeta0 = sigmahat*sqrt( 1/n + xbar^2/SXX )
SEbeta1 = sigmahat/sqrt(SXX)

t0 = beta0hat/SEbeta0
t1 = beta1hat/SEbeta1

p0 = 2*( 1 - pt(abs(t0),n-2))
p1 = 2*( 1 - pt(abs(t1),n-2))


# R makes this very easy!
# fit the regression model
m1 = lm(winter ~ fall , data = ftcollinstemp) # date = sdhks needed if they are part of a dataframe, and the same one   # linear model # regression of response on (~) covariate
# look at the results
summary(m1)


# predictions and prediction intervals
x01 = 46
x02 = 52

Ey01 = beta0hat + beta1hat*x01
Ey02 = beta0hat + beta1hat*x02

SEy01 = sigmahat*sqrt(1 + 1/n + (x01-xbar)^2/SXX)
SEy02 = sigmahat*sqrt(1 + 1/n + (x02-xbar)^2/SXX)

points(c(x01,x02),c(Ey01,Ey02),pch=16,col="magenta")
lines(c(x01,x01),Ey01+SEy01*c(-2,2),col="magenta")
lines(c(x02,x02),Ey02+SEy02*c(-2,2),col="magenta")






