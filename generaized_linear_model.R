
# logistic regression on 2003 NFL field goal data
fieldgoal <- read.csv("http://www4.stat.ncsu.edu/~guinness/datasets/fieldgoal2003.csv",header=TRUE)
attach(fieldgoal)

# meh
plot(yards,success)

# define total # of tries and successes at each yardage
yardvec <- seq(min(yards),max(yards))
successrate <- rep(0,length(yardvec))
numsuccess <- successrate
numtotal <- successrate
for( j in seq(along=yardvec) ){
  numsuccess[j] <- sum(yards==yardvec[j] & success==1)
  numtotal[j] <- sum(yards==yardvec[j])
}
successrate <- numsuccess/numtotal

# better plot
plot(yardvec,successrate,ylim=c(0,1),xlab="Yards",ylab="Success Rate")


# even better
plot(yardvec,successrate,ylim=c(0,1.1),cex=sqrt(numtotal)/2,
     xlab="Yards",ylab="Success Rate")

# fit glm using binomial responses with logit link
m1 <- glm( cbind( numsuccess, numtotal-numsuccess ) ~ yardvec, family = binomial(link="logit") )
summary(m1)

# plot with estimated success rates
plot(yardvec,successrate,ylim=c(0,1.1),cex=sqrt(numtotal)/2,
     xlab="Yards",ylab="Success Rate")
lines( yardvec, m1$fitted.values )


# clean up
detach(fieldgoal)



# seattle monthly rainfall total data
seattle = read.csv("http://www4.stat.ncsu.edu/~guinness/datasets/seattle.csv",header=TRUE)
attach(seattle)
month = factor(monthname,
               levels=c("jan","feb","mar","apr",
                        "may","jun","jul","aug",
                        "sep","oct","nov","dec"))

# meh
plot(month,precip)

# better
plot(monthnum+(year-2010)/30,precip,xlab="month",
     ylab="precipitation (inches)",axes=FALSE)
box()
axis(1,at=1:12,labels=levels(month))
axis(2,at=seq(0,16,by=2))
title("Precipitation in Seattle, WA, Jan 2005- Mar 2016")

# sqrt transformation
plot(monthnum+(year-2010)/30,sqrt(precip),xlab="month",
     ylab="precipitation (inches)",axes=FALSE,ylim=c(0,4))
box()
axis(1,at=1:12,labels=levels(month))
axis(2,at=(1:4),labels=(1:4)^2)

# simple analysis: fit linear model to sqrt(precip)
sinmonth = sin(2*pi*monthnum/12)
cosmonth = cos(2*pi*monthnum/12)
m1 = lm(sqrt(precip) ~ sinmonth + cosmonth )
summary(m1)
sigma <- summary(m1)$sigma


# replot sqrt data with fitted values
plot(monthnum+(year-2010)/30,sqrt(precip),xlab="month",
     ylab="precipitation (inches)",axes=FALSE,ylim=c(0,4))
box()
axis(1,at=1:12,labels=levels(month))
axis(2,at=(1:4),labels=(1:4)^2)
mon = (1:12)
cf1 = m1$coefficients
meanvec <- cf1[1]+cf1[2]*sin(2*pi*mon/12) + cf1[3]*cos(2*pi*mon/12) 
lines( mon, meanvec )
for(j in 1:12 ) lines( rep(j,2), meanvec[j] + 2*sigma*c(-1,1), col = "magenta")


# replot original data with fitted values
plot(monthnum+(year-2010)/30,precip,xlab="month",
     ylab="precipitation (inches)",axes=FALSE,ylim=c(0,16))
box()
axis(1,at=1:12,labels=levels(month))
axis(2,at=seq(0,16,by=4))
mon = 1:12
cf1 = m1$coefficients
medianvec <- cf1[1]+cf1[2]*sin(2*pi*mon/12) + cf1[3]*cos(2*pi*mon/12) 
lines( mon, medianvec^2 )
for(j in 1:12 ) lines( rep(j,2), (medianvec[j] + 2*sigma*c(-1,1))^2, col = "magenta")






# fit a generalized linear model
# by numerical maximization of the likelihood
precip[precip==0] = 1e-6 # doesn't handle exact zeros well
m2 <- glm( precip ~ sinmonth + cosmonth, family = Gamma(link="log") )
summary(m2)
cf2 = m2$coefficients





# plot the mean function
meanvec <- exp( cf2[1]+cf2[2]*sin(2*pi*mon/12)+cf2[3]*cos(2*pi*mon/12) )
plot(monthnum+(year-2010)/30,precip,xlab="month",
     ylab="precipitation (inches)",axes=FALSE)
box()
axis(1,at=1:12,labels=levels(month))
axis(2,at=seq(0,16,by=2))
lines(mon,meanvec)
for(j in 1:12){
  lo <- qgamma(0.025,meanvec[j]/dsp,scale=dsp)
  hi <- qgamma(0.975,meanvec[j]/dsp,scale=dsp)
  lines( rep(j,2), c(lo,hi), col="magenta")
}


# plot densities for a few months
dsp <- summary(m2)$dispersion
x <- seq(0,16,by=0.1)
jandens <- dgamma(x,meanvec[1]/dsp,scale=dsp)
juldens <- dgamma(x,meanvec[7]/dsp,scale=dsp)
aprdens <- dgamma(x,meanvec[4]/dsp,scale=dsp)
plot(x,jandens,type="l",ylim=c(0,0.8),xlab="Inches",ylab="Density")
points(precip[month=="jan"],rep(0.1,sum(month=="jan")))
text(8,0.25,"Jan")
lines(x,juldens)
points(precip[month=="jul"],rep(0.6,sum(month=="jul")))
text(1.5,0.7,"Jul")
lines(x,aprdens)
points(precip[month=="apr"],rep(0.3,sum(month=="apr")))
text(3.0,0.45,"Apr")






