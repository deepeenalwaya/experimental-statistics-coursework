horsedata <-read.table('http://www4.stat.ncsu.edu/~guinness/datasets/horses.txt',header=T)
horsedata

# clearly we need to transform the data
plot(horsedata$Odds,horsedata$Position,xlab="Odds",ylab="Finish Position")

# a first try at a plot
plot(log(horsedata$Odds),sqrt(horsedata$Position))

# customize the axes
plot(log(horsedata$Odds),sqrt(horsedata$Position),axes=FALSE,xlab="Odds",ylab="Position")
axis(1,at=log(c(2,4,16,64,256)),labels=c(2,4,16,64,256))
axis(2,at=1:4,labels=(1:4)^2)
box()
# Customize the axes
plot(log(horsedata$Odds),sqrt(horsedata$Position),axes=FALSE,xlab="Odds",ylab="Position"
     ,xlim=c(0,7),ylim=c(0.8,5))
axis(1,at=log(c(1,4,16,64,256,1024)),labels=c(1,4,16,64,256,1024),lwd=0,lwd.ticks=1)
axis(2,at=1:5,labels=(1:5)^2,lwd=0,lwd.ticks=1)
box(lwd=2)

# R can plot on the log scale automatically, but vertical axis still weird
plot(horsedata$Odds,sqrt(horsedata$Position),log="x",xlab="Odds",ylab="Position")


# plot position against Ratio of wins
plot(horsedata$Ratio,sqrt(horsedata$Position),axes=FALSE,xlab="Win Ratio",ylab="Position")
box()
axis(1,at = (0:5)/10,labels=(0:5)/10,lwd=0,lwd.ticks=1)
axis(2,at=1:4,labels=(1:4)^2,lwd=0,lwd.ticks=1)


# intercept model
m1 = lm( sqrt(Position) ~ 1, data=horsedata)
summary(m1)

# model with just log(odds) as a predictor
m21 = lm( sqrt(Position) ~ log(Odds), data=horsedata)
# model with just Ratio as a predictor
m22 = lm( sqrt(Position) ~ Ratio, data = horsedata )
anova(m1,m21)
anova(m1,m22)

# add the lines to the plots
par(mfrow=c(1,2))

plot(log(horsedata$Odds),sqrt(horsedata$Position),axes=FALSE,xlab="Odds",ylab="Position"
     ,xlim=c(0,7),ylim=c(0.8,5))
box()
axis(1,at=log(c(1,4,16,64,256,1024)),labels=c(1,4,16,64,256,1024),lwd=0,lwd.ticks=1)
axis(2,at=1:5,labels=(1:5)^2,lwd=0,lwd.ticks=1)
abline(m21$coefficients)

plot(horsedata$Ratio,sqrt(horsedata$Position),axes=FALSE,xlab="Win Ratio",ylab="Position")
box()
axis(1,at = (0:5)/10,labels=(0:5)/10,lwd=0,lwd.ticks=1)
axis(2,at=1:4,labels=(1:4)^2,lwd=0,lwd.ticks=1)
abline(m22$coefficients)

# fit the additive model
m3 = lm( sqrt(Position) ~ log(Odds) + Ratio, data=horsedata)
summary(m3)
anova(m21,m3)
anova(m22,m3)


# see that it is invariant to a rotation
x1 = log(horsedata$Odds)+horsedata$Ratio
x2 = log(horsedata$Odds)-horsedata$Ratio
m31 = lm( sqrt(horsedata$Position) ~ x1 + x2 )
summary(m31)

m31$coefficients[2]+m31$coefficients[3]
m31$coefficients[2]-m31$coefficients[3]
summary(m3)

# interaction model (these are all the same)
m4 = lm( sqrt(Position) ~ log(Odds)*Ratio, data=horsedata)
m41 = lm( sqrt(Position) ~ (log(Odds)+Ratio)^2, data=horsedata)  # m4 and m41 do the same thing

# you might think this fits the quadratic model, but it doesn't
m42 = lm( sqrt(Position) ~ log(Odds)*Ratio + log(Odds)^2 + Ratio^2, data=horsedata)
m43 = lm( sqrt(Position) ~ log(Odds)+Ratio+log(Odds):Ratio, data=horsedata)

anova(m3,m4)

