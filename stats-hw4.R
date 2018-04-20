library(alr4)
data(water)
water

location1=water$OPBPC
location1
location2=water$OPRC
location2
location3=water$OPSLAKE
location3
runoff=water$BSAAM
runoff

plot(location1,runoff,xlab="Snowfall total at location 1 : OPBPC (in inches)",ylab="Stream Runoff in Bishop,CA (in acre-feet)")
plot(location2,runoff,xlab="Snowfall total at location 2 : OPRC (in inches)",ylab="Stream Runoff in Bishop,CA (in acre-feet)")
plot(location3,runoff,xlab="Snowfall total at location 3 : OPSLAKE (in inches)",ylab="Stream Runoff in Bishop,CA (in acre-feet)")

model <- lm(runoff~location1+location2+location3)
summary(model)
betahat = model$coefficients
runoff_estimated = model$fitted.values
e = runoff-runoff_estimated
RSS = sum(e^2)
RSS
sigmasquare = RSS/(43-(3+1))
sigmasquare

model1 <- lm(runoff~location1)
summary(model1)

40.61/502.4
