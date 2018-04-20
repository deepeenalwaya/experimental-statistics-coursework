
rm(list = ls())

# read in the data and assign some variables
data = read.csv("http://www4.stat.ncsu.edu/~guinness/datasets/speedoflight.csv",header=T)
velocity = data$velocity
n = length(velocity)
trialname = data$trial

# get the variable types
class(trialname)
is.numeric(trialname)
is.factor(trialname)

# A first try at a plot
plot(trialname,velocity)

# reorder them
trialnum = rep(NA,n)
trialnum[trialname=="first"] = 1
trialnum[trialname=="second"] = 2
trialnum[trialname=="third"] = 3
trialnum[trialname=="fourth"] = 4
trialnum[trialname=="fifth"] = 5
class(trialnum)
is.numeric(trialnum)
is.factor(trialnum)

# plot again
plot(trialnum,velocity)

# plot as a factor
plot(as.factor(trialnum),velocity)

# jitter the horizontal values
plot(trialnum+0.05*rnorm(n),velocity,xlab="Trial Number")

# this treats trialnum as quantitative
m1 = lm( velocity ~ trialnum )
summary(m1)
plot(trialnum,velocity,xlab="Trial Number")
abline( m1$coefficients[1], m1$coefficients[2]*trialnum )


# This treats trialnum as a factor variable
m2 = lm( velocity ~ as.factor(trialnum) )
summary(m2)

# look at the design matrix
model.matrix(m2)
X = matrix(as.numeric(model.matrix(m2)),nrow=n )
X

cbind(trialnum,X)

mean( velocity[trialnum==1] )
mean( velocity[trialnum==2] )

# covariance matrix for the regression parameters
vcov(m2)
matrix(round(vcov(m2),3),nrow=5)






# transform back to the alpha parameterization
transmat = t(matrix(c(1,0,0,0,0,
                      1,1,0,0,0,
                      1,0,1,0,0,
                      1,0,0,1,0,
                      1,0,0,0,1),nrow=5,byrow=T))
alphahat = t(transmat) %*% m2$coefficients

# look at covariance matrix for the alphas
vcovalpha = t(transmat) %*% vcov(m2) %*% transmat
vcovalpha
matrix(round(vcovalpha,3),nrow=5)


# get the alpha estimates directly
# "-1" tells R not to use an intercept (beta_0)
m3 = lm( velocity ~ as.factor(trialnum) - 1 )
summary(m3)
matrix(round(vcov(m3),3),nrow=5)


# F test for trial variable
# full model is m2
SS1 = sum( m2$residuals^2 )
df1 = m2$df

# reduced model
m4 = lm( velocity ~ 1 )
summary(m4)
SS0 = sum( m4$residuals^2 )
df0 = m4$df

# compute F statistic
Fstat = (SS0-SS1)/(df0-df1)/( SS1/df1 )
pvalue = 1 - pf( F, df0-df1, df1 )

# some plots of the f distribution

xvals = seq(0,10,by=0.01)
fdist = df(xvals,df0-df1,df1)
plot(xvals,fdist,type="l")
abline(0,0)



# compare to p-value from m3
summary(m3)

# compare to p-value from m2
summary(m2)

# F test for whether last four trials are all the same
# only look at last four trials
inds = trialnum > 1

m5 = lm( velocity[inds] ~ as.factor(trialnum[inds]) )
summary(m5)

m6 = lm( velocity[inds] ~ 1 )
summary(m6)

SS1 = sum( m5$residuals^2 )
df1 = m5$df
SS0 = sum( m6$residuals^2 )
df0 = m6$df

Fstat = (SS0-SS1)/(df0-df1)/( SS1/df1 )
pvalue = 1 - pf( F, df0-df1, df1 )


# fit a model in which the last four have the same mean
lastfour = trialnum > 1

m6 = lm( velocity ~ as.factor(lastfour) )
summary(m6)
xvals = seq(0,10,by=0.01)
fdist = df(xvals,1,98)
plot(xvals,fdist,type="l")
abline(0,0)


# anova function can put this in a table
anova(m2)

# anova function can compare models
anova(m4,m2)  # compare constant mean model to full factor model
anova(m4,m6)  # compare constant mean model to equal means among last four
anova(m6,m2)  # compare equal means among last four to full factor model

anova(m4,m6,m2) # compare all three





