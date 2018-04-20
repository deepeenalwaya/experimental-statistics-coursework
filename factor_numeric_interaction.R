# save your work before running this
rm(list=ls())


library(alr4)
data(BGSall)
names(BGSall)

attach(BGSall)

# how is sex coded?
Sex
mean(HT18[Sex==0])
mean(HT18[Sex==1])
# boys are zero, girls are 1

# make some plots
plot(HT2,HT9,xlab="Height at age 2 (cm)", ylab ="Height at age 9 (cm)")
title("Berkeley Guidance Study Data")

plot(HT9,HT18,xlab="Height at age 9 (cm)", ylab ="Height at age 18 (cm)")
title("Berkeley Guidance Study Data")

plotsym = c(1,16)
plot(HT9,HT18,xlab="Height at age 9", ylab ="Height at age 18",pch=plotsym[Sex+1])
title("Berkeley Guidance Study Data")
legend("topleft",legend=c("Boys","Girls"),pch=plotsym)

# cor(HT2[Sex==0],HT9[Sex==0]) finding correlations

# model just depending on height at age 9
m1 = lm(HT18 ~ HT9 )
summary(m1)
model.matrix(m1)

# additive model in height at age 9 and sex
m2 = lm(HT18 ~ HT9 + Sex)             # beta10 is zero in R's constraint
summary(m2)
model.matrix(m2)


# why didn't we need to say as.factor(Sex)?
# this is because there are only two levels
m2a = lm(HT18 ~ HT9 + as.factor(Sex))
summary(m2a)
model.matrix(m2a)

# for factors with more than two levels, you wil need
# to use as.factor() if the variable is not already
# interpreted by R as a factor


# interaction model in height at age 9 and sex
m3 = lm(HT18 ~ HT9 * Sex)
summary(m3)
model.matrix(m3)

# same model
m3a = lm(HT18 ~ HT9 + Sex + HT9:Sex )
summary(m3a)

# can we force R to use a different parameterization?
m3b = lm(HT18 ~ HT9 * Sex - 1)
summary(m3b)
# that's not right!


# let's define the design matrix ourselves
X = cbind( (1-Sex), (1-Sex)*HT9, Sex, Sex*HT9 )
m3c = lm(HT18 ~ X)
summary(m3c)
# almost! we need to tell it not to add an intercept

m3d = lm(HT18 ~ X - 1)
summary(m3d)
summary(m3)
# that's it!



# looks like the interaction effect, if any, is not strong
# but we should do an F test to make sure
SS0 = sum( m2$residuals^2 )
df0 = m2$df

SS1 = sum(m3$residuals^2 )
df1 = m3$df

Fstat = (SS0-SS1)/(df0-df1)/(SS1/df1)
pvalue = 1-pf(Fstat,df0-df1,df1)
pvalue

summary(m3)

# using the anova function
anova(m2,m3)
anova(m1,m2,m3)


# make a plot with the additive model fits
plotsym = c(1,16)
plot(HT9,HT18,xlab="Height at age 9", ylab ="Height at age 18",pch=plotsym[Sex+1])
abline( m2$coefficients[1],m2$coefficients[2])
abline( m2$coefficients[1]+m2$coefficients[3], m2$coefficients[2],lwd=2 )
title("Berkeley Guidance Study Data")
legend("topleft",legend=c("Boys","Girls"),pch=plotsym)

plotsym = c(1,16)
plot(HT9,HT18,xlab="Height at age 9", ylab ="Height at age 18",pch=plotsym[Sex+1])
abline( m2$coefficients[1],m2$coefficients[2])
abline( m2$coefficients[1]+m2$coefficients[3], m2$coefficients[2] )
abline( m3$coefficients[1],m3$coefficients[2],col="blue")
abline( m3$coefficients[1]+m3$coefficients[3]
        ,m3$coefficients[2]+m3$coefficients[4],col="blue")
title("Berkeley Guidance Study Data")
legend("topleft",legend=c("Boys","Girls"),pch=plotsym)




# what about other predictors? Height at age 2 
# is good predictor of height at age 18
plot(HT2,HT18,xlab="Height at age 2 (cm)", ylab ="Height at age 18 (cm)",pch=plotsym[Sex+1])
title("Berkeley Guidance Study Data")
legend("topleft",legend=c("Boys","Girls"),pch=plotsym)

# model with just height at age 2 and sex
m4 = lm(HT18 ~ HT2 + Sex)
summary(m4)

# model with both heights and sex
m5 = lm(HT18 ~ HT2 + HT9 + Sex)
summary(m5)
# what's going on here?




# are the variances of the two groups different?
mgirls = lm(HT18 ~ HT9, subset = Sex==1)
summary(mgirls)

mboys = lm(HT18 ~ HT9, subset = Sex==0)
summary(mboys)

sigmasqgirls = sum( mgirls$residuals^2 )/mgirls$df
sigmasqboys = sum( mboys$residuals^2 )/mboys$df

Fstat = sigmasqgirls/sigmasqboys
onesidedpvalue = 1 - pf(Fstat,mgirls$df,mboys$df)


twosidedpvalue = pf(1/Fstat,mboys$df,mgirls$df) + 1-pf(Fstat,mgirls$df,mboys$df)
























