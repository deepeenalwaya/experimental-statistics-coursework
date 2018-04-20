data = read.table('http://www4.stat.ncsu.edu/~guinness/datasets/battery.txt',header=T)

# plot the data
type = data[,1]
temp = data[,2]
life = data[,3]

plot(temp+3*(type-2),log(life),type='n',axes=F,ylab="Lifetime (Hours)",xlab="Temperature (F)")
text(temp+3*(type-2),log(life),type) # 3*type-2 is a manual jitter
box()  # njk = 4
axis(1,at=c(15,70,125))
axis(2,at=log(c(20,60,100,140,180)),labels=c(20,60,100,140,180))


atyp = as.factor(type)
atmp = as.factor(temp)
# fit the main effects model
m1 = lm(log(life) ~ atmp + atyp)
model.matrix(m1)
summary(m1)
SS1 = sum( m1$residuals^2 )
df1 = m1$df

# fit the interaction model
m2 = lm(log(life) ~ atmp*atyp)
model.matrix(m2)
summary(m2)


# check if 3 is better at each temperature
# at 15 degrees, test whether tau3 =0 and tau3-tau2=0
vec = c(0,0,0,0, 1, 0, 0, 0, 0)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))


vec = c(0,0,0,-1, 1, 0, 0, 0, 0)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))



# at 70 degrees, test whether tau3+(alphatau)23 = 0 and tau3+(alphatau)23 - tau2 -(alphatau)22 = 0
vec = c(0,0,0,0,1,0,0,1,0)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))


vec = c(0,0,0,-1,1,-1,0,1,0)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))



# at 125 degrees, test whether tau3+(alphatau)33 = 0 and tau3+(alphatau)33 - tau2 -(alphatau)22 = 0
vec = c(0,0,0,0,1,0,0,0,1)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))


vec = c(0,0,0,-1,1,0,-1,0,1)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))




# test for lifetime for type 1 at 70 degrees different from lifetime of type 1 at 15 degrees
vec = c(0,1,0,0,0,0,0,0,0)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))


# test for an interaction between types 3 and 1 at temperatures 15 and 70
vec = c(0,0,0,0,0,0,0,1,0)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))

# 2 and 3 at temperatures 70 and 125
vec = c(0,0,0,0,0,1,-1,-1,1)
est = t(vec)%*%m2$coefficients
se = sqrt(t(vec)%*%vcov(m2)%*%vec)
2*(1-pt(abs(est/se),27))





# check if interaction model is better than additive model
SS2 = sum( m2$residuals^2 )
df2 = m2$df

F21 = (SS1 - SS2)/(df1-df2)/(SS2/df2)
p21 = 1-pf(F21,df1-df2,df2)

anova(m1,m2)



m3 = lm(log(life)~ atmp:atyp )
summary(m3)

m4 = lm(log(life) ~ atmp:atyp - 1 )
summary(m4)





# other models of interest
m3 = lm(log(life) ~ temp*atyp )
summary(m3)


