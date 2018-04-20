data = read.csv("http://www4.stat.ncsu.edu/~guinness/datasets/nozzledata.csv",header=T)

data

nozzletype = data$nozzle
velocity = data$velocity
shape = data$shape

n = length(nozzletype)

nozzlenum=rep(NA,n)
nozzlenum

nozzlenum[nozzletype=="5"]=1
nozzlenum[nozzletype=="4"]=2
nozzlenum[nozzletype=="3"]=3
nozzlenum[nozzletype=="2"]=4
nozzlenum[nozzletype=="9"]=5

nozzlenum

plot(velocity,shape,pch=nozzlenum,xlab="Velocity",ylab="Shape Representation",xlim=c(5,35),ylim=c(0.55,1.2),main="Shape Representation of Water flow vs. Velocity of Water Flow")
legend("bottomright", legend=c("Nozzle 5", "Nozzle 4","Nozzle 3","Nozzle 2","Nozzle 9"), pch=c(1,2,3,4,5), bty='o', cex=0.9)


model_additive = lm(shape ~ velocity + as.factor(nozzletype))
summary(model_additive)

abline(model_additive$coefficients[1],model_additive$coefficients[2])
abline(model_additive$coefficients[1]+model_additive$coefficients[3],model_additive$coefficients[2])
abline(model_additive$coefficients[1]+model_additive$coefficients[4],model_additive$coefficients[2])
abline(model_additive$coefficients[1]+model_additive$coefficients[5],model_additive$coefficients[2])
abline(model_additive$coefficients[1]+model_additive$coefficients[6],model_additive$coefficients[2])

model_interaction = lm(shape ~ velocity * as.factor(nozzletype))
summary(model_interaction)

plot(velocity,shape,pch=nozzlenum,xlab="Velocity",ylab="Shape Representation",xlim=c(5,35),ylim=c(0.55,1.2),main="Shape Representation of Water flow vs. Velocity of Water Flow")
legend("bottomright", legend=c("Nozzle 5", "Nozzle 4","Nozzle 3","Nozzle 2","Nozzle 9"), pch=c(1,2,3,4,5), bty='o', cex=0.9)

abline(model_interaction$coefficients[1],model_interaction$coefficients[2])
abline(model_interaction$coefficients[1]+model_interaction$coefficients[3],model_interaction$coefficients[2]+model_interaction$coefficients[7])
abline(model_interaction$coefficients[1]+model_interaction$coefficients[4],model_interaction$coefficients[2]+model_interaction$coefficients[8])
abline(model_interaction$coefficients[1]+model_interaction$coefficients[5],model_interaction$coefficients[2]+model_interaction$coefficients[9])
abline(model_interaction$coefficients[1]+model_interaction$coefficients[6],model_interaction$coefficients[2]+model_interaction$coefficients[10])

SS0 = sum(model_additive$residuals^2 )
df0=model_additive$df
SS1= sum(model_interaction$residuals^2)
df1=model_interaction$df

f_value= ((SS0-SS1)/(df0-df1))/(SS1/df1)
f_value
p_value= 1- pf(f_value,df0-df1,df1)
p_value

# p value is 0.010 which is statistically significant
# so null hypothesis is rejected
# the reduced model is not an accurate representation of the full model
# thus there is strong evidence of an interaction
