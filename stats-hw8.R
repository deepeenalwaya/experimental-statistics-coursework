data = read.csv('http://www4.stat.ncsu.edu/~guinness/datasets/carpet_age.csv',header=T)
data
attach(data)

model_additive = lm(age ~ cys_acid + met)
summary(model_additive)

model_interaction = lm(age ~ cys_acid * met)
summary(model_interaction)

ss0 = sum(model_additive$residuals^2)
ss0
df0 = model_additive$df
df0

ss1 = sum(model_interaction$residuals^2)
ss1
df1 = model_interaction$df
df1

df_n = df0-df1
df_n

df_d = df1
df_d

fstat = ((ss0-ss1)/(df0-df1))/(ss1/df1)
fstat

variance_additive = ss0/df0
variance_additive

stdev_additive = sqrt(variance_additive)
stdev_additive
