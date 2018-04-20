factor_A = c(1,2,1,2,1,2,1,2)
factor_B = c(1,1,2,2,1,1,2,2)
factor_C = c(1,1,1,1,2,2,2,2)

A = as.factor(rep(factor_A,8))
A
B = as.factor(rep(factor_B,8))
B
C = as.factor(rep(factor_C,8))
C
tester = as.factor(rep(1:8,each=8))
tester

rating = c(11,15,9,16,10,12,10,15,9,10,12,17,11,13,12,12,10,16,11,15,15,14,13,15,10,14,11,12,8,13,10,13,11,12,11,13,6,9,7,12,10,9,11,13,8,13,7,12,8,6,11,11,9,14,17,9,9,15,12,11,14,9,13,14)

# part a and b : full 3 factor interaction model 

model_1 = lm(rating ~ A*B*C)

model.matrix(model_1)
summary(model_1)

# part e : full 3 factor interaction model with additive tester effect

model_2 = lm(rating ~ A*B*C + tester)

model.matrix(model_2)
summary(model_2)

ss0 = sum(model_1$residuals^2)
ss0
ss1 = sum(model_2$residuals^2)
ss1
df0 = model_1$df
df0
df1 = model_2$df
df1

dfn = df0 - df1
dfn
dfd = df1
dfd

fstat = (ss0-ss1)/(dfn)/(ss1/dfd)
fstat

# P value is 0.0658 which is not quite statistically significant.

# part f : full interaction model with average rating of each brownie batch

rating_average = c(9.75,12.125,11,13.5,10.125,12.125,11.125,12.75)

model_3 = lm( rating_average ~ as.factor(factor_A)*as.factor(factor_B)*as.factor(factor_C) )
summary(model_3)

# part g : full interaction model using stdev of ratings as response

stdev_of_tester_ratings = c(1.035098339,3.52288437,0.9258201,2.267786838,3.090885218,2.031009601,3.356762896,1.982062418)

model_4 = lm( stdev_of_tester_ratings ~ as.factor(factor_A)*as.factor(factor_B)*as.factor(factor_C))
summary(model_4)

