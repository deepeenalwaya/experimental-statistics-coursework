

resistivity <- read.csv("http://www4.stat.ncsu.edu/~guinness/datasets/resistivity.csv")
resistivity # 2^5-1 design
n <- length(resistivity$resistivity)
res <- resistivity$resistivity
rA <- resistivity$A
rB <- resistivity$B
rC <- resistivity$C
rD <- resistivity$D
rE <- resistivity$E

# 1D plots
plot(rA,res) # Rule of thumb (not a scientific truth): if you move two of the points and the effect goes away, there may not be an effect
plot(rB,res)
plot(rC,res)
plot(rD,res)
plot(rE,res)

m1 <- lm( res ~ rA+rB+rC+rD+rE)
summary(m1)

# discard D and E cause they dont really have much of an additive effect, so they will likely not cause an interaction.

# three factor interaction model
m2 <- lm( res ~ rA*rB*rC )
summary(m2)

#interaction of C is not so strong so only keeping interaction between A & B

# two factor interaction in A and B, additive in C 
m3 <- lm( res ~ rA*rB + rC )
summary(m3)                    # dof 5



# compare the two models, m3 nested in m2
anova(m3,m2)

# look at some of the means
mean( res[rA == 0 & rB == 0 & rC == 0 ])
mean( res[rA == 1 & rB == 0 & rC == 0 ])
mean( res[rA == 0 & rB == 1 & rC == 0 ])
mean( res[rA == 1 & rB == 1 & rC == 0 ])
mean( res[rA == 0 & rB == 0 & rC == 1  ])
mean( res[rA == 1 & rB == 0 & rC == 1  ])
mean( res[rA == 0 & rB == 1 & rC == 1  ])
mean( res[rA == 1 & rB == 1 & rC == 1  ])
mean( res[rC == 0] )
mean( res[rC == 1] )

# look how the parameters get estimated
X = model.matrix(m3)
solve( t(X) %*% X ) %*% t(X)




# leafspring data
leafspring <- read.csv("http://www4.stat.ncsu.edu/~guinness/datasets/leafspring.csv")
leafspring # 2^6-2 design with 4 center points
n <- length(leafspring$shrinkage)

# plot of the shrinkages ( to get an idea of the distribution )
plot(leafspring$shrinkage,rnorm(n),ylim=c(-10,10),xlab="Shrinkage",ylab="",axes=FALSE)
axis(1,at=seq(0,60,by=10))

# fit the additive model, treating covariates as numeric
m1 <- lm( shrinkage ~ A+B+C+D+E+FF, data=leafspring)
summary(m1)

# looks like A and B are most important, so fit interaction
# model in A and B
m2 <- lm( shrinkage ~ A*B, data = leafspring )
summary(m2)

# plot the data against A, with plotting symbols to indicate B
jittervec = 0.1*rnorm(n)
plot(leafspring$A+jittervec,leafspring$shrinkage,type="n",
     ylim=c(0,65),xlab="A",ylab="Shrinkage", main="plotting symbol is B")
text(leafspring$A+jittervec, leafspring$shrinkage, leafspring$B )

# as an example, plot against C with D as plotting symbols
plot(leafspring$C,leafspring$shrinkage,type="n",ylim=c(0,65),
     main = "plotting symbol is D", xlab = "C", ylab = "shrinkage")
text( leafspring$C, leafspring$shrinkage, leafspring$D )






