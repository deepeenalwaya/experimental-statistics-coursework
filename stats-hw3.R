data(Htwt)
Htwt
y=Htwt$wt
x=Htwt$ht
plot(x,y,xlab="Height (cm)", ylab="Weight (kg)", pch=15, xlim=c(150,190),ylim=c(40,80), main="Scatterplot of Weight vs Height")
abline(lm(y~x))
summary(model)
