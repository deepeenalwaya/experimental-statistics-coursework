rm(list = ls())

choral = read.csv("http://www4.stat.ncsu.edu/~guinness/datasets/voice_heights.csv",header=T)
choral

gender=choral$gender
voice=choral$voice
height=choral$height

n=length(height)
n

plot(voice,height)

plot(as.factor(voice),height)

voicenum = rep(NA,n)

voicenum[voice=="soprano"]=1
voicenum[voice=="alto"]=2
voicenum[voice=="tenor"]=3
voicenum[voice=="bass"]=4

voicenum

gendernum = rep(NA,n)

gendernum[gender=="female"]=1
gendernum[gender=="male"]=2



plot(voicenum,height)
plot(voicenum+0.05*rnorm(n),height,pch=gendernum,xlab="Voice",ylab="Height")

legend("bottomright", legend=c("females", "males"), pch=c(1,2), bty="o", cex=0.9)


