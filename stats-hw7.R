data = read.csv('http://www4.stat.ncsu.edu/~guinness/datasets/eysenck.csv',header=T)

attach(data)
Age
Process
Words

words=Words

n = length(Age)
n

agetype=rep(0,n)
processtype=rep(0,n)

agetype[Age=="Older"]='O'
agetype[Age=="Younger"]='Y'
agetype

processtype[Process=="Counting"]=1
processtype[Process=="Rhyming"]=2
processtype[Process=="Adjective"]=3
processtype[Process=="Imagery"]=4
processtype[Process=="Intentional"]=5
processtype

processtype_jittered = processtype+0.15*rnorm(n)

plot(processtype_jittered,words,xaxt='n',type='n',xlab="Process",ylab="Words",xlim=c(1,6),pch=agetype)
axis(1,at=c(1,2,3,4,5),labels=c("Counting","Rhyming","Adjective","Imagery","Intentional"))
text(processtype_jittered,words,agetype)
legend("bottomright",legend=c("Older", "Younger"), pch=c('O','Y'), bty='o', cex=0.9)

processfactor=as.factor(processtype)
agefactor=as.factor(agetype)

model_additive = lm(words~processfactor+agefactor)
summary(model_additive)

model_interaction = lm(words~processfactor*agefactor)
summary(model_interaction)
