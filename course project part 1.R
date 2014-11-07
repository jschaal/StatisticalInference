lambda <- 0.2
n <- 40

sim <- replicate(n=1000,expr = mean(rexp(n,lambda)))
simHist <- hist(sim,breaks="Scott")

#h<-hist(g, breaks=10, density=10, col="lightgray", xlab="Accuracy", main="Overall") 
xfit<-seq(min(sim),max(sim),length=1000) 
yfit<-dnorm(xfit,mean=mean(sim),sd=sd(sim)) 
yfit <- yfit*diff(simHist$mids[1:2])*length(sim) 
lines(xfit, yfit, col="blue", lwd=2)
