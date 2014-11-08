lambda <- 0.2
n <- 40

sim <- replicate(n=1000,expr = mean(rexp(40,lambda)))
simHist <- hist(sim,breaks="Scott")

#h<-hist(g, breaks=10, density=10, col="lightgray", xlab="Accuracy", main="Overall") 
xfit<-seq(min(sim),max(sim),length=1000) 
yfit<-dnorm(xfit,mean=mean(sim),sd=sd(sim)) 
yfit <- yfit*diff(simHist$mids[1:2])*length(sim) 
lines(xfit, yfit, col="blue", lwd=2)

#code block 1
lambda <- 0.2
expMean <- 1/lambda
print(expMean)

#code block 2
set.seed(5555)
n <- 40
simTrial <- mean(rexp(n,lambda))
print(simTrial)

#code block 3
trials <- 1000
sim <- apply(matrix(rexp(n*trials,lambda),trials),1,mean)
simMean <- mean(sim)
simHist <- hist(sim,breaks="Scott",main = "Histogram of Simulation")
yMax <- max(simHist$counts)
meanText <- paste("mean = ",as.character(format(mean(sim),digits=4)))
points(simMean,yMax,col="red",cex=2,pch=21,bg="red")
text(x = simMean+1 ,y = yMax, meanText)


#code block 5
simMean <- mean(sim)
print(simMean)

#code block 6
simSD <- sd(sim)
print(simSD)

#code block7 
simHist <- hist(sim,breaks="Scott",main = "Histogram of Simulation")
xfit<-seq(min(sim),max(sim),length=1000) 
yfit<-dnorm(xfit,mean=mean(sim),sd=sd(sim)) 
yfit <- yfit*diff(simHist$mids[1:2])*length(sim) 
lines(xfit, yfit, col="blue", lwd=2)
points(simMean,yMax,col="red",cex=2,pch=21,bg="red")
text(x = simMean+1 ,y = yMax, meanText)

#code block 8
sim <- apply(matrix(rexp(n*trials,lambda),trials),1,mean)
simHist <- hist(sim,breaks="Scott",main = "Histogram of Simulation")
xfit<-seq(min(sim),max(sim),length=1000) 
yfit<-dnorm(xfit,mean=mean(sim),sd=sd(sim)) 
yfit <- yfit*diff(simHist$mids[1:2])*length(sim) 
lines(xfit, yfit, col="blue", lwd=2)
yMax <- max(simHist$counts)
meanText <- paste("mean = ",as.character(format(mean(sim),digits=4)))
points(simMean,yMax,col="red",cex=2,pch=21,bg="red")
text(x = simMean+1 ,y = yMax, meanText)

#code block 9
trials <- 10000
sim <- apply(matrix(rexp(n*trials,lambda),trials),1,mean)
simHist <- hist(sim,breaks="Scott",main = "Histogram of Simulation")
xfit<-seq(min(sim),max(sim),length=1000) 
yfit<-dnorm(xfit,mean=mean(sim),sd=sd(sim)) 
yfit <- yfit*diff(simHist$mids[1:2])*length(sim) 
lines(xfit, yfit, col="blue", lwd=2)
yMax <- max(simHist$counts)
meanText <- paste("mean = ",as.character(format(mean(sim),digits=4)))
points(simMean,yMax,col="red",cex=2,pch=21,bg="red")
text(x = simMean+1 ,y = yMax, meanText)




#old code block 7
trials <- 10000
sim <- replicate(n=trials,expr = mean(rexp(n,lambda)))
simHist <- hist(sim,breaks="Scott",main = "Histogram of Simulation")
xfit<-seq(min(sim),max(sim),length=1000) 
yfit<-dnorm(xfit,mean=mean(sim),sd=sd(sim)) 
yfit <- yfit*diff(simHist$mids[1:2])*length(sim) 
lines(xfit, yfit, col="blue", lwd=2)


set.seed(1)
n <- 40
trials <- 1000
hist(runif(trials))
sim <- apply(matrix(runif(n*trials),trials),1,mean)
hist(sim)
