#question 2
pnorm(70,mean=80,sd=10)

#question 3
qnorm(p = .95,mean=1100,sd=75)

#Question 4
smean <- 1100
ssd <- 75
n <-100
se <- qnorm(.95) * ssd/sqrt(n)
ci <- smean + se
print(ci)

#question 5
choose(5, 4) * .5 ^ 5 + choose(5, 5) * .5 ^ 5

#question 6
lambda <- 15
t <- 1
var <- lambda/t
se <- 15-14
ssd <- 10
n<- 100
q <- se / (ssd/sqrt(n))
pnorm(q) - pnorm(q,lower.tail = FALSE)  
pnorm(q) - pnorm(-q)

#question 7
#r really not needed as the sample is an estimate for what it is sampling, in this case the mean
mean(runif(1000))

#question 8
lamba <- 5
t <- 3
ppois(10,lamba*t)

