# Q2
pnorm(93,mean=100,sd=10)

# Q3
## b = event that bulding has  asbestos
## a = positivbe  test for asbestos

p.a_b <- .93
p.ac_bc <- .88
p.b <- 0.05

p.bc <- 1- p.b
p.a_bc <- 1 - p.a_b
p.ac_b <- 1- p.ac_bc

p.bc_ac <- p.ac_bc * p.bc / (p.ac_bc*p.bc + p.ac_b*p.b)

# Q4
mean <- 100
sd <- 10
n <- 50
pvar <-  sd^2/n
psd <- sqrt(pvar)
qnorm(.95,mean=mean, sd=pvar)

# Q5
choose(6,5)*0.5^6 + choose(6,6)*0.5^6
pbinom(4,size=6,prob = 0.5,lower.tail = FALSE)  ## prob 5 or greater since this is a discrete dist

# Q6
mean <- 0.5
sd <- sqrt(1/12)
n <- 100
psd <- sd/sqrt(n)
round(pnorm(.51,mean=mean,sd = psd,lower.tail = FALSE),3)

# Q7
dice <- 1:6
mean <-  sum(dice)/length(dice)
var <- sum((dice-mean)^2)/length(dice)
var2 <- sum(dice^2)/length(dice) - mean^2
n <- 10
pvar <- var/n

# Q8
l <- 16.5
t <- 2
ppois(20,lambda = l*t)*100

# manipulate
library(manipulate)
library(ggplot2)
k <- 1000
xvals <- seq(-5,5,length=k)
myplot <- function(df) {
    d <- data.frame(y = c(dnorm(xvals),dt(xvals,df)), 
                    x = xvals,
                    dist =factor(rep(c("Normal","T"),c(k,k))))
    g <- ggplot(d,aes(x=x,y=y))
    g <- g + geom_line(size=2,aes(color=dist))
    g
}

manipulate(myplot(mu),mu = slider(1,20,step=1))


#t test
g1 <- sleep$extra[1 : 10]
g2 <- sleep$extra[11 : 20]
x1 <- g1
x2 <- g2

difference <- g2 - g1
mn <- mean(difference)
#s <- sd(difference)
n <- 10

n1 <- length(g1)
n2 <- length(g2)
spj <- sqrt(((n1-1)*sd(g1)^2 + (n2-1)*sd(g2)^2)/(n1+n2-2))

Xj <- mean(g1)
Yj <- mean(g2)
mdj <- Yj-Xj
cj <- mdj  + c(-1,1) * qt(1-.05/2,n1+n2-2)*spj*sqrt(1/n1+1/n2)

sp <- sqrt( ((n1 - 1) * sd(x1)^2 + (n2-1) * sd(x2)^2) / (n1 + n2-2))
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1 / n1 + 1/n2)
rbind(
    md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd,  
    t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,
    t.test(g2, g1, paired = TRUE)$conf
)


## chick weight manual T tests

library(datasets)
data(ChickWeight)
library(reshape2)
##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
                 gain = time21 - time0
)

g <- ggplot(ChickWeight, aes(x = Time, y = weight, 
                             colour = Diet, group = Chick))
g <- g + geom_line()
g <- g + stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1, col = "black")
g <- g + facet_grid(. ~ Diet)
g

g <- ggplot(wideCW, aes(x = factor(Diet), y = gain, fill = factor(Diet)))
g <- g + geom_violin(col = "black", size = 2)
g

wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
    t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
    t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)

x1 <- wideCW14$gain[wideCW14$Diet==1 & !is.na(wideCW14$gain)]
x4 <- wideCW14$gain[wideCW14$Diet==4 & !is.na(wideCW14$gain)]

md1 <- mean(x1)
md4 <- mean(x4)
md <- md1-md4

sd1 <- sd(x1)
sd4 <- sd(x4)

n1 <- length(x1)
n4 <- length(x4)

sp <- sqrt(((n1-1)*sd1^2 + (n4-1)*sd4^2) / (n1 + n4-2))
semd <- sp * sqrt(1/n1 + 1/n4)

r1 <- md + c(-1,1)*qt(.975,n1+n4-2)*semd
r2 <- t.test(gain ~ Diet,paired = FALSE, var.equal = TRUE,data=wideCW14)$conf

r <- rbind(r1,r2)
r

# two side test
n <- 16
sd <- 10
mean <- 30
mu <- 32
ts <- sqrt(n)*(mu-mean)/sd
a <- .05
t <- qt(1-a/2,n-1)
abs(ts)>t

# t test example
t.test(father.son$sheight-father.son$fheight)
# or
t.test(father.son$sheight,father.son$fheight,paired=TRUE)
# manual
x1 <- father.son$sheight
x2 <- father.son$fheight
diffs <- x1-x2
md <- mean(diffs)
sd <- sd(diffs)
n <- length(diffs)
a <- 0.05
ts <- sqrt(n)*(md)/sd
t <- qt(1-a/2,n-1)

ts
t
abs(ts)>t
se <- sd/sqrt(n)
md + c(-1,1)*qt(1-a/2,n-1)*se

kids <- 8:0
cumsum(choose(8,kids)*.5^8)
## if this case since 7/8 is < 5%, we reject the hypothesis that p=.5
