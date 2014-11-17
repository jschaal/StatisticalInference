# Q 1
n <- 9
mean <- 1100
sd <- 30
mean + c(-1,1)*qt(.975,n-1)*sd/sqrt(n)

# Q 2
n <- 9
md <- -2
t <- qt(.975,8)
# we want the ci to be (...,0)
# md + t*s/sqrt(n) = 0
s <- -md*sqrt(n)/t
s
md + c(-1,1)*s*t/sqrt(n)

# Q 4
n1 <- 10
n2 <- 10
m1 <- 3
v1 <- .6
m2 <- 5
v2 <- .68
md <- m1-m2
a <- .05
t <- qt(1-a/2,n1+n2-2)

sp <- sqrt(((n1-1)*v1 + (n2-1)*v2)/(n1+n2-2))
semd <- sp*sqrt(1/n1+1/n2)
md + c(-1,1)*t*semd

# Q 6
n1 <- 100
n2 <- 100
m1 <- 4
v1 <- .5^2
m2 <- 6
v2 <- 4
md <- m2-m1
a <- .05
z <- qnorm(1-a/2)

sp <- sqrt(v1/n1 + v2/n2)
md + c(-1,1)*z*sp

#Q 7
n1 <- 9
n2 <- 9
m1 <- -3
m2 <- 1
sd1 <- 1.5
sd2 <- 1.8
md <- m1-m2
a<-.1

t <- qt(1-a/2,n1+n2-2)
sp <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
semd <- sp*sqrt(1/n1+1/n2)
md + c(-1,1)*t*semd
