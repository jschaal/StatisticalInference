#q1
data(mtcars)
md <- mean(mtcars$mpg)
n <- length(mtcars$mpg)
sd <- sd(mtcars$mpg)
a <- .05
z <- qnorm(1-a/2)
md + c(-1,1)*z*sd/sqrt(n)
t.test(mtcars$mpg)

#Q2
sd <- 1
n <- 9
a <- 0.05
x <- sd/sqrt(n)*qt(1-a/2,n-1)
round(x,2)

#Q3
x1 <- subset(mtcars$mpg,mtcars$cyl==4)
x2 <- subset(mtcars$mpg,mtcars$cyl==6)
t.test(x1,x2,var.equal = TRUE)

#q4
n1 <- 9
n2 <- 9
m1 <- 3
m2 <- 1
sd1 <- 1.5
sd2 <-1.8
sp <- (((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
