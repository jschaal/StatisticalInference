#   question 1
#   p.m_u_d = p.m + p.d - p.m_n_d
#   p.m_n_d = p.m + p.d - p.m_u_d
p.m_u_d <- .15
p.d <- .10
p.m <- .09
p.m_n_d <- p.m + p.d - p.m_u_d
print(p.m_n_d)

#question 5
mu <- -4 * .2 + 1*.8
sig2 <- (-4 - mu)^2*.2 + (1-mu)^2*.8

#question 10
p <- c(.1, .2, .3, .4)
x <- 2 : 5 
mu <- sum(x*p)
siq <- sum((x-mu)^2*p)