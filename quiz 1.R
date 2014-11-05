#Question #1
p.m_u_d <- .17
p.d <- .12
p.m_n_d <- .06
p.m <- p.m_u_d - p.d + p.m_n_d
print(p.m)

#Question #2
print(qunif(min = 0,max = 1,p = .75))

#question #3
p * x = (1-p) *y
p / (1-p) = x/y

#question #5
x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp
sum(x*p)

#question 6
p.p_d <- .75
p.d <- .3
p.n_nd <- .52
p.d_p <- (p.p_d * p.d) / 
    ((p.p_d * p.d)+ (1-p.n_nd)*(1-p.d))
