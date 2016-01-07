#Example 6.1
#R code --
combn(x=1:5, m =3)

#Example 6.2
#R code 6.1
library(PASWR2)
t(srs(popvalue = c(2,5,8,12,13), n= 3))

#Example 6.3
#R code 6.2
set.seed(13)
sample(x = 1:180, size = 5, replace = FALSE, prob = rep(1/180,180))


#Example 6.4
#R code 6.3
set.seed(13)
sample(x= 1:20, size = 5, replace = FALSE, prob = c(rep(1/26, 18), rep(4/26,2) ))

#R code 6.4
set.seed(13)
seq(sample(1:100,1),1000,100)

#Example 6.5
#R code 6.5
set.seed(13)
seq(sample(1:50,1),1000,20)

#Example 6.6
#R code 6.6
N <- 6
n <- 2
pop <- 1:N
rs <- expand.grid(Draw1 = pop, Draw2 = pop) #possible random sample
xbarN <- apply (rs, 1, mean) # mean of all rs values
s2N <- apply(rs, 1, var) # variance of all randome sample
RSV <-  cbind(rs, xbarN = xbarN, s2N = s2N )
head(RSV, n = 1) # First one row values for case 1 (random sampling)

# R code 6.7
library(MASS)
fractions(xtabs(~xbarN)/36)
fractions(xtabs(~s2N)/36)

#R code 6.8
T1 <- fractions(xtabs(~xbarN)/36)
T2 <- fractions(xtabs(~s2N)/36)
XBAR <- as.numeric(names(T1))    # Unique values of xbar
S2 <- as.numeric(names(T2))      # Unique values of s2
MU_xbarN <- sum(XBAR*T1)         # Expected value of xbarN
MU_xbarN

VAR_xbarN <- sum((XBAR - MU_xbarN)^2*T1) # var of xbarN
VAR_xbarN

MU_s2N <- sum(S2*T2)
MU_s2N


#R code 6.9
draw <- c("Draw1", "Draw2")
SRS <- srs(1:N, n) #possible simple random sampling
dimnames(SRS) <- list(null, draw)
xbarn <- apply(SRS, 1, mean)
s2n <- apply(srs, 1, var)
