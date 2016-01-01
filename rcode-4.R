#Eample 4.1
#R code 4.1
Watts <- c(40, 60, 75, 100, 120)
n <- length(Watts)
meanWatt <- (1/n) * sum(Watts)
varWatt <- (1/n) * sum((Watts - meanWatt)^2)
ans <- c(meanWatt, varWatt)
ans

#R code 4.2
opar<- par(no.readonly = TRUE)
par(mfrow = c(1,2), pty ="s")
x<- 0:8
px <- dbinom(x, 8, .3)
plot(x, px, type = "h", xlab = "x", ylab = "P(X = x)",
     main = " PDF of X~Bin(8, 0.3)")
xs <- rep( 0:8, round( dbinom(0:8, 8, 0.3)*100000, 0))
plot(ecdf(xs), main="CDF of X~Bin(8, 0.3)", ylab = expression(P(X<=x)), xlab = "x")
par(opar)


#Example 4.2
#R Code 4.3
bino.gen <- function(samples = 10000, n = 20, pi = 50){
  values <- sample(c(0,1), samples * n, replace = TRUE, prob = c(1-pi, pi) )
  value.mat <- matrix(values, ncol = n)
  Successes <- apply(value.mat, 1, sum)
  a1 <- round((table(Successes)/samples), 3)
  b1 <- round (dbinom(0:n, n , pi), 3)
  names (b1) <- 0:n
  hist(Successes, breaks = c((-0.5+0):(n+0.5)), freq = FALSE,
       ylab = "", col = 13, ylim = c(0, max(a1,b1)),
       main = "Theoritical vale superimposed over Histrogram of simulated values")
  x <- 0:n
  fx <- dbinom(x,n,pi)
  lines(x, fx, type ="h")
  lines(x, fx, type ="p", pch = 16)
  list(simulated.distribution = a1, theoritical.distribution = b1)
}

#R code 4.4
set.seed (31)
bino.gen(samples = 1000, n = 5, pi = 0.5)


#R Code 4.5
set.seed(123)
x <- rbinom(1000, 5, 0.5)
table(x)/1000


#Example 4.3
#R code --
sum(dbinom(6:10, 10, 0.33))
1- sum(dbinom(0:5, 10, 0.33))
pbinom( 5, 10, 0.33, lower = FALSE)
1 - pbinom(5, 10, 0.33)
