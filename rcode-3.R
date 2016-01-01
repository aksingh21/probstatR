#Example 3.2
#R code 3.2.2

#m : number of students
m   <- seq(10, 50, 5)
P.E <- function(m){
  c(Students = m, ProbAtL2SB = 1 - prod((365:(365 - m+1)/365)))
} 
t(sapply(m, P.E))

#Example 3.17
#R code 3.7
opar <- par(no.readonly = TRUE)
library(MASS) #used for fraction function
par(mfrow=c(1,2), pty = "s")
Omega <- expand.grid(coin1 = 0:1, coin2=0:1, coin3=0:1)
n.heads <- apply(Omega, 1, sum) 
cbind(Omega, n.heads)

T1 <- table(n.heads)/length(n.heads)
fractions(T1)

plot(T1, xlab = "x", ylab= "P(X = x)", yaxt = "n", main = "PDF for X")
axis(2, at = c(1/8, 3/8), labels = c("1/8", "3/8"), las =1)

plot(ecdf(n.heads), main = "CDF for X", ylab = "F(x)", xlab = "x", yaxt = "n")
axis(2, at = c(1/8, 4/8, 7/8, 1), labels = c("1/8", "4/8", "7/8", "1"), las = 1)
segments(1,1/8,1,4/8, lty = 2)
text(2.6, 2.5/8, "P(X=1) = F(1) - F(0)")
par(opar)


#Example 3.18
#R code 3.8
x <- c(1, 5, 30)
px <- c(0.5, 0.45, 0.05)
EX <- sum(x * px)
WM <- weighted.mean(x, px)
c(EX, WM)

#Example 3.19
#R code 3.9
x <- c(1, 5, 30)
px <- c(0.5, 0.45, 0.05)
EX <- sum((x-5)*px)
WM <- weighted.mean(x-5, px)
c(EX, WM)


#Example 3.21
#R code 3.10
opar <- par(no.readonly = TRUE) #read current parameters
par(mfrow = c(1,2))  # split device 1 row, 2 columns
f <- function(x){
  y <- 3/4 * (1-x^2)
  y[x < -1 | x >1] <- 0
  return (y)
}
curve(f, -2, 2, xlab = "x", ylab = "f(x)", main= "PDF of X")
F <- function(x){
  y <- -x^3/4 + 3* x/4 + 1/2
  y[x<= -1] <- 0
  y[x > 1] <-1
  return (y)
}

curve(F,-2,2,xlab ="x", ylab = "F(x)", main = "CDF for X")
par(opar)

#R code 3.11
library(ggplot2)
p <- ggplot(data.frame (x = c(-2,2)), aes(x =x))
p + stat_function(fun = f) + labs(x = "x", y = "f(x)", title = "PDF of X")
p + stat_function(fun = F) + labs(x="x", y= "f(x)", title = "CDF of X")

#R code 3.12
fx <- function(x){
  3/4 -3/4*x^2
} #define function of x
integrate(fx, lower = -0.5, upper = 1) # gives value and tolerance
ans <- integrate(fx, lower = -0.5, upper = 1)$value
ans
library(MASS) # for fraction
fractions(ans)

#Example 3.23
#R code 3.13
f <- function(x){ 2* cos(2*x) }
curve(f, 0, pi/4, xlab ="x",ylab="2cos(2x)")
abline(v=pi/12, lty=2 , lwd=2 )
#ggplot2 now
p <- ggplot(data.frame(x=c(0,pi/4)),aes(x=x))
p+ stat_function(fun = f) + labs(x= "x", y = "2cos(2x)")+
  geom_vline(xintercept = pi/12,lty = "dashed")

#Example 3.24
#R code 3.14
x<- seq(-1, 1, length = 10)
y<- dunif(x, -1, 1)
DF<- data.frame(fx = y)
previous_theme <- theme_set(theme_bw()) #set black and white theme
ggplot(data = DF, aes(x = x, y = fx)) +
  geom_area(fill = "skyblue3") +
  labs(x = "x", y="f(x)\n") +
  ylim (c(0,1)) +
  theme_set(previous_theme)

