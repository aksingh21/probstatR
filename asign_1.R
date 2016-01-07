locA <- c(2.87, 2.16, 3.14, 2.51, 1.80, 3.01, 2.16 )
locB <- c(3.23, 3.45, 2.78, 3.77, 2.97, 3.53, 3.01 )
locC <- c(2.25, 3.13, 2.44, 2.54, 3.27, 2.81, 1.36 )
score <- c(locA, locB, locC)

loc <- rep(c('A','B','C'), c(7,7,7)) 
location <- factor(loc)

df <- data.frame(score , location) 
rm(locA, locB, locC, score, loc, location)
df
