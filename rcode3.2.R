#m : number of students
m   <- seq(10, 50, 5)
P.E <- function(m){
  c(Students = m, ProbAtL2SB = 1 - prod((365:(365 - m+1)/365)))
} 
t(sapply(m, P.E))
