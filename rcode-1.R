#Example  1.6
SIMcraps <- function(n.games = 10000){
  opar <- par(no.readonly = TRUE)
  options (scipen = 999) # supress scientific notation in th eoutput
    #linebet returns 0 or 1 based on whether 'shooter'
    #looses or wins
  linebet <- function(){
    comeoutroll <- sum(sample(1:6,2 , replace = TRUE)) # first throw
    if(comeoutroll %in% c(7,11)){
      result <- 1 #win if comeout roll is 
    }else if(comeoutroll %in% c(2, 3, 12)){
      result <- 0
    } else{
      repeat{
        substhrow <- sum(sample(1:6,2,replace = TRUE))
        #subsequent throw
        if(substhrow == comeoutroll){
          result <- 1 # win if substhrow same as comeout roll
          break
        }else if(substhrow == 7 ){
          result <- 0 #loss if susbsthrow is 7
          break
        }
      }
    }
   result 
  }
    
  gameOutcome <- numeric (n.games) # vectors of zero of length n.games
  
  #play n.games simulated crap games
  for(i in 1:n.games){
    gameOutcome[i] <- linebet()
  }
  
  #gameoutcome is vector of wins and looses
  P.win <- mean(gameOutcome) #percentage of time shooter wins
  Actual.answer <- 244/495
  Error <- round(abs(Actual.answer - P.win)/Actual.answer *100, 4)
  cat("Simulated probabbility of win =", P.win, "based on ", n.games, "simulated games.","\n")
  cat("percentage simulation error based on actual answer 244/495 is ", Error,"%.","\n",sep =" ")
  par(opar)
  options(scipen = 0)
}