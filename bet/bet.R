setwd("/home/cainan/IC/bet")

playRandomly <- function(state,p){
  action <- rbinom(1,min(state,100 - state),runif(1,0.0,1.0))
  if(runif(1,0.0,1.0) <= p)
    s_ <- state + action
  else
    s_ <- state - action
  return (s_)
}

saveFiles <- function(states){
  write.csv(states,"betting.csv")
  png(file = "betting.png")
    plot(states)
  dev.off()
}

createStates <- function(){
  states <- rep(0,101)
  names(states) <- seq(0,100)
  states[["100"]] <- 1
  return (states)
}

main <- function(LF,p){
  counter <- 0
  S <- createStates()
  repeat {
    for(s in 1:99){
      s_ <- as.character(playRandomly(s,p)); s <- as.character(s)
      V <- S[[s_]] - S[[s]]
      S[[s]] <- round((S[[s]] + LF*V),3)
    }
    counter <- counter + 1
    if(counter == 15000)
      break
  }
  saveFiles(S)
}
main(0.01,0.4)
