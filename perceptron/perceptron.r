print(getwd())
#setwd("/home/cainan/R_projects/perceptron")

database <- read.csv("database.csv")

col <- 2
class <- c(-1,-1,1,1)
LF <- 0.1 #Learning Factor


v <- function(x,w){
  sum <- 0
  for (i in 1:length(x)){
    sum <- sum + (x[i]*w[i])
  }
  return (sum)
}

activation <- function(result){
  if(result != 0)
    return (TRUE)
  else
    return (FALSE)
}

#Função de treinamento
learn <- function(x,w,error){
  for(i in 1:length(w)){
    w[i] <- w[i] + LF*error*x[i] 
  }
  return(w)
}

#Lê da base e insere no vetor x
read_database <- function(row){
  x <- rep(0,3)
  for (i in 1:3){
      x[i] <- database[row,i]
  }
  return (x)
}

perceptron <- function(W){
  w <- W
  for (i in 1:4){
    x <- read_database(i)
    result <- v(x,w)
    active <- activation(result)
    epoch <- 0
    while (result != class(i)){
      error <- class[i] - result
      w <- learn(x,w,error)
      #print(w)
      epoch <- epoch + 1
      result <- v(x,w)
      if(epoch == 90)
        break
    }
    print(result)
  }
}

sort <- runif (3,-1,1)
print(sort)
weights <- c(sort)
perceptron(weights)





