print(getwd())
#setwd("/home/cainan/Inteligência_Computacional/perceptron/")

database <- read.csv("database.csv")

rows <- 4
cols <- 3
class <- c(-1,-1,1,1)
LF <- 0.1 #Learning Factor


v <- function(x,w){
  sum <- 0
  for (i in 1:cols){
    sum <- sum + (x[i]*w[i])
  }
  return (sum)
}

activation <- function(result){
  if(result != 0)
    return (TRUE)
  return (FALSE)
}

training <- function(result,line){
  if(result != class[line])
    return (TRUE)
  return (FALSE)
}

#Função de treinamento
learn <- function(x,w,error){
  for(i in 1:cols){
    w[i] <- w[i] + LF*error*x[i] 
  }
  return(w)
}

#Lê da base e insere no vetor x
read_database <- function(line){
  x <- rep(0,cols)
  for (i in 1:cols){
      x[i] <- database[line,i]
  }
  return (x)
}

perceptron <- function(W){
  w <- W
  for (i in 1:rows){
    x <- read_database(i)
    result <- v(x,w)
    active <- activation(result)
    learning <- training(result,i)
    epoch <- 0
    while (active && learning){
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





