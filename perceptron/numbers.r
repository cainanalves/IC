getwd()
#setwd("/home/cainan/Inteligência_Computacional/perceptron/")

# 1,1,1,1,1
# 1,0,0,0,1
# 1,0,0,0,1
# 1,1,1,1,1
# 1,0,0,0,1
# 1,0,0,0,1
# 1,1,1,1,1


database <- read.csv("numbers.csv")
rows <- 10
cols <- 37
LF <- 0.1 #Learning Factor

v <- function(x,w){
  sum <- 0
  for (i in 1:(cols - 1)){
    # print(x[i])
    # print(w[i])
    sum <- sum + (x[i]*w[i])
  }
  return (sum)
}

activation <- function(result){
  if(result >= 0)
    return (TRUE)
  return (FALSE)
}

training <- function(result,x,line){
  if(result != database[line,length(x)])
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
  x <- rep(0,cols-1)
  for (j in 1:(cols-1)){
      x[j] <- database[line,j]
  }
  return (x)
}

perceptron <- function(w){
  for (i in 1:rows){
    x <- read_database(i)
    result <- v(x,w)
    active <- activation(result)
    learning <- training(result,x,i)
    epoch <- 0
    while (active && learning){
      error <- database[i,length(x)] - result
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

sort <- runif (36,-1,1)
print(sort)
w <- c(sort)
perceptron(w)





