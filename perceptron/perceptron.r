setwd("/home/cainan/IC/perceptron/")
print(getwd())

# 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
# 1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,0,1
# 1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1
# 1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,0,0,0,1
# 1,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,0,0,1,0,0,0,0,1,1,1,1,1,0,1,1,1,1,0,0,0,0,1,0,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,1,0,1,0,0,1
# 1,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,1,0,1
# 1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,1
# 1,0,0,0,0,0,1,1,1,1,1,0,1,0,0,0,1,0,0,1,1,1,0,1,1,1,1,1,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,0,1
# 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

database <- read.csv("database.csv")

rows <- nrow(database)
cols <- ncol(database)

#######################
#class <- c(0,0,1,1) ##
#class <- c(0,0,0,1) ## AND
 class <- c(0,1,1,1) ## OR
#class <- c(1,1,0,1) ## IMPLIES
#######################


#Combinador Linear
v <- function(x,w){
  sum <- 0
  for (i in 1:cols){
    sum <- sum + (x[i]*w[i])
  }
  return (sum)
}

#Função de ativação
activation <- function(result){
  if(result >= 0)
    return (1)
  return (0)
}


#Função de treinamento
training <- function(x,w,error,LF){
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

learn <- function(active,line){
  if(active != class[line])
    return (TRUE)
  return (FALSE)
}

perceptron <- function(LF){
  w <- c(runif (3,-1,1))
  print(w)
  epoch <- 0
  for (i in 1:rows){
    x <- read_database(i)
    learning <- TRUE
    while (learning){
      result <- v(x,w)
      #print(result)
      active <- activation(result)
      error <- class[i] - active #Calculando o erro
      w <- training(x,w,error,LF) #Ajustando os pesos
      #print(w)
      learning <- learn(active,i)
      epoch <- epoch + 1
    }
    print(active)
  }
  format(data.frame("--- Épocas ---" = epoch,check.names = FALSE))
}

perceptron(0.1)





