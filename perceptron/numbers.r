setwd("/home/cainan/IC/perceptron")
getwd()

numbers <- list()

#Combinador Linear 
v <- function(x,w){
  sum <- 0
  for (i in 1:length(w)){
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
  for(i in 1:length(w)){
    w[i] <- w[i] + LF*error*x[i]
  }
  return(w)
}

#Lê da base e insere no vetor x
read_database <- function(line,database,cols){
  x <- c()
  for (i in 1:cols){
    x[i] <- database[line,i]
  }
  return (x)
}

number_decipher <- function(database){
  vector <- database$class
  for(i in 1:length(vector))
    if(vector[i] == 1)
      return (i-1)
}

permute <- function(x){
  position <- runif (1,1,(length(x)-1))
  aux <- x[position]
  x[position] <- x[position + 1]
  x[position + 1] <- aux
  return (x)
}

perceptron <- function(LF,database,name){
  rows <- nrow(database)
  cols <- ncol(database)
  w <- c(runif ((cols - 1),-1,1)) #Pesos sinápticos aleatórios (valores entre -1 e 1)
  errors <-c()
  count <- 1
  epoch <- 1
  ERROR <- TRUE
  while(ERROR){
    ERROR <- FALSE
    for (line in 1:rows){
      x <- read_database(line,database,cols)
      #x <- permute(x)
      result <- v(x,w)
      active <- activation(result)
      #print(active)
      if(!ERROR & (active != x[cols])){
        e <- x[cols] - result #Calculando o erro
        errors[count] <- abs(e)
        count <- count + 1
        w <- training(x,w,e,LF) #Ajustando os pesos
        ERROR <- TRUE
      }
    }
    epoch <- epoch + 1
  }
  
  plot(errors,type = "l",col = "blue",xlab = "",ylab = "Erro", main = name)
  number <- number_decipher(database)
  print(format(data.frame("Número" = number,"Épocas" = epoch,check.names = FALSE)))
  write.csv(errors,paste("errors/",name, sep = "", collapse = "")) #Guardando os erros
  write.csv(w,paste("weights/",name, sep = "", collapse = "")) #Guardando os pesos
}

start <- function(){
  cat("PERCEPTRON - Que número deseja treinar? (0 ... 9)") 
  learn <- readline()
  epoch <- c()
 
  if(learn == 0){
      database <- read.csv("numbers/zero.csv")
      perceptron(0.1,database,"numbers/zero.csv")
  }else if(learn == 1){
    database <- read.csv("numbers/one.csv")
    perceptron(0.1,database,"numbers/one.csv")
  }else if(learn == 2){
    database <- read.csv("numbers/two.csv")
    perceptron(0.1,database,"numbers/two.csv")
  }else if(learn == 3){
    database <- read.csv("numbers/three.csv")
    perceptron(0.1,database,"numbers/three.csv")
  }else if(learn == 4){
    database <- read.csv("numbers/four.csv")
    perceptron(0.1,database,"numbers/four.csv")
  }else if(learn == 5){
    database <- read.csv("numbers/five.csv")
    perceptron(0.1,database,"numbers/five.csv")
  }else if(learn == 6){
    database <- read.csv("numbers/six.csv")
    perceptron(0.1,database,"numbers/six.csv")
  }else if(learn == 7){
    database <- read.csv("numbers/seven.csv")
    perceptron(0.1,database,"numbers/seven.csv")
  }else if(learn == 8){
    database <- read.csv("numbers/eight.csv")
    perceptron(0.1,database,"numbers/eight.csv")
  }else if(learn == 9){
    database <- read.csv("numbers/nine.csv")
    perceptron(0.1,database,"numbers/nine.csv")
  }
  
}
start()
