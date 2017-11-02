   setwd("/home/cainan/IC/perceptron")
    getwd()
    
    #0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    #0,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0,0,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,0,1,0
    #0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,0
    #0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,0,0,0,1,0
    #0,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,0,1,0,0,0,0,1,1,1,1,0,1,1,1,1,1,0,0,0,1,0,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,1,0,1,0,0,1,0
    #0,1,0,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,1,0,1,0
    #0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,1,0
    #0,1,0,0,0,0,0,1,1,1,1,0,1,0,0,0,1,0,0,1,1,1,0,1,1,1,1,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,0,1,0
    #0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    
    logics <- list("logics/and.csv","logics/or.csv","logics/implies.csv")
    numbers <- list("numbers/zero.csv","numbers/one.csv","numbers/two.csv","numbers/three.csv","numbers/four.csv","numbers/five.csv",
                      "numbers/six.csv","numbers/seven.csv","numbers/eight.csv","numbers/nine.csv")
    
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
          if(active != x[cols]){
            e <- x[cols] - result #Calculando o erro
            errors[count] <- e
            count <- count + 1
            w <- training(x,w,e,LF) #Ajustando os pesos
            ERROR <- TRUE
          }
        }
        epoch <- epoch + 1
      }
      # write.csv(errors,paste("errors/",name, sep = "", collapse = "")) #Guardando os erros
      # write.csv(w,paste("weights/",name, sep = "", collapse = "")) #Guardando os pesos
      return (epoch)
    }
    
    start <- function(){
      cat("PERCEPTRON - O que deseja treinar?\n  1 - Operadores lógicos\n  2 - Dígitos (0 ... 9)") 
      learn <- readline()
      epoch <- c()
      if(learn == 1){
        for(i in logics){
          database <- read.csv(i)
          epoch[i] <- perceptron(0.1,database,i)
        }
        print(format(data.frame("Épocas" = epoch,check.names = FALSE)))
      }else if(learn == 2){
        number <- c()
        for(i in numbers){
          database <- read.csv(i)
          number[i] <- number_decipher(database)
          epoch[i] <- perceptron(0.1,database,i)
        }
        print(format(data.frame("Número" = number,"Épocas" = epoch,check.names = FALSE)))
      }
        
    }
    start()
