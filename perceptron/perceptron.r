    setwd("/home/cainan/IC/perceptron")
    getwd()
    
    #0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    #0,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0,0,1,1,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,0,1,0
    #0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,0
    #0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,0,0,0,1,0
    #0,1,1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,0,1,0,0,0,0,1,1,1,1,0,1,1,1,1,0,0,0,0,1,0,0,0,1,1,1,1,0,0,1,0,0,0,0,0,1,0,1,0,1,0,0,1,0
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
    
    perceptron <- function(LF,database,name){
      rows <- nrow(database)
      cols <- ncol(database)
      w <- c(runif ((cols - 1),-1,1)) #Pesos sinápticos aleatórios (valores entre -1 e 1)
      epoch <- 0
      ERROR <- TRUE
      while(ERROR){
        ERROR <- FALSE
        for (line in 1:rows){
          x <- read_database(line,database,cols)
          result <- v(x,w)
          active <- activation(result)
          #print(active)
          if(active != x[cols]){
            e <- x[cols] - active #Calculando o erro
            w <- training(x,w,e,LF) #Ajustando os pesos
            ERROR <- TRUE
          }
        }
        epoch <- epoch + 1
      }
      #write.csv(w,paste("weights/",name, sep = "", collapse = "")) #Guardando os pesos
      print(format(data.frame("Épocas" = epoch,check.names = FALSE)))
    }
    
    start <- function(){
      cat("PERCEPTRON - O que deseja treinar?\n  1 - Operadores lógicos\n  2 - Dígitos (0 ... 9)") 
      learn <- readline()
      if(learn == 1){
        databases <- logics
      }else if(learn == 2){
        databases <- numbers
      }
      for(i in databases){
        print(i)
        database <- read.csv(i)
        perceptron(0.1,database,i)
      }
        
    }
    
    start()
    
    