library("RSNNS")
setwd("/home/cainan/IC/perceptron/MLP")

read_database <- function(){
  cat("Que número deseja treinar? (0 ... 9)") 
  learn <- readline()
  
  if(learn == 0)
    archive <- "zero"
  else if(learn == 1)
    archive <- "one"
  else if(learn == 2)
    archive <- "two"
  else if(learn == 3)
    archive <- "three"
  else if(learn == 4)
    archive <- "four"
  else if(learn == 5)
    archive <- "five"
  else if(learn == 6)
    archive <- "six"
  else if(learn == 7)
    archive <- "seven"
  else if(learn == 8)
    archive <- "eight"
  else if(learn == 9)
    archive <- "nine"
  
  return (archive)
}

save_file <- function(model,archive){
  # Salvando PDF
  archivePDF <- paste("graphics/pdf/",archive,".pdf", sep = "", collapse = "")
  pdf(archivePDF, width = 14, height = 8)
  plotIterativeError(model,main = archive)
  dev.off()
  # Salvando PNG
  archivePNG <- paste("graphics/png/",archive,".png", sep = "", collapse = "")
  png(file = archivePNG)
  plotIterativeError(model,main = archive)
  dev.off()
}

start <- function(){
  archive <- read_database()
  base <- paste("../numbers/",archive,".csv", sep = "", collapse = "")
  database <- read.csv(base)
  data <- data.frame(database)
  inputsForTrain <- data[,c(1,36)]
  model <- mlp(x = inputsForTrain,y = database$class, maxit = 25)
  plotIterativeError(model,main = archive)
  #save_file(model,archive)
  
  # predictions <- predict(model,inputsForTrain)
  # plotRegressionError(predictions,database$class)
}

start()
