library("e1071")
library("rminer")
setwd("/home/cainan/IC/perceptron/SVM")

read_database <- function(){
  archive <- ""
  cat("Que número deseja treinar? (0 ... 9) ou 10 para treinar todos") 
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
  else if(learn == 10)
    archive <- "all"
  else if(learn == 11)
    archive <- "numbers"
  
  return (archive)
}

start <- function(){
  archive <- read_database()
  base <- paste("../numbers/",archive,".csv", sep = "", collapse = "")
  database <- read.csv(base)
  attach(database)
  
  h <- holdout(database$class, ratio = 0.75, mode = "stratified")
  training <- database[h$tr,]
  teste <- database[h$ts,]
  
  inputsForTrain <- subset(training, select = -class)
  test <- subset(training, select = class)
  
  model <- svm(inputsForTrain, test, type = "C-classification", kernel = "radial")
  summary(model)
  
  # 
  # plot(cmdscale(dist(database[,-1])),
  #      col = as.integer(database[,36]),
  #      pch = c("o","+")[1:150 %in% model$index + 1])
  
  
  # plot(model, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
  #pred <- predict(model,inputsForTrain)
  #m <- table(pred,test)
  # print(model) 
  # summary(model)
  
  
  }

start()
# Base de Dados: Flávius Jr. Vulgo Fi do Dono