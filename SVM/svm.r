library("e1071")
library("rminer")
setwd("/home/cainan/IC/perceptron/SVM")

save_file <- function(model,pred){
  # Salvando PDF
  pdf("graphics/pdf/modelo.pdf", width = 14, height = 8)
  plot(model$labels,type = "o",xlab = "Ordem de aprendizagem",ylab = "Número aprendido",main = "Modelo",col = "blue")
  dev.off()
  pdf("graphics/pdf/predição.pdf", width = 14, height = 8)
  plot(pred,xlab = "Números",ylab = "Quantidade",main = "Predição")
  dev.off()
  
  # Salvando PNG
  png(file = "graphics/png/modelo.png")
  plot(model$labels,type = "o",xlab = "Ordem de aprendizagem",ylab = "Número aprendido",main = "Modelo",col = "blue")
  dev.off()
  png(file = "graphics/png/predição.png")
  plot(pred,xlab = "Números",ylab = "Quantidade",main = "Predição")
  dev.off()
}

start <- function(){
  database <- read.csv("../numbers/numbers.csv")
  attach(database)
  
  h <- holdout(database$class, ratio = 0.75, mode = "stratified")
  training <- database[h$tr,]
  teste <- database[h$ts,]
  
  inputsForTrain <- subset(training, select = -class)
  test <- subset(training, select = class)
  
  inputsForTrain2 <- subset(teste, select = -class)
  test2 <- subset(teste, select = class)
  
  model <- svm(inputsForTrain, test, type = "C-classification", kernel = "radial")
  summary(model)
  
  pred <- predict(model,inputsForTrain2)
  # save_file(model,pred)
  
  # plot(model$labels,type = "o",xlab = "Ordem de aprendizagem",ylab = "Número aprendido",main = "Modelo",col = "blue")
  # plot(pred,xlab = "Números",ylab = "Quantidade",main = "Predição")
  
  }

start()
# Base de Dados: Flávius Jr. Vulgo Fi do Dono