library("RSNNS")
setwd("/home/cainan/IC/perceptron/MLP")

save_file <- function(model,pred){
  # Salvando PDF
  pdf("graphics/pdf/erros.pdf", width = 14, height = 8)
    plotIterativeError(model,main = "Modelo") # Linha vermelha: relação do conjunto de teste * erro de teste iterativo
  dev.off()
  pdf("graphics/pdf/predição.pdf", width = 14, height = 8)
    plot(pred[,1],main = "Predição",type = "o", xlab = "Ordem de predição", ylab = "Predição de peso")
    lines(pred[,2], type = "o", col = "blue")
  dev.off()
  
  # Salvando PNG
  png(file = "graphics/png/erros.png")
    plotIterativeError(model,main = "Modelo") # Linha vermelha: relação do conjunto de teste * erro de teste iterativo
  dev.off()
  png(file = "graphics/png/predição.png")
    plot(pred[,1],main = "Predição",type = "o", xlab = "Ordem de predição", ylab = "Predição de peso")
    lines(pred[,2], type = "o", col = "blue")
  dev.off()
}

start <- function(){
  database <- read.csv("../numbers/numbers.csv")
  ###
  numbers <- database[sample(1:nrow(database),length(1:nrow(database))),1:ncol(database)]
  numbersValues <- numbers[,1:4]
  numbersTargets <- decodeClassLabels(numbers[,5])
  numbers <- splitForTrainingAndTest(numbersValues, numbersTargets, ratio=0.15)
  numbers <- normTrainingAndTestSet(numbers)
  model <- mlp(numbers$inputsTrain, numbers$targetsTrain, size=5, learnFuncParams=c(0.1),
               maxit=50, inputsTest=numbers$inputsTest, targetsTest=numbers$targetsTest)
  ###
  # print(weightMatrix(model))
  # print(extractNetInfo(model))
  
  pred <- predict(model,numbers$inputsTest)
  print(summary(pred))
  # plot(pred[,1],main = "Predição",type = "o", xlab = "Ordem de predição", ylab = "Predição de peso")
  # lines(pred[,2], type = "o", col = "blue")
  
          #  Erros de ajuste X Iteração
  # plotIterativeError(model,main = "Modelo") # Linha vermelha: relação do conjunto de teste * erro de teste iterativo
  # save_file(model,pred)
  
}

start()
