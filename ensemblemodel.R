df <- read.csv(file.choose())
library(caret)

summary(df)

#drop employee count(9), employee number(10), over18(22), standardhours(27)

df_data <- df[-c(9,10,22,27)]
nrow(df_data)
table(df_data$Attrition)
prop.table(table(df_data$Attrition))

#Attrition rate is 16%

##EDA

library(funModeling) 
library(tidyverse) 
library(Hmisc)

basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}


basic_eda(df_data)


#Split data
library(caTools)
set.seed(1000)
sample<-sample.split(df_data,SplitRatio = 0.7)
p_train<-subset(df_data,sample==TRUE)
p_test<-subset(df_data,sample==FALSE)

#Neural Net
library(nnet)
set.seed(1000)
modelNN<-nnet(p_train$Attrition~.,p_train,size=21,rang=0.07,Hess=FALSE,decay=15e-4,maxit=2000)

predictionNN<-predict(modelNN,p_test,type=("class"))
table(predictionNN)
result <- table(p_test$Attrition, predictionNN)
result
accuracyNN <- (result[1]+result[4])/(nrow(p_test))


#CART
library(rpart)
library(rattle)

r.ctrl <- rpart.control(minsplit = 120, minbucket = 12, cp=0, xval=10)
modelCart <- rpart(p_train$Attrition~., data = p_train,control = r.ctrl, method = "class")
prp(modelCart)

PredictionCart <- predict(modelCart, newdata=p_test, type="class")


#CART Accuracy
#Confusion matrix 
resultCart <- table(p_test$Attrition, PredictionCart)


#CART model accuracy
result2 <- (resultCart[1]+resultCart[4])/(nrow(p_test))

##Ensemble model
predictionEnsemble <- data.frame(predictionCart= PredictionCart, predictionNN = predictionNN)

predictionEnsemble$predictionsEnsemble <- as.factor(ifelse(predictionEnsemble$predictionCart=='Yes' | predictionEnsemble$predictionNN=='Yes','Yes','No'))

resultEnsemble <- table(p_test$Attrition, predictionEnsemble$predictionsEnsemble)

accuracyEnsemble <- (resultEnsemble[1]+resultEnsemble[4])/(nrow(p_test))

