#RANDOM FOREST##

portugal_students_rf <- readRDS("portugal_preparation.RData")
source("Evaluacione_metrike.R")


#install.packages("randomForest")
library(randomForest)
library(caret)
library(ggplot2)
library(pROC)

set.seed(1010)
indeksiRF <- createDataPartition(portugal_students_rf$G3, p= .80, list = FALSE)
portugal_students_rf.train <- portugal_students_rf[indeksiRF,]
portugal_students_rf.test <- portugal_students_rf[-indeksiRF,]



# Create a Random Forest model with default parameters
modelRF1 <- randomForest(G3 ~ ., data = portugal_students_rf.train, importance = TRUE)
plot(modelRF1)
#kako raste broj stabala, out of bag error se smanjuje, pa cu za sledeci model povecati vrednost ntree parametra

#Number of trees:500
#Number of variables tried at each split:4 
#Error rate is 20.38%.

varImpPlot(modelRF1)

# Predikcija
predikcijaRF <- predict(modelRF1, newdata = portugal_students_rf.test, type = "class")
matricaRF<-table(predikcijaRF, portugal_students_rf.test$G3)
matricaRF
#Evaluacione metrike
metrikeRF<-getEvaluationMetrics(matricaRF)
#AUC with prob
predikcijaPROB1<-predict(object = modelRF1,newdata = portugal_students_rf.test,type = 'prob')
ROCKriva<-roc(response = as.numeric(portugal_students_rf.test$G3),
              predictor = predikcijaPROB1[,2] ,
              levels = c(1, 2))
auc<-ROCKriva$auc #0.780
names(auc)<-"AUC"
metrikeRF<-c(metrikeRF,auc)
metrikeRF

#tune random forest model
str(portugal_students_rf.train) #G3 je 21. varijabla po redu
t<-tuneRF(portugal_students_rf.train[,-21],
          portugal_students_rf.train[,21],
          stepFactor = 2,   
          plot = TRUE,
          ntreeTry = 1500, 
          trace = TRUE,
          improve = 0.01)
#mtry je 2 

modelRF2 <- randomForest(G3 ~ ., 
                         data = portugal_students_rf.train, 
                         mtry=2,ntree=1500,importance = TRUE) 
plot(modelRF2)
varImpPlot(modelRF2)
# Predikcija
predikcijaRF2 <- predict(modelRF2, newdata = portugal_students_rf.test, type = "class")
matricaRF2<-table(predikcijaRF2, portugal_students_rf.test$G3)
#Evaluacione metrike
metrikeRF2<-getEvaluationMetrics(matricaRF2)
#AUC with prob
predikcijaPROB2<-predict(object = modelRF2,newdata = portugal_students_rf.test,type = 'prob')
ROCKriva2<-roc(response = as.numeric(portugal_students_rf.test$G3),
               predictor = predikcijaPROB2[,2] ,
               levels = c(1, 2))
auc2<-ROCKriva2$auc #0.783
names(auc2)<-"AUC"
metrikeRF2<-c(metrikeRF2,auc2)
metrikeRF2

#treci model sa prvih 10 varijabli po MeanDecreaseGini
modelRF3 <- randomForest(G3 ~ failures+absences+Fedu+Medu+school+higher+Walc+Mjob+freetime+age,
                         data = portugal_students_rf.train, 
                         mtry=2,ntree=1500,importance = TRUE) 
plot(modelRF3)
varImpPlot(modelRF3)
# Predikcija
predikcijaRF3 <- predict(modelRF3, newdata = portugal_students_rf.test, type = "class")
matricaRF3<-table(predikcijaRF3, portugal_students_rf.test$G3)
#Evaluacione metrike
metrikeRF3<-getEvaluationMetrics(matricaRF3)
#AUC with prob
predikcijaPROB3<-predict(object = modelRF3,newdata = portugal_students_rf.test,type = 'prob')
ROCKriva3<-roc(response = as.numeric(portugal_students_rf.test$G3),
               predictor = predikcijaPROB3[,2] ,
               levels = c(1, 2))
auc3<-ROCKriva3$auc #0.8
names(auc3)<-"AUC"
metrikeRF3<-c(metrikeRF3,auc3)
metrikeRF3

compare<-data.frame(rbind(metrikeRF, metrikeRF2,metrikeRF3),
           row.names = c("default vrednosti", "ntree i mtry 1500 i 4","sa prvih 10"))
#zakljucujem da iako sam se forusirala na najznacajnije varijable nisam dobila neke znacajnije bolje rezultate