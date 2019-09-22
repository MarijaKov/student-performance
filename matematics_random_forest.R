#RANDOM FOREST##
#https://www.youtube.com/watch?v=dJclNIN-TPo
#https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
#https://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/

matematic_students_rf <- readRDS("matematics_preparation.RData")
source("Evaluacione_metrike.R")


#install.packages("randomForest")
library(randomForest)
library(caret)
library(ggplot2)
library(pROC)

set.seed(1010)
indeksiRF <- createDataPartition(matematic_students_rf$G3, p= .80, list = FALSE)
matematic_students_rf.train <- matematic_students_rf[indeksiRF,]
matematic_students_rf.test <- matematic_students_rf[-indeksiRF,]


# Model Random forest sa predefinisanim vrednostima parametara
modelRF1 <- randomForest(G3 ~ ., data = matematic_students_rf.train, importance = TRUE)
plot(modelRF1)
#kako raste broj stabala, out of bag error se smanjuje, pa cu za sledeci model povecati vrednost ntree parametra

#Number of trees:500
#Number of variables tried at each split:4 
#Error rate is 35.65%.
varImpPlot(modelRF1)


# Predikcija
predikcijaRF <- predict(modelRF1, newdata = matematic_students_rf.test, type = "class")
matricaRF<-table(predikcijaRF, matematic_students_rf.test$G3)
matricaRF
#Evaluacione metrike
metrikeRF<-getEvaluationMetrics(matricaRF)
#AUC with prob
predikcijaPROB1<-predict(object = modelRF1,newdata = matematic_students_rf.test,type = 'prob')
ROCKriva<-roc(response = as.numeric(matematic_students_rf.test$G3),
              predictor = predikcijaPROB1[,2] ,
              levels = c(1, 2))
auc<-ROCKriva$auc #0.745
names(auc)<-"AUC"
metrikeRF<-c(metrikeRF,auc)
metrikeRF


#tune random forest model
str(matematic_students_rf.train) #G3 je 17. varijabla po redu
t<-tuneRF(matematic_students_rf.train[,-17],
          matematic_students_rf.train[,17],
          stepFactor = 2,  
          plot = TRUE,
          ntreeTry = 1500, 
          trace = TRUE,
          improve = 0.01)
#mtry je 4 isto kao i default vrednost

modelRF2 <- randomForest(G3 ~ ., 
                         data = matematic_students_rf.train, 
                         mtry=4,ntree=1500,importance = TRUE) 
plot(modelRF2)
varImpPlot(modelRF2)
# Predikcija
predikcijaRF2 <- predict(modelRF2, newdata = matematic_students_rf.test, type = "class")
matricaRF2<-table(predikcijaRF2, matematic_students_rf.test$G3)
#Evaluacione metrike
metrikeRF2<-getEvaluationMetrics(matricaRF2)
#AUC with prob
predikcijaPROB2<-predict(object = modelRF2,newdata = matematic_students_rf.test,type = 'prob')
ROCKriva2<-roc(response = as.numeric(matematic_students_rf.test$G3),
              predictor = predikcijaPROB2[,2] ,
              levels = c(1, 2))
auc2<-ROCKriva2$auc #0.731
names(auc2)<-"AUC"
metrikeRF2<-c(metrikeRF2,auc2)
metrikeRF2


#treci model sa prvih 10 varijabli po MeanDecreaseGini
modelRF3 <- randomForest(G3 ~ failures+Mjob+goout+age+Fedu+Walc+studytime+Medu+Fjob+traveltime,
                         data = matematic_students_rf.train, 
                         mtry=4,ntree=2000,importance = TRUE)
plot(modelRF3)
varImpPlot(modelRF3)
# Predikcija
predikcijaRF3 <- predict(modelRF3, newdata = matematic_students_rf.test, type = "class")
matricaRF3<-table(predikcijaRF3, matematic_students_rf.test$G3)
#Evaluacione metrike
metrikeRF3<-getEvaluationMetrics(matricaRF3)
#AUC with prob
predikcijaPROB3<-predict(object = modelRF3,newdata = matematic_students_rf.test,type = 'prob')
ROCKriva3<-roc(response = as.numeric(matematic_students_rf.test$G3),
              predictor = predikcijaPROB3[,2] ,
              levels = c(1, 2))
auc3<-ROCKriva3$auc #0.666
names(auc3)<-"AUC"
metrikeRF3<-c(metrikeRF3,auc3)
metrikeRF3

data.frame(rbind(metrikeRF, metrikeRF2,metrikeRF3),
           row.names = c("default vrednosti", "ntree i mtry 1500 i 4","sa prvih 10"))
#zakljucujem da iako sam se forusirala na najznacajnije varijable nisam dobila bolje rezultate

