#stablo odlucivanja

portugal_students_tree <- readRDS("portugal_preparation.RData")
source("Evaluacione_metrike.R")


#ucitavanje paketa i podela dataseta na train i test

#install.packages('caret')
library(caret)

set.seed(1010)
indeksiDrvo <- createDataPartition(portugal_students_tree$G3, p= .80, list = FALSE)
portugal_students_tree.train <- portugal_students_tree[indeksiDrvo,]
portugal_students_tree.test <- portugal_students_tree[-indeksiDrvo,]

#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)


stablo<-rpart(G3~.,data=portugal_students_tree.train,method = 'class')
predikcijaDRVO<-predict(object = stablo,newdata = portugal_students_tree.test,type = 'class')
rpart.plot(stablo)


# Matrica konfuzije
stablo.cm = table(true = portugal_students_tree.test$G3, predicted = predikcijaDRVO)
stablo.cm

# Evaluacione metrike
metrikeDRVO = getEvaluationMetrics(stablo.cm)
#AUC with prob
predikcijaDRVOPROB<-predict(object = stablo,newdata = portugal_students_tree.test,type = 'prob')
ROCKriva<-roc(response = as.numeric(portugal_students_tree.test$G3),
               predictor = predikcijaDRVOPROB[,2] ,
               levels = c(1, 2))
auc<-ROCKriva$auc #0.77
names(auc)<-"AUC"
metrikeDRVO<-c(metrikeDRVO,auc)
metrikeDRVO

#variable importance
stablo$variable.importance

#krosvalidacija, nalazenje najboljeg cp
#install.packages('e1071')
library(e1071)

Folds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by= 0.001))

set.seed(123)
krosvalidacija <- train(G3 ~ .,
                        data = portugal_students_tree.train,
                        method = "rpart",
                        control = rpart.control(minsplit = 10),
                        trControl = Folds,
                        tuneGrid = cpGrid)
krosvalidacija
plot(krosvalidacija)
#uzimam cp=0.016
#Drvo sa najboljim cp
stablo2<-prune(stablo, cp = 0.016)
rpart.plot(stablo2)#plot decision tree 2


#predikcija nad test data setom
predikcijaDRVO2 <- predict(object = stablo2, newdata = portugal_students_tree.test, type = "class")


#matruca konfuzije za stablo2
stablo2.cm <- table(true = portugal_students_tree.test$G3, predicted=predikcijaDRVO2)
stablo2.cm 
stablo.cm

#metrike 
metrikeDRVO2 <- getEvaluationMetrics(stablo2.cm)
#AUC with prob
predikcijaDRVO2PROB<-predict(object = stablo2,newdata = portugal_students_tree.test,type = 'prob')
ROCKriva2<-roc(response = as.numeric(portugal_students_tree.test$G3),
              predictor = predikcijaDRVO2PROB[,2] ,
              levels = c(1, 2))
auc2<-ROCKriva2$auc #0.7898
names(auc2)<-"AUC"
metrikeDRVO2<-c(metrikeDRVO2,auc2)
metrikeDRVO2


#variable importance
stablo2$variable.importance

compare<-data.frame(rbind(metrikeDRVO,metrikeDRVO2),row.names = c("prvo drvo","drugo drvo"))
compare

