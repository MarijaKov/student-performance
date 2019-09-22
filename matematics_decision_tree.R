#stablo odlucivanja

matematic_students_tree <- readRDS("matematics_preparation.RData")
source("Evaluacione_metrike.R")


#ucitavanje paketa i podela dataseta na train i test

#install.packages('caret')
library(caret)
table(matematic_students_tree$G3)
set.seed(1010)
indeksiDrvo <- createDataPartition(matematic_students_tree$G3, p= .80, list = FALSE)
matematic_students_tree.train <- matematic_students_tree[indeksiDrvo,]
matematic_students_tree.test <- matematic_students_tree[-indeksiDrvo,]

#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('bnlearn')
library(bnlearn)
#install.packages('pROC')
library(pROC)

stablo<-rpart(G3~.,data=matematic_students_tree.train,method = 'class')
predikcijaDRVO<-predict(object = stablo,newdata = matematic_students_tree.test,type = 'class')
rpart.plot(stablo)
str(predikcijaDRVO)
# Matrica konfuzije
stablo.cm = table(true = matematic_students_tree.test$G3, predicted = predikcijaDRVO)
stablo.cm

# Evaluacione metrike
metrikeDRVO = getEvaluationMetrics(stablo.cm)
#AUC with prob
predikcijaDRVOPROB<-predict(object = stablo,newdata = matematic_students_tree.test,type = 'prob')
ROCKriva<-roc(response = as.numeric(matematic_students_tree.test$G3),
                        predictor = predikcijaDRVOPROB[,2] ,
                        levels = c(1, 2))
auc<-ROCKriva$auc #0.6892
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
                        data = matematic_students_tree.train,
                        method = "rpart",
                        control = rpart.control(minsplit = 10),
                        trControl = Folds,
                        tuneGrid = cpGrid)
krosvalidacija
plot(krosvalidacija)
#uzimam cp 0.05
#Drvo sa najboljim cp
stablo2<-prune(stablo, cp = 0.05)
rpart.plot(stablo2)#plot decision tree 2

#predikcija nad test data setom
predikcijaDRVO2 <- predict(object = stablo2, newdata = matematic_students_tree.test, type = "class")

#matruca konfuzije za stablo2
stablo2.cm <- table(true = matematic_students_tree.test$G3, predicted=predikcijaDRVO2)
stablo2.cm 
stablo.cm

#evaluacione metrike 
metrikeDRVO2 <- getEvaluationMetrics(stablo2.cm)
#AUC with prob
predikcijaDRVO2PROB<-predict(object = stablo2,newdata = matematic_students_tree.test,type = 'prob')
ROCKriva2<-roc(response = as.numeric(matematic_students_tree.test$G3),
              predictor = predikcijaDRVO2PROB[,2] ,
              levels = c(1, 2))
auc2<-ROCKriva2$auc #0.5606
names(auc2)<-"AUC"
metrikeDRVO2<-c(metrikeDRVO2,auc2)
metrikeDRVO2

#variable importance
stablo2$variable.importance

#Poredjenje metrika za oba modela
compare<-data.frame(rbind(metrikeDRVO,metrikeDRVO2),row.names = c("prvo drvo","drugo drvo"))
compare


