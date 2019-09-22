#Naivni Bajes

matematic_students_bayes <- readRDS("matematics_preparation.RData")
source("Evaluacione_metrike.R")


#install.packages('bnlearn')
library(bnlearn)
#install.packages('pROC')
library(pROC)
library(ggplot2)
library(caret)

#pretvaranje varijabli goout, Walc i failures u faktor varijable
matematic_students_bayes$failures <- as.factor(matematic_students_bayes$failures)
matematic_students_bayes$goout <- as.factor(matematic_students_bayes$goout)
matematic_students_bayes$Walc <- as.factor(matematic_students_bayes$Walc)

str(matematic_students_bayes)

#shapiro.test za varijablu age
shapiro.test(matematic_students_bayes$age)
#ne podleze noramlnoj raspodeli,pa radim diskretizaciju

matematic_students_bayes$age <- as.numeric(matematic_students_bayes$age)

summary(matematic_students_bayes$age)
ggplot(data=matematic_students_bayes,mapping=aes(x=age))+geom_histogram(bins = 30)


matematic_students_bayes$age <-cut(matematic_students_bayes$age, 
                                   breaks = c(0, 16, 18, max(matematic_students_bayes$age)), 
                                   include.lowest = FALSE, 
                                   right = TRUE, ordered_result = FALSE)


#podela na train i test
set.seed(1010)
indeksiNB <- createDataPartition(matematic_students_bayes$G3, p= .80, list = FALSE)
matematic_bayes.train <- matematic_students_bayes[indeksiNB,]
matematic_bayes.test <- matematic_students_bayes[-indeksiNB,]

# model i predikcija na osnovu modela
library(e1071)
modelNB<-naiveBayes(G3~.,data=matematic_bayes.train)
predikcijaNB<-predict(modelNB,newdata = matematic_bayes.test,type = "class")
print(modelNB)

#sada matrica konfuzije
matricaNB<- table (true = matematic_bayes.test$G3, predicted = predikcijaNB)
matricaNB
metrikeNB1<-getEvaluationMetrics(matricaNB)
metrikeNB1

#preciznost je 71% tj koliko je observacija koje smo predvideli kao pozitivne i stvarno pozitivno,
#odziv:od svih obs. koje su stvarno pozitivne 65% je udeo onih koje smo mi predvidele kao stvarno pozit.
#F1:kolko su metrike stvarno dobre(objedinjuje preciznost i odziv), 68%

#sada ROC kriva(daje prag ver koji ce da pokaze zeljenu osetljivost), nalazimo opt. vrednost parametra treshold pomocu ROC krive
predict.prob <- predict(modelNB, newdata = matematic_bayes.test, type = "raw")
#install.packages('pROC')
library(pROC)
ROCKriva<-roc(response = as.numeric(matematic_bayes.test$G3),
              predictor = predict.prob[,2],
              levels = c(1, 2))
auc<-ROCKriva$auc #0.769
names(auc)<-"AUC"
metrikeNB1<-c(metrikeNB1,auc)
metrikeNB1

plot.roc(ROCKriva, print.thres = TRUE, print.thres.best.method = "youden")
# youden pokazuje lokalni maksimum u kome je zbir specificnosti i osetljivosti najveci.
# sensitivity =0,780, specificity = 0.676, best threshold = 0.464
#krajnja predikcija sa najboljim treshold

krajnjapred<-ifelse(test=predict.prob[,2]>=0.464,yes='yes',no="no")
krajnjapred<-as.factor(krajnjapred)
krajnjapred.matrica<-table(true=matematic_bayes.test$G3,predicted=krajnjapred)
krajnjapred.matrica
metrikeNB2<-getEvaluationMetrics(krajnjapred.matrica)
metrikeNB2<-c(metrikeNB2,auc)
metrikeNB2
compare<-data.frame(rbind(metrikeNB1, metrikeNB2),
           row.names = c("default ts", "roc"))
compare

