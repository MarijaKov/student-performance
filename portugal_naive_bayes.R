#Naivni Bajes

portugal_students_bayes <- readRDS("portugal_preparation.RData")
source("Evaluacione_metrike.R")


#install.packages('bnlearn')
library(bnlearn)
#install.packages('pROC')
library(pROC)
library(caret)
library(ggplot2)

str(portugal_students_bayes)
#pretvaranje varijabli failures,famrel,freetime,walc u faktor varijable
portugal_students_bayes$failures <- as.factor(portugal_students_bayes$failures)
portugal_students_bayes$famrel <- as.factor(portugal_students_bayes$famrel)
portugal_students_bayes$freetime <- as.factor(portugal_students_bayes$freetime)
portugal_students_bayes$Walc <- as.factor(portugal_students_bayes$Walc)

str(portugal_students_bayes)

#shapiro.test za varijable age i absences
shapiro.test(portugal_students_bayes$age)
shapiro.test(portugal_students_bayes$absences)
#ne podlezu normalnoj raspodeli,pa radim diskretizaciju

portugal_students_bayes$age <- as.numeric(portugal_students_bayes$age)
portugal_students_bayes$absences <- as.numeric(portugal_students_bayes$absences)

summary(portugal_students_bayes$age)
ggplot(data=portugal_students_bayes,mapping=aes(x=age))+geom_histogram(bins = 30)

summary(portugal_students_bayes$absences)
ggplot(data=portugal_students_bayes,mapping=aes(x=absences))+geom_histogram(bins = 30)

zaDiskretizaciju<-c("age","absences")

diskretizovane<-discretize(data=portugal_students_bayes[,zaDiskretizaciju],
                           method = 'quantile',breaks = c(4,2))


str(diskretizovane)


#ubacivanje diskretizovanih u ds
kolone <- setdiff(names(portugal_students_bayes), names(diskretizovane))
portugal_bayes_new<- data.frame(cbind(portugal_students_bayes[,kolone], diskretizovane))
str(portugal_bayes_new)
#postavljam isti redosled kolona kao sto je bio u originalnom ds
portugal_bayes_new <- portugal_bayes_new[,names(portugal_students_bayes)]
str(portugal_bayes_new)


#podela na train i test
set.seed(1010)
indeksiNB <- createDataPartition(portugal_bayes_new$G3, p= .80, list = FALSE)
portugal_bayes.train <- portugal_bayes_new[indeksiNB,]
portugal_bayes.test <- portugal_bayes_new[-indeksiNB,]

# model i predikcija na osnovu modela
library(e1071)
modelNB<-naiveBayes(G3~.,data=portugal_bayes.train)
predikcijaNB<-predict(modelNB,newdata = portugal_bayes.test,type = "class")
#sada matrica konfuzije
matricaNB<- table (true = portugal_bayes.test$G3, predicted = predikcijaNB)
matricaNB
metrikeNB1<-getEvaluationMetrics(matricaNB)
metrikeNB1
#preciznost je 82% tj koliko je observacija koje smo predvideli kao pozitivne i stvarno pozitivno,
#odziv:od svih obs. koje su stvarno pozitivne 80% je udeo onih koje smo mi predvidele kao stvarno pozit.
#F1:kolko su metrike stvarno dobre(objedinjuje preciznost i odziv), 81%

#sada ROC kriva(daje prag ver koji ce da pokaze zeljenu osetljivost), nalazimo opt. vrednost parametra treshold pomocu ROC krive
predict.prob <- predict(modelNB, newdata = portugal_bayes.test, type = "raw")
#install.packages('pROC')
library(pROC)
ROCKriva<-roc(response = as.numeric(portugal_bayes.test$G3),
              predictor = predict.prob[,2],
              levels = c(1, 2))
auc<-ROCKriva$auc #0.798

names(auc)<-"AUC"
metrikeNB1<-c(metrikeNB1,auc)

plot.roc(ROCKriva, print.thres = TRUE, print.thres.best.method = "youden")
# youden pokazuje lokalni maksimum u kome je zbir specificnosti i osetljivosti najveci.
# sensitivity = 0.700, specificity = 0.744, best threshold = 0.720
#krajnja predikcija sa najboljim treshold

krajnjapred<-ifelse(test=predict.prob[,2]>=0.720,yes='yes',no="no")
krajnjapred<-as.factor(krajnjapred)
krajnjapred.matrica<-table(true=portugal_bayes.test$G3,predicted=krajnjapred)
krajnjapred.matrica
metrikeNB2<-getEvaluationMetrics(krajnjapred.matrica)
metrikeNB2<-c(metrikeNB2,auc)

metrikeNB2

compare<-data.frame(rbind(metrikeNB1, metrikeNB2),
           row.names = c("default ts", "roc"))

compare
