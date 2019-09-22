
#Ucitavanje podataka
matematic_students<-read.csv("student-mat.csv",sep=";",stringsAsFactors = FALSE)
summary (matematic_students)
str(matematic_students)
#dataset ima 33 varijabli i 395 observacija
#postoji 17 character varijabli i 16 numeric varijabli
#sada proveravam da li ima nedostajucih vrednosti
apply(matematic_students, MARGIN=2 ,FUN = function(x) unique (x))
apply(matematic_students, 2 ,FUN = function(x) sum(is.na(x)))
#nema nedostajucih vrednosti

str(matematic_students)
#prebacujem character varijable u factor varijable
for (i in 1:ncol(matematic_students)){
  if(is.character(matematic_students[,i])){
    matematic_students[,i]=factor(matematic_students[,i])
  }
}

str(matematic_students)
#varijable Medu, Fedu, traveltime i studytime prebacujem u faktor

matematic_students$Medu<-as.factor(matematic_students$Medu)
matematic_students$Fedu<-as.factor(matematic_students$Fedu)
matematic_students$traveltime<-as.factor(matematic_students$traveltime)
matematic_students$studytime<-as.factor(matematic_students$stu)
str(matematic_students)
#Sada varijablu G3 transformisem u binarnu varijablu i 
#ukoliko je ona veca od 10 njena vrednost ce biti yes u suprotnom ce biti no

matematic_students$G3<-ifelse(matematic_students$G3>10,yes='yes',no='no')
matematic_students$G3<-as.factor(matematic_students$G3)
str(matematic_students)
#vise mi nisu potrebne varijable G1 i G2
matematic_students$G1<-NULL
matematic_students$G2<-NULL

#install.packages("ggplot2")
library(ggplot2)


#Ispitivanje znacajnosti kategorickih i numerickih atributa
#koristim geom_bar za kategoricke atribute

#za school(dobra za predikciju)
ggplot(data = matematic_students, aes(x = school, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("School") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za sex(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = sex, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("Sex") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za address(dobra za predikciju)
ggplot(data = matematic_students, aes(x = address, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("address") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za famsize(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = famsize, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("famsize") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za Pstatus(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = Pstatus, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("Pstatus") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za Mjob(dobra za predikciju)
ggplot(data = matematic_students, aes(x = Mjob, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("Mjob") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za Fjob(dobra za predikciju)
ggplot(data = matematic_students, aes(x = Fjob, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("Fjob") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za reason(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = reason, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("reason") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za guardian(dobra za predikciju)
ggplot(data = matematic_students, aes(x = guardian, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("guardian") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za schoolsup (dobra za predikciju)
ggplot(data = matematic_students, aes(x = schoolsup, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("schoolsup") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za famsup(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = famsup, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("famsup") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za paid(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = paid, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("paid") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za activities(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = activities, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("activities") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za nursery(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = nursery, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("nursery") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za higher(dobra za predikciju)
ggplot(data = matematic_students, aes(x = higher, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("higher") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za internet(dobra za predikciju)
ggplot(data = matematic_students, aes(x = internet, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("internet") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za romantic(nije dobra za predikciju)
ggplot(data = matematic_students, aes(x = romantic, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("romantic") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))


#za Medu(dobra za predikciju)
ggplot(data = matematic_students, aes(x = Medu, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("medu") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za Fedu(dobra za predikciju)
ggplot(data = matematic_students, aes(x = Fedu, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("fedu") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za traveltime(dobra za predikciju)
ggplot(data = matematic_students, aes(x = traveltime, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("traveltime") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za studytime(dobra za predikciju)
ggplot(data = matematic_students, aes(x = studytime, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("studytime") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))


#za numericke atribute koristim geom_density

#za age(dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = age, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()


#za failures(dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = failures, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()


#za famrel(nije dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = famrel, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za freetime(nije dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = freetime, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

?kruskal.test
#provera preko Kruskal Wallis testa:Nulta hipoteza je da varijabla freetime
#nema uticaj na G3 izlaznu varijablu a Prva hipoteza je da ima uticaj
kruskal.test(freetime ~ G3, data = matematic_students)
#p-value = 0.7795 sto znaci da nema uticaj na izlaznu G3


#za goout(dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = goout, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za Dalc(nije dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = Dalc, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za Walc(dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = Walc, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za health(nije dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = health, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za absences(nije dobra za predikciju)
ggplot(data = matematic_students,
       mapping = aes(x = absences, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

##varijable koje nemaju dovoljan prediktivni potencijal i koje bih uklonila su:
#sex,famsize,pstatus,reason,famsup,paid,sctivities,nursery,romantic,famrel,freetime,dalc,health,absences


matematic_students$sex<-NULL
matematic_students$famsize<-NULL
matematic_students$Pstatus<-NULL
matematic_students$reason<-NULL
matematic_students$famsup<-NULL
matematic_students$paid<-NULL
matematic_students$activities<-NULL
matematic_students$nursery<-NULL
matematic_students$romantic<-NULL
matematic_students$famrel<-NULL
matematic_students$freetime<-NULL
matematic_students$Dalc<-NULL
matematic_students$health<-NULL
matematic_students$absences<-NULL
str(matematic_students)


saveRDS(matematic_students, "matematics_preparation.RData")

