
#Ucitavanje podataka
portugal_students<-read.csv("student-por.csv",sep=";",stringsAsFactors = FALSE)
summary (portugal_students)
str(portugal_students)
#dataset ima 33 varijabli i 649 observacija
#postoji 17 character varijabli i 16 numeric varijabli
#sada proveravam da li ima nedostajucih vrednosti
apply(portugal_students, MARGIN=2 ,FUN = function(x) unique (x))
apply(portugal_students, 2 ,FUN = function(x) sum(is.na(x)))
#nema nedostajucih vrednosti

str(portugal_students)
#prebacujem character varijable u factor varijable
for (i in 1:ncol(portugal_students)){
  if(is.character(portugal_students[,i])){
    portugal_students[,i]=factor(portugal_students[,i])
  }
}

str(portugal_students)
#varijable Medu, Fedu, traveltime i studytime prebacujem u faktor

portugal_students$Medu<-as.factor(portugal_students$Medu)
portugal_students$Fedu<-as.factor(portugal_students$Fedu)
portugal_students$traveltime<-as.factor(portugal_students$traveltime)
portugal_students$studytime<-as.factor(portugal_students$stu)
str(portugal_students)



#Sada varijablu G3 transformisem u binarnu varijablu i 
#ukoliko je ona veca od 10 njena vrednost ce biti yes u suprotnom ce biti no
#pozitivna klasa je Yes
portugal_students$G3<-ifelse(portugal_students$G3>10,yes='yes',no='no')
portugal_students$G3<-as.factor(portugal_students$G3)
str(portugal_students)
#vise mi nisu potrebne varijable G1 i G2
portugal_students$G1<-NULL
portugal_students$G2<-NULL

# install.packages("ggplot2")

library(ggplot2)


#Ispitivanje znacajnosti kategorickih i numerickih atributa
#koristim geom_bar za kategoricke atribute

#za school(dobra za predikciju)
ggplot(data = portugal_students, aes(x = school, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("School") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za sex (dobra za predikciju)
ggplot(data = portugal_students, aes(x = sex, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("Sex") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))
#provera preko chisquare testa
library(MASS)       
tbl = table(portugal_students$G3,portugal_students$sex)
tbl                  
chisq.test(tbl) #ukoliko je vrednost manja od 0.05 odbacujemo nultu hipotezu(da nema uticaj na izlaznu varijablu)
#vrednost je 0.01 sto znaci da sex ima uticaj na G3

#za address(dobra za predikciju)
ggplot(data = portugal_students, aes(x = address, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("address") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za famsize(nije dobra za predikciju)
ggplot(data = portugal_students, aes(x = famsize, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("famsize") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za Pstatus(nije dobra za predikciju)
ggplot(data = portugal_students, aes(x = Pstatus, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("Pstatus") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za Mjob(dobra za predikciju)
ggplot(data = portugal_students, aes(x = Mjob, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("Mjob") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za Fjob(dobra za predikciju)
ggplot(data = portugal_students, aes(x = Fjob, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("Fjob") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za reason(dobra za predikciju)
ggplot(data = portugal_students, aes(x = reason, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("reason") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za guardian(dobra za predikciju)
ggplot(data = portugal_students, aes(x = guardian, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("guardian") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za schoolsup (nije dobra za predikciju)
ggplot(data = portugal_students, aes(x = schoolsup, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("schoolsup") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za famsup(nije dobra za predikciju)
ggplot(data = portugal_students, aes(x = famsup, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("famsup") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za paid(nije dobra za predikciju)
ggplot(data = portugal_students, aes(x = paid, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("paid") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za activities(nije dobra za predikciju)
ggplot(data = portugal_students, aes(x = activities, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("activities") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za nursery(nije dobra za predikciju)
ggplot(data = portugal_students, aes(x = nursery, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("nursery") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za higher(dobra za predikciju)
ggplot(data = portugal_students, aes(x = higher, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("higher") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za internet(dobra za predikciju)
ggplot(data = portugal_students, aes(x = internet, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("internet") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za romantic(dobra za predikciju)
ggplot(data = portugal_students, aes(x = romantic, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("romantic") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))
#provera preko chisquare testa
library(MASS)        
tbl = table(portugal_students$G3,portugal_students$romantic)
tbl                 
chisq.test(tbl) #ukoliko je vrednost manja od 0.05 odbacujemo nultu hipotezu
#vrednost je 0.01 sto znaci da ima uticaj na izlaznu varijablu


#za Medu(dobra za predikciju)
ggplot(data = portugal_students, aes(x = Medu, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("medu") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za Fedu(dobra za predikciju)
ggplot(data = portugal_students, aes(x = Fedu, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("fedu") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za traveltime(dobra za predikciju)
ggplot(data = portugal_students, aes(x = traveltime, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("traveltime") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za studytime(dobra za predikciju)
ggplot(data = portugal_students, aes(x = studytime, fill = G3)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of Pass/Fail") + xlab("studytime") + 
  scale_y_continuous(labels = scales::percent, limits=c(0,1))

#za numericke atribute koristim geom_density

#za age(dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = age, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()


#za failures(dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = failures, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za famrel(dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = famrel, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za freetime(dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = freetime, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()


#za goout(nije dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = goout, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()
?kruskal.test
#provera preko Kruskal Wallis testa:Nulta hipoteza je da varijabla freetime
#nema uticaj na G3 izlaznu varijablu a Prva hipoteza je da ima uticaj
kruskal.test(goout ~ G3, data = portugal_students)
#p-value = 0.06 sto znaci da nema uticaj na izlaznu G3

#za Dalc(nije dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = Dalc, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()


#za Walc(dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = Walc, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za health(nije dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = health, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#za absences(dobra za predikciju)
ggplot(data = portugal_students,
       mapping = aes(x = absences, fill = G3)) +
  geom_density(alpha = 0.2)+
  theme_bw()

#varijable koje nemaju dovoljan prediktivni potencijal i koje bih uklonila su:
#famsize,Pstatus,schoolsup,famsup,paid,activities,nursery,goout,Dalc,health

portugal_students$famsize<-NULL
portugal_students$Pstatus<-NULL
portugal_students$schoolsup<-NULL
portugal_students$famsup<-NULL
portugal_students$paid<-NULL
portugal_students$activities<-NULL
portugal_students$nursery<-NULL
portugal_students$goout<-NULL
portugal_students$Dalc<-NULL
portugal_students$health<-NULL
str(portugal_students)


saveRDS(portugal_students, "portugal_preparation.RData")
