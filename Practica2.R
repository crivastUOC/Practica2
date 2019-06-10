# PRACTICA 2 - NETEJA I VALIDACIO DE DADES

# POL CASELLAS i CARLES RIVAS

# ---------------------------------------------------------------------




library('dplyr')




# Càrrega de les dades des d'els fitxers de train i test

dades <- bind_rows(read.csv('train.csv', stringsAsFactors = F), read.csv('test.csv', stringsAsFactors = F))




dim (dades)

str (dades)




# Els camps Sex i Embarked es fixen com a factors




factors<-c('Sex','Embarked')

dades[factors] <- lapply(dades[factors], function(x) as.factor(x))




str(dades)




summary (dades)




# Identificar el registre Fare mal informat

IdNoFare <- dades$PassengerId[is.na(dades$Fare)]

dades[IdNoFare,]




# Càlcul i assignació del preu mitja de la tarifa per aquest tipus de clients

dades$Fare[is.na(dades$Fare)] <- median(dades[dades$Pclass == '3' & dades$Embarked == 'S', ]$Fare, na.rm = TRUE)

dades[IdNoFare,]




# Identificar els registres Embarked no informats

IdNoEmbarked <- dades$PassengerId[dades$Embarked == '']

dades[IdNoEmbarked,]




# Càlcul de la tarifa mitjana per a la classe 1, per cada tipus d'embarcament possible.

median(dades[dades$Pclass == '1' & dades$Embarked == 'S', ]$Fare, na.rm = TRUE)

median(dades[dades$Pclass == '1' & dades$Embarked == 'Q', ]$Fare, na.rm = TRUE)

median(dades[dades$Pclass == '1' & dades$Embarked == 'C', ]$Fare, na.rm = TRUE)




# Assignació del més semblant 

dades$Embarked[c(IdNoEmbarked)] <- 'C'







# Aplicació del mètode mice per completar el camp edat a un conjunt del dataset de menor dimensionalitat 

library('mice')

library('randomForest')

set.seed(1000) 

dadesAge <- complete(mice(dades[, !names(dades) %in% c('PassengerId','Name', 'Ticket', 'Cabin', 'Survived')], method='rf'))




# Analisi gràfic conforme no s'ha introduit variació sobre la distribució inicial  

par(mfrow=c(1,2))

hist(dades$Age, freq=F, main='Age: Original', col='red', ylim=c(0,0.05))

hist(dadesAge$Age, freq=F, main='Age: Estimat', col='blue', ylim=c(0,0.05))




dades$Age <- dadesAge$Age 

summary (dades)




# Els valors 0 de Fare es converteixen en NA i es dedueixen amb l'algoritme KNN

dades$Fare[dades$Fare == 0] <- NA




# Es Comprova si hi ha NA,s a Fare

sapply(dades, function(x) sum(is.na(x)))




library(VIM)




# S'aplica l'algoritme kNN

dades.kNN <- kNN(dades[, !names(dades) %in% c('PassengerId','Name', 'Ticket', 'Cabin', 'Survived')])




# Es comprova si hi ha NA,s a Fare estimat

sapply(dades.kNN, function(x) sum(is.na(x)))




# Es comprova si hi ha zeros a Fare estimat

length(dades.kNN$Fare[dades.kNN$Fare == 0])




# S'assignen els valors de Fare estimats amb kNN al dataset original

dades$Fare = dades.kNN$Fare




# Creació del camp TamanyFamilia

dades$TamanyFamilia <- dades$SibSp + dades$Parch + 1




# Creació del camp TipusFamilia

dades$TipusFamilia[dades$TamanyFamilia == 1] <- 'solitari'

dades$TipusFamilia[dades$TamanyFamilia < 5 & dades$TamanyFamilia > 1] <- 'petita'

dades$TipusFamilia[dades$TamanyFamilia > 4] <- 'nombrosa'




# Conversió a factor

dades$TipusFamilia <- as.factor(dades$TipusFamilia)




# Es crea el camp TipusEdat en funció de franges d'edat i es converteix en factor

dades$TipusEdat[dades$Age < 18] <- 'Menor' 

dades$TipusEdat[dades$Age >= 18 & dades$Age <= 65] <- 'Adult'

dades$TipusEdat[dades$Age > 65] <- 'Anciá '

dades$TipusEdat <- as.factor(dades$TipusEdat)




#dataset final integrat i filtrat 

dades = dades[, !names(dades) %in% c('SibSp','Parch', 'Ticket', 'Cabin', 'TipusFamilia', "TipusEdat")]

summary (dades)




# Identificació dels valors extrems de les variables **Age** i **Fare** a partir de la funció boxplot:

outliers_age <- boxplot.stats(dades$Age)$out

outliers_fare <- boxplot.stats(dades$Fare)$out

outliers_age

outliers_fare




#dades<-dades[-which(dades$Age %in% outliers_age),]

#dades<-dades[-which(dades$Fare %in% outliers_fare),]







titanic_train <- dades[!is.na(dades$Survived),]

titanic_test <- dades[is.na(dades$Survived),]




# Exportacio dels fitxers validats

write.csv(dades, file = "dadesValidated.csv")

write.csv(titanic_train, file = "trainValidated.csv")

write.csv(titanic_test, file = "testValidated.csv")




# Hipótesi nul·la i alternativa




# Assumpció de normalitat




shapiro.test(titanic_train$Age)

shapiro.test(titanic_train$TamanyFamilia)

shapiro.test(titanic_train$Fare)

shapiro.test(titanic_train$Pclass)

length(titanic_train$Pclass)




# Homogeneitat de la variància




library(car)




fligner.test(Age ~ Survived, data = titanic_train)

fligner.test(TamanyFamilia ~ Survived, data = titanic_train)

fligner.test(Fare ~ Survived, data = titanic_train)

fligner.test(Pclass ~ Survived, data = titanic_train)




t.test(Age ~ Survived,data = titanic_train, var.equal=TRUE)

wilcox.test(TamanyFamilia ~ Survived,data = titanic_train, var.equal=TRUE)

wilcox.test(Fare ~ Survived,data = titanic_train, var.equal=TRUE)

wilcox.test(Pclass ~ Survived,data = titanic_train, var.equal=TRUE)




hist(dades$Age[dades$Survived == 0], main = "Histograma edat passatgers no supervivents", col = "red")

hist(dades$Age[dades$Survived == 1], main = "Histograma edat passatgers supervivents", col = "blue")




# Correlació




titanic_cor <- data.frame(titanic_train$Age, titanic_train$TamanyFamilia, titanic_train$Fare, titanic_train$Pclass)

cor(titanic_cor)




shapiro.test(titanic_train$Pclass)

shapiro.test(titanic_train$Age)

shapiro.test(titanic_train$Fare)

shapiro.test(titanic_train$TamanyFamilia)




cor.test(titanic_train$Pclass,titanic_train$Fare, method="spearman")

cor.test(titanic_train$Pclass,titanic_train$Age, method="spearman")

cor.test(titanic_train$TamanyFamilia,titanic_train$Age, method="spearman")




plot(titanic_train$Fare, titanic_train$Pclass, main="Diagrama dispersió Pclass i Fare")

plot(titanic_train$Age, titanic_train$Pclass, main="Diagrama dispersió³ Pclass i Age")

plot(titanic_train$Age, titanic_train$TamanyFamilia, main="Diagrama dispersió³ Age i TamanyFamilia")




# Regressió logística




titanic_train$sexR <- relevel(titanic_train$Sex, ref = "female")

titanic_train$EmbarkedR <- relevel(titanic_train$Embarked, ref = "S")




library('boot')
model1<- glm(Survived ~ sexR+TamanyFamilia+Age+EmbarkedR+Pclass,data=titanic_train)
cv.error1 = cv.glm(titanic_train,model1,K=10)$delta
summary(model1)
cv.error1
model2<- glm(Survived ~ sexR+Age+Pclass,data=titanic_train)
cv.error2 = cv.glm(titanic_train,model2,K=10)$delta
summary(model2)
cv.error2
model3<- glm(Survived ~ sexR+EmbarkedR+Pclass,data=titanic_train)
cv.error3 = cv.glm(titanic_train,model3,K=10)$delta
summary(model3)
cv.error3
model4<- glm(Survived ~ sexR+TamanyFamilia+Pclass,data=titanic_train)
cv.error4 = cv.glm(titanic_train,model4,K=10)$delta
summary(model4)
cv.error4
model5<- glm(Survived ~ sexR+Age,data=titanic_train)
cv.error5 = cv.glm(titanic_train,model5,K=10)$delta
summary(model5)
cv.error5






















titanic_test$sexR <- relevel(titanic_test$Sex, ref = "female")

titanic_test$EmbarkedR <- relevel(titanic_test$Embarked, ref = "S")




titanic_test$ProbSurvived <- predict(model1, titanic_test)




library(kableExtra)

kable(head(titanic_test), format = 'markdown')
counts = table(titanic_train$Survived, titanic_train$Sex)
barplot(counts, ylim=c(0, 1000), xlab="Sexe",ylab="Nombre de passatgers",main="Supervivència en funció del sexe",col = c("red", "blue"))
legend("topleft", fill=c("red", "blue"), legend=c("No sobrevivents", "Sobrevivents"))