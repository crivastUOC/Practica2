# PRACTICA 2 - NETEJA I VALIDACIO DE DADES
# POL CASELLAS i CARLES RIVAS
# ---------------------------------------------------------------------

library('dplyr')

# C�rrega de les dades des d'els fitxers de train i test
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

# C�lcul i assignaci� del preu mitja de la tarifa per aquest tipus de clients
dades$Fare[is.na(dades$Fare)] <- median(dades[dades$Pclass == '3' & dades$Embarked == 'S', ]$Fare, na.rm = TRUE)
dades[IdNoFare,]

# Identificar els registres Embarked no informats
IdNoEmbarked <- dades$PassengerId[dades$Embarked == '']
dades[IdNoEmbarked,]

# C�lcul de la tarifa mitjana per a la classe 1, per cada tipus d'embarcament possible.
median(dades[dades$Pclass == '1' & dades$Embarked == 'S', ]$Fare, na.rm = TRUE)
median(dades[dades$Pclass == '1' & dades$Embarked == 'Q', ]$Fare, na.rm = TRUE)
median(dades[dades$Pclass == '1' & dades$Embarked == 'C', ]$Fare, na.rm = TRUE)

# Assignaci� del m�s semblant 
dades$Embarked[c(IdNoEmbarked)] <- 'C'


# Aplicaci� del m�tode mice per completar el camp edat a un conjunt del dataset de menor dimensionalitat 
library('mice')
library('randomForest')
set.seed(1000) 
dadesAge <- complete(mice(dades[, !names(dades) %in% c('PassengerId','Name', 'Ticket', 'Cabin', 'Survived')], method='rf'))

# Analisi gr�fic conforme no s'ha introduit variaci� sobre la distribuci� inicial  
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

# Creaci� del camp TamanyFamilia
dades$TamanyFamilia <- dades$SibSp + dades$Parch + 1

# Creaci� del camp TipusFamilia
dades$TipusFamilia[dades$TamanyFamilia == 1] <- 'solitari'
dades$TipusFamilia[dades$TamanyFamilia < 5 & dades$TamanyFamilia > 1] <- 'petita'
dades$TipusFamilia[dades$TamanyFamilia > 4] <- 'nombrosa'

# Conversi� a factor
dades$TipusFamilia <- as.factor(dades$TipusFamilia)

# Es crea el camp TipusEdat en funci� de franges d'edat i es converteix en factor
dades$TipusEdat[dades$Age < 18] <- 'Menor' 
dades$TipusEdat[dades$Age >= 18 & dades$Age <= 65] <- 'Adult'
dades$TipusEdat[dades$Age > 65] <- 'Ancià'
dades$TipusEdat <- as.factor(dades$TipusEdat)

#dataset final integrat i filtrat 
dades = dades[, !names(dades) %in% c('SibSp','Parch', 'Ticket', 'Cabin', 'TipusFamilia', "TipusEdat")]
summary (dades)

# Identificaci� dels valors extrems de les variables **Age** i **Fare** a partir de la funci� boxplot:
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

# Hip�tesi nul�la i alternativa

# Assumpci� de normalitat

shapiro.test(titanic_train$Age)
shapiro.test(titanic_train$TamanyFamilia)
shapiro.test(titanic_train$Fare)
shapiro.test(titanic_train$Pclass)
length(titanic_train$Pclass)

# Homogeneitat de la vari�ncia

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

# Correlaci�

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
plot(titanic_train$Age, titanic_train$Pclass, main="Diagrama dispersió Pclass i Age")
plot(titanic_train$Age, titanic_train$TamanyFamilia, main="Diagrama dispersió Age i TamanyFamilia")

# Regressi� log�stica

titanic_train$sexR <- relevel(titanic_train$Sex, ref = "female")
titanic_train$EmbarkedR <- relevel(titanic_train$Embarked, ref = "S")

model1<- glm(Survived ~ sexR+TamanyFamilia+Age+EmbarkedR+Pclass,data=titanic_train)
summary(model1)
model2<- glm(Survived ~ sexR+Age+Pclass,data=titanic_train)
summary(model2)
model3<- glm(Survived ~ sexR+EmbarkedR+Pclass,data=titanic_train)
summary(model3)
model4<- glm(Survived ~ sexR+TamanyFamilia+Pclass,data=titanic_train)
summary(model4)
model5<- glm(Survived ~ sexR+Age,data=titanic_train)
summary(model5)

titanic_test$sexR <- relevel(titanic_test$Sex, ref = "female")
titanic_test$EmbarkedR <- relevel(titanic_test$Embarked, ref = "S")

titanic_test$ProbSurvived <- predict(model1, titanic_test)

library(kableExtra)
kable(head(titanic_test), format = 'markdown')