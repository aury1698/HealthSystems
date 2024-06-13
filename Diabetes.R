library(haven)
library(foreign)
library(ggplot2)

# installa il pacchetto se non è già installato
if (!require(foreign)) {
  install.packages("foreign")
}

# installa il pacchetto se non è già installato
if (!require(foreign)) {
  install.packages("ggplot2")
}

#Carica i dati "diabetes_dataset.csv"
data <- read.csv(file.choose())
head(data) # visualizza prime righe del dataframe
attach(data) # variabili utilizzabili direttamente

#Controlla se ci sono valori nulli
sum(is.na(data)) #Ci sono 160435 valori nulli

#Controlla se ci sono valori duplicati
sum(duplicated(data)) #Non ci sono valori duplicati

#Gestione dei valori nulli
data <- na.omit(data) #Rimuove i valori nulli
sum(is.na(data)) #Non ci sono più valori nulli

detach(data) # Rimuove il dataframe precedente dall'ambiente
attach(data) # Ri-attacca il dataframe aggiornato, senza valori nulli

# Distribuzioni di frequenza di DIABETE4
table(DIABETE4) # tabella di frequenza
table(DIABETE4) / length(DIABETE4) # frequenza relativa
table(DIABETE4) / length(DIABETE4) * 100 # frequenza percentuale

table(X_RFHYPE6) # tabella di frequenza
table(X_RFHYPE6) / length(X_RFHYPE6) # frequenza relativa
table(X_RFHYPE6) / length(X_RFHYPE6) * 100 # frequenza percentuale

table(TOLDHI3) # tabella di frequenza
table(TOLDHI3) / length(TOLDHI3) # frequenza relativa
table(TOLDHI3) / length(TOLDHI3) * 100 # frequenza percentuale

table(X_CHOLCH3) # tabella di frequenza
table(X_CHOLCH3) / length(X_CHOLCH3) # frequenza relativa
table(X_CHOLCH3) / length(X_CHOLCH3) * 100 # frequenza percentuale

table(X_BMI5) # tabella di frequenza
table(X_BMI5) / length(X_BMI5) # frequenza relativa
table(X_BMI5) / length(X_BMI5) * 100 # frequenza percentuale

table(SMOKE100) # tabella di frequenza
table(SMOKE100) / length(SMOKE100) # frequenza relativa
table(SMOKE100) / length(SMOKE100) * 100 # frequenza percentuale

table(X_MICHD) # tabella di frequenza
table(X_MICHD) / length(X_MICHD) # frequenza relativa
table(X_MICHD) / length(X_MICHD) * 100 # frequenza percentuale

table(CVDSTRK3) # tabella di frequenza
table(CVDSTRK3) / length(CVDSTRK3) # frequenza relativa
table(CVDSTRK3) / length(CVDSTRK3) * 100 # frequenza percentuale

table(X_TOTINDA) # tabella di frequenza
table(X_TOTINDA) / length(X_TOTINDA) # frequenza relativa
table(X_TOTINDA) / length(X_TOTINDA) * 100 # frequenza percentuale

table(X_FRTLT1A) # tabella di frequenza
table(X_FRTLT1A) / length(X_FRTLT1A) # frequenza relativa
table(X_FRTLT1A) / length(X_FRTLT1A) *100 # frequenza percentuale

table(X_VEGLT1A) # tabella di frequenza
table(X_VEGLT1A) / length(X_VEGLT1A) # frequenza relativa
table(X_VEGLT1A) / length(X_VEGLT1A) * 100 # frequenza percentuale

table(X_RFDRHV7) # tabella di frequenza
table(X_RFDRHV7) / length(X_RFDRHV7) # frequenza relativa
table(X_RFDRHV7) / length(X_RFDRHV7) * 100 # frequenza percentuale

table(X_HLTHPLN) # tabella di frequenza
table(X_HLTHPLN) / length(X_HLTHPLN) # frequenza relativa
table(X_HLTHPLN) / length(X_HLTHPLN)*100 # frequenza percentuale

table(MEDCOST1) # tabella di frequenza
table(MEDCOST1) / length(MEDCOST1) # frequenza relativa
table(MEDCOST1) / length(MEDCOST1) * 100 # frequenza percentuale

table(GENHLTH) # tabella di frequenza
table(GENHLTH) / length(GENHLTH) # frequenza relativa
table(GENHLTH) / length(GENHLTH) * 100 # frequenza percentuale

table(MENTHLTH) # tabella di frequenza
table(MENTHLTH) / length(MENTHLTH) # frequenza relativa
table(MENTHLTH) / length(MENTHLTH) * 100 # frequenza percentuale

table(PHYSHLTH) # tabella di frequenza
table(PHYSHLTH) / length(PHYSHLTH) # frequenza relativa
table(PHYSHLTH) / length(PHYSHLTH) * 100 # frequenza percentuale

table(DIFFWALK) # tabella di frequenza
table(DIFFWALK) / length(DIFFWALK) # frequenza relativa
table(DIFFWALK) / length(DIFFWALK) * 100 # frequenza percentuale

table(SEXVAR) # tabella di frequenza
table(SEXVAR) / length(SEXVAR) # frequenza relativa
table(SEXVAR) / length(SEXVAR) * 100 # frequenza percentuale

table(X_AGEG5YR) # tabella di frequenza
table(X_AGEG5YR) / length(X_AGEG5YR) # frequenza relativa
table(X_AGEG5YR) / length(X_AGEG5YR) * 100 # frequenza percentuale

table(EDUCA) # tabella di frequenza
table(EDUCA) / length(EDUCA) # frequenza relativa
table(EDUCA) / length(EDUCA) * 100 # frequenza percentuale

table(INCOME3) # tabella di frequenza
table(INCOME3) / length(INCOME3) # frequenza relativa
table(INCOME3) / length(INCOME3) * 100 # frequenza percentuale

#EDA: controllo della media e mediana per ogni variabile
mean(DIABETE4)
median(DIABETE4)
var(DIABETE4)

mean(X_RFHYPE6)
median(X_RFHYPE6)
var(X_RFHYPE6)

mean(TOLDHI3)
median(TOLDHI3)
var(TOLDHI3)

mean(X_CHOLCH3)
median(X_CHOLCH3)
var(X_CHOLCH3)

mean(X_BMI5)
median(X_BMI5)
var(X_BMI5)

mean(SMOKE100)
median(SMOKE100)
var(SMOKE100)

mean(X_MICHD)
median(X_MICHD)
var(X_MICHD)

mean(CVDSTRK3)
median(CVDSTRK3)
var(CVDSTRK3)

mean(X_TOTINDA)
median(X_TOTINDA)
var(X_TOTINDA)

mean(X_FRTLT1A)
median(X_FRTLT1A)
var(X_FRTLT1A)

mean(X_VEGLT1A)
median(X_VEGLT1A)
var(X_VEGLT1A)

mean(X_RFDRHV7)
median(X_RFDRHV7)
var(X_RFDRHV7)

mean(X_HLTHPLN)
median(X_HLTHPLN)
var(X_HLTHPLN)

mean(MEDCOST1)
median(MEDCOST1)
var(MEDCOST1)

mean(GENHLTH)
median(GENHLTH)
var(GENHLTH)

mean(MENTHLTH)
median(MENTHLTH)
var(MENTHLTH)

mean(PHYSHLTH)
median(PHYSHLTH)
var(PHYSHLTH)

mean(DIFFWALK) 
median(DIFFWALK)
var(DIFFWALK)

mean(SEXVAR)
median(SEXVAR)
var(SEXVAR)

mean(X_AGEG5YR)
median(X_AGEG5YR)
var(X_AGEG5YR)

mean(EDUCA)
median(EDUCA)
var(EDUCA)

mean(INCOME3)
median(INCOME3)
var(INCOME3)

#EDA: Box and Whisker plot per ogni variabile: mettere label e titolo
boxplot(DIABETE4, main = "DIABETE4", xlab = "DIABETE4")
boxplot(X_RFHYPE6, main = "X_RFHYPE6", xlab = "X_RFHYPE6")
boxplot(TOLDHI3, main = "TOLDHI3", xlab = "TOLDHI3")
boxplot(X_CHOLCH3, main = "X_CHOLCH3", xlab = "X_CHOLCH3")
boxplot(X_BMI5, main = "X_BMI5", xlab = "X_BMI5")
boxplot(SMOKE100, main = "SMOKE100", xlab = "SMOKE100")
boxplot(X_MICHD, main = "X_MICHD", xlab = "X_MICHD")
boxplot(CVDSTRK3, main = "CVDSTRK3", xlab = "CVDSTRK3")
boxplot(X_TOTINDA, main = "X_TOTINDA", xlab = "X_TOTINDA")
boxplot(X_FRTLT1A, main = "X_FRTLT1A", xlab = "X_FRTLT1A")
boxplot(X_VEGLT1A, main = "X_VEGLT1A", xlab = "X_VEGLT1A")
boxplot(X_RFDRHV7, main = "X_RFDRHV7", xlab = "X_RFDRHV7")
boxplot(X_HLTHPLN,  main = "X_HLTHPLN", xlab = "X_HLTHPLN")
boxplot(MEDCOST1, main = "MEDCOST1", xlab = "MEDCOST1")
boxplot(GENHLTH, main = "GENHLTH", xlab = "GENHLTH")
boxplot(MENTHLTH, main = "MENTHLTH", xlab = "MENTHLTH")
boxplot(PHYSHLTH, main = "PHYSHLTH", xlab = "PHYSHLTH")
boxplot(DIFFWALK, main = "DIFFWALK", xlab = "DIFFWALK")
boxplot(SEXVAR, main = "SEXVAR", xlab = "SEXVAR")
boxplot(X_AGEG5YR, main = "X_AGEG5YR", xlab = "X_AGEG5YR")
boxplot(EDUCA, main = "EDUCA", xlab = "EDUCA")
boxplot(INCOME3, main = "INCOME3", xlab = "INCOME3")

#EDA: Istogramma per ogni variabile
# Istogramma di DIABETE4
ggplot(data, aes(x = DIABETE4)) + geom_bar() +
  labs(title = "Distribuzione di DIABETE4",
       x = "DIABETE4",
       y = "Frequenza")

# Istogramma di X_RFHYPE6
ggplot(data, aes(x = X_RFHYPE6)) + geom_bar() +
  labs(title = "Distribuzione di X_RFHYPE6",
       x = "X_RFHYPE6",
       y = "Frequenza")

# Istogramma di TOLDHI3 
ggplot(data, aes(x = TOLDHI3)) + geom_bar() +
  labs(title = "Distribuzione di TOLDHI3",
       x = "TOLDHI3",
       y = "Frequenza")

# Istogramma di X_CHOLCH3
ggplot(data, aes(x = X_CHOLCH3)) + geom_bar() +
  labs(title = "Distribuzione di X_CHOLCH3",
       x = "X_CHOLCH3",
       y = "Frequenza")

# Istogramma di X_BMI5
ggplot(data, aes(x = X_BMI5)) + geom_bar() +
  labs(title = "Distribuzione di X_BMI5",
       x = "X_BMI5",
       y = "Frequenza")

# Istogramma di SMOKE100
ggplot(data, aes(x = SMOKE100)) + geom_bar() +
  labs(title = "Distribuzione di SMOKE100",
       x = "SMOKE100",
       y = "Frequenza")

# Istogramma di X_MICHD
ggplot(data, aes(x = X_MICHD)) + geom_bar() +
  labs(title = "Distribuzione di X_MICHD",
       x = "X_MICHD",
       y = "Frequenza")

# Istogramma di CVDSTRK3
ggplot(data, aes(x = CVDSTRK3)) + geom_bar() +
  labs(title = "Distribuzione di CVDSTRK3",
       x = "CVDSTRK3",
       y = "Frequenza")

# Istogramma di X_TOTINDA
ggplot(data, aes(x = X_TOTINDA)) + geom_bar() +
  labs(title = "Distribuzione di X_TOTINDA",
       x = "X_TOTINDA",
       y = "Frequenza")

# Istogramma di X_FRTLT1A
ggplot(data, aes(x = X_FRTLT1A)) + geom_bar() +
  labs(title = "Distribuzione di X_FRTLT1A",
       x = "X_FRTLT1A",
       y = "Frequenza")

# Istogramma di X_VEGLT1A
ggplot(data, aes(x = X_VEGLT1A)) + geom_bar() +
  labs(title = "Distribuzione di X_VEGLT1A",
       x = "X_VEGLT1A",
       y = "Frequenza")

# Istogramma di X_RFDRHV7
ggplot(data, aes(x = X_RFDRHV7)) + geom_bar() +
  labs(title = "Distribuzione di X_RFDRHV7",
       x = "X_RFDRHV7",
       y = "Frequenza")

# Istogramma di X_HLTHPLN
ggplot(data, aes(x = X_HLTHPLN)) + geom_bar() +
  labs(title = "Distribuzione di X_HLTHPLN",
       x = "X_HLTHPLN",
       y = "Frequenza")

# Istogramma di MEDCOST1
ggplot(data, aes(x = MEDCOST1)) + geom_bar() +
  labs(title = "Distribuzione di MEDCOST1",
       x = "MEDCOST1",
       y = "Frequenza")

# Istogramma di GENHLTH
ggplot(data, aes(x = GENHLTH)) + geom_bar() +
  labs(title = "Distribuzione di GENHLTH",
       x = "GENHLTH",
       y = "Frequenza")

# Istogramma di MENTHLTH
ggplot(data, aes(x = MENTHLTH)) + geom_bar() +
  labs(title = "Distribuzione di MENTHLTH",
       x = "MENTHLTH",
       y = "Frequenza")
       
# Istogramma di PHYSHLTH
ggplot(data, aes(x = PHYSHLTH)) + geom_bar() +
  labs(title = "Distribuzione di PHYSHLTH",
       x = "PHYSHLTH",
       y = "Frequenza")

# Istogramma di DIFFWALK
ggplot(data, aes(x = DIFFWALK)) + geom_bar() +
  labs(title = "Distribuzione di DIFFWALK",
       x = "DIFFWALK",
       y = "Frequenza")

# Istogramma di SEXVAR
ggplot(data, aes(x = SEXVAR)) + geom_bar() +
  labs(title = "Distribuzione di SEXVAR",
       x = "SEXVAR",
       y = "Frequenza")

# Istogramma di X_AGEG5YR
ggplot(data, aes(x = X_AGEG5YR)) + geom_bar() +
  labs(title = "Distribuzione di X_AGEG5YR",
       x = "X_AGEG5YR",
       y = "Frequenza")

# Istogramma di EDUCA
ggplot(data, aes(x = EDUCA)) + geom_bar() +
  labs(title = "Distribuzione di EDUCA",
       x = "EDUCA",
       y = "Frequenza")

# Istogramma di INCOME3
ggplot(data, aes(x = INCOME3)) + geom_bar() +
  labs(title = "Distribuzione di INCOME3",
       x = "INCOME3",
       y = "Frequenza")

