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


# Distribuzioni di frequenza di DIABETE4
table(DIABETE4) # tabella di frequenza
table(DIABETE4)/length(DIABETE4) # frequenza relativa
table(DIABETE4)/length(DIABETE4)*100 # frequenza percentuale

table(X_RFHYPE6) # tabella di frequenza
table(X_RFHYPE6)/length(X_RFHYPE6) # frequenza relativa
table(X_RFHYPE6)/length(X_RFHYPE6)*100 # frequenza percentuale

table(TOLDHI3) # tabella di frequenza
table(TOLDHI3)/length(TOLDHI3) # frequenza relativa
table(TOLDHI3)/length(TOLDHI3)*100 # frequenza percentuale

table(X_CHOLCH3) # tabella di frequenza
table(X_CHOLCH3)/length(X_CHOLCH3) # frequenza relativa
table(X_CHOLCH3)/length(X_CHOLCH3)*100 # frequenza percentuale

table(X_BMI5) # tabella di frequenza
table(X_BMI5)/length(X_BMI5) # frequenza relativa
table(X_BMI5)/length(X_BMI5)*100 # frequenza percentuale

table(SMOKE100) # tabella di frequenza
table(SMOKE100)/length(SMOKE100) # frequenza relativa
table(SMOKE100)/length(SMOKE100)*100 # frequenza percentuale

table(X_MICHD) # tabella di frequenza
table(X_MICHD)/length(X_MICHD) # frequenza relativa
table(X_MICHD)/length(X_MICHD)*100 # frequenza percentuale

table(CVDSTRK3) # tabella di frequenza
table(CVDSTRK3)/length(CVDSTRK3) # frequenza relativa
table(CVDSTRK3)/length(CVDSTRK3)*100 # frequenza percentuale

table(X_TOTINDA) # tabella di frequenza
table(X_TOTINDA)/length(X_TOTINDA) # frequenza relativa
table(X_TOTINDA)/length(X_TOTINDA)*100 # frequenza percentuale

table(X_FRTLT1A) # tabella di frequenza
table(X_FRTLT1A)/length(X_FRTLT1A) # frequenza relativa
table(X_FRTLT1A)/length(X_FRTLT1A)*100 # frequenza percentuale

table(X_VEGLT1A) # tabella di frequenza
table(X_VEGLT1A)/length(X_VEGLT1A) # frequenza relativa
table(X_VEGLT1A)/length(X_VEGLT1A)*100 # frequenza percentuale

table(X_RFDRHV7) # tabella di frequenza
table(X_RFDRHV7)/length(X_RFDRHV7) # frequenza relativa
table(X_RFDRHV7)/length(X_RFDRHV7)*100 # frequenza percentuale

table(X_HLTHPLN) # tabella di frequenza
table(X_HLTHPLN)/length(X_HLTHPLN) # frequenza relativa
table(X_HLTHPLN)/length(X_HLTHPLN)*100 # frequenza percentuale

table(MEDCOST1) # tabella di frequenza
table(MEDCOST1)/length(MEDCOST1) # frequenza relativa
table(MEDCOST1)/length(MEDCOST1)*100 # frequenza percentuale

table(GENHLTH) # tabella di frequenza
table(GENHLTH)/length(GENHLTH) # frequenza relativa
table(GENHLTH)/length(GENHLTH)*100 # frequenza percentuale

table(MENTHLTH) # tabella di frequenza
table(MENTHLTH)/length(MENTHLTH) # frequenza relativa
table(MENTHLTH)/length(MENTHLTH)*100 # frequenza percentuale

table(PHYSHLTH) # tabella di frequenza
table(PHYSHLTH)/length(PHYSHLTH) # frequenza relativa
table(PHYSHLTH)/length(PHYSHLTH)*100 # frequenza percentuale

table(DIFFWALK) # tabella di frequenza
table(DIFFWALK)/length(DIFFWALK) # frequenza relativa
table(DIFFWALK)/length(DIFFWALK)*100 # frequenza percentuale

table(SEXVAR) # tabella di frequenza
table(SEXVAR)/length(SEXVAR) # frequenza relativa
table(SEXVAR)/length(SEXVAR)*100 # frequenza percentuale

table(X_AGEG5YR) # tabella di frequenza
table(X_AGEG5YR)/length(X_AGEG5YR) # frequenza relativa
table(X_AGEG5YR)/length(X_AGEG5YR)*100 # frequenza percentuale

table(EDUCA) # tabella di frequenza
table(EDUCA)/length(EDUCA) # frequenza relativa
table(EDUCA)/length(EDUCA)*100 # frequenza percentuale

table(INCOME3) # tabella di frequenza
table(INCOME3)/length(INCOME3) # frequenza relativa
table(INCOME3)/length(INCOME3)*100 # frequenza percentuale

