# Prendere variabili categoriche e fare test chi^2 di pearson per l'indipendenza : Slide test di adattamento - pag.14
# Esempio del professore
# xmat <- matrix(c(10,18,1,22),nrow=2,byrow=T) 
# xmat
# chisq.test(xmat)
# fisher.test(xmat) # test esatto di Fisher

# noi abbiamo 13 variabili categoriche, dobbiamo scegliere 2 variabili e fare il test
# sono 13*12/2 = 78 coppie di variabili, salviamo i p-value per valutarli dopo

# H_0 : la pdf congiunta = prodotto delle pdf marginali
# H_1 : la pdf congiunta != prodotto delle pdf marginali

# quindi se accetto H_0 allora le variabili sono indipendenti, altrimenti sono dipendenti

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/chisq.test

#Ecco alcune delle nostre variabili categoriche in Diabetes.R su cui possiamo fare il test: 
#1) Diabetes 
# Ever told) (you had) diabetes? (If  ́Yes ́ and respondent is female, ask  ́Was this only when your 
# were pregnant? ́. If Respondent says pre-diabetes or borderline diabetes, use response code 4
# categorical = columns[2]
# # 0 - Dont't know/Not sure/Refused/Blank
# # 1 - Yes / Yes but during pregnancy
# # 2 - No
# # 3 - Pre-Diabetes/Borderline



# 2) EDA: HighBloodPressure
# # Adults who have been told they have high blood pressure by a doctor, nurse, or other health professional
# categorical <- c(categorical, columns[3])
# # 0 - Dont't know/Not sure/Refused/Blank
# # 1 No
# # 2 Yes

#proviamo a prendere queste due variabili del dataset "diabetes:dataset_processed" e fare il test chi quadro per vedere se sono indipendenti
#Il prof lo fa in "test-parametrici.R"
library(haven)
library(foreign)
library(ggplot2)
library(GGally)

# installa il pacchetto se non è già installato
if (!require(foreign)) {
  install.packages("foreign")
}

# installa il pacchetto se non è già installato
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

# install the package if it's not already installed
if (!require(dplyr)) {
  install.packages("GGally")
}

# Carica i dati "diabetes_dataset_processed.csv"
data <- read.csv("./data/diabetes_dataset_processed.csv")
head(data) # visualizza prime righe del dataframe
attach(data) # variabili utilizzabili direttamente, ignorare errore
categorical_columns = c("X", "Diabetes", "CholesterolCheck", "BMI", "Smoker", "GeneralHealth", 
                    "Age", "Education", "Income")
ordinal_data <- data[, ordinal_columns]
attach(ordinal_data)
rm(data)
columns = colnames(data)

#Prendiamo Diabetes e HighBloodPressure per fare i test chi quadro e vedere se sono indipendenti
diabetes <- data$Diabetes
highBloodPressure <- data$HighBloodPressure


# Create the contingency table
contingencyTable <- table(diabetes, highBloodPressure)

# Perform the chi-squared test
chisqResult <- chisq.test(contingencyTable)

# Print the result
print(chisqResult)  # test chi quadrato di Pearson
#Dato che il p-value è molto basso, possiamo rifiutare l'ipotesi nulla e concludere che le due variabili Diabetes e High Blood Pressure non sono indipendenti

#Proviamo a fare il test esatto di Fisher per le stesse variabili
# fisherResult <- fisher.test(contingencyTable, workspace = 2e+06) --> out of space

fisherResult <- fisher.test(contingencyTable, simulate.p.value = TRUE) # test esatto di Fisher con p-value simulato cioè approssimato con Monte Carlo perché la tabella è troppo grande
print(fisherResult) # test esatto di Fisher --> p-value = 0.0004998 quindi rifiutiamo l'ipotesi nulla e concludiamo che le due variabili Diabetes e High Blood Pressure non sono indipendenti


#Proviamo a fare il test chi quadro per altre coppie di variabili categoriche, come Diabetes con ToldHighColesterol
toldHighColesterol <- data$ToldHighColesterol

# Create the contingency table
contingencyTable2 <- table(diabetes, toldHighColesterol)

# Perform the chi-squared test
chisqResult2 <- chisq.test(contingencyTable2)
print(chisqResult2)  # test chi quadrato di Pearson. Il p-value viene < 2.2e-16, quindi rifiutiamo l'ipotesi nulla e concludiamo che le due variabili Diabetes e ToldHighColesterol non sono indipendenti

#Proviamo a fare il test esatto di Fisher per le stesse variabili
fisherResult2 <- fisher.test(contingencyTable2, simulate.p.value = TRUE) # test esatto di Fisher con p-value simulato cioè approssimato con Monte Carlo perché la tabella è troppo grande
print(fisherResult2) # test esatto di Fisher --> p-value = 0.0004998 quindi rifiutiamo l'ipotesi nulla e concludiamo che le due variabili Diabetes e ToldHighColesterol non sono indipendenti
#Anche qui rifiutiamo l'ipotesi nulla e concludiamo che le due variabili Diabetes e ToldHighColesterol non sono indipendenti

#Matrice di correlazione di Spearman

ggcorr(ordinal_data, 
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")
