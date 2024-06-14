library(haven)
library(foreign)
library(ggplot2)

# installa il pacchetto se non è già installato
if (!require(foreign)) {
  install.packages("foreign")
}

# installa il pacchetto se non è già installato
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

# Carica i dati "diabetes_dataset.csv"
data <- read.csv(file.choose())
head(data) # visualizza prime righe del dataframe
attach(data) # variabili utilizzabili direttamente

# Controlla se ci sono valori nulli
sum(is.na(data)) # Ci sono 160435 valori nulli

# Controlla se ci sono valori duplicati
sum(duplicated(data)) # Non ci sono valori duplicati

# Gestione dei valori nulli
data <- na.omit(data) # Rimuove i valori nulli
sum(is.na(data)) # Non ci sono più valori nulli

detach(data) # Rimuove il dataframe precedente dall'ambiente
attach(data) # Ri-attacca il dataframe aggiornato, senza valori nulli

#EDA per le variabili del dataset

# EDA: DIABETE4
# Ever told) (you had) diabetes? (If  ́Yes ́ and respondent is female, ask  ́Was this only when your 
#were pregnant? ́. If Respondent says pre-diabetes or borderline diabetes, use response code 4

mean(DIABETE4)
median(DIABETE4)
var(DIABETE4)
summary(DIABETE4)

boxplot(DIABETE4, main = "DIABETE4", xlab = "DIABETE4")

ggplot(data, aes(x = DIABETE4)) + geom_bar() +
  labs(title = "Distribuzione di DIABETE4",
       x = "DIABETE4",
       y = "Frequenza")

# EDA: X_RFHYPE6 
# Adults who have been told they have high blood pressure by a doctor, nurse, or other health professional

mean(X_RFHYPE6)
median(X_RFHYPE6)
var(X_RFHYPE6)
summary(X_RFHYPE6)

boxplot(X_RFHYPE6, main = "X_RFHYPE6", xlab = "X_RFHYPE6")

ggplot(data, aes(x = X_RFHYPE6)) + geom_bar() +
  labs(title = "Distribuzione di X_RFHYPE6",
       x = "X_RFHYPE6",
       y = "Frequenza")

# EDA: TOLDHI3
# Have you ever been told by a doctor, nurse or other health professional that your cholesterol is high

mean(TOLDHI3)
median(TOLDHI3)
var(TOLDHI3)

boxplot(TOLDHI3, main = "TOLDHI3", xlab = "TOLDHI3")

ggplot(data, aes(x = TOLDHI3)) + geom_bar() +
  labs(title = "Distribuzione di TOLDHI3",
       x = "TOLDHI3",
       y = "Frequenza")

# EDA: X_CHOLCH3
# About how long has it been since you last had your cholesterol checked

mean(X_CHOLCH3)
median(X_CHOLCH3)
var(X_CHOLCH3)

boxplot(X_CHOLCH3, main = "X_CHOLCH3", xlab = "X_CHOLCH3")

ggplot(data, aes(x = X_CHOLCH3)) + geom_bar() +
  labs(title = "Distribuzione di X_CHOLCH3",
       x = "X_CHOLCH3",
       y = "Frequenza")

# EDA: X_BMI5
# Body Mass Index

mean(X_BMI5)
median(X_BMI5)
var(X_BMI5)

boxplot(X_BMI5, main = "X_BMI5", xlab = "X_BMI5")  #mi sembra ci siano molti outlier

ggplot(data, aes(x = X_BMI5)) + geom_bar() +
  labs(title = "Distribuzione di X_BMI5",
       x = "X_BMI5",
       y = "Frequenza")

# EDA: SMOKE100
# Four-level smoker status: Everyday smoker, Someday smoker, Former smoker, Non-smoker

mean(SMOKE100)
median(SMOKE100)
var(SMOKE100)

boxplot(SMOKE100, main = "SMOKE100", xlab = "SMOKE100")

ggplot(data, aes(x = SMOKE100)) + geom_bar() +
  labs(title = "Distribuzione di SMOKE100",
       x = "SMOKE100",
       y = "Frequenza")

# EDA: X_MICHD
# Respondents that have ever reported having coronary heart disease (CHD) or myocardial infarction (MI)

mean(X_MICHD)
median(X_MICHD)
var(X_MICHD)

boxplot(X_MICHD, main = "X_MICHD", xlab = "X_MICHD")

ggplot(data, aes(x = X_MICHD)) + geom_bar() +
  labs(title = "Distribuzione di X_MICHD",
       x = "X_MICHD",
       y = "Frequenza")

# EDA: CVDSTRK3
# Ever told you had a stroke.

mean(CVDSTRK3)
median(CVDSTRK3)
var(CVDSTRK3)

boxplot(CVDSTRK3, main = "CVDSTRK3", xlab = "CVDSTRK3")

ggplot(data, aes(x = CVDSTRK3)) + geom_bar() +
  labs(title = "Distribuzione di CVDSTRK3",
       x = "CVDSTRK3",
       y = "Frequenza")

# EDA: X_TOTINDA
# Adults who reported doing physical activity or exercise during the past 30 days other than their regular job

mean(X_TOTINDA)
median(X_TOTINDA)
var(X_TOTINDA)

boxplot(X_TOTINDA, main = "X_TOTINDA", xlab = "X_TOTINDA")

ggplot(data, aes(x = X_TOTINDA)) + geom_bar() +
  labs(title = "Distribuzione di X_TOTINDA",
       x = "X_TOTINDA",
       y = "Frequenza")

# EDA: X_FRTLT1A
# Consume Fruit 1 or more times per day

mean(X_FRTLT1A)
median(X_FRTLT1A)
var(X_FRTLT1A)

boxplot(X_FRTLT1A, main = "X_FRTLT1A", xlab = "X_FRTLT1A")

ggplot(data, aes(x = X_FRTLT1A)) + geom_bar() +
  labs(title = "Distribuzione di X_FRTLT1A",
       x = "X_FRTLT1A",
       y = "Frequenza")

# EDA: X_VEGLT1A
# Consume Vegetables 1 or more times per day

mean(X_VEGLT1A)
median(X_VEGLT1A)
var(X_VEGLT1A)

boxplot(X_VEGLT1A, main = "X_VEGLT1A", xlab = "X_VEGLT1A")

ggplot(data, aes(x = X_VEGLT1A)) + geom_bar() +
  labs(title = "Distribuzione di X_VEGLT1A",
       x = "X_VEGLT1A",
       y = "Frequenza")

# EDA: X_RFDRHV7
# Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week)

mean(X_RFDRHV7)
median(X_RFDRHV7)
var(X_RFDRHV7)

boxplot(X_RFDRHV7, main = "X_RFDRHV7", xlab = "X_RFDRHV7")

ggplot(data, aes(x = X_RFDRHV7)) + geom_bar() +
  labs(title = "Distribuzione di X_RFDRHV7",
       x = "X_RFDRHV7",
       y = "Frequenza")

# EDA: X_HLTHPLN
# Adults who had some form of health insurance

mean(X_HLTHPLN)
median(X_HLTHPLN)
var(X_HLTHPLN)

boxplot(X_HLTHPLN, main = "X_HLTHPLN", xlab = "X_HLTHPLN")

ggplot(data, aes(x = X_HLTHPLN)) + geom_bar() +
  labs(title = "Distribuzione di X_HLTHPLN",
       x = "X_HLTHPLN",
       y = "Frequenza")

# EDA: MEDCOST1
# Was there a time in the past 12 months when you needed to see a doctor but could not because you could not afford it?

mean(MEDCOST1)
median(MEDCOST1)
var(MEDCOST1)

boxplot(MEDCOST1, main = "MEDCOST1", xlab = "MEDCOST1")

ggplot(data, aes(x = MEDCOST1)) + geom_bar() +
  labs(title = "Distribuzione di MEDCOST1",
       x = "MEDCOST1",
       y = "Frequenza")

# EDA: GENHLTH
# Would you say that in general your health is

mean(GENHLTH)
median(GENHLTH)
var(GENHLTH)

boxplot(GENHLTH, main = "GENHLTH", xlab = "GENHLTH")

ggplot(data, aes(x = GENHLTH)) + geom_bar() +
  labs(title = "Distribuzione di GENHLTH",
       x = "GENHLTH",
       y = "Frequenza")

# EDA: MENTHLTH
# Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good

mean(MENTHLTH)
median(MENTHLTH)
var(MENTHLTH)

boxplot(MENTHLTH, main = "MENTHLTH", xlab = "MENTHLTH")

ggplot(data, aes(x = MENTHLTH)) + geom_bar() +
  labs(title = "Distribuzione di MENTHLTH",
       x = "MENTHLTH",
       y = "Frequenza")

# EDA: PHYSHLTH
# Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good

mean(PHYSHLTH)
median(PHYSHLTH)
var(PHYSHLTH)

boxplot(PHYSHLTH, main = "PHYSHLTH", xlab = "PHYSHLTH")

ggplot(data, aes(x = PHYSHLTH)) + geom_bar() +
  labs(title = "Distribuzione di PHYSHLTH",
       x = "PHYSHLTH",
       y = "Frequenza")

# EDA: DIFFWALK
# Do you have serious difficulty walking or climbing stairs?

mean(DIFFWALK)
median(DIFFWALK)
var(DIFFWALK)

boxplot(DIFFWALK, main = "DIFFWALK", xlab = "DIFFWALK")

ggplot(data, aes(x = DIFFWALK)) + geom_bar() +
  labs(title = "Distribuzione di DIFFWALK",
       x = "DIFFWALK",
       y = "Frequenza")

# EDA: SEXVAR
# Sex of Respondent

mean(SEXVAR)
median(SEXVAR)
var(SEXVAR)

boxplot(SEXVAR, main = "SEXVAR", xlab = "SEXVAR")

ggplot(data, aes(x = SEXVAR)) + geom_bar() +
  labs(title = "Distribuzione di SEXVAR",
       x = "SEXVAR",
       y = "Frequenza")

# EDA: X_AGEG5YR
# Fourteen-level age category

mean(X_AGEG5YR)
median(X_AGEG5YR)
var(X_AGEG5YR)

boxplot(X_AGEG5YR, main = "X_AGEG5YR", xlab = "X_AGEG5YR")

ggplot(data, aes(x = X_AGEG5YR)) + geom_bar() +
  labs(title = "Distribuzione di X_AGEG5YR",
       x = "X_AGEG5YR",
       y = "Frequenza")

# EDA: EDUCA
# What is the highest grade or year of school you completed

mean(EDUCA)
median(EDUCA)
var(EDUCA)

boxplot(EDUCA, main = "EDUCA", xlab = "EDUCA")

ggplot(data, aes(x = EDUCA)) + geom_bar() +
  labs(title = "Distribuzione di EDUCA",
       x = "EDUCA",
       y = "Frequenza")

# EDA: INCOME3
# Is your annual household income from all sources

mean(INCOME3)
median(INCOME3)
var(INCOME3)

boxplot(INCOME3, main = "INCOME3", xlab = "INCOME3")

ggplot(data, aes(x = INCOME3)) + geom_bar() +
  labs(title = "Distribuzione di INCOME3",
       x = "INCOME3",
       y = "Frequenza")


#EDA: Correlazione tra variabili numeriche (da capire se rilevante)
correlation_matrix <- cor(data, use = "complete.obs")
library(corrplot)
if (!require(corrplot)) {
  install.packages("corrplot")
}
corrplot(correlation_matrix, method = "circle")

# Provvisorio: grafico di che fa vedere una variabile in funzione di un'altra (qui Sesso)
ggplot(data, aes(x = X_AGEG5YR, fill = factor(SEXVAR))) + 
  geom_bar(position = "dodge") +
  labs(title = "Distribuzione di X_AGEG5YR per SEXVAR",
       x = "X_AGEG5YR",
       y = "Frequenza")


#Provvisiorio:  forse ha senso grafico del diabete in funzione di età e sesso (?)
# Prevalenza del diabete per fasce di età
ggplot(data, aes(x = X_AGEG5YR, fill = factor(DIABETE4))) + 
  geom_bar(position = "fill") +
  labs(title = "Prevalenza del diabete per fasce di età",
       x = "X_AGEG5YR",
       y = "Proporzione")

#Prevalenza del diabete per fasce di età 
ggplot(data, aes(x = X_AGEG5YR, fill = factor(DIABETE4))) + 
  geom_bar(position = "dodge") +
  labs(title = "Prevalenza del diabete per fasce di età",
       x = "X_AGEG5YR",
       y = "Frequenza")


















