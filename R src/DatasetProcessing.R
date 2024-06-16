# installa il pacchetto se non è già installato
if (!require(foreign)) {
  install.packages("foreign")
}

# install the package if it's not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
}

library(haven)
library(foreign)
library(dplyr)

# carica dati in formato xpt da file con header
data <- read.xport(file.choose())
head(data) # visualizza prime righe del dataframe
attach(data) # variabili utilizzabili direttamente

colnames(data) # visualizza i nomi delle colonne

# seleziona solo le colonne specificate
selected_columns <- c("DIABETE4", "X_RFHYPE6", "TOLDHI3",
                      "CHOLCHK3", "X_BMI5CAT", "X_SMOKER3",
                      "X_MICHD", "CVDSTRK3", "X_TOTINDA", "X_FRTLT1A",
                      "X_VEGLT1A", "X_RFDRHV7", "X_HLTHPLN",
                      "MEDCOST1", "GENHLTH", "MENTHLTH",
                      "PHYSHLTH", "DIFFWALK", "SEXVAR", "X_AGEG5YR",
                      "X_EDUCAG", "X_INCOMG1")
new_data <- data[, selected_columns]
# visualizza le prime righe del nuovo dataset
head(new_data)
detach(data) # rimuove il dataset precedente
attach(new_data) 

#Rinomina le colonne del dataset in modo più chiaro in inglese
colnames(new_data) <- c("Diabetes", "HighBloodPressure", "ToldHighColesterol",
                        "CholesterolCheck", "BMI", "Smoker",
                        "HeartDisease", "Stroke", "PhysicalActivity", "Fruit",
                        "Vegetables", "HeavyDrinker", "HealthPlan",
                        "MedicalCost", "GeneralHealth", "MentalHealth",
                        "PhysicalHealth", "WalkingDifficulty", "Sex", "Age",
                        "Education", "Income")

colnames(new_data) # visualizza i nomi delle colonne


#####Aggiustiamo la colonna DIABETE

# Accorpa i valori 1 e 2 della colonna "Diabetes" in un unico valore 1
new_data <- new_data %>%
  mutate(Diabetes = ifelse(Diabetes %in% c(1, 2), 1, Diabetes))
# Verifica i cambiamenti
table(new_data$Diabetes) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


# Accorpa i valori 7 e 9 della colonna "Diabetes" in un unico valore 7
new_data <- new_data %>%
  mutate(Diabetes = ifelse(Diabetes %in% c(7, 9), 7, Diabetes))

# Verifica i cambiamenti
table(new_data$Diabetes) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

# Accorpa i valori 7 e 9 della colonna "Diabetes" in un unico valore 7
new_data <- new_data %>%
  mutate(Diabetes = ifelse(Diabetes %in% c(7, NA), 7, Diabetes))
# Verifica i cambiamenti
table(new_data$Diabetes) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora le colonne sono 1, 3, 4, 7 --> le faremo diventare 0 (al posto di 7), 1, 2, 3
#Mappa 7 in 0
new_data <- new_data %>%
  mutate(Diabetes = ifelse(Diabetes %in% c(7), 0, Diabetes))
# Verifica i cambiamenti  
table(new_data$Diabetes) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappa 3 in 2
new_data <- new_data %>%
  mutate(Diabetes = ifelse(Diabetes %in% c(3), 2, Diabetes))
# Verifica i cambiamenti
table(new_data$Diabetes) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappa 4 in 3
new_data <- new_data %>%
  mutate(Diabetes = ifelse(Diabetes %in% c(4), 3, Diabetes))
# Verifica i cambiamenti
table(new_data$Diabetes) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna HIGHBLOODPRESSURE
#Mappa 9 in 0
new_data <- new_data %>%
  mutate(HighBloodPressure = ifelse(HighBloodPressure %in% c(0, 9), 0, HighBloodPressure))
# Verifica i cambiamenti
table(new_data$HighBloodPressure) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


####Aggiustiamo la colonna TOLDHIGHCOLERESTEROL
#Mappa 9 in 7
new_data <- new_data %>%
  mutate(ToldHighColesterol = ifelse(ToldHighColesterol %in% c(7, 9), 7, ToldHighColesterol))
# Verifica i cambiamenti
table(new_data$ToldHighColesterol) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappa NA in 7
new_data <- new_data %>%
  mutate(ToldHighColesterol = ifelse(ToldHighColesterol %in% c(7, NA), 7, ToldHighColesterol))
# Verifica i cambiamenti
table(new_data$ToldHighColesterol) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora le colonne sono 1, 2, 7 (Yes, No, Not Sure/Don't Know/Refused)
#Di nuovo, la colonna 7 diventerà 0
new_data <- new_data %>%
  mutate(ToldHighColesterol = ifelse(ToldHighColesterol %in% c(7), 0, ToldHighColesterol))
# Verifica i cambiamenti
table(new_data$ToldHighColesterol) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna CHOLESTEROLCHECK
new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(0, NA), 0, CholesterolCheck))

table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo 9 in 0
new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(0, 9), 0, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo 7 in 0
new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(0, 7), 0, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato
  
#Mappo 8 in 7
new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(7, 8), 7, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna BMI
new_data <- new_data %>%
  mutate(BMI = ifelse(BMI %in% c(0, NA), 0, BMI))

table(new_data$BMI) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna SMOKER
#Mappo 9 in 0
new_data <- new_data %>%
  mutate(Smoker = ifelse(Smoker %in% c(0, 9), 0, Smoker))
table(new_data$Smoker) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna HEARTDISEASE
new_data <- new_data %>%
  mutate(HeartDisease = ifelse(HeartDisease %in% c(0, NA), 0, HeartDisease))

table(new_data$HeartDisease) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

######Aggiustiamo la colonna STROKE
new_data <- new_data %>%
  mutate(Stroke = ifelse(Stroke %in% c(7, 9), 7, Stroke))
table(new_data$Stroke) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(Stroke = ifelse(Stroke %in% c(7, NA), 7, Stroke))

table(new_data$Stroke) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora le colonne sono 1, 3, 7 (Yes, No, Not Sure/Don't Know/Refused)
#Di nuovo, la colonna 7 diventerà 0
new_data <- new_data %>%
  mutate(Stroke = ifelse(Stroke %in% c(7), 0, Stroke))
table(new_data$Stroke) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

######Aggiustiamo la colonna PHYSICALACTIVITY
new_data <- new_data %>%
  mutate(PhysicalActivity = ifelse(PhysicalActivity %in% c(0, 9), 0, PhysicalActivity))
table(new_data$PhysicalActivity) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

######Aggiustiamo la colonna FRUIT
#Mappo 9 in 0
new_data <- new_data %>%
  mutate(Fruit = ifelse(Fruit %in% c(0, 9), 0, Fruit))
table(new_data$Fruit) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

######Aggiustiamo la colonna VEGETABLES
#Mappo 9 in 0
new_data <- new_data %>%
  mutate(Vegetables = ifelse(Vegetables %in% c(0, 9), 0, Vegetables))
table(new_data$Vegetables) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

######Aggiustiamo la colonna HEAVYDRINKER
new_data <- new_data %>%
  mutate(HeavyDrinker = ifelse(HeavyDrinker %in% c(0, 9), 0, HeavyDrinker))
table(new_data$HeavyDrinker) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

######Aggiustiamo la colonna HEALTHPLAN
#Mappo 9 in 0
new_data <- new_data %>%
  mutate(HealthPlan = ifelse(HealthPlan %in% c(0, 9), 0, HealthPlan))
table(new_data$HealthPlan) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna MEDICAL COST
new_data <- new_data %>%
  mutate(MedicalCost = ifelse(MedicalCost %in% c(7, 9), 7, MedicalCost))
table(new_data$MedicalCost) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(MedicalCost = ifelse(MedicalCost %in% c(7, NA), 7, MedicalCost))
table(new_data$MedicalCost) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora le colonne sono 1, 2, 7 (Yes, No, Not Sure/Don't Know/Refused)
#Di nuovo, la colonna 7 diventerà 0
new_data <- new_data %>%
  mutate(MedicalCost = ifelse(MedicalCost %in% c(7), 0, MedicalCost))
table(new_data$MedicalCost) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato



######Aggiustiamo la colonna GENERALHEALTH
new_data <- new_data %>%
  mutate(GeneralHealth = ifelse(GeneralHealth %in% c(7, 9), 7, GeneralHealth))
table(new_data$GeneralHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(GeneralHealth = ifelse(GeneralHealth %in% c(7, NA), 7, GeneralHealth))
table(new_data$GeneralHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora le colonne sono 1, 2, 3, 4, 5, 7 (Excellent, Very Good, Good, Fair, Poor, Not Sure/Don't Know/Refused/Missing)
#Di nuovo, la colonna 7 diventerà 0
new_data <- new_data %>%
  mutate(GeneralHealth = ifelse(GeneralHealth %in% c(7), 0, GeneralHealth))
table(new_data$GeneralHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato



######Aggiustiamo la colonna MENTALHEALTH
new_data <- new_data %>%
  mutate(MentalHealth = ifelse(MentalHealth %in% c(77, 99), 77, MentalHealth))
table(new_data$MentalHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(MentalHealth = ifelse(MentalHealth %in% c(77, NA), 77, MentalHealth))
table(new_data$MentalHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#mappo 88 in 0
new_data <- new_data %>%
  mutate(MentalHealth = ifelse(MentalHealth %in% c(88), 0, MentalHealth))

table(new_data$MentalHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora le colonne sono da 0 a 30, e la 77 (Not Sure/Don't Know/Refused/Missing)
#N.B.  La 77 diventerà 31 dato che i giorni considerati sono 30!
new_data <- new_data %>%
  mutate(MentalHealth = ifelse(MentalHealth %in% c(77), 31, MentalHealth))
table(new_data$MentalHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna PHYSICALHEALTH
new_data <- new_data %>%
  mutate(PhysicalHealth = ifelse(PhysicalHealth %in% c(77, 99), 77, PhysicalHealth))
table(new_data$PhysicalHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(PhysicalHealth = ifelse(PhysicalHealth %in% c(77, NA), 77, PhysicalHealth))
table(new_data$PhysicalHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#mappo 88 in 0
new_data <- new_data %>%
  mutate(PhysicalHealth = ifelse(PhysicalHealth %in% c(88), 0, PhysicalHealth))
table(new_data$PhysicalHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora le colonne sono da 0 a 30, e la 77 (Not Sure/Don't Know/Refused/Missing)
#N.B. Anche qui la 77 diventerà 31 dato che i giorni considerati sono 30!
new_data <- new_data %>%
  mutate(PhysicalHealth = ifelse(PhysicalHealth %in% c(77), 31, PhysicalHealth))
table(new_data$PhysicalHealth) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna WALKINGDIFFICULTY
new_data <- new_data %>%
  mutate(WalkingDifficulty = ifelse(WalkingDifficulty %in% c(7, 9), 7, WalkingDifficulty))
table(new_data$WalkingDifficulty) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(WalkingDifficulty = ifelse(WalkingDifficulty %in% c(7, NA), 7, WalkingDifficulty))
table(new_data$WalkingDifficulty) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato
#Ora le colonne sono 1, 2, 7 (Yes, No, Not Sure/Don't Know/Refused)
#Di nuovo la 7 diventerà 0

new_data <- new_data %>%
  mutate(WalkingDifficulty = ifelse(WalkingDifficulty %in% c(7), 0, WalkingDifficulty))
table(new_data$WalkingDifficulty) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#####Aggiustiamo la colonna AGE
new_data <- new_data %>%
  mutate(Age = ifelse(Age %in% c(0, 14), 0, Age))
table(new_data$Age) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#####Aggiustiamo la colonna EDUCATION
#Mappo 9 in 0
new_data <- new_data %>%
  mutate(Education = ifelse(Education %in% c(0, 9), 0, Education))
table(new_data$Education) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

######Aggiustiamo la colonna INCOME
#Mappo 9 in 0
new_data <- new_data %>%
  mutate(Income = ifelse(Income %in% c(0, 9), 0, Income))
table(new_data$Income) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato


write.csv(new_data, file = "./data/diabetes_dataset_temp.csv")


# visualizza le prime righe del dataset con colonne rinominate
head(new_data)
attach(new_data)

#Controlla che non ci siano valori mancanti
sum(is.na(new_data))


# esporta il nuovo dataset finale in un file CSV
write.csv(new_data, file = "./data/diabetes_dataset_processed.csv")
