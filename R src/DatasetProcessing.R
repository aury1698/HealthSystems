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
                      "X_EDUCAG", "X_INCOMG1", "CHECKUP1", "BLDSUGAR", "FEETCHK3")
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
                        "Education", "Income", "Checkup", "BloodSugar", "FeetCheck")

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

#Invertiamo l'ordine per far sì che il caso peggiore, cioè "never", sia ultimo:  1-->8; 2-->1; 3-->2;  4--> 3 etc 8-->7, facendo ATTENZIONE a non sovrascrivere i valori già modificati
new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(1), 8, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(2), 1, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(3), 2, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(4), 3, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(5), 4, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(6), 5, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(7), 6, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

new_data <- new_data %>%
  mutate(CholesterolCheck = ifelse(CholesterolCheck %in% c(8), 7, CholesterolCheck))
table(new_data$CholesterolCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora le colonne sono Don't know/Refused, anytime < one year, 1 year but < 2 years, 2 years but < 3 years, 3 years but < 4 years, 4 years but < 5 years, 5 years or more, Never


######Aggiustiamo la colonna BMI
new_data <- new_data %>%
  mutate(BMI = ifelse(BMI %in% c(0, NA), 0, BMI))

table(new_data$BMI) # Per vedere la distribuzione dei valori 
head(new_data) # Visualizza le prime righe del dataframe modificato


######Aggiustiamo la colonna SMOKER
#Mappo 9 in 0
new_data <- new_data %>%
  mutate(Smoker = ifelse(Smoker %in% c(0, 9), 0, Smoker))
table(new_data$Smoker) # Per vedere la distribuzione dei valori 
head(new_data) # Visualizza le prime righe del dataframe modificato

#Ora invertiamo l' ordine, perché anziché avere everyday, someday, former, never avremo: Everyday-->4; Someday-->3; Former-->2; Never -->1. Attenzione a non sovrascrivere i valori, perché rendendo Everyday 1, poi come faccio a mappare il vecchio "1" in 4?
# Usiamo un valore intermedio (e.g., 7 perché 0 già usato) per evitare overwriting
new_data <- new_data %>% mutate(Smoker = ifelse(Smoker == 1, 7, Smoker))
new_data <- new_data %>% mutate(Smoker = ifelse(Smoker == 4, 1, Smoker))
new_data <- new_data %>% mutate(Smoker = ifelse(Smoker == 7, 4, Smoker))

# E ora scambiamo 2 e 3
new_data <- new_data %>% mutate(Smoker = ifelse(Smoker == 2, 7, Smoker))
new_data <- new_data %>% mutate(Smoker = ifelse(Smoker == 3, 2, Smoker))
new_data <- new_data %>% mutate(Smoker = ifelse(Smoker == 7, 3, Smoker))

table(new_data$Smoker) # Per vedere la distribuzione dei valori
#Ora le colonne sono 1, 2, 3, 4 (Never, Former, Someday, Everyday)



######Aggiustiamo la colonna HEARTDISEASE
new_data <- new_data %>%
  mutate(HeartDisease = ifelse(HeartDisease %in% c(0, NA), 0, HeartDisease))

table(new_data$HeartDisease) # Per vedere la distribuzione dei valori 
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

#Anche qui invertiamo l' ordine delle categorie (No high school grad sarà ultimo, il worst case). Attenzione anche qui a non sovrascrivere i valori già modificati. Usare un valore intermedio (e.g., 7 perché 0 è usato!) per evitare overwriting
#Quindi 1 --> 4; 2 --> 3; 3 --> 2; 4 --> 1 ma usando il valore intermedio
new_data <- new_data %>% mutate(Education = ifelse(Education == 1, 7, Education))
new_data <- new_data %>% mutate(Education = ifelse(Education == 4, 1, Education))
new_data <- new_data %>% mutate(Education = ifelse(Education == 7, 4, Education))

new_data <- new_data %>% mutate(Education = ifelse(Education == 2, 7, Education))
new_data <- new_data %>% mutate(Education = ifelse(Education == 3, 2, Education))
new_data <- new_data %>% mutate(Education = ifelse(Education == 7, 3, Education))

table(new_data$Education) # Per vedere la distribuzione dei valori 



######Aggiustiamo la colonna INCOME
#Mappo 9 in 0
new_data <- new_data %>%
  mutate(Income = ifelse(Income %in% c(0, 9), 0, Income))
table(new_data$Income) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Anche qui invertiremo, mettendo quelli con reddito minore (<15K) alla fine
#Usiamo sempre un valore intermedio (e.g., 11 perché 7 è usato) per evitare overwriting
#Quindi, usando 11 come segnaposto per gli swap, avremo 7--> 1; 6--> 2; 5--> 3; 4--> 4; 3--> 5; 2--> 6; 1--> 7
new_data <- new_data %>% mutate(Income = ifelse(Income == 1, 11, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 7, 1, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 11, 7, Income))
table(new_data$Income) # Per vedere la distribuzione dei valori 

new_data <- new_data %>% mutate(Income = ifelse(Income == 2, 11, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 6, 2, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 11, 6, Income))
table(new_data$Income) # Per vedere la distribuzione dei valori 

new_data <- new_data %>% mutate(Income = ifelse(Income == 3, 11, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 5, 3, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 11, 5, Income))
table(new_data$Income) # Per vedere la distribuzione dei valori

new_data <- new_data %>% mutate(Income = ifelse(Income == 5, 11, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 3, 5, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 11, 3, Income))
table(new_data$Income) # Per vedere la distribuzione dei valori

new_data <- new_data %>% mutate(Income = ifelse(Income == 6, 11, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 2, 6, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 11, 2, Income))
table(new_data$Income) # Per vedere la distribuzione dei valori

new_data <- new_data %>% mutate(Income = ifelse(Income == 7, 11, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 1, 7, Income))
new_data <- new_data %>% mutate(Income = ifelse(Income == 11, 1, Income))

table(new_data$Income) # Per vedere la distribuzione dei valori 
#Ora abbiamo # 0 - Dont't know/Not sure/Refused/Blank
# 1 - 200k+
# 2 - 100k< <200k
# 3 - 50k< <100k
# 4 - 35k< <50k
# 5 - 25k< <35k
# 6 - 15k< <25k
# 7 - Less than 15k


######Aggiustiamo la colonna CHECKUP
new_data <- new_data %>%
  mutate(Checkup = ifelse(Checkup %in% c(0, 7), 0, Checkup))
table(new_data$Checkup) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Metto 9 in 0
new_data <- new_data %>%
  mutate(Checkup = ifelse(Checkup %in% c(0, 9), 0, Checkup))
table(new_data$Checkup) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Metto NA in 0 
new_data <- new_data %>%
  mutate(Checkup = ifelse(Checkup %in% c(0, NA), 0, Checkup))
table(new_data$Checkup) # Per vedere la distribuzione dei valori 
head(new_data) # Visualizza le prime righe del dataframe modificato

#Metto 8 in 5 
new_data <- new_data %>%
  mutate(Checkup = ifelse(Checkup %in% c(5, 8), 5, Checkup))
table(new_data$Checkup) # Per vedere la distribuzione dei valori 
head(new_data) # Visualizza le prime righe del dataframe modificato



############Aggiustiamo la colonna BLOODSUGAR
#Mappo 888 (never) in 0
new_data <- new_data %>%
  mutate(BloodSugar = ifelse(BloodSugar %in% c(0, 888), 0, BloodSugar))
table(new_data$BloodSugar) # Per vedere la distribuzione dei valori
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo 777 (Not Sure/Don't Know/Refused/Missing) in 42069
new_data <- new_data %>%
  mutate(BloodSugar = ifelse(BloodSugar %in% c(42069, 777), 42069, BloodSugar))
table(new_data$BloodSugar) # Per vedere la distribuzione dei valori
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo 999 (Refused) in 42069
new_data <- new_data %>%
  mutate(BloodSugar = ifelse(BloodSugar %in% c(42069, 999), 42069, BloodSugar))
table(new_data$BloodSugar) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo NA in 42069
new_data <- new_data %>%
  mutate(BloodSugar = ifelse(BloodSugar %in% c(42069, NA), 42069, BloodSugar))
table(new_data$BloodSugar) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Se il valore è compreso fra 401-499 allora lo mappiamo in 01-99 che sono le volte che il paziente ha fatto il test all'anno (togliamo il "4")
new_data <- new_data %>%
  mutate(BloodSugar = ifelse(BloodSugar %in% c(401:499), BloodSugar - 400, BloodSugar))
table(new_data$BloodSugar) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Se il valore è compreso fra 301 e 399 allora mappiamo in 01-99 che sono le volte che il paziente ha fatto il test al mese (togliamo il "3") e moltiplico per 12 il valore per consierare il check annuale
new_data <- new_data %>%
  mutate(BloodSugar = ifelse(BloodSugar %in% c(301:399), (BloodSugar - 300) * 12, BloodSugar))
table(new_data$BloodSugar) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Se il valore è compreso fra 201 e 299 allora mappiamo in 01-99 che sono le volte che il paziente ha fatto il test alla settimana (togliamo il "2") e moltiplico per 52 il valore per consierare il check annuale
new_data <- new_data %>%
  mutate(BloodSugar = ifelse(BloodSugar %in% c(201:299), (BloodSugar - 200) * 52, BloodSugar))
table(new_data$BloodSugar) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Se il valore è compreso fra 101 e 199 allora mappiamo in 01-99 che sono le volte che il paziente ha fatto il test al giorno (togliamo il "1") e moltiplico per 365 il valore per consierare il check annuale
new_data <- new_data %>%
  mutate(BloodSugar = ifelse(BloodSugar %in% c(101:199), (BloodSugar - 100) * 365, BloodSugar))
table(new_data$BloodSugar) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato



#####Aggiustiamo la colonna FEETCHK3 proprio come abbiamo fatto per BloodSugar
#Mappo 888 (never) in 0
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(0, 888), 0, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo 777 (Not Sure/Don't Know/Refused/Missing) in 42069
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(42069, 777), 42069, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo 999 (Refused) in 42069
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(42069, 999), 42069, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo NA in 42069
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(42069, NA), 42069, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Mappo 555 (persone senza piedi) in 42069
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(42069, 555), 42069, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Se il valore è compreso fra 401-499 allora lo mappiamo in 01-99 che sono le volte che il paziente ha fatto il test all'anno (togliamo il "4")
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(401:499), FeetCheck - 400, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Se il valore è compreso fra 301 e 399 allora mappiamo in 01-99 che sono le volte che il paziente ha fatto il test al mese (togliamo il "3") e moltiplico per 12 il valore per consierare il check annuale
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(301:399), (FeetCheck - 300) * 12, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Se il valore è compreso fra 201 e 299 allora mappiamo in 01-99 che sono le volte che il paziente ha fatto il test alla settimana (togliamo il "2") e moltiplico per 52 il valore per consierare il check annuale
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(201:299), (FeetCheck - 200) * 52, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato

#Se il valore è compreso fra 101 e 199 allora mappiamo in 01-99 che sono le volte che il paziente ha fatto il test al giorno (togliamo il "1") e moltiplico per 365 il valore per consierare il check annuale
new_data <- new_data %>%
  mutate(FeetCheck = ifelse(FeetCheck %in% c(101:199), (FeetCheck - 100) * 365, FeetCheck))
table(new_data$FeetCheck) # Per vedere la distribuzione dei valori nella colonna Diabetes
head(new_data) # Visualizza le prime righe del dataframe modificato



write.csv(new_data, file = "./data/diabetes_dataset_temp.csv")


# visualizza le prime righe del dataset con colonne rinominate
head(new_data)
attach(new_data)

#Controlla che non ci siano valori mancanti
sum(is.na(new_data))


# esporta il nuovo dataset finale in un file CSV
write.csv(new_data, file = "./data/diabetes_dataset_processed.csv")

# Diabetes
# Ever told) (you had) diabetes? (If  ́Yes ́ and respondent is female, ask  ́Was this only when your 
# were pregnant? ́. If Respondent says pre-diabetes or borderline diabetes, use response code 4
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes / Yes but during pregnancy
# 2 - No
# 3 - Pre-Diabetes/Borderline

# HighBloodPressure 
# Adults who have been told they have high blood pressure by a doctor, nurse, or other health professional
# 0 - Dont't know/Not sure/Refused/Blank
# 1 No
# 2 Yes

# ToldHighColesterol
# Have you ever been told by a doctor, nurse or other health professional that your cholesterol is high
# 0 - Dont't know/Not sure/Refused/Blank
# 1 Yes
# 2 No

# CholesterolCheck
# About how long has it been since you last had your cholesterol checked
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Never
# 2 - Less than a year
# 3 - Less than two years
# 4 - Less than three years
# 5 - Less than four years
# 6 - Less than five years
# 7 - Five years or more

# BMI
# Body Mass Index
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Underweight
# 2 - Normal weight
# 3 - Overweight
# 4 - Obese

# Smoker
# Four-level smoker status: Everyday smoker, Someday smoker, Former smoker, Non-smoker
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Everyday Smoker
# 2 - Someday Smoker
# 3 - Former Smoker
# 4 - Never

# HeartDisease
# Respondents that have ever reported having coronary heart disease (CHD) or myocardial infarction (MI)
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No

# Stroke
# Ever told you had a stroke.
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No

# PhysicalActivity
# Adults who reported doing physical activity or exercise during the past 30 days other than their regular job
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No

# Fruit
# Consume Fruit 1 or more times per day
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No

# Vegetables
# Consume Vegetables 1 or more times per day
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No

# HeavyDrinker
# Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week)
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - No
# 2 - Yes

# HealthPlan
# Adults who had some form of health insurance
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No

# MedicalCost
# Was there a time in the past 12 months when you needed to see a doctor but could not because you could not afford it?
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No

# GeneralHealth
# Would you say that in general your health is
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Excellent
# 2 - Very Good
# 3 - Good
# 4 - Fair
# 5 - Poor

# MentalHealth
# Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good
# 0-30 - Days of not good mental health
# 31 - Dont't know/Not sure/Refused/Blank

# PhysicalHealth
# Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good
# 0-30 - Days of not good mental health
# 31 - Dont't know/Not sure/Refused/Blank

# WalkingDifficulty
# Do you have serious difficulty walking or climbing stairs?
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No

# Sex
# Sex of Respondent
# 1 - Male
# 2 - Female

# Age
# Fourteen-level age category
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - 18 / 24
# 2 - 25 / 29
# 3 - 30 / 34
# 4 - 35 / 39
# 5 - 40 / 44
# 6 - 45 / 49
# 7 - 50 / 54
# 8 - 55 / 59
# 9 - 60 / 64
# 10 - 65 / 69
# 11 - 70 / 74
# 12 - 75 / 79
# 13 - 80+

# Education
# What is the highest grade or year of school you completed
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Non high school graduate
# 2 - High school graduate
# 3 - Attended college
# 4 - College degree

# Income
# Is your annual household income from all sources
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Less than 15k
# 2 - 15k< <25k
# 3 - 25k< <35k
# 4 - 35k< <50k
# 5 - 50k< <100k
# 6 - 100k< <200k
# 7 - 200k+
