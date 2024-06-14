library(haven)
library(foreign)

# installa il pacchetto se non è già installato
if (!require(foreign)) {
  install.packages("foreign")
}


# carica dati in formato xpt da file con header
data <- read.xport(file.choose())
head(data) # visualizza prime righe del dataframe
attach(data) # variabili utilizzabili direttamente

colnames(data) # visualizza i nomi delle colonne

# seleziona solo le colonne specificate
selected_columns <- c("DIABETE4", "X_RFHYPE6", "TOLDHI3",
                      "X_CHOLCH3", "X_BMI5", "X_SMOKER3",
                      "X_MICHD", "CVDSTRK3", "X_TOTINDA", "X_FRTLT1A",
                      "X_VEGLT1A", "X_RFDRHV7", "X_HLTHPLN",
                      "MEDCOST1", "GENHLTH", "MENTHLTH",
                      "PHYSHLTH", "DIFFWALK", "SEXVAR", "X_AGEG5YR",
                      "EDUCA", "INCOME3")
new_data <- data[, selected_columns]

# esporta il nuovo dataset in un file CSV
write.csv(new_data, file = "./data/diabetes_dataset.csv")

# visualizza le prime righe del nuovo dataset
head(new_data)
attach(new_data) 

#Rinomina le colonne del dataset in modo più chiaro in inglese
colnames(new_data) <- c("Diabetes", "HighBloodPressure", "ToldHighColesterol",
                        "CholesterolCheck", "BMI", "Smoker",
                        "HeartDisease", "Stroke", "PhysicalActivity", "Fruit",
                        "Vegetables", "HeavyDrinker", "HealthPlan",
                        "MedicalCost", "GeneralHealth", "MentalHealth",
                        "PhysicalHealth", "WalkingDifficulty", "Sex", "Age",
                        "Education", "Income")

# visualizza le prime righe del dataset con colonne rinominate
head(new_data)
attach(new_data) 
