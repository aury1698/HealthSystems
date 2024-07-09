# installa il pacchetto se non è già installato
if (!require(foreign)) {
  install.packages("foreign")
}

# installa il pacchetto se non è già installato
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

# installa il pacchetto se non è già installato
if (!require(ggplot2)) {
  install.packages('gmodels')
}


library(haven)
library(foreign)
library(ggplot2)
library(gmodels)


# Carica i dati "diabetes_dataset.csv"
data <- read.csv('../data/diabetes_dataset_processed.csv', header = TRUE, sep = ",")
head(data) # visualizza prime righe del dataframe
attach(data) # variabili utilizzabili direttamente, ignorare errore
columns = colnames(data)

# Controlla se ci sono valori nulli
sum(is.na(data))

# Controlla se ci sono valori duplicati
sum(duplicated(data)) # Non ci sono valori duplicati


#EDA per le variabili del dataset

# EDA: Diabetes
# Ever told) (you had) diabetes? (If  ́Yes ́ and respondent is female, ask  ́Was this only when your 
# were pregnant? ́. If Respondent says pre-diabetes or borderline diabetes, use response code 4
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes / Yes but during pregnancy
# 2 - No
# 3 - Pre-Diabetes/Borderline
mean(Diabetes)
median(Diabetes)
var(Diabetes)
summary(Diabetes)

table(Diabetes)
prop.table(table(Diabetes))

boxplot(Diabetes, main = "Diabetes", xlab = "Diabetes")

ggplot(data, aes(x = Diabetes)) + geom_bar() +
  labs(title = "Distribuzione di Diabetes",
       x = "Diabetes",
       y = "Frequenza")

basic_eda <- function(x, name){
  print("Mean")
  print(mean(x))
  print("Median")
  print(median(x))
  print("Var")
  print(var(x))
  print("Summary")
  print(summary(x))
  
  print("Values")
  print(table(x))
  print(prop.table(table(x)))
  
  print("Values with Diabetes")
  print(table(x[Diabetes == 1]))
  print(prop.table(table(x[Diabetes == 1])))
    
  print("Values without Diabetes")
  print(table(x[Diabetes == 2]))
  print(prop.table(table(x[Diabetes == 2])))
  
  print("Values with Borderline Diabetes")
  print(table(x[Diabetes == 3]))
  print(prop.table(table(x[Diabetes == 3])))
  
  CrossTable(x=x, y=Diabetes)
  
  ggplot(data, aes(x = x)) + geom_bar() +
  labs(title = paste("Distribuzione di", name, sep = " "),
      x = name,
      y = "Frequenza")
 
}

combined_eda <- function(x, y){
  CrossTable(x=x[Diabetes == 1], y=y[Diabetes == 1])
  CrossTable(x=x[Diabetes == 2], y=y[Diabetes == 2])
  CrossTable(x=x[Diabetes == 3], y=y[Diabetes == 3])
}

combined_graph <- function(x, y){
  # tocca vedere se si può fare, alle brutte sta qua e si copia
  # Assuming 'data' is your dataframe
  # First, reshape the data to a long format for both HighBloodPressure and ToldHighColesterol
  data_long <- pivot_longer(data, 
                            cols = c(x, y), 
                            names_to = "Condition", 
                            values_to = "Value")
  
  # Convert 'Value' to a factor for better plotting
  data_long$Value <- factor(data_long$Value, levels = c("0", "1", "2"),
                            labels = c("Don't know/Not sure/Refused/Blank", "Yes", "No"))
  
  # Define a named vector with new labels for Diabetes
  diabetes_labels <- setNames(c("Don't know/Not sure/Refused/Blank", " With Diabetes", "Without Diabetes", "Pre-Diabetes"), c("0", "1", "2", "3"))
  
  # Assuming 'data' is your dataframe and the rest of your code is unchanged
  # Modify the facet_wrap call to use the new diabetes labels
  ggplot(data_long, aes(x = Condition, fill = Value)) +
    geom_bar(position = "dodge") +
    facet_wrap(~ Diabetes, labeller = labeller(Diabetes = diabetes_labels)) +
    labs(title = paste(x, "and", y, "distribution divided by Diabetes", sep = " "),
         x = "",
         y = "Frequency",
         fill = "Answers") +
    theme_minimal()
}

# EDA: HighBloodPressure
# Adults who have been told they have high blood pressure by a doctor, nurse, or other health professional
# 0 - Dont't know/Not sure/Refused/Blank
# 1 Yes
# 2 No

basic_eda(HighBloodPressure, 'HighBloodPressure')
# mean(HighBloodPressure)
# median(HighBloodPressure)
# var(HighBloodPressure)
# summary(HighBloodPressure)

# table(HighBloodPressure)
# prop.table(table(HighBloodPressure))

# table(HighBloodPressure[Diabetes == 1])
# prop.table(table(HighBloodPressure[Diabetes == 1]))
# # Sembra che avere diabete e pressione alta siano neg. correlate

# table(HighBloodPressure[Diabetes == 2])
# prop.table(table(HighBloodPressure[Diabetes == 2]))
# # mentre non avere diabete e alta pressione correlate

# table(HighBloodPressure[Diabetes == 3])
# prop.table(table(HighBloodPressure[Diabetes == 3]))
# #qua siamo più vicini ad una distribuzione uguale

# CrossTable(x=HighBloodPressure, y=Diabetes)

# boxplot(HighBloodPressure, main = "HighBloodPressure", xlab = "HighBloodPressure")

# ggplot(data, aes(x = HighBloodPressure)) + geom_bar() +
#   labs(title = "Distribuzione di HighBloodPressure",
#        x = "HighBloodPressure",
#        y = "Frequenza")


# EDA: ToldHighColesterol
# Have you ever been told by a doctor, nurse or other health professional that your cholesterol is high
# 0 - Dont't know/Not sure/Refused/Blank
# 1 Yes
# 2 No
basic_eda(ToldHighColesterol, 'ToldHighColesterol')
combined_eda(HighBloodPressure, ToldHighColesterol)
combined_graph('HighBloodPressure', 'ToldHighColesterol')


# mean(ToldHighColesterol)
# median(ToldHighColesterol)
# var(ToldHighColesterol)

# table(ToldHighColesterol)
# prop.table(table(ToldHighColesterol))

# table(ToldHighColesterol[Diabetes == 1])
# prop.table(table(ToldHighColesterol[Diabetes == 1]))
# # avere diabete e colesterolo alto sembrano legate

# table(ToldHighColesterol[Diabetes == 2])
# prop.table(table(ToldHighColesterol[Diabetes == 2]))
# # mentre non avere diabete e non avere colesterolo sembrano andare insieme
# # alto numero di valori nulli per la struttura del questionario

# table(ToldHighColesterol[Diabetes == 3])
# prop.table(table(ToldHighColesterol[Diabetes == 3]))
# # simile ad avere il colesterolo

# boxplot(ToldHighColesterol, main = "ToldHighColesterol", xlab = "ToldHighColesterol")

# ggplot(data, aes(x = ToldHighColesterol)) + geom_bar() +
#   labs(title = "Distribuzione di ToldHighColesterol",
#        x = "ToldHighColesterol",
#        y = "Frequenza")

# EDA: CholesterolCheck
# About how long has it been since you last had your cholesterol checked
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Never
# 2 - Less than a year
# 3 - Less than two years
# 4 - Less than three years
# 5 - Less than four years
# 6 - Less than five years
# 7 - Five years or more
basic_eda(CholesterolCheck, 'CholesterolCheck')
# mean(CholesterolCheck)
# median(CholesterolCheck)
# var(CholesterolCheck)

# boxplot(CholesterolCheck, main = "CholesterolCheck", xlab = "CholesterolCheck")

# ggplot(data, aes(x = CholesterolCheck)) + geom_bar() +
#   labs(title = "Distribuzione di CholesterolCheck",
#        x = "CholesterolCheck",
#        y = "Frequenza")

# EDA: BMI
# Body Mass Index
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Underweight
# 2 - Normal weight
# 3 - Overweight
# 4 - Obese
basic_eda(BMI, 'BMI')
# mean(BMI)
# median(BMI)
# var(BMI)

# boxplot(BMI, main = "BMI", xlab = "BMI")  

# ggplot(data, aes(x = BMI)) + geom_bar() +
#   labs(title = "Distribuzione di BMI",
#        x = "BMI",
#        y = "Frequenza")

# EDA: Smoker
# Four-level smoker status: Everyday smoker, Someday smoker, Former smoker, Non-smoker
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Everyday Smoker
# 2 - Someday Smoker
# 3 - Former Smoker
# 4 - Never
basic_eda(Smoker, 'Smoker')
# mean(Smoker)
# median(Smoker)
# var(Smoker)

# boxplot(Smoker, main = "Smoker", xlab = "Smoker")

# ggplot(data, aes(x = Smoker)) + geom_bar() +
#   labs(title = "Distribuzione di Smoker",
#        x = "Smoker",
#        y = "Frequenza")

# EDA: HeartDisease
# Respondents that have ever reported having coronary heart disease (CHD) or myocardial infarction (MI)
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(HeartDisease, 'HeartDisease')
# mean(HeartDisease)
# median(HeartDisease)
# var(HeartDisease)

# boxplot(HeartDisease, main = "HeartDisease", xlab = "HeartDisease")

# ggplot(data, aes(x = HeartDisease)) + geom_bar() +
#   labs(title = "Distribuzione di HeartDisease",
#        x = "HeartDisease",
#        y = "Frequenza")

# EDA: Stroke
# Ever told you had a stroke.
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(Stroke, 'Stroke')
combined_graph('Stroke', 'HeartDisease')
# mean(Stroke)
# median(Stroke)
# var(Stroke)

# boxplot(Stroke, main = "Stroke", xlab = "Stroke")

# ggplot(data, aes(x = Stroke)) + geom_bar() +
#   labs(title = "Distribuzione di Stroke",
#        x = "Stroke",
#        y = "Frequenza")

# EDA: PhysicalActivity
# Adults who reported doing physical activity or exercise during the past 30 days other than their regular job
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(PhysicalActivity, 'PhysicalActivity')
# mean(PhysicalActivity)
# median(PhysicalActivity)
# var(PhysicalActivity)

# boxplot(PhysicalActivity, main = "PhysicalActivity", xlab = "PhysicalActivity")

# ggplot(data, aes(x = PhysicalActivity)) + geom_bar() +
#   labs(title = "Distribuzione di PhysicalActivity",
#        x = "PhysicalActivity",
#        y = "Frequenza")

# EDA: Fruit
# Consume Fruit 1 or more times per day
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(Fruit, 'Fruit')
# mean(Fruit)
# median(Fruit)
# var(Fruit)

# boxplot(Fruit, main = "Fruit", xlab = "Fruit")

# ggplot(data, aes(x = Fruit)) + geom_bar() +
#   labs(title = "Distribuzione di Fruit",
#        x = "Fruit",
#        y = "Frequenza")

# EDA: Vegetables
# Consume Vegetables 1 or more times per day
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(Vegetables, 'Vegetables')
# mean(Vegetables)
# median(Vegetables)
# var(Vegetables)

# boxplot(Vegetables, main = "Vegetables", xlab = "Vegetables")

# ggplot(data, aes(x = Vegetables)) + geom_bar() +
#   labs(title = "Distribuzione di Vegetables",
#        x = "Vegetables",
#        y = "Frequenza")

# EDA: HeavyDrinker
# Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week)
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - No
# 2 - Yes
basic_eda(HeavyDrinker, 'HeavyDrinker')
# mean(HeavyDrinker)
# median(HeavyDrinker)
# var(HeavyDrinker)

# boxplot(HeavyDrinker, main = "HeavyDrinker", xlab = "HeavyDrinker")

# ggplot(data, aes(x = HeavyDrinker)) + geom_bar() +
#   labs(title = "Distribuzione di HeavyDrinker",
#        x = "HeavyDrinker",
#        y = "Frequenza")

# EDA: HealthPlan
# Adults who had some form of health insurance
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(HealthPlan, 'HealthPlan')
# mean(HealthPlan)
# median(HealthPlan)
# var(HealthPlan)

# boxplot(HealthPlan, main = "HealthPlan", xlab = "HealthPlan")

# ggplot(data, aes(x = HealthPlan)) + geom_bar() +
#   labs(title = "Distribuzione di HealthPlan",
#        x = "HealthPlan",
#        y = "Frequenza")

# EDA: MedicalCost
# Was there a time in the past 12 months when you needed to see a doctor but could not because you could not afford it?
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(MedicalCost, 'MedicalCost')
# mean(MedicalCost)
# median(MedicalCost)
# var(MedicalCost)

# boxplot(MedicalCost, main = "MedicalCost", xlab = "MedicalCost")

# ggplot(data, aes(x = MedicalCost)) + geom_bar() +
#   labs(title = "Distribuzione di MedicalCost",
#        x = "MedicalCost",
#        y = "Frequenza")

# EDA: GeneralHealth
# Would you say that in general your health is
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Excellent
# 2 - Very Good
# 3 - Good
# 4 - Fair
# 5 - Poor
basic_eda(GeneralHealth, 'GeneralHealth')
# mean(GeneralHealth)
# median(GeneralHealth)
# var(GeneralHealth)

# boxplot(GeneralHealth, main = "GeneralHealth", xlab = "GeneralHealth")

# ggplot(data, aes(x = GeneralHealth)) + geom_bar() +
#   labs(title = "Distribuzione di GeneralHealth",
#        x = "GeneralHealth",
#        y = "Frequenza")

# EDA: MentalHealth
# Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good
# 0-30 - Days of not good mental health
# 31 - Dont't know/Not sure/Refused/Blank
basic_eda(MentalHealth, 'MentalHealth')
# mean(MentalHealth)
# median(MentalHealth)
# var(MentalHealth)

# boxplot(MentalHealth, main = "MentalHealth", xlab = "MentalHealth")

# ggplot(data, aes(x = MentalHealth)) + geom_bar() +
#   labs(title = "Distribuzione di MentalHealth",
#        x = "MentalHealth",
#        y = "Frequenza")

# EDA: PhysicalHealth
# Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good
# 0-30 - Days of not good mental health
# 31 - Dont't know/Not sure/Refused/Blank
basic_eda(PhysicalHealth, 'PhysicalHealth')
# mean(PhysicalHealth)
# median(PhysicalHealth)
# var(PhysicalHealth)

# boxplot(PhysicalHealth, main = "PhysicalHealth", xlab = "PhysicalHealth")

# ggplot(data, aes(x = PhysicalHealth)) + geom_bar() +
#   labs(title = "Distribuzione di PhysicalHealth",
#        x = "PhysicalHealth",
#        y = "Frequenza")

# EDA: WalkingDifficulty
# Do you have serious difficulty walking or climbing stairs?
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(WalkingDifficulty, 'WalkingDifficulty')
# mean(WalkingDifficulty)
# median(WalkingDifficulty)
# var(WalkingDifficulty)

# boxplot(WalkingDifficulty, main = "WalkingDifficulty", xlab = "WalkingDifficulty")

# ggplot(data, aes(x = WalkingDifficulty)) + geom_bar() +
#   labs(title = "Distribuzione di WalkingDifficulty",
#        x = "WalkingDifficulty",
#        y = "Frequenza")

# EDA: Sex
# Sex of Respondent
# 1 - Male
# 2 - Female
basic_eda(Sex, 'Sex')
# mean(Sex)
# median(Sex)
# var(Sex)

# boxplot(Sex, main = "Sex", xlab = "Sex")

# ggplot(data, aes(x = Sex)) + geom_bar() +
#   labs(title = "Distribuzione di Sex",
#        x = "Sex",
#        y = "Frequenza")

# EDA: Age
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
basic_eda(Age, 'Age')
# mean(Age)
# median(Age)
# var(Age)

# boxplot(Age, main = "Age", xlab = "Age")

# ggplot(data, aes(x = Age)) + geom_bar() +
#   labs(title = "Distribuzione di Age",
#        x = "Age",
#        y = "Frequenza")

# EDA: Education
# What is the highest grade or year of school you completed
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Non high school graduate
# 2 - High school graduate
# 3 - Attended college
# 4 - College degree
basic_eda(Education, 'Education')
# mean(Education)
# median(Education)
# var(Education)

# boxplot(Education, main = "Education", xlab = "Education")

# ggplot(data, aes(x = Education)) + geom_bar() +
#   labs(title = "Distribuzione di Education",
#        x = "Education",
#        y = "Frequenza")

# EDA: Income
# Is your annual household income from all sources
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Less than 15k
# 2 - 15k< <25k
# 3 - 25k< <35k
# 4 - 35k< <50k
# 5 - 50k< <100k
# 6 - 100k< <200k
# 7 - 200k+
basic_eda(Income, 'Income')
# mean(Income)
# median(Income)
# var(Income)

# boxplot(Income, main = "Income", xlab = "Income")

# ggplot(data, aes(x = Income)) + geom_bar() +
#   labs(title = "Distribuzione di Income",
#        x = "Income",
#        y = "Frequenza")

#EDA: CheckUp 
#About how long has it been since you last visited a doctor for a routine checkup?
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Less than 1 year
# 2 - 1-2 years
# 3 - 2-5 years
# 4 - 5+ years
# 5 - Never
basic_eda(Checkup, 'Checkup')
# mean(Checkup)
# median(Checkup)
# var(Checkup)

# boxplot(Checkup, main = "CheckUp", xlab = "CheckUp")

# ggplot(data, aes(x = Checkup)) + geom_bar() +
#   labs(title = "Distribuzione di CheckUp",
#        x = "CheckUp",
#        y = "Frequenza")


#EDA: BloodSugar
#About how often do you check your blood for glucose or sugar? 
# 42069 - Not sure/Refused/Blank
# Times per day, per week, per month, per year
basic_eda(BloodSugar, 'BloodSugar')
# mean(BloodSugar)
# median(BloodSugar)
# var(BloodSugar)

# boxplot(BloodSugar, main = "BloodSugar", xlab = "BloodSugar")

# ggplot(data, aes(x = BloodSugar)) + geom_bar() +
#   labs(title = "Distribuzione di BloodSugar",
#        x = "BloodSugar",
#        y = "Frequenza")

#EDA: FeetCheck
#Including times when checked by a family member or friend, about how often do you check your feet for sores or irritations?
# 42069 - Not sure/Refused/Blank
# Times per day, per week, per month, per year
basic_eda(FeetCheck, 'FeetCheck')
# mean(FeetCheck)
# median(FeetCheck)
# var(FeetCheck)

# boxplot(FeetCheck, main = "FeetCheck", xlab = "FeetCheck")

# ggplot(data, aes(x = FeetCheck)) + geom_bar() +
#   labs(title = "Distribuzione di FeetCheck",
#        x = "FeetCheck",
#        y = "Frequenza")

# #EDA: Correlazione tra variabili numeriche (da capire se rilevante)
# correlation_matrix <- cor(data, use = "complete.obs")
# library(corrplot)
# if (!require(corrplot)) {
#   install.packages("corrplot")
# }
# corrplot(correlation_matrix, method = "circle")

# # Provvisorio: grafico di che fa vedere una variabile in funzione di un'altra (qui Sesso)
# ggplot(data, aes(x = Age, fill = factor(Sex))) + 
#   geom_bar(position = "dodge") +
#   labs(title = "Distribuzione di Age per Sex",
#        x = "Age",
#        y = "Frequenza")

# #Provvisiorio:  forse ha senso grafico del diabete in funzione di età e sesso (?)
# # Prevalenza del diabete per fasce di età
# ggplot(data, aes(x = Age, fill = factor(Diabetes))) + 
#   geom_bar(position = "fill") +
#   labs(title = "Prevalenza del diabete per fasce di età",
#        x = "Age",
#        y = "Proporzione")

# #Prevalenza del diabete per fasce di età 
# ggplot(data, aes(x = Age, fill = factor(Diabetes))) + 
#   geom_bar(position = "dodge") +
#   labs(title = "Prevalenza del diabete per fasce di età",
#        x = "Age",
#        y = "Frequenza") 


