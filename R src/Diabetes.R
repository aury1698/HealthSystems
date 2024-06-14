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

mean(Diabetes)
median(Diabetes)
var(Diabetes)
summary(Diabetes)

boxplot(Diabetes, main = "Diabetes", xlab = "Diabetes")

ggplot(data, aes(x = Diabetes)) + geom_bar() +
  labs(title = "Distribuzione di Diabetes",
       x = "Diabetes",
       y = "Frequenza")


# EDA: HighBloodPressure 
# Adults who have been told they have high blood pressure by a doctor, nurse, or other health professional

mean(HighBloodPressure)
median(HighBloodPressure)
var(HighBloodPressure)
summary(HighBloodPressure)

boxplot(HighBloodPressure, main = "HighBloodPressure", xlab = "HighBloodPressure")

ggplot(data, aes(x = HighBloodPressure)) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "HighBloodPressure",
       y = "Frequenza")

# EDA: ToldHighColesterol
# Have you ever been told by a doctor, nurse or other health professional that your cholesterol is high

mean(ToldHighColesterol)
median(ToldHighColesterol)
var(ToldHighColesterol)

boxplot(ToldHighColesterol, main = "ToldHighColesterol", xlab = "ToldHighColesterol")

ggplot(data, aes(x = ToldHighColesterol)) + geom_bar() +
  labs(title = "Distribuzione di ToldHighColesterol",
       x = "ToldHighColesterol",
       y = "Frequenza")

# EDA: CholesterolCheck
# About how long has it been since you last had your cholesterol checked

mean(CholesterolCheck)
median(CholesterolCheck)
var(CholesterolCheck)

boxplot(CholesterolCheck, main = "CholesterolCheck", xlab = "CholesterolCheck")

ggplot(data, aes(x = CholesterolCheck)) + geom_bar() +
  labs(title = "Distribuzione di CholesterolCheck",
       x = "CholesterolCheck",
       y = "Frequenza")

# EDA: BMI
# Body Mass Index

mean(BMI)
median(BMI)
var(BMI)

boxplot(BMI, main = "BMI", xlab = "BMI")  #mi sembra ci siano molti outlier

ggplot(data, aes(x = BMI)) + geom_bar() +
  labs(title = "Distribuzione di BMI",
       x = "BMI",
       y = "Frequenza")

# EDA: Smoker
# Four-level smoker status: Everyday smoker, Someday smoker, Former smoker, Non-smoker

mean(Smoker)
median(Smoker)
var(Smoker)

boxplot(Smoker, main = "Smoker", xlab = "Smoker")

ggplot(data, aes(x = Smoker)) + geom_bar() +
  labs(title = "Distribuzione di Smoker",
       x = "Smoker",
       y = "Frequenza")

# EDA: HeartDisease
# Respondents that have ever reported having coronary heart disease (CHD) or myocardial infarction (MI)

mean(HeartDisease)
median(HeartDisease)
var(HeartDisease)

boxplot(HeartDisease, main = "HeartDisease", xlab = "HeartDisease")

ggplot(data, aes(x = HeartDisease)) + geom_bar() +
  labs(title = "Distribuzione di HeartDisease",
       x = "HeartDisease",
       y = "Frequenza")

# EDA: Stroke
# Ever told you had a stroke.

mean(Stroke)
median(Stroke)
var(Stroke)

boxplot(Stroke, main = "Stroke", xlab = "Stroke")

ggplot(data, aes(x = Stroke)) + geom_bar() +
  labs(title = "Distribuzione di Stroke",
       x = "Stroke",
       y = "Frequenza")

# EDA: PhysicalActivity
# Adults who reported doing physical activity or exercise during the past 30 days other than their regular job

mean(PhysicalActivity)
median(PhysicalActivity)
var(PhysicalActivity)

boxplot(PhysicalActivity, main = "PhysicalActivity", xlab = "PhysicalActivity")

ggplot(data, aes(x = PhysicalActivity)) + geom_bar() +
  labs(title = "Distribuzione di PhysicalActivity",
       x = "PhysicalActivity",
       y = "Frequenza")

# EDA: Fruit
# Consume Fruit 1 or more times per day

mean(Fruit)
median(Fruit)
var(Fruit)

boxplot(Fruit, main = "Fruit", xlab = "Fruit")

ggplot(data, aes(x = Fruit)) + geom_bar() +
  labs(title = "Distribuzione di Fruit",
       x = "Fruit",
       y = "Frequenza")

# EDA: Vegetables
# Consume Vegetables 1 or more times per day

mean(Vegetables)
median(Vegetables)
var(Vegetables)

boxplot(Vegetables, main = "Vegetables", xlab = "Vegetables")

ggplot(data, aes(x = Vegetables)) + geom_bar() +
  labs(title = "Distribuzione di Vegetables",
       x = "Vegetables",
       y = "Frequenza")

# EDA: HeavyDrinker
# Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week)

mean(HeavyDrinker)
median(HeavyDrinker)
var(HeavyDrinker)

boxplot(HeavyDrinker, main = "HeavyDrinker", xlab = "HeavyDrinker")

ggplot(data, aes(x = HeavyDrinker)) + geom_bar() +
  labs(title = "Distribuzione di HeavyDrinker",
       x = "HeavyDrinker",
       y = "Frequenza")

# EDA: HealthPlan
# Adults who had some form of health insurance

mean(HealthPlan)
median(HealthPlan)
var(HealthPlan)

boxplot(HealthPlan, main = "HealthPlan", xlab = "HealthPlan")

ggplot(data, aes(x = HealthPlan)) + geom_bar() +
  labs(title = "Distribuzione di HealthPlan",
       x = "HealthPlan",
       y = "Frequenza")

# EDA: MedicalCost
# Was there a time in the past 12 months when you needed to see a doctor but could not because you could not afford it?

mean(MedicalCost)
median(MedicalCost)
var(MedicalCost)

boxplot(MedicalCost, main = "MedicalCost", xlab = "MedicalCost")

ggplot(data, aes(x = MedicalCost)) + geom_bar() +
  labs(title = "Distribuzione di MedicalCost",
       x = "MedicalCost",
       y = "Frequenza")

# EDA: GeneralHealth
# Would you say that in general your health is

mean(GeneralHealth)
median(GeneralHealth)
var(GeneralHealth)

boxplot(GeneralHealth, main = "GeneralHealth", xlab = "GeneralHealth")

ggplot(data, aes(x = GeneralHealth)) + geom_bar() +
  labs(title = "Distribuzione di GeneralHealth",
       x = "GeneralHealth",
       y = "Frequenza")

# EDA: MentalHealth
# Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good

mean(MentalHealth)
median(MentalHealth)
var(MentalHealth)

boxplot(MentalHealth, main = "MentalHealth", xlab = "MentalHealth")

ggplot(data, aes(x = MentalHealth)) + geom_bar() +
  labs(title = "Distribuzione di MentalHealth",
       x = "MentalHealth",
       y = "Frequenza")

# EDA: PhysicalHealth
# Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good

mean(PhysicalHealth)
median(PhysicalHealth)
var(PhysicalHealth)

boxplot(PhysicalHealth, main = "PhysicalHealth", xlab = "PhysicalHealth")

ggplot(data, aes(x = PhysicalHealth)) + geom_bar() +
  labs(title = "Distribuzione di PhysicalHealth",
       x = "PhysicalHealth",
       y = "Frequenza")

# EDA: WalkingDifficulty
# Do you have serious difficulty walking or climbing stairs?

mean(WalkingDifficulty)
median(WalkingDifficulty)
var(WalkingDifficulty)

boxplot(WalkingDifficulty, main = "WalkingDifficulty", xlab = "WalkingDifficulty")

ggplot(data, aes(x = WalkingDifficulty)) + geom_bar() +
  labs(title = "Distribuzione di WalkingDifficulty",
       x = "WalkingDifficulty",
       y = "Frequenza")

# EDA: Sex
# Sex of Respondent

mean(Sex)
median(Sex)
var(Sex)

boxplot(Sex, main = "Sex", xlab = "Sex")

ggplot(data, aes(x = Sex)) + geom_bar() +
  labs(title = "Distribuzione di Sex",
       x = "Sex",
       y = "Frequenza")

# EDA: Age
# Fourteen-level age category

mean(Age)
median(Age)
var(Age)

boxplot(Age, main = "Age", xlab = "Age")

ggplot(data, aes(x = Age)) + geom_bar() +
  labs(title = "Distribuzione di Age",
       x = "Age",
       y = "Frequenza")

# EDA: Education
# What is the highest grade or year of school you completed

mean(Education)
median(Education)
var(Education)

boxplot(Education, main = "Education", xlab = "Education")

ggplot(data, aes(x = Education)) + geom_bar() +
  labs(title = "Distribuzione di Education",
       x = "Education",
       y = "Frequenza")

# EDA: Income
# Is your annual household income from all sources

mean(Income)
median(Income)
var(Income)

boxplot(Income, main = "Income", xlab = "Income")

ggplot(data, aes(x = Income)) + geom_bar() +
  labs(title = "Distribuzione di Income",
       x = "Income",
       y = "Frequenza")


#EDA: Correlazione tra variabili numeriche (da capire se rilevante)
correlation_matrix <- cor(data, use = "complete.obs")
library(corrplot)
if (!require(corrplot)) {
  install.packages("corrplot")
}
corrplot(correlation_matrix, method = "circle")

# Provvisorio: grafico di che fa vedere una variabile in funzione di un'altra (qui Sesso)
ggplot(data, aes(x = Age, fill = factor(Sex))) + 
  geom_bar(position = "dodge") +
  labs(title = "Distribuzione di Age per Sex",
       x = "Age",
       y = "Frequenza")


#Provvisiorio:  forse ha senso grafico del diabete in funzione di età e sesso (?)
# Prevalenza del diabete per fasce di età
ggplot(data, aes(x = Age, fill = factor(Diabetes))) + 
  geom_bar(position = "fill") +
  labs(title = "Prevalenza del diabete per fasce di età",
       x = "Age",
       y = "Proporzione")

#Prevalenza del diabete per fasce di età 
ggplot(data, aes(x = Age, fill = factor(Diabetes))) + 
  geom_bar(position = "dodge") +
  labs(title = "Prevalenza del diabete per fasce di età",
       x = "Age",
       y = "Frequenza")



















