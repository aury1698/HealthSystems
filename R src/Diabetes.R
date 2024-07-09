# installa il pacchetto se non è già installato
if (!require(foreign)) {
  install.packages("foreign")
}

# installa il pacchetto se non è già installato
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

# installa il pacchetto se non è già installato
if (!require(gmodels)) {
  install.packages('gmodels')
}

# installa il pacchetto se non è già installato
if (!require(tidyr)) {
  install.packages('tidyr')
}

library(haven)
library(foreign)
library(ggplot2)
library(gmodels)
library(tidyr)
library(stringr)

# Carica i dati "diabetes_dataset.csv"
data <- read.csv('../data/diabetes_dataset_processed.csv', header = TRUE, sep = ",")
head(data) # visualizza prime righe del dataframe
attach(data) # variabili utilizzabili direttamente, ignorare errore
columns = colnames(data)

# Controlla se ci sono valori nulli
sum(is.na(data))

# Controlla se ci sono valori duplicati
sum(duplicated(data)) # Non ci sono valori duplicati


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

#boxplot(Diabetes, main = "Diabetes", xlab = "Diabetes")

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
  
  normal_plot <- ggplot(data, aes(x = x)) + geom_bar() +
    labs(title = paste("Distribuzione di", name, sep = " "),
      x = name,
      y = "Frequency") +
    scale_x_discrete(labels = c("0" = "Dont't know/Not sure/Refused/Blank", "1" = "Yes", "2" = "No")) # Custom x-axis labels
  
  print(normal_plot)
  
  if(str_equal(name, "CholesterolCheck")){  
    custom_label = c("0" = "Dont't know/Not sure/Refused/Blank", "1" = "Never", "2" = "Less than a year", "3" = "Less than two years", "4" = "Less than three years", "5" = "Less than four years", "6" = "Less than five years", "7" = "Five years or more")
  }else if(str_equal(name, "BMI")){
    custom_label = c("0" = "Don't know/Not sure/Refused/Blank", "1" = "Underweight", "2" = "Normal weight", "3" = "Overweight", "4" = "Obese")
  }else if(str_equal(name, "Smoker")){
    custom_label = c("0" = "Don't know/Not sure/Refused/Blank", "1" = "Never", "2" = "Former Smoker", "3" = "Someday Smoker", "4" = "Everyday Smoker")
  }else if(str_equal(name, "GeneralHealth")){
    custom_label = c("0" = "Don't know/Not sure/Refused/Blank", "1" = "Excellent", "2" = "Very Good", "3" = "Good", "4" = "Fair", "5" = "Poor")
  }else if(str_equal(name, "Age")){
    custom_label = c("0" = "Don't know/Not sure/Refused/Blank", "1" = "[18 - 24]", "2" = "[25 - 29]", "3" = "[30 - 34]", "4" =  "[35 - 39]", "5" = "[40 - 44]", "6" = "[45 - 49]", "7" = "[50 - 54]", "8" = "[55 - 59]", "9" = "[60 - 64]", "10" = "[65 - 69]", "11"= "[70 - 74]", "12" = "[75 - 79]", "13" = "80+")
  }else if(str_equal(name, "Education")){
    custom_label = c("0" = "Don't know/Not sure/Refused/Blank", "1" = "College degree", "2" = "Attended college", "3" = "High school graduate", "4" = "Attended high school")
  }else if(str_equal(name, "Income")){
    custom_label = c("0" = "Don't know/Not sure/Refused/Blank", "1" = "200k+", "2" = "100k< <200k", "3" = "50k< <100k", "4" = "35k< <50k", "5" = "25k< <35k", "6" = "15k< <25k", "7" = "Less than 15k")
  }else if(str_equal(name, "Checkup")){
    custom_label = c("0" = "Don't know/Not sure/Refused/Blank", "1" = "Within 1 year", "2" = "Within 2 years", "3" = "Within 5 years", "4" = "5+ years", "5" = "Never")
  }else{
    custom_label = c("0" = "Don't know/Not sure/Refused/Blank", "1" = "Yes", "2" = "No")
  }
  # Plot distribution of x with respect to Diabetes groups
  grouped_plot <- ggplot(data, aes(x = factor(x), fill = factor(Diabetes))) + 
    geom_bar(position = "dodge") +
    scale_fill_manual(values = c("1" = "blue", "2" = "red", "3" = "green"), 
                      labels = c("1" = "Yes", "2" = "No", "3" = "Pre-Diabetes")) +
    labs(title = paste("Distribution of", name, "by Diabetes Status"),
         x = name,
         y = "Frequency",
         fill = "Diabetes Status") +
    theme_minimal() +
    scale_x_discrete(labels = custom_label) # Custom x-axis labels
  print(grouped_plot)
}

combined_eda <- function(x,y){
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
  
  # filtrare nomi per mettere cose giuste
  # Convert 'Value' to a factor for better plotting
  if ((x == 'Smoker') | (y == 'Smoker')){
    data_long$Value <- factor(data_long$Value, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Don't know/Not sure/Refused/Blank", "Everyday Smoker", "Someday Smoker", "Former Smoker", "Never"))
  } else if ((x == 'Age') | (y =='Age')){
    data_long$Value <- factor(data_long$Value, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
                              labels = c("Don't know/Not sure/Refused/Blank", "Male - [18 - 24]", "Female - [25 - 29]", "[30 - 34]", "[35 - 39]", "[40 - 44]", "[45 - 49]", "[50 - 54]", "[55 - 59]", "[60 - 64]", "[65 - 69]", "[70 - 74]", "[75 - 79]", "80+"))
  }else{
    data_long$Value <- factor(data_long$Value, levels = c("0", "1", "2"),
                              labels = c("Don't know/Not sure/Refused/Blank", "Yes", "No"))
  }
  
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

# EDA: ToldHighColesterol
# Have you ever been told by a doctor, nurse or other health professional that your cholesterol is high
# 0 - Dont't know/Not sure/Refused/Blank
# 1 Yes
# 2 No
basic_eda(ToldHighColesterol, 'ToldHighColesterol')
combined_eda(HighBloodPressure, ToldHighColesterol)
combined_graph('HighBloodPressure', 'ToldHighColesterol')

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

# EDA: BMI
# Body Mass Index
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Underweight
# 2 - Normal weight
# 3 - Overweight
# 4 - Obese
basic_eda(BMI, 'BMI')


# EDA: Smoker
# Four-level smoker status: Everyday smoker, Someday smoker, Former smoker, Non-smoker
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Everyday Smoker
# 2 - Someday Smoker
# 3 - Former Smoker
# 4 - Never
basic_eda(Smoker, 'Smoker')

# EDA: HeartDisease
# Respondents that have ever reported having coronary heart disease (CHD) or myocardial infarction (MI)
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(HeartDisease, 'HeartDisease')

# EDA: Stroke
# Ever told you had a stroke.
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(Stroke, 'Stroke')
combined_graph('Stroke', 'HeartDisease')

# EDA: PhysicalActivity
# Adults who reported doing physical activity or exercise during the past 30 days other than their regular job
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(PhysicalActivity, 'PhysicalActivity')

# EDA: Fruit
# Consume Fruit 1 or more times per day
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(Fruit, 'Fruit')

# EDA: Vegetables
# Consume Vegetables 1 or more times per day
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(Vegetables, 'Vegetables')

# EDA: HeavyDrinker
# Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week)
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - No
# 2 - Yes
basic_eda(HeavyDrinker, 'HeavyDrinker')

# EDA: HealthPlan
# Adults who had some form of health insurance
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(HealthPlan, 'HealthPlan')

# EDA: MedicalCost
# Was there a time in the past 12 months when you needed to see a doctor but could not because you could not afford it?
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(MedicalCost, 'MedicalCost')

# EDA: GeneralHealth
# Would you say that in general your health is
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Excellent
# 2 - Very Good
# 3 - Good
# 4 - Fair
# 5 - Poor
basic_eda(GeneralHealth, 'GeneralHealth')

# EDA: MentalHealth
# Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good
# 0-30 - Days of not good mental health
# 31 - Dont't know/Not sure/Refused/Blank
basic_eda(MentalHealth, 'MentalHealth')

# EDA: PhysicalHealth
# Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good
# 0-30 - Days of not good mental health
# 31 - Dont't know/Not sure/Refused/Blank
basic_eda(PhysicalHealth, 'PhysicalHealth')

# EDA: WalkingDifficulty
# Do you have serious difficulty walking or climbing stairs?
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Yes
# 2 - No
basic_eda(WalkingDifficulty, 'WalkingDifficulty')

# EDA: Sex
# Sex of Respondent
# 1 - Male
# 2 - Female
basic_eda(Sex, 'Sex')

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

# EDA: Education
# What is the highest grade or year of school you completed
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Non high school graduate
# 2 - High school graduate
# 3 - Attended college
# 4 - College degree
basic_eda(Education, 'Education')

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

#EDA: CheckUp 
#About how long has it been since you last visited a doctor for a routine checkup?
# 0 - Dont't know/Not sure/Refused/Blank
# 1 - Less than 1 year
# 2 - 1-2 years
# 3 - 2-5 years
# 4 - 5+ years
# 5 - Never
basic_eda(Checkup, 'Checkup')

#EDA: BloodSugar
#About how often do you check your blood for glucose or sugar? 
# 42069 - Not sure/Refused/Blank
# Times per day, per week, per month, per year
basic_eda(BloodSugar, 'BloodSugar')

#EDA: FeetCheck
#Including times when checked by a family member or friend, about how often do you check your feet for sores or irritations?
# 42069 - Not sure/Refused/Blank
# Times per day, per week, per month, per year
basic_eda(FeetCheck, 'FeetCheck')

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


