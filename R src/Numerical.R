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

data <- read.csv("../data/diabetes_dataset_processed.csv", header = TRUE,
                 sep = ",")
numerical_columns <- c("X", "Diabetes", "MentalHealth", "PhysicalHealth", "BloodSugar", "FeetCheck")
numerical_data <- data[, numerical_columns]
attach(numerical_data)
rm(data)

# Coeff. di corr. lineare
with_diabetes = numerical_data[numerical_data["Diabetes"] == 1,]
without_diabetes = numerical_data[numerical_data["Diabetes"] == 2,]
pre_diabetes = numerical_data[numerical_data["Diabetes"] == 3,]

MentalHealth1 <- as.numeric(as.character(with_diabetes$MentalHealth))
MentalHealth2 <- as.numeric(as.character(without_diabetes$MentalHealth))
MentalHealth3 <- as.numeric(as.character(pre_diabetes$MentalHealth))

cor(numerical_data[numerical_data["MentalHealth" < 31] &
                     numerical_data["PhysicalHealth" < 31] &
                     numerical_data["BloodSugar"] < 42069 &
                     numerical_data["FeetCheck"]< 42069])

# Controllo normalità con shapiro test
# shapiro ha dim. massima di 5k campioni
library(nortest)
ad.test(numerical_data[numerical_data["MentalHealth"] < 31])
ad.test(numerical_data[numerical_data["PhysicalHealth"] < 31])
ad.test(numerical_data[numerical_data["BloodSugar"] < 42069])
ad.test(numerical_data[numerical_data["FeetCheck"] < 42069])

# Controllo omoschedasticità con test di Bartlett
bartlett.test(MentalHealth ~ Diabetes, data = numerical_data)
bartlett.test(PhysicalHealth ~ Diabetes, data = numerical_data)
bartlett.test(BloodSugar ~ Diabetes, data = numerical_data)
bartlett.test(FeetCheck ~ Diabetes, data = numerical_data)
# ANOVA se normali e omoschedastici
# realisticamente lavoriramo con anova a misure ripetute se sono normali
# probabilmente non lo sono, quindi la scaletta rimane più o meno la stessa

# verifica diff.soggetti
# verifica diff.gruppi
# verifica sfericità con greehouse
# post-hoc con bonferroni
# coeff. di correlazione intraclasse
# test non parametrico di friedman
# analisi post-hoc non parametrica