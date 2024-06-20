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

if (!require(nortest)) {
  install.packages("nortest")
}

if (!require(corrplot)) {
  install.packages("corrplot")
}

library(haven)
library(foreign)
library(ggplot2)
library(GGally)
library(nortest)
library(corrplot)

data <- read.csv("../data/diabetes_dataset_processed.csv", header = TRUE,
                 sep = ",")
numerical_columns <- c("X", "Diabetes", "MentalHealth", "PhysicalHealth",
                       "BloodSugar", "FeetCheck")
numerical_data <- data[, numerical_columns]
attach(numerical_data)
rm(data)

# Coeff. di corr. lineare
with_diabetes = numerical_data[numerical_data["Diabetes"] == 1,]
# without_diabetes = numerical_data[numerical_data["Diabetes"] == 2,]

# non ci metto dentro BloodSugar e FeetCheck perchè a chi non ha il diabete 
# non sono state chieste
temp_columns <- c("X", "Diabetes", "MentalHealth", "PhysicalHealth")
without_diabetes <- numerical_data[, temp_columns]
without_diabetes = without_diabetes[without_diabetes["Diabetes"] == 2,]

pre_diabetes <- numerical_data[, temp_columns]
pre_diabetes = pre_diabetes[numerical_data["Diabetes"] == 3,]

cor.test(Diabetes[Diabetes > 0 & MentalHealth <31],
         MentalHealth[Diabetes > 0 & MentalHealth <31],
         na.action=na.omit)

# occhio alla correlazione perchè nel questionario non tutte le domande
# sono state fatte a tutte le persone

# Controllo normalità con shapiro test
# shapiro ha dim. massima di 5k campioni

# ad.test(numerical_data[numerical_data["MentalHealth"] < 31])
# ad.test(numerical_data[numerical_data["PhysicalHealth"] < 31])
# ad.test(numerical_data[numerical_data["BloodSugar"] < 42069])
#ad.test(numerical_data[numerical_data["FeetCheck"] < 42069])

# verifica diff.gruppi

ad.test(with_diabetes[with_diabetes["MentalHealth"] < 31 &
                        with_diabetes["PhysicalHealth"] < 31 &
                        with_diabetes["BloodSugar"] < 42069 &
                        with_diabetes["FeetCheck"] < 42069])
ad.test(without_diabetes[without_diabetes["MentalHealth"] < 31 &
                        without_diabetes["PhysicalHealth"] < 31])
ad.test(pre_diabetes[pre_diabetes["MentalHealth"] < 31 &
                        pre_diabetes["PhysicalHealth"] < 31] )

# Lontani dalla normalità, anche a causa del grande numero di campioni

# Controllo omoschedasticità con test di Bartlett
bartlett.test(MentalHealth[Diabetes > 0 & MentalHealth <31] 
              ~ Diabetes[Diabetes > 0 & MentalHealth <31],
              data = numerical_data)
bartlett.test(PhysicalHealth[Diabetes > 0 & PhysicalHealth <31] 
              ~ Diabetes[Diabetes > 0 & PhysicalHealth <31],
              data = numerical_data)

# bartlett.test(BloodSugar[Diabetes > 0 & BloodSugar < 42069] 
#              ~ Diabetes[Diabetes > 0 & BloodSugar < 42069],
#              data = numerical_data)
# bartlett.test(FeetCheck[Diabetes > 0 & FeetCheck < 42069] 
#              ~ Diabetes[Diabetes > 0 & FeetCheck < 42069],
#              data = numerical_data)

# Dato che il gruppo 2 e gruppo 3 hanno solo BloodSugar e FeetCheck pari a 42069
# L'unico gruppo visto in questi ultimi due test diventa solo il gruppo 1 e quindi
# il test fallisce

# ANOVA se normali e omoschedastici -> no
# realisticamente lavoriramo con anova a misure ripetute se sono normali-> manco
# probabilmente non lo sono, quindi la scaletta rimane più o meno la stessa -> solo probabilmente? sicuro sicuro?

# Kruskal perchè nun so nurmèli
kruskal.test(MentalHealth[MentalHealth < 31] ~ Diabetes[MentalHealth < 31],
             data = numerical_data)
kruskal.test(PhysicalHealth[PhysicalHealth < 31] ~ Diabetes[PhysicalHealth < 31],
             data = numerical_data)
# kruskal.test(BloodSugar[BloodSugar < 42069] ~ Diabetes[BloodSugar < 42069],
#              data = numerical_data)
# kruskal.test(FeetCheck[FeetCheck < 42069] ~ Diabetes[FeetCheck < 42069],
#              data = numerical_data)
# Non si può fare per lo stesso motivo cui sopra


# verifica sfericità con greehouse -> ma che roba è tienila lontana dai miei bambini
# post-hoc con bonferroni
dunnTest(MentalHealth[MentalHealth < 31] ~ Diabetes[MentalHealth < 31],
         data = numerical_data, method = "bonferroni")
dunnTest(PhysicalHealth[PhysicalHealth < 31] ~ Diabetes[PhysicalHealth < 31],
         data = numerical_data, method = "bonferroni")