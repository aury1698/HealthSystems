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
library(FSA)

data <- read.csv("../data/diabetes_dataset_processed.csv", header = TRUE,
                 sep = ",")
numerical_columns <- c("Diabetes", "MentalHealth", "PhysicalHealth",
                       "BloodSugar", "FeetCheck")
numerical_data <- data[, numerical_columns]
rm(data)
attach(numerical_data)


# Coeff. di corr. lineare
with_diabetes = numerical_data[numerical_data["Diabetes"] == 1,]
# without_diabetes = numerical_data[numerical_data["Diabetes"] == 2,]

# non ci metto dentro BloodSugar e FeetCheck perchè a chi non ha il diabete 
# non sono state chieste
temp_columns <- c("Diabetes", "MentalHealth", "PhysicalHealth")
without_diabetes <- numerical_data[, temp_columns]
without_diabetes = without_diabetes[without_diabetes["Diabetes"] == 2,]

pre_diabetes <- numerical_data[, temp_columns]
pre_diabetes = pre_diabetes[numerical_data["Diabetes"] == 3,]

cor.test(Diabetes[Diabetes > 0 & MentalHealth <31],
         MentalHealth[Diabetes > 0 & MentalHealth <31],
         na.action=na.omit)

cor.test(Diabetes[Diabetes > 0 & PhysicalHealth <31],
         PhysicalHealth[Diabetes > 0 & PhysicalHealth <31],
         na.action=na.omit)

cor.test(Diabetes[Diabetes > 0 & BloodSugar <42069],
         BloodSugar[Diabetes > 0 & BloodSugar <42069],
         na.action=na.omit)

cor.test(Diabetes[Diabetes > 0 & FeetCheck <42069],
         FeetCheck[Diabetes > 0 & FeetCheck <42069],
         na.action=na.omit)

data_with_null=numerical_data
# ho controllato che feetcheck e bloodsugar non avessero 31 come valore
data_with_null[data_with_null == 31] <- NA
data_with_null[data_with_null == 42069] <- NA

#corrplot(cor(data_with_null))
#ggcorrplot(cor(data_with_null))

ggcorr(data_with_null, 
       method = c("pairwise", "pearson"), name = "Pearson corr. coeff.",
       hjust = 0.85, size = 5,
       nbreaks = 5,
       label = TRUE,
       label_size = 3,
       color = "grey50")

ggcorr(data_with_null, 
       method = c("pairwise", "spearman"), name = "Spearman corr. coeff.",
       hjust = 0.85, size = 5,
       nbreaks = 5,
       label = TRUE,
       label_size = 3,
       color = "grey50")


# occhio alla correlazione perchè nel questionario non tutte le domande
# sono state fatte a tutte le persone

par(mfrow=c(2,2))
qqnorm(MentalHealth[MentalHealth < 31],main = "MentalHealth")
qqline(MentalHealth[MentalHealth < 31])
qqnorm(PhysicalHealth[PhysicalHealth < 31],main = "PhysicalHealth")
qqline(PhysicalHealth[PhysicalHealth < 31])
qqnorm(BloodSugar[BloodSugar < 42069],main = "BloodSugar")
qqline(BloodSugar[BloodSugar < 42069])
qqnorm(FeetCheck[FeetCheck < 42069],main = "FeetCheck")
qqline(FeetCheck[FeetCheck < 42069])

# Controllo normalità con shapiro test
# shapiro ha dim. massima di 5k campioni

ad.test(MentalHealth[MentalHealth < 31])
ad.test(MentalHealth[MentalHealth < 31 & Diabetes==0])
ad.test(MentalHealth[MentalHealth < 31 & Diabetes==1])
ad.test(MentalHealth[MentalHealth < 31 & Diabetes==2])
ad.test(MentalHealth[MentalHealth < 31 & Diabetes==3])

ad.test(PhysicalHealth[PhysicalHealth < 31])
ad.test(PhysicalHealth[PhysicalHealth < 31 & Diabetes==0])
ad.test(PhysicalHealth[PhysicalHealth < 31 & Diabetes==1])
ad.test(PhysicalHealth[PhysicalHealth < 31 & Diabetes==2])
ad.test(PhysicalHealth[PhysicalHealth < 31 & Diabetes==3])

ad.test(BloodSugar[BloodSugar < 42069])
ad.test(BloodSugar[BloodSugar < 42069 & Diabetes==1])
ad.test(BloodSugar[BloodSugar < 42069 & Diabetes==2])
ad.test(BloodSugar[BloodSugar < 42069 & Diabetes==3])

ad.test(FeetCheck[FeetCheck < 42069])
ad.test(FeetCheck[FeetCheck < 42069 & Diabetes==1])
ad.test(FeetCheck[FeetCheck < 42069 & Diabetes==2])
ad.test(FeetCheck[FeetCheck < 42069 & Diabetes==3])

################################# roba sotto è vecchia NO##############

#ad.test(numerical_data[numerical_data["MentalHealth"] < 31])
#ad.test(numerical_data[numerical_data["PhysicalHealth"] < 31])
#ad.test(numerical_data[numerical_data["BloodSugar"] < 42069])
#ad.test(numerical_data[numerical_data["FeetCheck"] < 42069])
############################################################
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

bartlett.test(BloodSugar[Diabetes > 0 & BloodSugar < 42069] 
            ~ Diabetes[Diabetes > 0 & BloodSugar < 42069],
            data = numerical_data)
bartlett.test(FeetCheck[Diabetes > 0 & FeetCheck < 42069] 
            ~ Diabetes[Diabetes > 0 & FeetCheck < 42069],
            data = numerical_data)

# Dato che il gruppo 2 e gruppo 3 hanno solo BloodSugar e FeetCheck pari a 42069
# L'unico gruppo visto in questi ultimi due test diventa solo il gruppo 1 e quindi
# il test fallisce

# Kruskal perchè non sono normali
kruskal.test(MentalHealth[MentalHealth < 31] ~ Diabetes[MentalHealth < 31],
             data = numerical_data)
kruskal.test(PhysicalHealth[PhysicalHealth < 31] ~ Diabetes[PhysicalHealth < 31],
             data = numerical_data)
kruskal.test(BloodSugar[BloodSugar < 42069] ~ Diabetes[BloodSugar < 42069],
              data = numerical_data)
kruskal.test(FeetCheck[FeetCheck < 42069] ~ Diabetes[FeetCheck < 42069],
              data = numerical_data)
# Non si può fare per lo stesso motivo cui sopra


# post-hoc con bonferroni
dunnTest(MentalHealth[MentalHealth < 31 & Diabetes > 0] ~ 
           Diabetes[MentalHealth < 31 & Diabetes > 0],
         data = numerical_data, method = "bonferroni")
dunnTest(PhysicalHealth[PhysicalHealth < 31 & Diabetes > 0] ~ 
           Diabetes[PhysicalHealth < 31 & Diabetes > 0],
         data = numerical_data, method = "bonferroni")
dunnTest(BloodSugar[BloodSugar < 42069 & Diabetes > 0] ~
           Diabetes[BloodSugar < 42069 & Diabetes > 0],
         data = numerical_data, method = "bonferroni")
dunnTest(FeetCheck[FeetCheck < 42069 & Diabetes > 0] ~ 
           Diabetes[FeetCheck < 42069 & Diabetes > 0],
         data = numerical_data, method = "bonferroni")
