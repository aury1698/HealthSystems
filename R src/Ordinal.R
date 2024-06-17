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

# Carica i dati "diabetes_dataset.csv"
data <- read.csv("../data/diabetes_dataset_processed.csv", header = TRUE,
                 sep = ",")
ordinal_columns <- c("X", "Diabetes", "CholesterolCheck", "BMI", "Smoker",
                     "GeneralHealth", "Age", "Education", "Income")
ordinal_data <- data[, ordinal_columns]
attach(ordinal_data)
rm(data)

# Coefficiente di correlazione di correlazione di Spearman
# 28 coeff. in totale
corr_matrix <- setNames(data.frame(matrix(0 ,nrow=8,ncol=8),
                            row.names=c("Diabetes", "CholesterolCheck", "BMI", "Smoker", "GeneralHealth", 
                                          "Age", "Education", "Income")),
                            c("Diabetes", "CholesterolCheck", "BMI", "Smoker", "GeneralHealth", 
                                "Age", "Education", "Income", "CheckUp"))

corr <- cor.test(Diabetes, CholesterolCheck, method = "spearman", exact=FALSE)
corr_matrix["Diabetes","CholesterolCheck"] <- corr$estimate
corr <- cor.test(Diabetes, BMI, method = "spearman", exact=FALSE)
corr_matrix["Diabetes","BMI"] <- corr$estimate
corr <- cor.test(Diabetes, Smoker, method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Smoker"] <- corr$estimate
corr <- cor.test(Diabetes, GeneralHealth, method = "spearman", exact=FALSE)
corr_matrix["Diabetes","GeneralHealth"] <- corr$estimate
corr <- cor.test(Diabetes, Age, method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Age"] <- corr$estimate
corr <- cor.test(Diabetes, Education, method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Education"] <- corr$estimate
corr <- cor.test(Diabetes, Income, method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Income"] <- corr$estimate
corr <- cor.test(Diabetes, CheckUp, method = "spearman", exact=FALSE)
corr_matrix["Diabetes","CheckUp"] <- corr$estimate

corr <- cor.test(CholesterolCheck, BMI, method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","BMI"] <- corr$estimate
corr <- cor.test(CholesterolCheck, Smoker, method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Smoker"] <- corr$estimate
corr <- cor.test(CholesterolCheck, GeneralHealth, method = "spearman",
                 exact=FALSE)
corr_matrix["CholesterolCheck","GeneralHealth"] <- corr$estimate
corr <- cor.test(CholesterolCheck, Age, method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Age"] <- corr$estimate
corr <- cor.test(CholesterolCheck, Education, method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Education"] <- corr$estimate
corr <- cor.test(CholesterolCheck, Income, method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Income"] <- corr$estimate
corr <- cor.test(CholesterolCheck, CheckUp, method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","CheckUp"] <- corr$estimate

corr <- cor.test(BMI, Smoker, method = "spearman", exact=FALSE)
corr_matrix["BMI","Smoker"] <- corr$estimate
corr <- cor.test(BMI, GeneralHealth, method = "spearman", exact=FALSE)
corr_matrix["BMI","GeneralHealth"] <- corr$estimate
corr <- cor.test(BMI, Age, method = "spearman", exact=FALSE)
corr_matrix["BMI","Age"] <- corr$estimate
corr <- cor.test(BMI, Education, method = "spearman", exact=FALSE)
corr_matrix["BMI","Education"] <- corr$estimate
corr <- cor.test(BMI, Income, method = "spearman", exact=FALSE)
corr_matrix["BMI","Income"] <- corr$estimate
corr <- cor.test(BMI, CheckUp, method = "spearman", exact=FALSE)
corr_matrix["BMI","CheckUp"] <- corr$estimate

corr <- cor.test(Smoker, GeneralHealth, method = "spearman", exact=FALSE)
corr_matrix["Smoker","GeneralHealth"] <- corr$estimate
corr <- cor.test(Smoker, Age, method = "spearman", exact=FALSE)
corr_matrix["Smoker","Age"] <- corr$estimate
corr <- cor.test(Smoker, Education, method = "spearman", exact=FALSE)
corr_matrix["Smoker","Education"] <- corr$estimate
corr <- cor.test(Smoker, Income, method = "spearman", exact=FALSE)
corr_matrix["Smoker","Income"] <- corr$estimate
corr <- cor.test(Smoker, CheckUp, method = "spearman", exact=FALSE)
corr_matrix["Smoker","CheckUp"] <- corr$estimate

corr <- cor.test(GeneralHealth, Age, method = "spearman", exact=FALSE)
corr_matrix["GeneralHealth","Age"] <- corr$estimate
corr <- cor.test(GeneralHealth, Education, method = "spearman", exact=FALSE)
corr_matrix["GeneralHealth","Education"] <- corr$estimate
corr <- cor.test(GeneralHealth, Income, method = "spearman", exact=FALSE)
corr_matrix["GeneralHealth","Income"] <- corr$estimate
corr <- cor.test(GeneralHealth, CheckUp, method = "spearman", exact=FALSE)
corr_matrix["GeneralHealth","CheckUp"] <- corr$estimate

corr <- cor.test(Age, Education, method = "spearman", exact=FALSE)
corr_matrix["Age","Education"] <- corr$estimate
corr <- cor.test(Age, Income, method = "spearman", exact=FALSE)
corr_matrix["Age","Income"] <- corr$estimate
corr <- cor.test(Age, CheckUp, method = "spearman", exact=FALSE)
corr_matrix["Age","CheckUp"] <- corr$estimate

corr <- cor.test(Education, Income, method = "spearman", exact=FALSE)
corr_matrix["Education","Income"] <- corr$estimate
corr <- cor.test(Education, CheckUp, method = "spearman", exact=FALSE)
corr_matrix["Education","CheckUp"] <- corr$estimate

corr <- cor.test(Income, CheckUp, method = "spearman", exact=FALSE)
corr_matrix["Income","CheckUp"] <- corr$estimate

corr_matrix

# https://www.rdocumentation.org/packages/GGally/versions/2.2.1/topics/ggcorr

ggcorr(ordinal_data, 
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")

# I gruppi che confrontiamo sono 3: Diabete si, Diabete No, Pre-Diabete
# Per dati ordinali possiamo fare test di Kruscal-Wallis : Slide ANOVA pag.7
# oppure test di Friedman : Slide ANOVA pag.20
# In questo caso abbiamo 3 gruppi ma non sono misure ripetute perchè non abbiamo
# misurato "la stessa cosa" più volte -> Kruscal-Wallis
# https://www.statology.org/friedman-test-r/

with_diabetes = ordinal_data[ordinal_data["Diabetes"] == 1,]
without_diabetes = ordinal_data[ordinal_data["Diabetes"] == 2,]
pre_diabetes = ordinal_data[ordinal_data["Diabetes"] == 3,]

# plot the pdfs of the three groups

ggplot(ordinal_data, aes(x = CholesterolCheck, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "CholesterolCheck") +
  theme_minimal()

ggplot(ordinal_data, aes(x = BMI, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "BMI") +
  theme_minimal()

ggplot(ordinal_data, aes(x = Smoker, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "Smoker") +
  theme_minimal()

ggplot(ordinal_data, aes(x = GeneralHealth, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "GeneralHealth") +
  theme_minimal()

ggplot(ordinal_data, aes(x = Age, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "Age") +
  theme_minimal()

ggplot(ordinal_data, aes(x = Education, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "Education") +
  theme_minimal()

ggplot(ordinal_data, aes(x = Income, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "Income") +
  theme_minimal()

ggplot(ordinal_data, aes(x = CheckUp, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "CheckUp") +
  theme_minimal()
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kruskal.test
CholesterolCheck1 <- as.numeric(as.character(with_diabetes$CholesterolCheck))
CholesterolCheck2 <- as.numeric(as.character(without_diabetes$CholesterolCheck))
CholesterolCheck3 <- as.numeric(as.character(pre_diabetes$CholesterolCheck))
kruskal.test(list(CholesterolCheck1[CholesterolCheck1 > 0],
                  CholesterolCheck2[CholesterolCheck2 > 0],
                  CholesterolCheck3[CholesterolCheck3 > 0]))

BMI1 <- as.numeric(as.character(with_diabetes$BMI))
BMI2 <- as.numeric(as.character(without_diabetes$BMI))
BMI3 <- as.numeric(as.character(pre_diabetes$BMI))
kruskal.test(list(BMI1[BMI1 > 0],
                  BMI2[BMI2 > 0],
                  BMI3[BMI3 > 0]))

Smoker1 <- as.numeric(as.character(with_diabetes$Smoker))
Smoker2 <- as.numeric(as.character(without_diabetes$Smoker))
Smoker3 <- as.numeric(as.character(pre_diabetes$Smoker))
kruskal.test(list(Smoker1[Smoker1 > 0],
                  Smoker2[Smoker2 > 0],
                  Smoker3[Smoker3 > 0]))

GeneralHealth1 <- as.numeric(as.character(with_diabetes$GeneralHealth))
GeneralHealth2 <- as.numeric(as.character(without_diabetes$GeneralHealth))
GeneralHealth3 <- as.numeric(as.character(pre_diabetes$GeneralHealth))
kruskal.test(list(GeneralHealth1[GeneralHealth1 > 0],
                  GeneralHealth2[GeneralHealth2 > 0],
                  GeneralHealth3[GeneralHealth3 > 0]))

Age1 <- as.numeric(as.character(with_diabetes$Age))
Age2 <- as.numeric(as.character(without_diabetes$Age))
Age3 <- as.numeric(as.character(pre_diabetes$Age))
kruskal.test(list(Age1[Age1 > 0],
                  Age2[Age2 > 0],
                  Age3[Age3 > 0]))

Education1 <- as.numeric(as.character(with_diabetes$Education))
Education2 <- as.numeric(as.character(without_diabetes$Education))
Education3 <- as.numeric(as.character(pre_diabetes$Education))
kruskal.test(list(Education1[Education1 > 0],
                  Education2[Education2 > 0],
                  Education3[Education3 > 0]))

Income1 <- as.numeric(as.character(with_diabetes$Income))
Income2 <- as.numeric(as.character(without_diabetes$Income))
Income3 <- as.numeric(as.character(pre_diabetes$Income))
kruskal.test(list(Income1[Income1 > 0],
                  Income2[Income2 > 0],
                  Income3[Income3 > 0]))

CheckUp1 <- as.numeric(as.character(with_diabetes$CheckUp))
CheckUp2 <- as.numeric(as.character(without_diabetes$CheckUp))
CheckUp3 <- as.numeric(as.character(pre_diabetes$CheckUp))
kruskal.test(list(CheckUp1[CheckUp1 > 0],
                  CheckUp2[CheckUp2 > 0],
                  CheckUp3[CheckUp3 > 0]))
                  
# kruskal.test(CholesterolCheck ~ Diabetes, data = ordinal_data)
# kruskal.test(BMI ~ Diabetes, data = ordinal_data)
# kruskal.test(Smoker ~ Diabetes, data = ordinal_data)
# kruskal.test(GeneralHealth ~ Diabetes, data = ordinal_data)
# kruskal.test(Age ~ Diabetes, data = ordinal_data)
# kruskal.test(Education ~ Diabetes, data = ordinal_data)
# kruskal.test(Income ~ Diabetes, data = ordinal_data)

# verifica diff.soggetti
# verifica diff.gruppi
# verifica sfericità con greehouse
# post-hoc con bonferroni
# coeff. di correlazione intraclasse
# test non parametrico di friedman
# analisi post-hoc non parametrica Wilcoxson

# si può fare questa roba con dati ordinali?


