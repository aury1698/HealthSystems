if (!require(foreign)) {
  install.packages("foreign")
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
}

if (!require(dplyr)) {
  install.packages("GGally")
}

if (!require(FSA)) {
  install.packages("FSA")
}

library(haven)
library(foreign)
library(ggplot2)
library(GGally)
library(FSA)


# Load data "diabetes_dataset.csv"
data <- read.csv("../data/diabetes_dataset_processed.csv", header = TRUE,
                 sep = ",")
ordinal_columns <- c("X", "Diabetes", "CholesterolCheck", "BMI", "Smoker",
                     "GeneralHealth", "Age", "Education", "Income", "Checkup")
ordinal_data <- data[, ordinal_columns]
attach(ordinal_data)
rm(data)

# Spearman correlation coeff.
# 36 possible pairs
corr_matrix <- setNames(data.frame(matrix(0 ,nrow=9,ncol=9),
                            row.names=c("Diabetes", "CholesterolCheck", "BMI", "Smoker", "GeneralHealth", 
                                          "Age", "Education", "Income", "Checkup")),
                            c("Diabetes", "CholesterolCheck", "BMI", "Smoker", "GeneralHealth", 
                                "Age", "Education", "Income", "Checkup"))

corr <- cor.test(Diabetes[CholesterolCheck > 0 & Diabetes > 0],
                 CholesterolCheck[CholesterolCheck > 0 & Diabetes > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Diabetes","CholesterolCheck"] <- corr$estimate
corr <- cor.test(Diabetes[BMI > 0 & Diabetes > 0],
                 BMI[BMI > 0 & Diabetes > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Diabetes","BMI"] <- corr$estimate
corr <- cor.test(Diabetes[Smoker > 0 & Diabetes > 0],
                 Smoker[Smoker > 0 & Diabetes > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Smoker"] <- corr$estimate
corr <- cor.test(Diabetes[GeneralHealth > 0 & Diabetes > 0],
                 GeneralHealth[GeneralHealth > 0 & Diabetes > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Diabetes","GeneralHealth"] <- corr$estimate
corr <- cor.test(Diabetes[Age > 0 & Diabetes > 0],
                 Age[Age > 0 & Diabetes > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Age"] <- corr$estimate
corr <- cor.test(Diabetes[Education > 0 & Diabetes > 0],
                 Education[Education > 0 & Diabetes > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Education"] <- corr$estimate
corr <- cor.test(Diabetes[Income > 0 & Diabetes > 0],
                 Income[Income > 0 & Diabetes > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Income"] <- corr$estimate
corr <- cor.test(Diabetes[Checkup > 0 & Diabetes > 0],
                 Checkup[Checkup > 0 & Diabetes > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Diabetes","Checkup"] <- corr$estimate

corr <- cor.test(CholesterolCheck[BMI > 0 & CholesterolCheck > 0],
                 BMI[BMI > 0 & CholesterolCheck > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","BMI"] <- corr$estimate
corr <- cor.test(CholesterolCheck[Smoker > 0 & CholesterolCheck > 0],
                 Smoker[Smoker > 0 & CholesterolCheck > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Smoker"] <- corr$estimate
corr <- cor.test(CholesterolCheck[GeneralHealth > 0 & CholesterolCheck > 0],
                 GeneralHealth[GeneralHealth > 0 & CholesterolCheck > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","GeneralHealth"] <- corr$estimate
corr <- cor.test(CholesterolCheck[Age > 0 & CholesterolCheck > 0],
                 Age[Age > 0 & CholesterolCheck > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Age"] <- corr$estimate
corr <- cor.test(CholesterolCheck[Education > 0 & CholesterolCheck > 0],
                 Education[Education > 0 & CholesterolCheck > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Education"] <- corr$estimate
corr <- cor.test(CholesterolCheck[Income > 0 & CholesterolCheck > 0],
                 Income[Income > 0 & CholesterolCheck > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Income"] <- corr$estimate
corr <- cor.test(CholesterolCheck[Checkup > 0 & CholesterolCheck > 0],
                 Checkup[Checkup > 0 & CholesterolCheck > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["CholesterolCheck","Checkup"] <- corr$estimate

corr <- cor.test(BMI[Smoker > 0 & BMI > 0],
                 Smoker[Smoker > 0 & BMI > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["BMI","Smoker"] <- corr$estimate
corr <- cor.test(BMI[GeneralHealth > 0 & BMI > 0],
                 GeneralHealth[GeneralHealth > 0 & BMI > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["BMI","GeneralHealth"] <- corr$estimate
corr <- cor.test(BMI[Age > 0 & BMI > 0],
                 Age[Age > 0 & BMI > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["BMI","Age"] <- corr$estimate
corr <- cor.test(BMI[Education > 0 & BMI > 0],
                 Education[Education > 0 & BMI > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["BMI","Education"] <- corr$estimate
corr <- cor.test(BMI[Income > 0 & BMI > 0],
                 Income[Income > 0 & BMI > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["BMI","Income"] <- corr$estimate
corr <- cor.test(BMI[Checkup > 0 & BMI > 0],
                 Checkup[Checkup > 0 & BMI > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["BMI","Checkup"] <- corr$estimate

corr <- cor.test(Smoker[GeneralHealth > 0 & Smoker > 0],
                 GeneralHealth[GeneralHealth > 0 & Smoker > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Smoker","GeneralHealth"] <- corr$estimate
corr <- cor.test(Smoker[Age > 0 & Smoker > 0],
                 Age[Age > 0 & Smoker > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Smoker","Age"] <- corr$estimate
corr <- cor.test(Smoker[Education > 0 & Smoker > 0],
                 Education[Education > 0 & Smoker > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Smoker","Education"] <- corr$estimate
corr <- cor.test(Smoker[Income > 0 & Smoker > 0],
                 Income[Income > 0 & Smoker > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Smoker","Income"] <- corr$estimate
corr <- cor.test(Smoker[Checkup > 0 & Smoker > 0],
                 Checkup[Checkup > 0 & Smoker > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Smoker","Checkup"] <- corr$estimate

corr <- cor.test(GeneralHealth[Age > 0 & GeneralHealth > 0],
                 Age[Age > 0 & GeneralHealth > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["GeneralHealth","Age"] <- corr$estimate
corr <- cor.test(GeneralHealth[Education > 0 & GeneralHealth > 0],
                 Education[Education > 0 & GeneralHealth > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["GeneralHealth","Education"] <- corr$estimate
corr <- cor.test(GeneralHealth[Income > 0 & GeneralHealth > 0],
                 Income[Income > 0 & GeneralHealth > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["GeneralHealth","Income"] <- corr$estimate
corr <- cor.test(GeneralHealth[Checkup > 0 & GeneralHealth > 0],
                 Checkup[Checkup > 0 & GeneralHealth > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["GeneralHealth","Checkup"] <- corr$estimate

corr <- cor.test(Age[Education > 0 & Age > 0],
                 Education[Education > 0 & Age > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Age","Education"] <- corr$estimate
corr <- cor.test(Age[Income > 0 & Age > 0],
                 Income[Income > 0 & Age > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Age","Income"] <- corr$estimate
corr <- cor.test(Age[Checkup > 0 & Age > 0],
                 Checkup[Checkup > 0 & Age > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Age","Checkup"] <- corr$estimate

corr <- cor.test(Education[Income > 0 & Education > 0],
                 Income[Income > 0 & Education > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Education","Income"] <- corr$estimate
corr <- cor.test(Education[Checkup > 0 & Education > 0],
                 Checkup[Checkup > 0 & Education > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Education","Checkup"] <- corr$estimate

corr <- cor.test(Income[Checkup > 0 & Income > 0],
                 Checkup[Checkup > 0 & Income > 0],
                 method = "spearman", exact=FALSE)
corr_matrix["Income","Checkup"] <- corr$estimate

corr_matrix

# https://www.rdocumentation.org/packages/GGally/versions/2.2.1/topics/ggcorr

# 0 deve essere Na e indicare come sono gestiti gli NA per ottenere stessi risultati
data_with_null=ordinal_data
data_with_null[data_with_null == 0] <- NA
ggcorr(data_with_null, 
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


# verifica diff.soggetti
# verifica diff.gruppi
# Concetti come varianza non sono applicabili a dati ordinali, perciò ragionamenti tipo
# differenze between e within gruops non si possono fare

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

ggplot(ordinal_data, aes(x = Checkup, fill = factor(Diabetes))) +
  geom_bar() +
  labs(title = "Checkup") +
  theme_minimal()
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kruskal.test
# CholesterolCheck1 <- as.numeric(as.character(with_diabetes$CholesterolCheck))
# CholesterolCheck2 <- as.numeric(as.character(without_diabetes$CholesterolCheck))
# CholesterolCheck3 <- as.numeric(as.character(pre_diabetes$CholesterolCheck))
# kruskal.test(list(CholesterolCheck1[CholesterolCheck1 > 0],
#                   CholesterolCheck2[CholesterolCheck2 > 0],
#                   CholesterolCheck3[CholesterolCheck3 > 0]))

# BMI1 <- as.numeric(as.character(with_diabetes$BMI))
# BMI2 <- as.numeric(as.character(without_diabetes$BMI))
# BMI3 <- as.numeric(as.character(pre_diabetes$BMI))
# kruskal.test(list(BMI1[BMI1 > 0],
#                   BMI2[BMI2 > 0],
#                   BMI3[BMI3 > 0]))

# Smoker1 <- as.numeric(as.character(with_diabetes$Smoker))
# Smoker2 <- as.numeric(as.character(without_diabetes$Smoker))
# Smoker3 <- as.numeric(as.character(pre_diabetes$Smoker))
# kruskal.test(list(Smoker1[Smoker1 > 0],
#                   Smoker2[Smoker2 > 0],
#                   Smoker3[Smoker3 > 0]))

# GeneralHealth1 <- as.numeric(as.character(with_diabetes$GeneralHealth))
# GeneralHealth2 <- as.numeric(as.character(without_diabetes$GeneralHealth))
# GeneralHealth3 <- as.numeric(as.character(pre_diabetes$GeneralHealth))
# kruskal.test(list(GeneralHealth1[GeneralHealth1 > 0],
#                   GeneralHealth2[GeneralHealth2 > 0],
#                   GeneralHealth3[GeneralHealth3 > 0]))

# Age1 <- as.numeric(as.character(with_diabetes$Age))
# Age2 <- as.numeric(as.character(without_diabetes$Age))
# Age3 <- as.numeric(as.character(pre_diabetes$Age))
# kruskal.test(list(Age1[Age1 > 0],
#                   Age2[Age2 > 0],
#                   Age3[Age3 > 0]))

# Education1 <- as.numeric(as.character(with_diabetes$Education))
# Education2 <- as.numeric(as.character(without_diabetes$Education))
# Education3 <- as.numeric(as.character(pre_diabetes$Education))
# kruskal.test(list(Education1[Education1 > 0],
#                   Education2[Education2 > 0],
#                   Education3[Education3 > 0]))

# Income1 <- as.numeric(as.character(with_diabetes$Income))
# Income2 <- as.numeric(as.character(without_diabetes$Income))
# Income3 <- as.numeric(as.character(pre_diabetes$Income))
# kruskal.test(list(Income1[Income1 > 0],
#                   Income2[Income2 > 0],
#                   Income3[Income3 > 0]))

# Checkup1 <- as.numeric(as.character(with_diabetes$Checkup))
# Checkup2 <- as.numeric(as.character(without_diabetes$Checkup))
# Checkup3 <- as.numeric(as.character(pre_diabetes$Checkup))
# kruskal.test(list(Checkup1[Checkup1 > 0],
#                   Checkup2[Checkup2 > 0],
#                   Checkup3[Checkup3 > 0]))
                  
kruskal.test(CholesterolCheck[CholesterolCheck > 0] ~ Diabetes[CholesterolCheck > 0],
             data = ordinal_data)
kruskal.test(BMI[BMI > 0] ~ Diabetes[BMI > 0], data = ordinal_data)
kruskal.test(Smoker[Smoker > 0] ~ Diabetes[Smoker > 0], data = ordinal_data)
kruskal.test(GeneralHealth[GeneralHealth > 0] ~ Diabetes[GeneralHealth > 0],
             data = ordinal_data)
kruskal.test(Age[Age > 0] ~ Diabetes[Age > 0], data = ordinal_data)
kruskal.test(Education[Education > 0] ~ Diabetes[Education > 0], data = ordinal_data)
kruskal.test(Income[Income > 0] ~ Diabetes[Income > 0], data = ordinal_data)
kruskal.test(Checkup[Checkup > 0] ~ Diabetes[Checkup > 0], data = ordinal_data)


# verifica sfericità con greehouse
# non applicabile a dati ordinali
# post-hoc con bonferroni
# Dunn perchè è spesso accoppiato al kruskal-Wallis
dunnTest(CholesterolCheck[CholesterolCheck > 0] ~ Diabetes[CholesterolCheck > 0],
         data = ordinal_data, method = "bonferroni")
dunnTest(BMI[BMI > 0] ~ Diabetes[BMI > 0], data = ordinal_data, method = "bonferroni")
dunnTest(Smoker[Smoker > 0] ~ Diabetes[Smoker > 0], data = ordinal_data,
         method = "bonferroni")
dunnTest(GeneralHealth[GeneralHealth > 0] ~ Diabetes[GeneralHealth > 0],
         data = ordinal_data, method = "bonferroni")
dunnTest(Age[Age > 0] ~ Diabetes[Age > 0], data = ordinal_data, method = "bonferroni")
dunnTest(Education[Education > 0] ~ Diabetes[Education > 0], data = ordinal_data,
         method = "bonferroni")
dunnTest(Income[Income > 0] ~ Diabetes[Income > 0], data = ordinal_data,
         method = "bonferroni")
dunnTest(Checkup[Checkup > 0] ~ Diabetes[Checkup > 0], data = ordinal_data,
         method = "bonferroni")




