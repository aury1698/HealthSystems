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
ordinal_columns = c("X", "Diabetes", "CholesterolCheck", "BMI", "Smoker", "GeneralHealth", 
                    "Age", "Education", "Income")
ordinal_data <- data[, ordinal_columns]
attach(ordinal_data)
rm(data)

# Coefficiente di correlazione di correlazione di Spearman
# 28 coeff. in totale
# Devo togliere gli zeri? non ho più vettori di stessa lunghezza
corr_matrix <- setNames(data.frame(matrix(0 ,nrow=8,ncol=8),
                            row.names=c("Diabetes", "CholesterolCheck", "BMI", "Smoker", "GeneralHealth", 
                                          "Age", "Education", "Income")),
                            c("Diabetes", "CholesterolCheck", "BMI", "Smoker", "GeneralHealth", 
                                "Age", "Education", "Income"))

corr <- cor.test(Diabetes, CholesterolCheck, method = 'spearman', exact=FALSE)
corr_matrix["Diabetes", "CholesterolCheck"] <- corr$p.value
corr <-cor.test(Diabetes, BMI, method = 'spearman', exact=FALSE)
corr_matrix["Diabetes", "BMI"] <- corr$p.value
corr <-cor.test(Diabetes, Smoker, method = 'spearman', exact=FALSE)
corr_matrix["Diabetes", "Smoker"] <- corr$p.value
corr <-cor.test(Diabetes, GeneralHealth, method = 'spearman', exact=FALSE)
corr_matrix["Diabetes", "GeneralHealth"] <- corr$p.value
corr <-cor.test(Diabetes, Age, method = 'spearman', exact=FALSE)
corr_matrix["Diabetes", "Age"] <- corr$p.value
corr <-cor.test(Diabetes, Education, method = 'spearman', exact=FALSE)
corr_matrix["Diabetes", "Education"] <- corr$p.value
corr <-cor.test(Diabetes, Income, method = 'spearman', exact=FALSE)
corr_matrix["Diabetes", "Income"] <- corr$p.value

corr <-cor.test(CholesterolCheck, BMI, method = 'spearman', exact=FALSE)
corr_matrix["CholesterolCheck", "BMI"] <- corr$p.value
corr <-cor.test(CholesterolCheck, Smoker, method = 'spearman', exact=FALSE)
corr_matrix["CholesterolCheck", "Smoker"] <- corr$p.value
corr <-cor.test(CholesterolCheck, GeneralHealth, method = 'spearman', exact=FALSE)
corr_matrix["CholesterolCheck", "GeneralHealth"] <- corr$p.value
corr <-cor.test(CholesterolCheck, Age, method = 'spearman', exact=FALSE)
corr_matrix["CholesterolCheck", "Age"] <- corr$p.value
corr <-cor.test(CholesterolCheck, Education, method = 'spearman', exact=FALSE)
corr_matrix["CholesterolCheck", "Education"] <- corr$p.value
corr <-cor.test(CholesterolCheck, Income, method = 'spearman', exact=FALSE)
corr_matrix["CholesterolCheck", "Income"] <- corr$p.value

corr <-cor.test(BMI, Smoker, method = 'spearman', exact=FALSE)
corr_matrix["BMI", "Smoker"] <- corr$p.value
corr <-cor.test(BMI, GeneralHealth, method = 'spearman', exact=FALSE)
corr_matrix["BMI", "GeneralHealth"] <- corr$p.value
corr <-cor.test(BMI, Age, method = 'spearman', exact=FALSE)
corr_matrix["BMI", "Age"] <- corr$p.value
corr <-cor.test(BMI, Education, method = 'spearman', exact=FALSE)
corr_matrix["BMI", "Education"] <- corr$p.value
corr <-cor.test(BMI, Income, method = 'spearman', exact=FALSE)
corr_matrix["BMI", "Income"] <- corr$p.value

corr <-cor.test(Smoker, GeneralHealth, method = 'spearman', exact=FALSE)
corr_matrix["Smoker", "GeneralHealth"] <- corr$p.value
corr <-cor.test(Smoker, Age, method = 'spearman', exact=FALSE)
corr_matrix["Smoker", "Age"] <- corr$p.value
corr <-cor.test(Smoker, Education, method = 'spearman', exact=FALSE)
corr_matrix["Smoker", "Education"] <- corr$p.value
corr <-cor.test(Smoker, Income, method = 'spearman', exact=FALSE)
corr_matrix["Smoker", "Income"] <- corr$p.value

corr <-cor.test(GeneralHealth, Age, method = 'spearman', exact=FALSE)
corr_matrix["GeneralHealth", "Age"] <- corr$p.value
corr <-cor.test(GeneralHealth, Education, method = 'spearman', exact=FALSE)
corr_matrix["GeneralHealth", "Education"] <- corr$p.value
corr <-cor.test(GeneralHealth, Income, method = 'spearman', exact=FALSE)
corr_matrix["GeneralHealth", "Income"] <- corr$p.value

corr <-cor.test(Age, Education, method = 'spearman', exact=FALSE)
corr_matrix["Age", "Education"] <- corr$p.value
corr <-cor.test(Age, Income, method = 'spearman', exact=FALSE)
corr_matrix["Age", "Income"] <- corr$p.value

corr <-cor.test(Education, Income, method = 'spearman', exact=FALSE)
corr_matrix["Education", "Income"] <- corr$p.value

install.packages("GGally")
library(GGally)
ggcorr(ordinal_data, 
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")
# I gruppi che confrontiamo sono 3: Diabete si, Diabete No, Pre-Diabete
# Per dati ordinali possiamo fare test di Kruscal-Wallis : Slide ANOVA pag.7
# oppure test di Friedman : Slide ANOVA pag.20
# Dobbiamo controllare l'indipendeza tra variabile ordinale e variabile categorica
# Teoricamente sono ripetuti perchè il questionario è uno e hanno chiesto
# la stessa cosa a tutti i pazienti -> sempre gli stessi soggetti divisi in gruppi a 
# cui chiedo diverse domande -> Friedman
# vedere se friedman si può fare tra più variabili o due per volta
# https://www.statology.org/friedman-test-r/
# Quali sono i blocchi e quali sono i gruppi?
friedman.test(Diabetes, CholesterolCheck, X, exact = FALSE)


# verifica diff.soggetti
# verifica diff.gruppi
# verifica sfericità con greehouse
# post-hoc con bonferroni
# coeff. di correlazione intraclasse
# test non parametrico di friedman
# analisi post-hoc non parametrica Wilcoxson

# si può fare questa roba con dati ordinali?