# Prendere variabili categoriche e fare test chi^2 di pearson per l'indipendenza : Slide test di adattamento - pag.14
# Esempio del professore
# xmat <- matrix(c(10,18,1,22),nrow=2,byrow=T) 
# xmat
# chisq.test(xmat)
# fisher.test(xmat) # test esatto di Fisher

# H_0 : la pdf congiunta = prodotto delle pdf marginali
# H_1 : la pdf congiunta != prodotto delle pdf marginali
# quindi se accetto H_0 allora le variabili sono indipendenti, altrimenti sono dipendenti

#Link utile: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/chisq.test

# Noi abbiamo 13 variabili categoriche, dobbiamo scegliere 2 variabili e fare il test
# Sono 13*12/2 = 78 coppie di variabili, salviamo i p-value per valutarli dopo
#Proviamo a prendere queste due variabili del dataset "diabetes:dataset_processed" e fare il test chi quadro per vedere se sono indipendenti
#Il prof lo fa in "test-parametrici.R"
library(haven)
library(foreign)
library(ggplot2)
library(GGally)
library(egg)

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

if (!require(egg)) {
  install.packages("egg")
}


# Carica i dati "diabetes_dataset_processed.csv"
data <- read.csv("../data/diabetes_dataset_processed.csv")
head(data) # visualizza prime righe del dataframe
attach(data) # variabili utilizzabili direttamente, ignorare errore
categorical_columns = c("Sex", "Fruit", "MedicalCost",
                       "HeartDisease", "Stroke", "PhysicalActivity", "WalkingDifficulty",
                       "Vegetables", "HeavyDrinker", "HealthPlan", "ToldHighColesterol",
                       "HighBloodPressure", "Diabetes")
categorical_data <- data[, categorical_columns]
rm(data)
attach(categorical_data)




# #Prendiamo Diabetes e HighBloodPressure per fare i test chi quadro e vedere se sono indipendenti
# diabetes <- Diabetes
# highBloodPressure <- HighBloodPressure


# # Create the contingency table
# contingencyTable <- table(diabetes, highBloodPressure)

# # Perform the chi-squared test
# chisqResult <- chisq.test(contingencyTable)

# # Print the result
# print(chisqResult)  # test chi quadrato di Pearson
# #Dato che il p-value è molto basso, possiamo rifiutare l'ipotesi nulla e concludere che le due variabili Diabetes e High Blood Pressure non sono indipendenti

# #Proviamo a fare il test esatto di Fisher per le stesse variabili
# # fisherResult <- fisher.test(contingencyTable, workspace = 2e+06) --> out of space

# fisherResult <- fisher.test(contingencyTable, simulate.p.value = TRUE) # test esatto di Fisher con p-value simulato cioè approssimato con Monte Carlo perché la tabella è troppo grande
# print(fisherResult) # test esatto di Fisher --> p-value = 0.0004998 quindi rifiutiamo l'ipotesi nulla e concludiamo che le due variabili Diabetes e High Blood Pressure non sono indipendenti


# #Proviamo a fare il test chi quadro per altre coppie di variabili categoriche, come Diabetes con ToldHighColesterol
# toldHighColesterol <- ToldHighColesterol

# # Create the contingency table
# contingencyTable2 <- table(diabetes, toldHighColesterol)

# # Perform the chi-squared test
# chisqResult2 <- chisq.test(contingencyTable2)
# print(chisqResult2)  # test chi quadrato di Pearson. Il p-value viene < 2.2e-16, quindi rifiutiamo l'ipotesi nulla e concludiamo che le due variabili Diabetes e ToldHighColesterol non sono indipendenti

# #Proviamo a fare il test esatto di Fisher per le stesse variabili
# fisherResult2 <- fisher.test(contingencyTable2, simulate.p.value = TRUE) # test esatto di Fisher con p-value simulato cioè approssimato con Monte Carlo perché la tabella è troppo grande
# print(fisherResult2) # test esatto di Fisher --> p-value = 0.0004998 quindi rifiutiamo l'ipotesi nulla e concludiamo che le due variabili Diabetes e ToldHighColesterol non sono indipendenti
# #Anche qui rifiutiamo l'ipotesi nulla e concludiamo che le due variabili Diabetes e ToldHighColesterol non sono indipendenti


#Prima visualizziamo le nostre variabili categoriche

ggplot(categorical_data, aes(x = Diabetes)) + geom_bar() +
  labs(title = "Distribuzione di Diabetes",
       x = "Diabetes",
       y = "Frequenza")

ggplot(categorical_data, aes(x = HighBloodPressure, fill = factor(Diabetes))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "HighBloodPressure",
       y = "Frequenza") +
  theme_minimal()


ggplot(categorical_data, aes(x = ToldHighColesterol)) + geom_bar() +
  labs(title = "Distribuzione di ToldHighColesterol",
       x = "ToldHighColesterol",
       y = "Frequenza")

ggplot(categorical_data, aes(x = HeartDisease)) + geom_bar() +
  labs(title = "Distribuzione di HeartDisease",
       x = "HeartDisease",
       y = "Frequenza")

ggplot(categorical_data, aes(x = Stroke)) + geom_bar() +
  labs(title = "Distribuzione di Stroke",
       x = "Stroke",
       y = "Frequenza")

ggplot(categorical_data, aes(x = PhysicalActivity)) + geom_bar() +
  labs(title = "Distribuzione di PhysicalActivity",
       x = "PhysicalActivity",
       y = "Frequenza")

ggplot(categorical_data, aes(x = Fruit)) + geom_bar() +
  labs(title = "Distribuzione di Fruit",
       x = "Fruit",
       y = "Frequenza")

ggplot(categorical_data, aes(x = Vegetables)) + geom_bar() +
  labs(title = "Distribuzione di Vegetables",
       x = "Vegetables",
       y = "Frequenza")

ggplot(categorical_data, aes(x = HeavyDrinker)) + geom_bar() +
  labs(title = "Distribuzione di HeavyDrinker",
       x = "HeavyDrinker",
       y = "Frequenza")

ggplot(categorical_data, aes(x = HealthPlan)) + geom_bar() +
  labs(title = "Distribuzione di HealthPlan",
       x = "HealthPlan",
       y = "Frequenza")

ggplot(categorical_data, aes(x = MedicalCost)) + geom_bar() +
  labs(title = "Distribuzione di MedicalCost",
       x = "MedicalCost",
       y = "Frequenza")

ggplot(categorical_data, aes(x = WalkingDifficulty)) + geom_bar() +
  labs(title = "Distribuzione di WalkingDifficulty",
       x = "WalkingDifficulty",
       y = "Frequenza")

ggplot(categorical_data, aes(x= Sex)) + geom_bar() +
  labs(title = "Distribuzione di Sesso",
       x = "Sesso",
       y = "Frequenza")

#############################
# I wat to put all the plots in a image in 4 rows and 3 columns

plot1 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(HighBloodPressure))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal() +
  theme(legend.position = "none")

plot2 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(ToldHighColesterol))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot3 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(HeartDisease))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot4 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(Stroke))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot5 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(PhysicalActivity))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot6 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(Fruit))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot7 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(Vegetables))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot8 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(HeavyDrinker))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot9 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(HealthPlan))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot10 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(MedicalCost))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot11 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(WalkingDifficulty))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

plot12 <- ggplot(categorical_data, aes(x = Diabetes, fill = factor(Sex))) + geom_bar() +
  labs(title = "Distribuzione di HighBloodPressure",
       x = "Diabetes",
       y = "Frequenza") +
  theme_minimal()+
  theme(legend.position = "none")

figure <- ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
                    plot10, plot11, plot12,
                    labels = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C"),
                    ncol = 4, nrow = 3)

# Combine plots into a grid arrangement with a common legend
figure1 <- ggarrange(plot1, plot2, plot3, plot4,
                     ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

figure2 <- ggarrange(plot5, plot6, plot7, plot8,
                     ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

figure3 <- ggarrange(plot9, plot10, plot11, plot12,
                     ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

#creo figura solo con "HeartDisease", "Fruit", "Stroke", "Sex" che risultano le più rilvanti 
figureRelevant <- ggarrange(plot3, plot6, plot4, plot12,
                            ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")



# #provo a mettere 6 plot in due figure diverse : va diminuito il font del titolo in caso e vedere se leggibile
# figure4 <- ggarrange(plot1, plot2, plot3, plot4, plot5, plot6,
#                      ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")

# figure5 <- ggarrange(plot7, plot8, plot9, plot10, plot11, plot12,
#                       ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")


# Salva le figure con legenda comune
#ggsave("./results/categorical_plots1.png", figure1, width = 8, height = 6, dpi = 72)
#ggsave("./results/categorical_plots2.png", figure2, width = 8, height = 6, dpi = 72)
#ggsave("./results/categorical_plots3.png", figure3, width = 8, height = 6, dpi = 72)
#ggsave("./results/categorical_plots_relevant.png", figureRelevant, width = 8, height = 6, dpi = 72)

#############################
#colnames(categorical_data) <- c("Diabetes", "HighBloodPressure", "ToldHighColesterol",
#                                "HeartDisease", "Stroke", "PhysicalActivity", "Fruit",
#                                "Vegetables", "HeavyDrinker", "HealthPlan", "MedicalCost",
#                                "WalkingDifficulty", "Sex")

#categorical_data = categorical_data[,c("Sex", "Fruit", "MedicalCost",
#                                       "HeartDisease", "Stroke", "PhysicalActivity", "WalkingDifficulty",
#                                       "Vegetables", "HeavyDrinker", "HealthPlan", "ToldHighColesterol",
#                                       "HighBloodPressure", "Diabetes") ]
#Matrice di correlazione di Spearman

data_with_null=categorical_data
data_with_null[data_with_null == 0] <- NA

ggcorr(data_with_null, 
       method = c("pairwise", "spearman"),
       name = "Spearman's rank corre. coeff.",
       #legend.position = "left",
       legend.size = 12,
       hjust = 0.9, size = 5, 
       nbreaks = 7,
       label = TRUE,
       label_size = 5,
       color = "grey50")

categorical_data = categorical_data[, c("Diabetes", "HighBloodPressure", "ToldHighColesterol",
                                      "HeartDisease", "Stroke", "PhysicalActivity", "Fruit",
                                      "Vegetables", "HeavyDrinker", "HealthPlan", "MedicalCost",
                                      "WalkingDifficulty", "Sex")]

# Creiamo un dataframe vuoto per salvare i risultati
results <- data.frame()

# Iteriamo su tutte le possibili combinazioni di 2 variabili
for(i in 1:(length(categorical_columns)-1)) {
  for(j in (i+1):length(categorical_columns)) {
    # Prendiamo le due variabili
    var1 <- categorical_data[,i]
    var2 <- categorical_data[,j]
    
    # Creiamo la tabella di contingenza
    contingencyTable <- table(var1[var1 > 0 & var2 > 0], var2[var1 > 0 & var2 > 0])
    
    # Eseguiamo il test chi quadro
    chisqResult <- chisq.test(contingencyTable)
    
    # Eseguiamo il test esatto di Fisher
    fisherResult <- tryCatch({
      fisher.test(contingencyTable, simulate.p.value = TRUE)
    }, error = function(e) {
      list(p.value = NA)
    })
    
    # Salviamo i risultati
    results <- rbind(results, data.frame(
      Var1 = names(categorical_data)[i],
      Var2 = names(categorical_data)[j],
      ChiSquare_p_value = chisqResult$p.value,
      Fisher_p_value = fisherResult$p.value
    ))
  }
}

# Visualizziamo i risultati
print(results)
#Il p-value viene < 2.2e-16 per tutte le coppie, quindi rifiutiamo l'ipotesi nulla e concludiamo che le variabili, prese a due a due, non sono indipendenti

# Scrivi i risultati in un file CSV
write.csv(results, file = "../results/chiSquare_fisher_results.csv", row.names = FALSE)

#Stampa solo i risultati con p-value < 0.05 per il test chi quadro, ovvero le coppie di variabili categoriche che sono dipendenti 
print(results[results$ChiSquare_p_value < 0.05,])
#Tutte le coppie di variabili categoriche sono dipendenti

#Stampa solo i risultati con p-value > 0.05 per il test chi quadro, ovvero le coppie di variabili categoriche che sono indipendenti
print(results[results$ChiSquare_p_value > 0.05,])
#Non abbiamo nessuna coppia di variabili categoriche che sono indipendenti
