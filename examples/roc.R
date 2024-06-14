# carica dataset aorta
# misure del calibro dell'aneurisma dell'aorta addominale
# in 48 pazienti, classificati in due gruppi: 
# piccolo calibro (<50 mm): basso rischio di rottura (L)
# grande calibro (>50 mm): alto rischio di rottura (H)
# per pazienti H, necessario intervento chirurgico
# CT: misure effettuate con TAC
# US: misure effettuate con ecografo ad ultrasuoni
# (US meno invasivo e costoso)
aorta <- read.table(file.choose(),header=T)
head(aorta)
attach(aorta)

# obiettivo: valutare accuratezza diagnostica 
# del nuovo strumento US (meno invasivo)
# usando CT come golden standard

# analisi esplorativa
cor(CT,US) #correlazione tra due vettori, quella lineare
plot(CT,US) # scatterplot
abline(0,1)

# coefficiente di concordanza (CC) di Lin
library(epiR)
epi.ccc(US,CT)$rho.c
# conferma che la correlazione lineare abbia coeff. unitarioi

# grafico di Bland-Altman
library(BlandAltmanLeh)
library(ggplot2)
pl = bland.altman.plot(US,CT,graph.sys = "ggplot2")
plot(pl)
# non c'è bias significativo perchè l'intervallo di confidenza contiene lo zero
# le differenze più grandi le troviamo per valori piccoli di aneurisma, che sono
# i pazieneti a basso rischio, è un bene per noi perchè il contrario sarebbe stato
# peggiore, sembra che posso usare il metodo meno invasivo per trovare i casi a rischio

mean(US-CT) # bias
sd(US-CT) # dev std
mean(US-CT)+c(-1.96,1.96)*sd(US-CT) # intervallo conf.



# suddivisione pazienti L e H
Aneur = rep(0,48) #v. dicotomica
for(i in 1:48)
  if(CT[i]>=50)
    Aneur[i] <- 1
Aneur
table(Aneur)
table(Aneur)/48*100
US_L <- US[Aneur==0]
US_H <- US[Aneur==1]
CT_L <- CT[Aneur==0]
CT_H <- CT[Aneur==1]

par(mfrow=c(1,2))
boxplot(US_L,US_H,names=c("L","H"),main="US")
boxplot(CT_L,CT_H,names=c("L","H"),main="CT")
# nel golden standard (CT) le distribuzioni sono completamente separate, normale che sia cosi
# nell'altro test ho che le due distribuzioni si sovrappongono leggermente

plot(density(US_L),xlim=c(0,80),ylim=c(0,0.07),
     main="US")
lines(density(US_H),lty=2)
plot(density(CT_L),xlim=c(0,80),ylim=c(0,0.07),
     main="CT")
lines(density(CT_H),lty=2)
# plotto le pdf con kernel density smoothing, anche nel caso della tac si sovrappongono sembra
# ma deriva dallo smoothing, è un artefatto dell'algoritmo. se non facessi smoothing,
# gli istogrammi sarebbero separati

# verifica normalità
qqnorm(US_L); qqline(US_L)
qqnorm(US_H); qqline(US_H)
shapiro.test(US_H)
plot(US_H) # outlier
shapiro.test(US_H[-8]) # rimozione outlier, prendo tutti elementi tranne l'8
shapiro.test(US_L)
# si può assumere normalità, anche se US_H sta lì lì

# verifica omoschedasticità
var.test(US_L,US_H)
# eteroschedasticità
# non posso fare test t-student, uso l'alternativa

# verifica uguaglianza medie (Welch)
t.test(US_L,US_H)
# quindi diff. medie significativa
# voglio che le differenza tra le medie sia significativa perchè vuol dire che 
# che le due pdf sono più separabili

# matrice di confusione
TN <- length(CT[US<50 & CT<50]) # true negatives
TN
FN <- length(CT[US<50 & CT>=50]) # false negatives
FN
FP <- length(CT[US>=50 & CT<50]) # false positives
FP
TP <- length(CT[US>=50 & CT>=50]) # true positives
TP
confusion_matrix <- matrix(c(TP,FP,FN,TN),byrow=T,nrow = 2)
confusion_matrix

accuracy <- (TP+TN)/(TP+FN+FP+TN)
accuracy
sensitivity <- TP/(TP+FN)
sensitivity
specificity <- TN/(FP+TN)
specificity

# ROC
par(mfrow=c(1,1))
library(verification)
v <- c(US_L,US_H)
labels <- c(rep(0,length(US_L)), rep(1,length(US_H)))
roc.plot(labels,v)
roc.area(labels,v) # il p-value si riferisce a H0: auc=0.5
# AUC è superiore a 0.9 
# quindi US è altamente accurato

# ROC alternativa
library(pROC)
ROC_alt <- roc(labels,v)
plot(ROC_alt)
ROC_alt <- roc(labels,v, auc=T) # calcola anche AUC
ROC_alt


# scelta della soglia ottimale
par(mfrow=c(1,2))
library(ROCR)
pred <- prediction(v,labels)
perf <- performance(pred,"tpr","fpr") # ROC: tpr vs fpr
plot(perf)
perf1 <- performance(pred,"acc") # accuratezza
plot(perf1)
# max accuratezza con soglia circa 50

# criterio di Youden 
library(pROC)
coords(ROC_alt,x="best", best.method="youden")


