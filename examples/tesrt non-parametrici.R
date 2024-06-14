################################################################
### Test non parametrico ad un campione 
################################################################
# carica dataset spessore
spessore <- read.table(file.choose(),header=T)
head(spessore)
attach(spessore)
#il valore di spessore è pari a 4 mm?

summary(Spessore) # media e mediana sono vicini al valore cercato
par(mfrow=c(1,3))
boxplot(Spessore)
hist(Spessore, prob=T)
lines(density(Spessore))
qqnorm(Spessore)
qqline(Spessore)
# dai grafici si comincia a vedere come non sia molto simmetrica, 
# sbilanciata verso il basso
shapiro.test(Spessore) 
# distribuzione non normale
# non si può applicare il test t di Student



# Test non parametrico sulla mediana
# test di Wilcoxon, confronto con mediana dei soggetti sani = 4
wilcox.test(Spessore,mu=4) # H0: mediana = 4
wilcox.test(Spessore,mu=4,conf.int=T, exact=F)
# però il test di wilcox ci permette di dire che la mediana sia 4 con abbastanza confidenza
# guardando anche l'intervallo di confidenza



################################################################
### Test non parametrici a due campioni
################################################################
# test di Mann-Whitney
Spes1 <- Spessore[Gruppo==1]
Spes2 <- Spessore[Gruppo==2]
# ipotesi nulla è che i due campioni provengano da popolazioni con stessa distribuzione
# quindi con la stessa mediana
wilcox.test(Spes1,Spes2)
wilcox.test(Spes1,Spes2,conf.int=T, exact=F) 
# non c'è differenza significativa fra le mediane dei 2 gruppi perchè p-value sopra la soglia



# test di Kolmogorov Smirnov a due campioni
f1 <- ecdf(Spes1)
f1
f2 <- ecdf(Spes2)
par(mfrow=c(1,1))
plot(f1, main="ecdf")
plot(f2, add=T, lty=2)
ks.test(Spes1,Spes2) # H0: stessa distribuzione
ks.test(Spes1,Spes2,alternative="greater")
ks.test(Spes1,Spes2,alternative="less")
# i 2 campioni sono generati dalla stessa distribuzione

# dai p-value posso dire che sicuramente m2 non è più grande di m1, ma noi formuliamo 
# ipotesi cautelative, i p-value mi fanno solo dire quello, dalle summary poi 
# effettivamente vedo come è il rapporto tra le mediane
summary(Spes1)
summary(Spes2)

# carica dataset arm
arm <- read.table(file.choose(),header=T)
attach(arm)

# differenza della variabile FIM
# prima e dopo la terapia
diff1 <- FIMPOST[TRATTAMENTO==1]-FIMPRE[TRATTAMENTO==1]
par(mfrow=c(1,2))
boxplot(diff1)
qqnorm(diff1)
qqline(diff1)
shapiro.test(diff1)
summary(diff1)
# ho outlier e una distribuzione non normale

wilcox.test(diff1,mu=4) # H0: mediana = 4
diff2 <- diff1[diff1 < 13] # elimina gli outlier guardando il box plot
length(diff1); length(diff2)
boxplot(diff2)
qqnorm(diff2); qqline(diff2)
shapiro.test(diff2)
wilcox.test(diff2,mu=4)
# stessa conclusione con e senza outlier
# quindi test Wilcoxon è robusto in presenza di outlier



################################################################
# coefficiente di correlazione di Spearman
################################################################
# confronto fra due test diagnostici
# indice di gravità malattia: da 1 a 25 (scala ordinale)

# x: valori del primo test diagnostico
x <- c(8, 16, 7, 24, 2, 15, 1, 23, 6, 22)
# y: valori del secondo test diagnostico
y <- c(10, 17, 4, 25, 5, 9, 3, 24, 6, 20)
cor.test(x,y,method="spearman")
# oltre al calcolo della rho verifica delle ipotesi, dall'output vedo che 
# h0: sono scorrelate, h1: c'è correlazione
plot(x,y)
abline(0,1) # q=0, m=1
# prima di fare il test posso fare questa come analisi esplorativa
# con lo scatter plot vedo come si distribuiscono i dati
# i test sono fortemente correlati (positivamente)

