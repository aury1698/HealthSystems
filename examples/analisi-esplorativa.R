# ANALISI ESPLORATIVA, dataset "arm"
# FIM: Functional Independence Measure

# carica dati da file con header
arm = read.table(file.choose(),header=T) 
head(arm) # visualizza prime righe del dataframe
arm$Eta
attach(arm) # variabili utilizzabili direttamente
Eta


# Distribuzioni di frequenza
table(Sesso) # tabella di frequenza
table(Sesso)/length(Sesso) # frequenza relativa
table(Sesso)/length(Sesso)*100 # frequenza percentuale

classi = 30 + 5 * (0:10) # classi di Età
classi
table(cut(Eta, breaks=classi))
table(cut(Eta, breaks = 5)) # 5 intervalli di pari ampiezza
freqcum = cumsum(table(cut(Eta, breaks=5)))/length(Eta)
freqcum # frequenze cumulate relative


# Rappresentazioni grafiche
table(TRATTAMENTO)
pie(table(Sesso), labels=c("F","M")) # pie chart
pie(table(TRATTAMENTO), labels=c("casi","controlli"))

hist(Eta,prob=T) # usa frequenze relative
lines(density(Eta)) # kernel density smoothing 
hist(FIMPRE,prob=T,ylim=c(0,0.05))
lines(density(FIMPRE))
hist(FIMPOST,prob=T,ylim=c(0,0.05)) 
lines(density(FIMPOST))

plot(density(FIMPRE), ylim=c(0,0.045), main="FIMPRE e FIMPOST")
lines(density(FIMPOST),lty=2)
legend("topleft",c("FIMPRE", "FIMPOST"), lty=c(1,2))
# fimpost sta su valori più grandi rispetto a fimpre, 
# il trattamento fa ottenere score migliori

# funzione di distribuzione cumulata (CDF) empirica
plot(ecdf(Eta),main="Funzione di ripartizione")
ecdf(Eta)(60) # P(Eta < 60)
plot(ecdf(FIMPRE),main="Funzione di ripartizione")
plot(ecdf(FIMPOST),add=T,lty=2)
legend("topleft",c("FIMPRE", "FIMPOST"), lty=c(1,2))
# ancora una volta la fimpost sta a destra della fimpre

# diagramma di dispersione
plot(FIMPRE, FIMPOST,ylim=c(75,135))
abline(0,1) # bisettrice
plot(FIMPRE[TRATTAMENTO==1],FIMPOST[TRATTAMENTO==1],pch=2,
     xlab="FIMPRE", ylab="FIMPOST",ylim=c(75,135))
points(FIMPRE[TRATTAMENTO==2],FIMPOST[TRATTAMENTO==2], pch=3)
legend("topleft",c("casi","controlli"), pch=c(2,3))
abline(0,1) # bisettrice
# stesse conclusioni, i casi dove fimpost non migliori derivano da 
# trattamento convenzionale

# Statistiche di sintesi
mean(Eta)
var(Eta)
sd(Eta)
median(Eta)

mean(FIMPRE)
mean(FIMPOST)
mean(FIMPRE[TRATTAMENTO==1])
mean(FIMPRE[TRATTAMENTO==2])
mean(FIMPOST[TRATTAMENTO==1])
mean(FIMPOST[TRATTAMENTO==2])
# per come sono usciti fuori i gruppi, ho delle statistiche diverse che bisogna considerare
diff1 = FIMPOST[TRATTAMENTO==1] - FIMPRE[TRATTAMENTO==1]
mean(diff1)
diff2 = FIMPOST[TRATTAMENTO==2] - FIMPRE[TRATTAMENTO==2]
mean(diff2)
# qua 'compenso' i valori iniziali diversi, si vede che il trattamento 1 ha dato guadagno migliore

quantile(Eta,0.25)
range(Eta)
IQR(Eta)
summary(Eta)

boxplot(FIMPRE,FIMPOST,names=c("FIMPRE","FIMPOST"),
        ylab="FIM") # box and whiskers, scatola e baffi
# altezza scatola è l'iqr, da primo a terzo quartile
# i baffi si estendono dal 1o e 3o quartile 
# fino ai valori osservati più vicini alla distanza 1.5*IQR
# oltre questa distanza sono identificati come outlier

# boxplot differenziati per tipo di terapia
par(mfrow=c(1,2))
boxplot(FIMPRE[TRATTAMENTO==1], FIMPOST[TRATTAMENTO==1],
        main="casi",ylim=c(70,140),names=c("FIMPRE","FIMPOST"))
boxplot(FIMPRE[TRATTAMENTO==2], FIMPOST[TRATTAMENTO==2],
        main="controlli", ylim=c(70,140),names=c("FIMPRE","FIMPOST"))

# coefficiente di correlazione lineare
cor(FIMPRE,FIMPOST)
cor(FIMPRE[TRATTAMENTO==1],FIMPOST[TRATTAMENTO==1])
cor(FIMPRE[TRATTAMENTO==2],FIMPOST[TRATTAMENTO==2])



# Forma della distribuzione
skew = function(x){
  n=length(x)
  s3=sqrt(var(x)*(n-1)/n)^3
  mx=mean(x)
  sk=sum((x-mx)^3)/s3
  sk/n
}
skew(Eta)
skew(FIMPRE)
skew(FIMPOST)


# libreria ggplot2
library(ggplot2)
FIM=c(FIMPRE,FIMPOST)
time=c(rep("PRE",47),rep("POST",47))
qplot(time, FIM, geom=c("boxplot","jitter"),
      fill=time, main="FIM", xlab="", ylab="") # boxplot
qplot(FIM, geom="density", fill=time, alpha=I(.5),
      main="FIMPRE VS FIMPOST", xlab="FIM", ylab="density",
      xlim=c(75,140)) # kernel density
ggplot(arm[,3:5], aes(x=FIMPRE, y=FIMPOST))+geom_point(shape=1)

