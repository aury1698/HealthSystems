# dataset arm

#carica dataset
arm = read.table(file.choose(),header=T) 
attach(arm) 


# verifica grafica della normalità
#confronto istogramma e PDF teorica
par(mfrow=c(1,3))
hist(Eta, prob=T, xlim=c(20,100), ylim=c(0,0.05))
curve(dnorm(x,mean(Eta),sd(Eta)), 20, 100, add=T) # metodo dei momenti, o dell'analogia
# curve disegna il grafico di una funzione
hist(FIMPRE, prob=T, xlim=c(40,140), ylim=c(0,0.04))
curve(dnorm(x,mean(FIMPRE),sd(FIMPRE)), 40, 140, add=T)
hist(FIMPOST, prob=T, xlim=c(40,140), ylim=c(0,0.04))
curve(dnorm(x,mean(FIMPOST),sd(FIMPOST)), 40, 140, add=T)

#confronto ECDF e CDF teorica
par(mfrow=c(1,3))
plot(ecdf(Eta))
curve(pnorm(x,mean(Eta),sd(Eta)),min(Eta),max(Eta),add=T)
plot(ecdf(FIMPRE))
curve(pnorm(x,mean(FIMPRE),sd(FIMPRE)),min(FIMPRE),max(FIMPRE),add=T)
plot(ecdf(FIMPOST))
curve(pnorm(x,mean(FIMPOST),sd(FIMPOST)),min(FIMPOST),max(FIMPOST),add=T)

# q-q plot
# confronto quantili campionari con quelli teorici normali
qqnorm(Eta)
qqline(Eta) # retta interquartilica
qqnorm(FIMPRE)
qqline(FIMPRE)
qqnorm(FIMPOST)
qqline(FIMPOST)


# test di normalità
ks.test(Eta,pnorm,mean(Eta),sd(Eta)) # test di Kolmogorov Smirnov

library(nortest)
lillie.test(Eta) # test di Lilliefors
lillie.test(FIMPRE)
lillie.test(FIMPOST)

shapiro.test(Eta)
shapiro.test(FIMPRE)
shapiro.test(FIMPOST)


# trasformazione di Box-Cox
library(MASS)
par(mfrow=c(1,2))
boxcox(lm(FIMPRE~1), lambda = seq(0,10,0.1))
title("FIMPRE")
boxcox(lm(FIMPOST~1), lambda = seq(0,10,0.1))
title("FIMPOST")

# verifica normalità variabili trasformate
shapiro.test((FIMPRE^6-1)/6)
shapiro.test((FIMPOST^6-1)/6) 

par(mfrow=c(1,2))
qqnorm((FIMPRE^6-1)/6)
qqline((FIMPRE^6-1)/6)
qqnorm((FIMPOST^6-1)/6)
qqline((FIMPOST^6-1)/6)