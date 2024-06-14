# carica dataset ciclisti:
# tempo di resistenza in un particolare allenamento
# misurato su 9 ciclisti 
# per 4 diverse dosi di caffeina [mg / kg di peso corporeo]
ciclisti <- read.table(file.choose(),header=T)
head(ciclisti)
attach(ciclisti)

summary(time[dose=="0"])
summary(time[dose=="5"]) # circa 4 tazzine di espresso per persone di 70kg
summary(time[dose=="9"])
summary(time[dose=="13"])
# i tempi di resistenza rimangono gli stessi in media 
# o cambiano in base alla assunzione di caffeina?
boxplot(time ~ dose, names=c("0","5","9","13"), 
        xlab="dose caffeina [mg/kg]",
        ylab="tempo [min]")

# trasforma variabili in factors (v. qualitative)
subj <- as.factor(subj)
dose <- as.factor(dose)


# organizza dati in tabella
time1 <- matrix(time,ncol=4,byrow=T, 
                dimnames=list(subj=1:9, cond=c("0","5","9","13")))
time1 # righe soggetti, colonne label, quella della teoria anova ripetuta pag.15

# applica la funzione "mean" per colonne, MARGIN=2
apply(time1, MARGIN=2, FUN=mean)

# applica la funzione "mean" per righe, MARGIN=1
apply(time1, MARGIN=1, FUN=mean) # grande variabilità tra soggetti

mean(time1)

#  non ci interessa la variabilità tra soggetti, ma quella tra classi
library(lattice)
bwplot(time ~ dose | subj) 
# vediamo una grossa variabilità tra i soggetti e come si comporta ogni soggetto
# in funzione della dose, non sembra esserci un trend chiaro tra dose caffeina e prestazioni


par(mfrow=c(2,2))
qqnorm(time[dose=="0"], main="dose 0")
qqline(time[dose=="0"])
qqnorm(time[dose=="5"], main="dose 5")
qqline(time[dose=="5"])
qqnorm(time[dose=="9"], main="dose 9")
qqline(time[dose=="9"])
qqnorm(time[dose=="13"], main="dose 13")
qqline(time[dose=="13"])

# test di adattamento alla normalità
shapiro.test(time[dose=="0"])
shapiro.test(time[dose=="5"])
shapiro.test(time[dose=="9"])
shapiro.test(time[dose=="13"])

# test di Bartlett per omoschedasticità
bartlett.test(time, dose)
# i dati sono normali
# manca la sfericità per poter applicare anova

# ANOVA a misure ripetute
analisi <- aov(time ~ dose + Error(subj/dose))
# il termine Error(subj/dose) indica che il fattore dose è entro i soggetti
# cioè i dati sono misure ripetute sui soggetti nei vari livelli di dose

summary(analisi)
# quindi c'è una differenza significativa tra le misure effettuate alle 4 dosi
# ma non sappiamo tra quali dosi, serve analisi post-hoc

# verifica differenze fra i soggetti: 
# statistica test F_sub = MQ_sub/MQ_res
MQ_sub <- 694.7 # sum of squares preso da anova
MQ_res <- 52.57 # residuals preso sempre da anova
F_sub <- MQ_sub/MQ_res
F_sub
p_value <- 1 - pf(F_sub, 8, 24) # il p-value è stat.test nella cdf e calcolo integrale
# 8 gradi di libertà(9 soggetti - 1) e 24 gdl al denominatore(8 * 3)
p_value
# anche le differenze fra soggetti sono significative

# verifica sfericità scrivendo a mano la stat. di test
# statistica Greenhouse-Geisser
S <- var(time1) # matrice di covarianza
J <- 4
numeratore <- J^2*mean(diag(S)-mean(S))^2
denominatore <- (J-1)*(sum(S^2)-2*J*sum(apply(S,1,mean)^2)+
                         +J^2*mean(S)^2)
epsilon <- numeratore/denominatore
epsilon
# epsilon -> 1 dati sferici, epsilon -> 0 mi allontano dalla sfericità
# piccola deviazione dalla sfericità, non influenza risultato


# analisi post-hoc
pairwise.t.test(time,dose,paired=T,p.adj="bonferroni")
pairwise.t.test(time,dose,paired=T,p.adj="BH")
# differenze significative tra non assumere caffeina ed assumerla, ma praticamente
# nessuna differenza tra le dosi, come prima bonferroni molto restrittiva, sign. solo
# la coppia 0-9, con BH vedo 0-5, 0-9 e 0-13


# coefficiente di correlazione intraclasse
library(irr)
icc(time1)
# correlazione 0.65 
# fra coppie di osservazioni dello stesso soggetto a dosi diverse
# lo stesso ciclista ottiene prestazioni diverse a seconda della dose





# test non parametrico di Friedman, anova a misure ripetute non param.
friedman.test(time, dose, subj)
# p-value basso, ci sono diff. significative

# analisi post-hoc non parametrica
pairwise.wilcox.test(time,dose,paired=T,p.adjust.method = "bonferroni")
pairwise.wilcox.test(time,dose,paired=T,p.adjust.method = "BH")



