################################################################
### ANOVA
################################################################
# carica dataset HIV
HIV <- read.table(file.choose(),header=T)
head(HIV)
attach(HIV)
# ho 4 gruppi quindi devo usare anova

# operazione preliminare per gestire dataset, 
# cambio da var. quantitativa a qualitativa(non un valore con un senso ma una label)
# trattare stato come v. qualitativa (factor)
# factor indica ad R di considerare la var. come qualitativa
stato <- as.factor(stato) 

# analisi esplorativa della variabile manitolo
boxplot(manitolo ~ stato) # 'manitolo vs stato', 
# faccio boxplot dei lavori di manitolo suddivisa per stato
# vogliamo capire se le medie dei valori di manitolo sono uguali tra i vari stati
summary(HIV[stato=="1",3])
summary(HIV[stato=="2",3])
summary(HIV[stato=="3",3])
summary(HIV[stato=="4",3])

par(mfrow=c(2,2))
qqnorm(manitolo[stato=="1"],main = "Stato 1")
qqline(manitolo[stato=="1"])
qqnorm(manitolo[stato=="2"],main = "Stato 2")
qqline(manitolo[stato=="2"])
qqnorm(manitolo[stato=="3"],main = "Stato 3")
qqline(manitolo[stato=="3"])
qqnorm(manitolo[stato=="4"],main = "Stato 4")
qqline(manitolo[stato=="4"])
shapiro.test(HIV[stato=="1",3])
shapiro.test(HIV[stato=="2",3])
shapiro.test(HIV[stato=="3",3])
shapiro.test(HIV[stato=="4",3])
# normalità confermata per tutte le categorie

# plot intervalli di confidenza al 95% per la media con numerosità dei gruppi 
library(gplots)
par(mfrow=c(1,1))
plotmeans(manitolo~stato) 
# per gruppi piccoli incertezza maggiore

# verifica omoschedasticità
bartlett.test(manitolo, stato)
# si può applicare ANOVA

# ANOVA
aov(manitolo ~ stato)
# ci dà la sum of squares within e between di anova, 
# degrees of freedom sono stati -1 e campioni - 4(num. classi)
# non c'è il p-value
results_ANOVA <- aov(manitolo ~ stato)
summary(results_ANOVA)
# non lo chiama p-value ma è la probabilità che la stat. test sia maggiore di F
# dove F è il valore riportato sempre dall'output. F di fisher, test ad una coda
# accetto ipotesi nulla, non ci sono differenze statist. significative tra i gruppi
qf(0.95,3,55) # quantile Fisher
# ipotesi nulla accettata
# diff. medie non statisticamente signif.
plot(seq(0,5,0.1),df(seq(0,5,0.1),3,55),type="l") #pdf
plot(seq(0,5,0.1),pf(seq(0,5,0.1),3,55),type="l") #cdf
# se guardo il quantile, ho un valore 0.595 che lascia una probabilità del 60%


# analisi esplorativa della variabile lattosio
par(mfrow=c(1,1))
boxplot(lattosio ~ stato) # stavoltà c'è abbastanza differenza tra la var. dei gruppi
summary(HIV[stato=="1",4])
summary(HIV[stato=="2",4])
summary(HIV[stato=="3",4])
summary(HIV[stato=="4",4])
par(mfrow=c(2,2))
qqnorm(lattosio[stato=="1"],main = "Stato 1")
qqline(lattosio[stato=="1"])
qqnorm(lattosio[stato=="2"],main = "Stato 2")
qqline(lattosio[stato=="2"])
qqnorm(lattosio[stato=="3"],main = "Stato 3")
qqline(lattosio[stato=="3"])
qqnorm(lattosio[stato=="4"],main = "Stato 4")
qqline(lattosio[stato=="4"])
# sembrano esserci abbastanza differenze dalla normalità
shapiro.test(HIV[stato=="1",4])
shapiro.test(HIV[stato=="2",4])
shapiro.test(HIV[stato=="3",4])
shapiro.test(HIV[stato=="4",4])
# normalità non verificata per il primo e il terzo gruppo



bartlett.test(lattosio, stato)
# eteroschedasticità

# quindi non si può applicare ANOVA

# test non parametrico di Kruskal Wallis
kruskal.test(lattosio,stato) # kruskal.test(valori, gruppi)
# mediane sono uguali, il test di kruskal parla di mediane



# altro esempio
# carica dataset autismo
aut <- read.table(file.choose(),header=T)
head(aut)
attach(aut)
# sono tempi di risposta a 4 trattamenti diversi
par(mfrow=c(1,1))
boxplot(a,b,c,d)
# vediamo subito differenze significative tra le distribuzioni
shapiro.test(a)
shapiro.test(b)
shapiro.test(c)
shapiro.test(d)
# il terzo gruppo è quello più al limite della normalità, posso decidere di 
# accettare normalità e procedere, potrebbe essere l'outlier che ci da questo risultato

par(mfrow=c(2,2))
qqnorm(a,main = "a"); qqline(a)
qqnorm(b,main = "b"); qqline(b)
qqnorm(c,main = "c"); qqline(c)
qqnorm(d,main = "d"); qqline(d)
# si accetta normalità perchè effettivamente si vede che nel gruppo c l'outlier 
# è l'unico a non distribuirsi normalmente

# crea v. qualitativa (factor) gruppi
dati <- c(a,b,c,d)
gruppi <- factor(rep(letters[1:4], each=12))
gruppi
bartlett.test(dati,gruppi)
# omoschedasticità accettata

# quindi si può applicare ANOVA
summary(aov(dati~gruppi)) # fa vedere anche significatività
# ipotesi nulla rifiutata, ANOVA è significativa, ci sono diff. significative 
# tra le medie dei gruppi

# ha senso solo se anova è significativa, serve solo per capire dove stanno 
# le diff. tra gruppi
# test post-hoc, correzione di Bonferroni
pairwise.t.test(dati,gruppi,p.adj="bonferroni")
# confronti significativi: a b, b c, c d dove i p-value sono bassi
# la coppia a-d non viene considerato significativa, colpa di bonferroni che
# è una correzione molto restrittiva, considera significativi pochi test

# procedura di Benjamini-Hochberg
pairwise.t.test(dati,gruppi,p.adj="BH")
# significativo anche a d 
# correzione meno restrittiva di Bonferroni

# test post-hoc non parametrico
pairwise.wilcox.test(dati,gruppi,p.adj="bonferroni")
# risultati diversi dei test parametrici, valgono solo b-c e c-d
# preferire test parametrici, potenza maggiore

pairwise.wilcox.test(dati,gruppi,p.adj="BH")
# risultato comparabile ai test parametrici


