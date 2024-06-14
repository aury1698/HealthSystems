################################################################
### Test sulla media di una popolazione normale
################################################################
# assumiamo che il peso medio della popolazione sia 3,42kg, assumere
# integratori cambia qualcosa?
# media nota e varianza ignota 
# prima verifichiamo la normalità poi uso la stat. di t-student
#carica dataset pesoneo
pesoneo = read.csv(file.choose(),header=T) 
attach(pesoneo) 

head(pesoneo)

# analisi esplorativa
summary(pesoneo)
sd(peso)
mean(peso) # la media osservata sembrerebbe più grande della media di riferimento

hist(peso)# non sembrano esserci particolari asimmetrie

par(mfrow=c(1,2))
boxplot(peso)
qqnorm(peso) # sembra che dal q-q test i dati siano effettivamente normali
qqline(peso)

#test di adattamento alla distr. normale
shapiro.test(peso)
# p-value molto grande, non ci sono evidenze contro h0, quindi accetto la normalità dei dati
# e ora posso fare la stat. di test
# test t di Student a un campione
# sulla media di una popolazione normale
t.test(peso, mu=3.42) # var. da utilizzare e la mu di riferimento
t.test(peso, mu=3.42, conf.level=0.99)
t.test(peso, mu=3.42, alternative="greater") # cambia formulazione ipotesi, 
# h0 uguale ma h1 ora è mu > mu0, diventa test ad una sola coda
# con questo ultimo test cambia il p-value, cambia la pdf di riferimento, infatti abbiamo visto
# come la media fosse più alta, il p-value si avvicina alla soglia per farci rifiutare h0
# df. degrees of fredoom pari al numero di campioni meno 1
# il peso di questo campione di neonati non è significativamente diverso dalla popolazione complessiva
# l'intervallo di confidenza contiene il valore di mu, conferma ulteriormente h0

################################################################
### Test sui parametri di due popolazioni normali
################################################################
# carica dataset spessore
spessore = read.table(file.choose(),header=T) 
attach(spessore)
head(spessore)
# spessore cutaneo su due malattie diverse
Spes1<-Spessore[Gruppo==1]
Spes2<-Spessore[Gruppo==2]
par(mfrow=c(1,1))
boxplot(Spes1,Spes2, names=c("Spes1","Spes2")) # ci sono chiare differenze tra le pdf
# ma sono statisticamente rilevanti queste differenze?
qqnorm(Spes1)
qqline(Spes1)
# forse lieve asimmetria
qqnorm(Spes2)
qqline(Spes2)
# anche qua
summary(Spes1)
summary(Spes2)
shapiro.test(Spes1)
# dovremmo rifiutare l'ipotesi nulla qua
shapiro.test(Spes2)
# mentre qua accettare l'ipotesi di normalità
# si decide comunque di andare avanti con il test di t-student, anche se non è proprio rigoroso

# test di omoschedasticità delle due distribuzioni per capire quale stat. di test utilizzare
var.test(Spes1,Spes2)
# fa il fisher test per capire se le varianze sono uguali, gradi di lib. dipendono dal num. campioni

# test t di Student a due campioni, ho verificato che la var. sia uguale sopra
t.test(Spes1,Spes2, var.equal=T)
t.test(Spes1,Spes2, var.equal=T, alternative="greater")
t.test(Spes1,Spes2) # test di Welch



################################################################
### Test su una proporzione
################################################################
# test z su una proporzione
z = (20/29-0.5)/sqrt(0.5*(1-0.5)/29)
z
2*(1-pnorm(z)) # p-value

binom.test(20,29)



################################################################
### Test su dati appaiati 
### (misure ripetute, dati non indipendenti)
################################################################
# carica dataset ansia
ansia = read.table(file.choose(),header=T) 
attach(ansia)
head(ansia)
boxplot(farmaco,placebo, names=c("farmaco","placebo"))
summary(farmaco)
summary(placebo)
diff = farmaco - placebo
summary(diff)
par(mfrow=c(1,2))
boxplot(diff)
qqnorm(diff)
qqline(diff)
shapiro.test(diff)
t.test(diff,mu=0)
t.test(farmaco,placebo,paired=T) # comando equivalente



################################################################
### Test sulla media di una popolazione
### e sulle medie di due popolazioni
### (Teorema Centrale del Limite)
################################################################
# carica dataset creatin
creatin = read.table(file.choose(),header=T) 
attach(creatin)
group1=creat[pat=="AI"]
group2=creat[pat=="IMA"]
summary(group1)
summary(group2)
par(mfrow=c(1,3))
boxplot(group1,group2)
qqnorm(group1); qqline(group1)
qqnorm(group2); qqline(group2)
shapiro.test(group1)
shapiro.test(group2)

t.test(group1,mu=75) # media nei pazienti sani: 75
t.test(group1,group2)



################################################################
### Test su due proporzioni
### tabella di contingenza 2x2
################################################################
malati <- c(92,15)
pazienti <- c(113,35)
prop.test(malati,pazienti) # test per il confronto di proporzioni

xmat <- matrix(c(92,15,21,20),byrow=T,nrow=2)
xmat
chisq.test(xmat) # test chi quadrato di Pearson

# esempio di un piccolo campione
xmat <- matrix(c(10,18,1,22),nrow=2,byrow=T) 
xmat
chisq.test(xmat)
fisher.test(xmat)# test esatto di Fisher

# esempio di campioni dipendenti tra loro
xmat <- matrix(c(25,18,4,7),byrow=T,nrow=2)
mcnemar.test(xmat)
