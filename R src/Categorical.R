# Prendere variabili categoriche e fare test chi^2 di pearson per l'indipendenza : Slide test di adattamento - pag.14
# Esempio didel professore
# xmat <- matrix(c(10,18,1,22),nrow=2,byrow=T) 
# xmat
# chisq.test(xmat)
# fisher.test(xmat) # test esatto di Fisher

# noi abbiamo 13 variabili categoriche, dobbiamo scegliere 2 variabili e fare il test
# sono 13*12/2 = 78 coppie di variabili, salviamo i p-value per valutarli dopo

# H_0 : la pdf congiunta = prodotto delle pdf marginali
# H_1 : la pdf congiunta != prodotto delle pdf marginali

# quindi se accetto H_0 allora le variabili sono indipendenti, altrimenti sono dipendenti

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/chisq.test

