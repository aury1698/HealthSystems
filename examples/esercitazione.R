  students = read.table(file.choose(),header=T) 
  attach(students) 

  head(students)  
  table(low)  
  table(high)
  par(mfrow=c(1,2))
  hist(low,prob=T) # usa frequenze relative
  lines(density(low)) # kernel density smoothing
  curve(dnorm(x,mean(low),sd(low)), 0, 70,add=T)
  hist(high,prob=T) # usa frequenze relative
  lines(density(high)) # kernel density smoothing 
  curve(dnorm(x,mean(high),sd(high)), 0, 70,add=T)
  
  par(mfrow=c(1,2))
  plot(ecdf(low))
  curve(pnorm(x,mean(low),sd(low)),min(low),max(low),add=T)
  plot(ecdf(high))
  curve(pnorm(x,mean(high),sd(high)),min(high),max(high),add=T)

  qqnorm(low)
  qqline(low)
  # low è heavy tails
  qqnorm(high)
  qqline(high)
  # high è molto simile a normale

  ks.test(low,pnorm,mean(low),sd(low))  
  ks.test(high,pnorm,mean(high),sd(high))
  
  library(nortest)
  lillie.test(low)
  lillie.test(high)
  
  shapiro.test(low)
  # rifiuto normalità di low
  shapiro.test(high)
  # sopra la soglia, non rifiuto ipotesi e high è normale
  