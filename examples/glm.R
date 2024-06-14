# Logistic regression and GLM

#################################
# case study: 
# efficacy of a new antibiotic (new). 
# compared with an antibiotic already in use (strepto)
# in the treatment of leptospirosis in dogs.
################################################
# Treatment | Cured | Not cured | Total
# --------------------------------------
# new       | 52    | 10        | 62      x = 1
# strepto   | 40    | 21        | 61      x = 0
# --------------------------------------
# Total     | 92    | 31        | 123
#             y = 1   y = 0
################################################
# genero i vettori di risposta e di ingresso, dicotomici

y <- c(rep(1,92), rep(0,31))
x <- c(rep(1,52), rep(0,40), rep(1,10), rep(0,21))

# logistic regression ottenuta come glm specifica
# binomial distribution family
fit <- glm(y~x,binomial) 
summary(fit) # mi da intercetta beta1 e il coefficente molt. di x beta2
# both parameters beta_1 and beta_2 are significant, il modello è adeguato a rappr. i dati

OR = exp(fit[["coefficients"]][["x"]]) # Odds Ratio = exp(beta_2),
# sto estraendo dalla variabile/oggetto fit il coeff. beta2
print(OR)
# the odds of recovery with the new treatment 
# are 2.7 times higher than old


#################################################
# Second case study, on obstetrics (70 cases observed):
# cesarean and natural deliveries (parti, cerco di capire la prob. di parto cesario)
# influence of explanatory variables on probability of cesarean
# Dependent variable y = 1: cesarean; y = 0: natural
# explanatory variables: 
#     - bmi (continuous) peso [kg]/altezza[m]^2
#     - ind (binary: 0, 1)
#     - pvd (binary: 0, 1)
#################################################
rm(list=ls()) # clear workspace
# load dataset 'parti'
parti <- read.table(file.choose(),header=T)
head(parti)
attach(parti)

# exploratory data analysis
plot(bmi, y) # bmi continuo posso plottare, sembra esserci influenza/correlazione tra le variabili
# ind e pvd sono var. esplicative dicotomiche, con y dicotomica non si riesce a graficare
sum(y==ind)/length(y)*100 # perc. agreement between y and ind
sum(y==pvd)/length(y)*100 # perc. agreement between y and pvd

# logistic regression model:
# logit(pi) = beta_1 + beta_2*bmi + beta_3*ind + beta_4*pvd
# as.factor perchè ind e pvd sono numeriche ma devono diventare categoriche
fit <- glm(y ~ bmi + as.factor(ind) + as.factor(pvd), binomial)
summary(fit) # the variable 'fit' contains the fitted model
# all parameters are significant
# all explanatory variables increase the prob. of cesarean

# to evaluate the accuracy of the fitted model
# build the confusion matrix of the prediction
table(y, predict(fit, type="response")>0.5) # inserisco una soglia e faccio 'detection'
# predict mi da la stima di pigreco, una probabilità, per decidere quanto vale y confronto con soglia
accuracy <- (38+25)/70
print(accuracy)
# righe 0/1 verità, false/true sono le mie predizioni

# ROC 
library(ROCR)
v <- c(predict(fit,type="response"))
labels = y
pred = prediction(v,labels)
perf = performance(pred, "tpr", "fpr")
plot(perf, lwd=1, type="l", colorize=T) # color: threshold on y cosi vedo come varia la soglia
prediction_performance <- performance(pred,"auc")
prediction_performance@y.values # area under the curve (AUC)



#################################################
# Third case study: Water quality control
# Example of GLM not logistic regression
# dataset 'ceriodaphnia'
# response variable: Cerio (count): number of micro-organisms
# explanatory variables: 
#     - Conc: concentration of toxic substance 
#     - Strain (categorical): strain of micro-organism
#################################################
# load dataset 'ceriodaphnia'
cerio <- read.table(file.choose(),header=T, sep=",")
head(cerio)
attach(cerio)
Strain=as.factor(Strain)
plot(Conc[Strain=="1"],Cerio[Strain=="1"], 
     xlab="Conc", ylab="Cerio")
points(Conc[Strain=="2"],Cerio[Strain=="2"],pch='x')

# Poisson regression, since the response variable is a count
fit <- glm(Cerio~Conc*Strain,poisson)
summary(fit)
# il * rappresenta l'interazione tra le variabili, conc:strain2 nell'output

# all coefficients beta are significant, except for interaction
# repeat the fit without interaction
fit <- glm(Cerio~Conc+Strain,poisson)
summary(fit)
# estimated (fitted) model is:
# exp(4.45 - 1.54Conc - 0.27Strain)

# graphical representation of model
curve(exp(4.45 - 1.54*x), add=T, lty=1) # first strain
curve(exp(4.45 - 1.54*x - 0.27), add=T, lty=2) # second strain
legend("topright", c("data Strain1", "data Strain2",
                     "model Strain1", "model Strain2"),  
       pch=c("o","x","",""), 
       lty=c(0,0,1,2))
# nr. of micro-organisms decreases as concentration increases
# Second strain is lower than first, sembra esserci una differenza significativa
# tra gli strain che prima non si vedeva bene se non nella conc. più bassa

