# Introduction to R - Wasserman

############################################################
### BASICS
############################################################
x <- 5 
y = "Hello there"
y
y = sqrt(10)
z = x + y
z

x = 1:5 ### the vector (1,2,3,4,5)
print(x)
x = seq(1,5,length=5) ### same thing
print(x)
x = seq(0,10,length=101) ### 0.0, 0.1, ..., 10.0
print(x)
x = 1:5
x[1] = 17
print(x)
x[1] = 1
x[3:5] = 0
print(x)
w = x[-3] ### everything except the third element of x
print(w)
y = c(1,5,2,4,7)
y
y[2]
y[-3]
y[c(1,4,5)]
i = (1:3)
z = c(9,10,11)
y[i] = z
print(y)


y = y^2
print(y)
y = 1:10
y = log(y)
y
y = exp(y)
y
x = c(5,4,3,2,1,5,4,3,2,1)
z = x + y
z ### R carries out operations on
  ### vectors, element by element.

#NOTE: If you add vectors of different lengths 
#then R automatically repeats the smaller vector
#to make it bigger. This generates a warning 
#if the length of the longer vector is not 
# a multiple of the length of the
# shorter vector.

x = 1
y = 1:10
x + y

x = 1:3
y = 1:4
x + y

x = 1:10
y = c(5,4,3,2,1,5,4,3,2,1)
x == 2 ### This is a logical vector.
z = (x == 2)
print(z)
z = (x<5); print(z) ### You can put two commands
                    ### on a line if you use a semi-colon.
x[x<5] = y[x<5] ### Do you see what this is doing?
print(x)
sort(y)
rank(y)
order(y)
o = order(y)
y[o]



############################################################
### MATRICES AND LISTS
############################################################
junk = c(1, 2, 3, 4, 5, 0.5, 2, 6, 0, 1, 1, 0)
m = matrix(junk,ncol=3)
print(m)
m = matrix(junk,ncol=3,byrow=T)
print(m) ### see the difference?

dim(m)
y = m[,1] ### y is column 1 of m
y
x = m[2,] ### x is row 2 of m
x
z = m[1,2]
print(z)
zz = t(z) ### take the transpose
zz
new = matrix( 1:9, 3 , 3)
print(new)
hello = z + new
print(hello)
m[1,3]
subm = m[2:4, 2:3]
m[1,]
m[2,3] = 7
m[,c(2,3)]
m[-2,]

x1 = 1:3
x2 = c(7,6,6)
x3 = c(12,19,21)
A = cbind(x1,x2,x3) ### Bind vectors x1, x2, and x3 into a matrix.
                    ### Treats each as a column.
A
A = rbind(x1,x2,x3) ### Bind vectors x1, x2, and x3 into a matrix.
                    ### Treats each as a row.
A
x = 1:20
A = matrix(x,4,5) ### Change vector x
                  ### into a 4 by 5 matrix.
dim(A) ### get the dimensions of a matrix
nrow(A) ### number of rows
ncol(A) ### number of columns
apply(A,1,sum) ### apply the sum function to the rows of A
apply(A,2,sum) ### apply the sum function to the columns of A

B = matrix(rnorm(30),5,6)
A %*% B ### multiply matrices
t(A) ### transpose of A

x = 1:3
A = outer(x,x,FUN="*") ### outer product
print(A)
sum(diag(A)) ### trace of A
A = diag(1:3)
print(A)
solve(A) ### inverse of A
det(A) ### determinant of A

who = list(name="Joe", age=45, married=T)  #list
print(who)
print(who$name)
print(who[[1]])
print(who$age)
print(who[[2]])
print(who$married)
print(who[[3]])
names(who)
who$name = c("Joe","Steve","Mary")
who$age = c(45,23)
who$married = c(T,F,T)
who



############################################################
### FOR LOOPS ETC.
############################################################
for(i in 1:10){
  print(i+1)
}

x = 101:200
y = 1:100
z = rep(0,100) ### rep means repeat
help(rep)
for(i in 1:100){
  z[i] = x[i] + y[i]
}

w = x + y
print(w-z)
### As this example shows, we can often avoid using loops since
### R works directly with vectors.
### Loops can be slow so avoid them if possible.

### if statements
for(i in 1:10){
  if( i == 4)print(i)
}
for(i in 1:10){
  if( i != 4)print(i) ### != means ``not equal to''
}
for(i in 1:10){
  if( i < 4)print(i)
}
for(i in 1:10){
  if( i <= 4)print(i)
}
for(i in 1:10){
  if( i >= 4)print(i)
}



############################################################
### FUNCTIONS
############################################################
my.fun = function(x,y){
  ##### This function takes x and y as input.
  ##### It returns the mean of x minus the mean of y
  a = mean(x)-mean(y)
  return(a)
}
x = runif(50,0,1)
y = runif(50,0,3)
output = my.fun(x,y)
print(output)

my.fun = function(x,y){
  mx = mean(x)
  my = mean(y)
  d = mx-my
  return(c(meanx=mx,meany=my,difference=d))
}
x = runif(50,0,1)
y = runif(50,0,3)
output = my.fun(x,y)
print(output)
names(output)
output$difference # this gives an error: $ operator is invalid for atomic vectors
output["difference"]
output[[3]]

### The following function will compute the square root of A:
sqrt.fun = function(A){
  e = eigen(A,symmetric=TRUE)
  sqrt.A = e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
  return(sqrt.A)
}
A = diag(1:3)
B = sqrt.fun(A)
print(B)
B %*% B



############################################################
### STATISTICS
############################################################
x = runif(100,0,1) ### generate 100 numbers randomly between 0 and 1
y = rnorm(10,0,1) ### 10 random Normals, mean 0, standard deviation 1
mean(y)
median(y)
range(y)
max(y)
min(y)
sqrt(var(y))
summary(y)

y = rpois(500,4) ### 500 random Poisson(4)
pnorm(2,0,1) ### P(Z < 2) where Z ~ N(0,1)
pnorm(2,1,4) ### P(Z < 2) where Z ~ N(1,4^2)
qnorm(.3,0,1) ### find x such that P(Z < x)=.3 where Z ~ N(0,1)
pchisq(3,6) ### P(X < 3) where X ~ chi-squared with 6 degrees
            ### of freedom



############################################################
### PLOTS
############################################################
#There are many options related to plotting. 
#You control them with the par command,
#which stands for \plotting pararameters." Type help(par).
x = 1:10
y = 1 + x + rnorm(10,0,1)
plot(x,y)
plot(x,y,type="h")
plot(x,y,type="l")
plot(x,y,type="l",lwd=3)
plot(x,y,type="l",lwd=3,col=6)
plot(x,y,type="l",lwd=3,col=6,xlab="x",ylab="y")
plot(1:20,1:20,pch=1:20)
plot(1:20,1:20,pch=20)

par(mfrow=c(3,2)) ### put 6 plots per page, in a 3 by 2 configuration
for(i in 1:6){
  plot(x,y+i,type="l",lwd=3,col=6,xlab="x",ylab="y")
}

postscript("plot.ps") ### put the plots into a postscript file
### you have to do this if you use BATCH
plot(x,y,type="l",lwd=3,col=6,xlab="x",ylab="y")
dev.off() ### This turns the printing device off.
### This will close the postscript file so you
### can print it.
### Now you can print the file our view it with
### a previewer such as ghostview.

par(mfrow=c(1,1)) ### return to 1 plot per page
y = rpois(500,4) ### 500 random Poisson(4)
hist(y) ### histogram
hist(y,nclass=50)
x = seq(-3,3,length=1000)
f = dnorm(x,0,1) ### normal density
plot(x,f,type="l",lwd=3,col=4)


