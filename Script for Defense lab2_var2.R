setwd("C:/Users/fatal/Desktop")
getwd()
dir()
A<-read.table("Population.txt",header = T, sep="");A
str(A)

# 1 task
# Calculate main characteristics for each variable: mean, median, minimum, maximum, quartile Q1 and Q3, variance, standard deviation;
summary(A)
var(A$age);var(A$mileage); var(A$price)
sd(A$age);sd(A$mileage); sd(A$price)


# 2 task
# Set seed as your student ID. Select simple random SAMPLE
# of 200 cars and calculate main characteristics;
set.seed(1750)
help(sample)
sample(nrow(A),10) 

S=A[sample(nrow(A),200,F), ];S
summary(S)
var(S$age);var(S$mileage); var(S$price)
sd(S$age);sd(S$mileage); sd(S$price)


# 3 task
#Using sample data test the hypothesis that mean of the price  is significantly different from 15000, 
#using the a significance level ??=0,01.
mean(S$price) # a = 15000
t.test(S$price, mu=15000,conf.level = 0.99)


# 4 task
#Using sample data draw empirical density functions for variable mileage.
par(mfrow=c(3,1))
hist(S$mileage,freq=FALSE,main="edf for mileage")


# 5 task
#Using sample data draw a graph where price of the car depends on mileage. 
#Calculated regression line and test is this dependency significant. 
#Calculate prognosis what should be the price of the car with 100000 mileage 
y=S$price
x=S$mileage
cor(x, y)

lm.out=lm(y~x)
lm.out
summary(lm.out)

par(mfrow = c(1, 1))
plot(y~x)
abline(lm.out, col="red")

lm.out$coefficients
lm.out$coefficients%*%c(1,100000)
lm.out$coefficients[1]+lm.out$coefficients[2]*100000