#Hypothesis test
#Dataset Unemployment_2019 has been imported
View(Unemployment_2019)
x = Unemployment_2019$Year

mean(x)

sd(x)

#Ho: mu - 5.394513603
#H1: mu > 5.394513603
#one-sided 95% confidence interval for mu
t.test(Unemployment_2019$Year, mu = 5.394513603, alternative = "greater",conf.level = 0.95)

xbar = 6.942394 #sample mean
mu0 = 5.394513603 # claim value
s = 4.881099
n = 231  #sample size
t = (xbar-mu0)/(s/sqrt(n))
t
alpha = 0.05

t.alpha = qt(1-alpha, df = n-1)
t.alpha   #critical value
t > t.alpha

#Correlation test
# Dataset Malaysia_unemployment_rate has been imported

View(Malaysia_unemployment_rate)
x = Malaysia_unemployment_rate$Year
y = Malaysia_unemployment_rate$Rate
cor(x,y)

#Correlation test
cor.test(x,y,method = "pearson")

# Scatter plot
plot(x,y, main = "scatter plot of unemployment rate 
     against year against year from 1991 to 2019 in 
     Malaysia", xlim = c(1991,2019),
     ylim = c(2.4, 3.8), xlab = "YEAR", ylab = "Unemployment Rate in %")

#linear Regression
mod <- lm(y~x)
summary(mod)

#To check only coefficient 
coef(mod)

#For the regression line
abline(mod)
abline(mod, col=2,lwd=3)  # For red line

#Goodness of fit using chi-square test

Observed <- c(Malaysia_unemployment_rate$Rate)
Observed

#sum of the rates 
n <- sum(Malaysia_unemployment_rate$Rate)
n

#All country
k <- 29 #Run this first to run k
k

#Expected result
E <- n/k #Run this first to run E
E

#Setting all expected result
Expected <- c(3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069,3.313069)
Expected
 
#(????_???????????_???? )^2/????
((Observed - Expected)^2)/Expected

#????^2=??????(????_???????????_???? )^2/????
x2 <- sum(((Observed - Expected)^2)/Expected)
x2

alpha = 0.05
x2.alpha = qt(1-alpha, df = n-1)
x2.alpha     # critical value
x2 < x2.alpha

