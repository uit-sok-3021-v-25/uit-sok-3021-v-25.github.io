
# Example 13.2

rm(list=ls())
library(tidyverse)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/fred5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/fred5.rdata"))


head(fred5)

# convert the variables to ts() object
y <- ts(fred5$y, start = c(1986,1), frequency = 4)
cons <- ts(fred5$cons, start = c(1986,1), frequency = 4)


# stationarity test 
library(urca)
?ur.df

summary(ur.df(y,type = "trend", selectlags = "BIC"))
# The tau value is -0.4273
# The critical value at 5% level is -3.43 
# conclusion: keep H0: Non-stationary /unit root

summary(ur.df(cons,type = "trend", selectlags = "BIC"))
# keep Ho: Non-stationary 

# Stationarity test in first difference 
summary(ur.df(diff(y),type = "none", selectlags = "BIC")) # Reject Ho
summary(ur.df(diff(cons),type = "none", selectlags = "BIC")) #Rject Ho

#Hence, both y and cons are non-stationary in levels (original form), 
# but they are stationary at thier first differences. Thus, we can 
# apply cointegration test. 

#cointegration test 
library(dynlm)
coint <- dynlm(cons ~ y)
summary(coint)

# the residual from the cointegrated model 
ehat <- resid(coint)

#check whether the residual is stationary or not 
# if the residual is stationary then, we can say 
#the two variables, are cointegrated

summary(ur.df(ehat,type = "none", selectlags = "BIC")) 
# The tau value is  -3.0458 
# The t_CV  at 5% is -3.37
# Since the tau values is greater than the t_CV in absolute value 
# we can keep Ho: no cointegration 
#Hence, the two variables, i.e., cons and y, are not cointegrated.
#So we can estimate VAR: Vector Autoregressive model 

library(vars)
# generate first difference of the variables 
DC <- diff(cons)
Dy <- diff(y)

varmat <- as.matrix(cbind(DC,Dy)) 
head(varmat)

?VAR

varfit <- VAR(varmat, p=1)
summary(varfit)

# Impulse reponse function 
?irf

plot(irf(varfit))


# Foecast error variance 

fevd(varfit)

plot(fevd(varfit))








