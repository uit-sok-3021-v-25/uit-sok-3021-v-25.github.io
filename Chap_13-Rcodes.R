rm(list=ls())
library(tidyverse)

# Example 13.1

#' Estimating a VEC model
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/gdp.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/gdp.rdata"))
str(gdp)
head(gdp)

usa <- ts(gdp$usa, start=c(1970,1), end=c(2000,4), frequency=4)
aus <- ts(gdp$aus, start=c(1970,1), end=c(2000,4), frequency=4)

ts.plot(usa,aus, type="l", lty=c(1,2), col=c(1,2), main="GDP")
legend("topleft", border=NULL, legend=c("USA","AUS"), lty=c(1,2), col=c(1,2))

#' They have a common trend, and a non constant mean, 
#' the series are probably nonstationary.
library(urca)
summary(ur.df(usa, type = "trend", selectlags = "BIC"))#keep Ho: non-stationary
summary(ur.df(aus, type = "trend", selectlags = "BIC"))#keep Ho

# first difference 
summary(ur.df(diff(usa), type = "none", selectlags = "BIC"))# Reject Ho
summary(ur.df(diff(aus), type = "none", selectlags = "BIC"))# Reject H0
#' The stationarity tests indicate that both series are I(1).
#' Hence, we can perform cointegration test 


#' Check for cointegration.
#' Estimate the long-run relationship
library(dynlm)
fit1 <- dynlm(aus~0+usa)
summary(fit1) 
#' The intercept term is omitted because it has 
#' no economic meaning.
#' If USA were to increase by 1 unit, Australia would increase 
#' by 0.985 unit. 

#' residuals 
ehat <- resid(fit1)
plot(ehat)

# check the stationarity of the residual
# 1. Autocorrelation 
library(forecast)
ggAcf(ehat) + labs(title = "Correlogram for the residual")
#' A visual inspection of the the time series suggests that 
#' the residuals may be stationary 

#' A formal test
# 2) Engle-Granger test 
fit2 <- dynlm(d(ehat)~L(ehat)-1)
summary(fit2)
# A 5% level critical value is -2.76.
#' Our test rejects the null of no cointegration, meaning that the series are cointegrated.
#' With cointegrated series we can construct a VEC model to better understand the causal relationship between the two variables.

# Alternatively 
summary(ur.df(ehat, type = "none", selectlags = "BIC"))# Reject H0

#' A cointegration test using the Johansen Approach 

library(vars)

joh=ca.jo(cbind(aus,usa), type = "trace", ecdet = c("const"), K = 2, spec = "longrun")
summary(joh)

#Example 13.9: Vector (Error correction model)
vecaus<- dynlm(d(aus)~L(ehat))
vecusa <- dynlm(d(usa)~L(ehat))

summary(vecaus)
summary(vecusa)

#' The coefficient on the error correction term e_(t-1) is significant for Australia, 
#' suggesting that changes in the US (large) economy do affect Australian (small) economy.
#' 
#' The error correction coefficient in the US equation is not statistically significant,
#' suggesting that changes in Australia do not influence American economy.
#' 

#Fit a VECM with Engle-Granger 2OLS estimator:
vecm.eg <- VECM(cbind(aus,usa), lag=1, estim = "2OLS")
vecm.eg
summary(vecm.eg)

??VECM

#Fit a VECM with Johansen MLE estimator:
vecm.jo <- VECM(gdp, lag=1, estim="ML")
vecm.jo
summary(vecm.jo)

#' The VEC model is a multivariate dynamic model that incorporates a cointegration equation.
#' It is relevant when we have two variables that are both I(1), but are cointegrated.
