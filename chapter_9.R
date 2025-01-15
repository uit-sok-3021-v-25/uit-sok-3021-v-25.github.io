
#' Chapter 9
#' ==========================

rm(list=ls())
library(forecast)
library(broom)
library(tidyverse)

#' Example 9.1

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

str(usmacro)

head(usmacro)

usmacro %>% ggplot(aes(x=dateid01, y=u)) + geom_line() + ylab("Unemployment Rate") + xlab("Year") +
  labs(title = "Figure 9.2a: U.S. Quarterly unemployment rate 1948:Q1 to 2016:Q1")

ggplot(usmacro, aes(x=dateid01, y=g)) + geom_line() + ylab("Growth Rate") + xlab("Year") +
  labs(title = "Figure 9.2b: U.S. Quarterly GDP growth rate 1948:Q1 to 2016:Q1")

#' Example 9.2
#' The autocorrelation function (acf) is a set of correlation coefficients
#' between a time series and various lags of itself.

#' The default base r function:
acf(usmacro$u) 
#' Note that the autocorrelation for lag 0 is always 1,so there is no need to plot it.

#' We can print the autocorrelation coefficients.
acf(usmacro$u)$acf


#' The forecast package gives a nicer ggplot, with the ar(0)=1 removed
usmacro %>% select(u) %>% ggAcf() +
  labs(title = "Figure 9.4: Correlogram for U.S. Quarterly unemployment rate")

usmacro %>% select(g) %>% ggAcf() +
  labs(title = "Figure 9.5: Correlogram for growth rate in U.S. GDP")

#' Figure 9.6
#' Some time series plots on random variables.
par(mfrow=c(3,1))
plot((rnorm(300, mean=10)), type="l", main="Figure 9.6 (a) Time series of a stationary variable", xlab="t", ylab="y")
plot(cumsum(rnorm(300)), type="l", main="Figure 9.6 (b) Time series of a nonstationary variable", xlab="t", ylab="x")
plot(cumsum(rnorm(300, mean=0.2)), type="l", main="Figure 9.6 (c) Time series of a nonstationary variable with trend", xlab="t", ylab="z")
par(mfrow=c(1,1))


# Estimation and forecasting using AR model  
#####################################################

#Example 9.6 Forecasting an unemployment using AR(2) model 
usmacro %>% select(u) %>% Arima(., order=c(2,0,0)) %>% tidy() 

# ARIMA forecasts
usmacro %>% select(u) %>% Arima(., order=c(2,0,0)) %>%forecast(h=3)

# Plot the forecast value 
usmacro %>% select(u) %>% Arima(., order=c(2,0,0)) %>%  forecast(h=20) %>% autoplot()


# Example 9.7 Forecasting unemployment with an ARDL(2,1) model
###############################################################

usmacro.lag <- cbind( u = usmacro[,"u"],
                      g = usmacro[,"g"],
                      gLag1 = stats::lag(usmacro[,"g"],-1))

head(usmacro.lag)

Arima(usmacro.lag[,"u"], order=c(2,0,0), xreg = usmacro.lag[,"gLag1"]) %>% tidy()

# save & remove tidy() at the end 
fit2 <- Arima(usmacro.lag[,"u"], order=c(2,0,0), xreg = usmacro.lag[,"gLag1"]) 

# Forecasting 
fc2 <- forecast(fit2, h=3,xreg=cbind(xreg = c(usmacro[,"g"][273],0.869,1.069))) 

fc2

# plot the forecast value and interval 
autoplot(fc2) + ylab("Unemployment") +
  ggtitle("Forecast unemployment with future GDP growth")

#' Another package, forecasting using ARDL(2,1) model 
##################################################################
rm(list=ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

#install.packages("dLagM")
library(dLagM)  
#?ardlDlm

rem.p = c(0)  # p is the lag of x
rem.q = c(0)  # q is the lag of y 

#' Note that, here, the role of p and q are changed in this package 

remove = list(p = rem.p , q = rem.q)   

model.ardl = ardlDlm(x = usmacro[,"g"],
                     y = usmacro[,"u"], p = 1 , q = 2, remove=remove) 

summary(model.ardl)


# Table 9.4, page 435 
x = c(usmacro[,"g"][273],0.869,1.069) # points  where forecast of the y-variable to be made 

fc <- dLagM::forecast(model = model.ardl,x= x, h = length(x), interval = TRUE) 
fc

# Create a data frame for forecast values and intervals
forecast_data <- data.frame(
  Time = (length(usmacro$u) + 1):(length(usmacro$u) + length(x)),  # Next 3 periods
  Forecast = fc$forecasts[, 2],  # Forecast values
  Lower_Bound = fc$forecasts[, 1],  # Lower bound of the forecast
  Upper_Bound = fc$forecasts[, 3]  # Upper bound of the forecast
)

# Plot actual data and forecast with intervals
library(ggplot2)
ggplot() +
  # Plot actual data
  geom_line(data = data.frame(Time = 1:length(usmacro$u), Value = usmacro$u), aes(x = Time, y = Value), color = "blue") +
  # Plot forecasted data
  geom_line(data = forecast_data, aes(x = Time, y = Forecast), color = "red") +
  # Plot the forecast interval
  geom_ribbon(data = forecast_data, aes(x = Time, ymin = Lower_Bound, ymax = Upper_Bound), fill = "gray", alpha = 0.4) +
  labs(x = "Time", y = "Value", title = "Actual vs Forecast with 95% Confidence Interval") +
  theme_minimal()


#  Estimate ARDL(2,1) Model using the package dynlm
##########################################################
library(dynlm)
fit <- dynlm(u ~ L(u,1)+L(u,2)+L(g,1),data=ts(usmacro)) 
summary(fit)

#' Alternatively 
fit2 <- dynlm(u ~ L(u,1:2)+L(g,1),data=ts(usmacro)) 
summary(fit2)



#' Lag selection criteria 
##########################################

# Example 9.8

usmacro %>% dplyr::select(u) %>% Arima(., order=c(0,0,0)) %>% glance()   
usmacro %>% dplyr::select(u) %>% Arima(., order=c(1,0,0)) %>% glance()

# the auto.arima() function from the forecast package 
# selects best model based on information criteria , aic and bic 
usmacro %>% dplyr::select(u) %>% auto.arima()

auto.arima(usmacro[,"u"], xreg = usmacro[,"g"]) 
auto.arima(usmacro[,"u"], xreg = usmacro[,"g"], ic = "aic") 
auto.arima(usmacro[,"u"], xreg = usmacro[,"g"], ic = "bic")

#
library(dynlm)
BIC(dynlm(u~L(u,1)+L(g,0:8),data=ts(usmacro)))



# loop 'BIC()' over multiple ADL models 
order <- 1:8

BICs <- sapply(order, function(x) 
  BIC(dynlm(u ~ L(u, 1:x),data = ts(usmacro,frequency = 4,start=c(1948,1)))))

BICs

# select the AR model with the smallest BIC
BICs[ which.min(BICs)]

#
BICs <- sapply(order, function(x) 
  BIC(dynlm(u~ L(u, x) + L(g, 1:x),data = ts(usmacro,frequency = 4,start=c(1948,1))))) 

BICs   

################################################
# Estimate ARDL and select optimal lag of the model 

library(dynlm)

# Initialize variables
p_max <- 8  # Maximum lag for dependent variable
q_max <- 8  # Maximum lag for independent variable


# Initialize storage for model summaries
aic_values <- matrix(NA, nrow = p_max, ncol = q_max)
bic_values <- matrix(NA, nrow = p_max, ncol = q_max)


# Loop through possible lag lengths
for (p in 1:p_max) {
  for (q in 1:q_max) {
    # Define the model formula
    model_formula <- as.formula(paste(
      "u ~", 
      paste(paste0("L(u, ", 1:p, ")"), collapse = " + "), "+",
      paste(paste0("L(g, ", 0:q, ")"), collapse = " + ")
    ))
    
    # Fit the model
    model <- dynlm(model_formula, data = ts(usmacro))
    
    # Calculate AIC and BIC
    aic_values[p, q] <- AIC(model)  # Correctly using AIC
    bic_values[p, q] <- BIC(model)  # Correctly using BIC
  }
}

# The optimal lag lengths based on AIC
optimal_aic_index <- which(aic_values == min(aic_values, na.rm = TRUE), arr.ind = TRUE)
optimal_bic_index <- which(bic_values == min(bic_values, na.rm = TRUE), arr.ind = TRUE)

# optimal lag, based on AIC
cat("Optimal lag length based on AIC: p =", optimal_aic_index[1], ", q =", optimal_aic_index[2], "\n")

# optimal lag, based on BIC
cat("Optimal lag length based on BIC: p =", optimal_bic_index[1], ", q =", optimal_bic_index[2], "\n")

###########################################

# more detail look at the link 
#https://www.econometrics-with-r.org/14-6-llsuic.html



# Example 9.9 Testing for Granger causality

#H0: x doesn't granger cause y (i.e., x doesn't contribute to the forecast of y)

library(lmtest)
grangertest(u ~ g, order = 1, data = usmacro)
grangertest(u ~ g, order = 2, data = usmacro)

#' g does granger cause u



# Testing for Serial Correlation 

#' Example 9.10, page 439 
#' 

require(dynlm)
require(mosaic)
#' Now we must work on the ts objects directly.
#' 
#' Create ts data

g <- usmacro %>% dplyr::select(g) %>% ts(., start = c(1948,1), frequency = 4)
u <- usmacro %>% dplyr::select(u) %>% ts(., start = c(1948,1), frequency = 4)

#using ts() function will help us to let R know it is time series data and to enter
#time variable



#'Estimate ARDL(2,1)
#
fit1 <- dynlm(u~L(u,1)+L(u,2)+L(g,1)) 
summary(fit1)

library(forecast)
residuals(fit1) %>% ggAcf() +
  labs(title = "Figure 9.7: Correlogram from ARDL(2,1) model")

#The correlations for its residuals are generally small and insignificant. 
#There are some correlation at lag 7 , 8 and 17
#however, these correlations are at long lags and barely insignificant.


#' Lagrange multiplier(Lm)/Breusch-Godfrey test for serial correlation
#' H0: No autocorrelation vs H1: there is autocorrelation/Serial correlation 
require(lmtest)
bgtest(fit1) 
bgtest(fit1, order=2) 
bgtest(fit1, order=3)
bgtest(fit1, order=4) 

#' The Durbin-Watson test(the test statistic does not rely on large samples like that of LM test)
#' , HO:no serial correlation 
dwtest(fit1)   


#' Estimate ARDL(1,1)
#
fit2 <- dynlm(u~L(u,1)+L(g,1))
summary(fit2)


residuals(fit2) %>% ggAcf() +
  labs(title = "Figure 9.8: Correlogram from ARDL(1,1) model")

#The first two autocorrelations are significant.
#We conclude that that the errors are serially correlated. 
#More lags are needed to improve the forecasting specifcation, 
#and the least squares standard errors. 



#Lagrange multiplier(Lm)/Breusch-Godfrey test for seral correlation
# Example 9.12 LM test
# H0: No autocorrelation
bgtest(fit2) #order one by deafualt, null is rejected
bgtest(fit2, order=2) 
bgtest(fit2, order=3) 
bgtest(fit2, order=4) 
bgtest(fit2, order=40) 

#' The Durbin-Watson test
#' , HO:no serial correlation 
dwtest(fit2)  #Ho is rejected 
#conclusion: Model 1 is preferred than model 2.





#' Finite distributed lags 

#' Example 9.13  Okun's Law

rm(list=ls())


#' Okuan's Law - is an economic model that gives relationship 
#' between unemployment(u) and growth rate of the economy.
#' Specifically,the change in u is related to the output growth rate 

#' 
#'  Data definition:
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/okun5_aus.def")
# okun5_aus.def
# 
# g  u
# 
# Obs: 153 quarterly observations on Australian macro variables from 1978Q2 to 2016Q2
# 
# g	growth rate: percentage change in Australian GDP
# u	Unemployment rate

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/okun5_aus.rdata"))

# rename the data frame 
okun <- okun5_aus

# Time series plots 
plot(okun$dateid01,okun$g, type="l", xlab = "year", ylab = "growth rate (g)")
plot(okun$dateid01,okun$u, type="l", xlab = "year", ylab = "unemployment rate (u)")


# use ts() to transform to time series object. 
#This will help us to let R know it is time series data and to enter time variable

G <- ts(okun$g, start=c(1978,2), freq=4)
U <- ts(okun$u, start=c(1978,2), freq=4)



#################################################
#' Note that in `base R`lag(x,-1) is the lag operator, lag(x) is a lead (t+1) operator.

t=ts(1:5)
cbind(t,stats::lag(t),stats::lag(t,-1))

cbind(t,stats::lag(t,-1))
cbind(t,stats::lag(t))

x=cbind(U,stats::lag(U,-1),diff(U),G,stats::lag(G,-1),stats::lag(G,-2),stats::lag(G,-3),stats::lag(G,-4))
head(x,10)
tail(x,10)
#View(x)


#' or:
#' Note that in this setup, the first 4 observations are removed, 
#' and we have no missing variables.
data=ts.intersect(U,lU=stats::lag(U,-1),DU=diff(U),G,lG=stats::lag(G,-1),l2G=stats::lag(G,-2),
                  l3G=stats::lag(G,-3),l4G=stats::lag(G,-4), dframe=TRUE)
head(data)
tail(data)

#' dimensions of the data.
dim(x) 
dim(data)
length(U)

#' Without using the dynamic linear regression 
mod4 <- lm(DU~G+lG+l2G+l3G+l4G, data=data)
summary(mod4)

################################################


#' Using the dynamic package
require(dynlm)
fit3 <- dynlm(diff(U)~G+L(G,1)+L(G,2)+L(G,3)+L(G,4)+L(G,5))
summary(fit3)

#' A bit faster model specification
fit4 <- dynlm(diff(U)~G+L(G,1:5))
summary(fit4)


library(broom)
coefs <- tidy(fit4)

# lag coefficients 
coefs$estimate

#distributed-lag-weights or multipliers from the model
cumsum(coefs$estimate[2:6]) 

plot(x=1:5, cumsum(coefs$estimate[2:6]),"l")





rm(list=ls())

#' Example 9.14 Phillips Curve

#' The philps curve has a long history in macroeconomics as a tool for describing the relationship 
#' between inflation and unemployment. 
#' Specifically, inflation at time t is related to the change in 
#' the unemployment rate from period t-1 to period t.
#
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/phillips5_aus.def")

# phillips5_aus.def
# 
# inf u du
# 
# Obs: 117 quarterly observations on Australian macro variables from 1987Q1 to 2016Q1
# 
# inf	inflation rate calculated as the percentage change from the previous period in the Australian CPI
# u	Unemployment rate
# du	Change in the unemployment rate from the previous quarter

library(tidyverse)
library(dynlm)
library(forecast)

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/phillips5_aus.rdata"))

# rename data
phillips <- phillips5_aus

head(phillips)

INF <- phillips %>% dplyr::select(inf) %>% ts(., start=c(1987,1), freq=4)
U <- phillips %>% dplyr::select(u) %>% ts(., start=c(1987,1), freq=4)

autoplot(INF) + labs(title = "Figure 9.10: Time series for the Australian inflation rate: 1987:Q1 to 2016:Q1")

#Equation(9.65)

fit <- dynlm(INF~ d(U))
summary(fit)

resid(fit) %>% ggAcf() +
  labs(title = "Figure 9.11: Correlogram for residuals from Phillips curve")

resid(fit) %>% autoplot() +
  labs(title = "Residuals from Phillips curve")

require(lmtest)

# H0: No autocorrelation
bgtest(fit)
bgtest(fit, order=6)

#' The Durbin-Watson test
dwtest(fit)

require(sandwich)
sqrt(diag(vcov(fit))) # ols se 
sqrt(diag(vcovHAC(fit))) # robust se


# OLS estimates and White HCE standard errors
coeftest(fit, vcov=vcovHAC, type = c("HAC") )

# OLS estimates and standard errors
coeftest(fit)


#######################################
# type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5")
# type[1]
# hc= function(mod,x) list(paste(type[x]),sqrt(diag(vcovHC(mod, type=type[x]))))  
# sqrt(diag(vcovHC(fit, type="HC0")))
# 
# hc(fit,4) 
################################################

#' Using least squares with HAC standard errors overcomes 
#' the negative consequences that autocorrelated errors 
#' have for least squares standard errors. 
#' However, it doesn't address the issue of finding an estimator 
#' that is better in the sense that it has a lower variance.
#' one way to proceed is to make an assumption about the model 
#' that generates the autocorrelated errors and to drive an
#'  estimator compatible with this assumption. 
#' If we assume the error model in this case is given by an AR(1) process,
#' this can be combined with the model above to give us: 

# inf_t = alpha*(1-rho)+rho*inf_(t-1)+beta_0*DU_t-rho*beta_0*DU_(t-1)+v_t 

# You can see page 452-453 on the textbook for more detail




#' Nonlinear Least Squares, p. 453

names(phillips)

data <- ts.intersect(INF, stats::lag(INF,-1), diff(U), stats::lag(diff(U),-1), dframe=TRUE)
head(data)

names(data) <- c("inf","inf.L1","Du","Du.L1")
head(data)

# Equation 9.68

#' some starting(initial) values:
rho = 0.5
b1 = 0.7
b2 = -.5

# Do the NLS (Nonlinear Least Squares) fit 
fit2 <- nls(inf ~ b1*(1-rho) +rho*inf.L1+ b2*Du  - rho*b2*Du.L1, data=data, start=list(rho=rho,b1=b1,b2=b2))
summary(fit2)  

# OLS  
fit3 <- lm(inf~Du, data = data)   ## 9.69
summary(fit3)


#' Fix autocorrelation when there is only one lag 

#install.packages("orcutt")
library(orcutt)

#just see what it does 
??cochrane.orcutt  
#It is an interactive method using to solve first order autocorrelation problems.

fit.co <- cochrane.orcutt(fit3)
fit.co
summary(fit.co)

#Generalized Least Squares, gls() function. 
#This function fits a linear model using generalized least squares. 
#the errors are allowed to be correlated and/or have unequal variances.

# ?gls 
library(nlme)  

fit.gls.1 <- gls(inf~Du, data = data, correlation = corARMA(p=1), method = "ML")
summary(fit.gls.1)

fit.gls.2 <- update(fit.gls.1, correlation = corARMA(p=2)) 
summary(fit.gls.2)

fit.gls.0 <- update(fit.gls.1, correlation = NULL)  
summary(fit.gls.0)

#' comparison of different models
anova(fit.gls.1,fit.gls.0) 

#' the first model(fit.gls.1)is preferred compared 
#' to the model with no AR term(fit.gls.0)

anova(fit.gls.1,fit.gls.2) 


#-----------------------------------------------------------------
# Geometrically declining lags
library(dLagM)
model.koyck = koyckDlm(x = phillips$u,
                       y = phillips$inf)
summary(model.koyck)

#------------------------------------------------------------



#' Example 9.18

#' Computing multipliers for an infinite Lag Okun's Law model 
#Steps 1 - Begin by estimating an ARDL model 
#step 2   - Use the formula on page 460 to compute the 
#          multiplier estimates for the infinite Lag okun's law model 

rm(list=ls())

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/okun5_aus.rdata"))
# rename data
okun <- okun5_aus
names(okun)

U <- ts(okun$u, start=c(1978,2), freq=4)
G <- ts(okun$g, start=c(1978,2), freq=4)

model <- dynlm(d(U)~L(d(U), 1:2)+G+L(G,1))
summary(model)

#' The impact multiplier and the delay multipliers for the 
#' first 4 quarters are 
b0=coef(model)[4]
b1=coef(model)[5]+b0*coef(model)[2]

b2=coef(model)[2]*b1+b0*coef(model)[3]
b3=coef(model)[2]*b2+b1*coef(model)[3]
b4=coef(model)[2]*b3+b2*coef(model)[3]
# and so on

b=c(b0,b1,b2,b3,b4)
b
plot(0:4,b, type="l", main="Figure 9.12")




