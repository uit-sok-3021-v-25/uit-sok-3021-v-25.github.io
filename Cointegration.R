# cointegration 
#Ho: unit root (non-stationary) vs H1: stationary

rm(list = ls())
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usdata5.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usdata5.rdata"))
head(usdata5)

# Obs: 749   Monthly U.S. Data from 1954M8 to 2016M12
# 
# infn=	Annual inflation rate for each month (obtained using infn = 100*(ln(cpi)-ln(cpi(-12)))
# where CPI is the consumer price index from FRED series CPIAUCSL
# br=	3-year bond rate, percent (3-Year Treasury Constant Maturity Rate, FRED series G3)
# ffr= Federal funds rate, percent (FRED series FEDFUNDS) 

br <- ts(usdata5$br, start = c(1954,8), frequency = 12) 
ffr <- ts(usdata5$ffr, start = c(1954,8), frequency = 12)
infn <- ts(usdata5$infn, start = c(1954,8), frequency = 12)

plot.ts(br)
plot.ts(ffr)

library(urca)
summary(ur.df(ffr, type = "drift", selectlags = "BIC")) 
summary(ur.df(br, type = "drift", selectlags = "BIC")) 

# The null hypothesis is nonstationarity, it now rejected on both series
plot(diff(ffr))
plot(diff(br))

summary(ur.df(diff(ffr), type = "none", selectlags = "BIC")) 
summary(ur.df(diff(br), type = "none", selectlags = "BIC"))


#' Co-integration:- the relationship between I(1) variables 

#' H0: no cointegration vs cointegration 

library(tidyverse)
usdata5 %>% 
  ggplot(aes(x = dateid01, y= br))+
  geom_line(col="blue")+
  geom_line(aes(x= dateid01, y= ffr), col="red")
# From the plot we can see that the two series 
# are moving together. This is a sign 
# of cointegrated series. 

# Check the cointegration 

#' Two step procedure by Engle and Granger
library(dynlm)
fit <- dynlm(br~ffr)
# Extract the residuals from the model 
e=resid(fit) 

# Stationarity test of the residuals 
fit_res <- dynlm(d(e) ~ 0+L(e,1)+L(d(e),1:2))
summary(fit_res)
# Compare the t-value of first lag of e with the 5% critical value
# T_c = 3.37 (See Table 12.4 in the text book).
# Reject H0: no-cointegration when t-value <= t_c


#' Alternatively, just check whether the error is stationary or not 
summary(ur.df(e, type = "none", selectlags = "BIC"))

# Alternatively 
library(tseries)
adf.test(e)
pp.test(e, type = "Z(t_alpha)")

#' Another cointegration approach, Johansen test for co-integration, 
#' H0: no co-integration vs H1: Co-integration
library(urca)
?ca.jo
johansen <- ca.jo(cbind(br, ffr), ecdet = "const", type = "trace")
summary(johansen)


###########################################
#####Error Correction Model
#################################################

# A relationship between I(1) variables (or co-integration) is often referred to as 
# long-run relationship while a relationship between I(0) variables is often referred to as a short-run relationship.
# Error correction model is a dynamic relationship between I(0) variables, which embeds a cointegrating relationship


# Error correction model,
Error_corr=dynlm(diff(br)~L(e,1)+L(diff(br),1:2)+L(diff(ffr),0:2))
summary(Error_corr)

# Error correction model, when F is the dep.variable 
Error_corr=dynlm(diff(ffr)~L(e,1)+L(diff(ffr),1:2)+L(diff(br),0:2))
summary(Error_corr)

#Using VECM function 
library(tsDyn)
Vector_Error <- VECM(cbind(br, ffr), lag = 1, include = "none")
summary(Vector_Error)

