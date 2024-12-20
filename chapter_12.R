
# Chapter 12

rm(list=ls())
library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/gdp5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/gdp5.rdata"))

# Obs: 132 quarterly observations, U.S. data from 1984Q1 to 2016Q4
head(gdp5)
str(gdp5)
gdp.ts <- ts(gdp5$gdp, start = c(1984,1), frequency = 4)


browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usdata5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usdata5.rdata"))

# Obs: 749   Monthly U.S. Data from 1954M8 to 2016M12
# 
# infn =	Annual inflation rate for each month (obtained using infn = 100*(ln(cpi)-ln(cpi(-12)))
#        where CPI is the consumer price index from FRED series CPIAUCSL
# br =	3-year bond rate, percent (3-Year Treasury Constant Maturity Rate, FRED series G3)
# ffr	= Federal funds rate, percent (FRED series FEDFUNDS) 

head(usdata5)                                           
br.ts <- ts(usdata5$br, start = c(1954,8), frequency = 12)
ffr.ts <- ts(usdata5$ffr, start = c(1954,8), frequency = 12)
infn.ts <- ts(usdata5$infn, start = c(1954,8), frequency = 12)

# Copy Figure 12.1
par(mfrow=c(4,2))
# GDP
plot(gdp.ts)
plot(diff(gdp.ts))
# INF
plot(infn.ts)
plot(diff(infn.ts))
# FFR
plot(ffr.ts)
plot(diff(ffr.ts))
# BR
plot(br.ts)
plot(diff(br.ts))
#
par(mfrow=c(1,1))
####                                           

# Table 12.1
round(mean(window(gdp.ts, start=c(1984,2), end=c(2000,3))),2)
round(mean(window(gdp.ts, start=c(2000,4), end=c(2016,4))),2)

round(mean(window(infn.ts, start=c(1954,8), end=c(1985,10))),2)
round(mean(window(infn.ts, start=c(1985,11), end=c(2016,12))),2)

round(mean(window(ffr.ts, start=c(1954,8), end=c(1985,10))),2)
round(mean(window(ffr.ts, start=c(1985,11), end=c(2016,12))),2)

round(mean(window(br.ts, start=c(1954,8), end=c(1985,10))),2)
round(mean(window(br.ts, start=c(1985,11), end=c(2016,12))),2)

# diffs
round(mean(window(diff(gdp.ts), start=c(1984,2), end=c(2000,3))),3)
round(mean(window(diff(gdp.ts), start=c(2000,4), end=c(2016,4))),3)

round(mean(window(diff(infn.ts), start=c(1954,9), end=c(1985,10))),2)
round(mean(window(diff(infn.ts), start=c(1985,11), end=c(2016,12))),3)

round(mean(window(diff(ffr.ts), start=c(1954,9), end=c(1985,10))),2)
round(mean(window(diff(ffr.ts), start=c(1985,11), end=c(2016,12))),2)

round(mean(window(diff(br.ts), start=c(1954,9), end=c(1985,10))),2)
round(mean(window(diff(br.ts), start=c(1985,11), end=c(2016,12))),2)



########################################################
#  Example 12.2 A deterministic trend for wheat yield
############################################################

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/toody5.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/toody5.rdata"))
head(toody5)
str(toody5)


# Data on wheat yield over time, in Australia.
# Look how the yield is fluctuating an increasing trend

library(rockchalk) 

fit1 <- lm(y~t, data = toody5) 
summary(fit1)
plotCurves(fit1, plotx = "t", type="l",lty=2, main="a) Yield") 

#' Alternatively 
toody5 %>% ggplot(aes(x=dateid01,y=y))+geom_line()+geom_smooth(method = lm,se=FALSE)

#' Look the graph, the observations fluctuate around the increasing trend with 
#' a particularly bad (very low yield) year in 1969( see the graph for the rainfall in the fig. below;
#' we discover there is a slight downward trend and very little rainfall in 1969)

fit2 <- lm(rain~t, data = toody5)
summary(fit2)
plotCurves(fit2, plotx = "t", type="l", lty=2, main="b) Rain")

#' Alternatively 
toody5 %>% ggplot(aes(x=dateid01,y=rain))+geom_line()+geom_smooth(method = lm,se=FALSE)


#' Let us estimate the relationship between yield and rainfall. 
#' There are two alternatives to estimate the relationship. 

#'############################################
#' Alternative 1: 
###################################################
#' Just include trend in the regression.
#' Furthermore, there are decreasing returns to rainfall,so we include RAIN^2 as well as RAIN in the model
#' leading to the following estimated equation.

fit3 <- lm(y~t+rain+I(rain^2), data = toody5)
summary(fit3) 
plotCurves(fit3, plotx = "t", type="l", lty=2, main="a) Yield") 

# If the dependent variable is given in log form
fit4 <- lm(log(y)~t+rain+I(rain^2), data = toody5)
summary(fit4) # 12.10



###########################################################
#  Alternative 2
#############################################################
#' Detrend Yield, RAIN, and RAIN^2 and 
#' estimate the detrended model.

#' First, estimating the trends.
#' That is detrend the variables, y, RAIN, and RAIN^2 

summary(lm(y~t, data = toody5))
summary(lm(rain~t, data = toody5))
summary(lm(I(rain^2)~t, data = toody5))

# Equation 12.11

#' Now apply OLS using the detrended variables 
#' Notices that the estimates should be identical with model "fit3" above.
summary(lm(resid(lm(y~t, data = toody5))~0+resid(lm(rain~t, data = toody5))+resid(lm(rain^2~t, data = toody5))))

#' The standard errors from the two different models are not exactly equal. 
#' The standard error discrepancy arises from the different degrees of freedom used to estimate the error variance. 



#############################################################
### Simulation of different types of AR(1) and random walk model 
##############################################################

set.seed(1234)

#' Simulate an AR1 model, with no intercept (or drift)
a=arima.sim(list(order=c(1,0,0), ar=.7), n=500)

plot(a, main=expression(paste("(a) ",y[t],"=","0.7",y[t-1]+v[t])))
abline(h=mean(a), col="red")

#' ARMAacf () function from stats package 
#' compute the theoretical autocorrelation function or partial autocorrelation function 
#' for ARMA process for an ARMA process
ARMAacf(ar=c(.7),lag.max=10) 
.7    # p y0
.7^2  # p^2 y0
.7^3  # p^3 y0

acfa=acf(a, lag.max=10,type="correlation")
acfa

#install.packages("astsa")
library(astsa)
acf2(a)
cbind(acfa$acf,ARMAacf(ar=c(.7),lag.max=10))

# Estimation of AR(1) model.
#Note that R presents the "mean" of the series in the ARIMA output,not the intercept.

arima(a, order = c(1, 0, 0))  
#' ar(a, aic = T)

#' In the forecast package, the Arima function labels the mean correctly
#install.packages("forecast")
library(forecast)
Arima(a, order = c(1, 0, 0))  

set.seed(1234)
# Simulate an AR1 model, with intercept (or drift)
b=arima.sim(list(order=c(1,0,0), ar=.7), n=500) + 1 

mean(b)
plot(b, main=expression(paste("(b) ",y[t],"=","1+0.7",y[t-1]+v[t])))
abline(h=mean(b), col="blue")

# Estimation of AR(1) model
Arima(b, order = c(1, 0, 0))  

# The result is telling you that the estimated model is 
# b(t) = 1.0113 + .7303*b(t-1) + v(t)
# whereas, it should be telling you the estimated model is
# b(t) - 1.0113 = .7303*[b(t-1) - 1.0113] + v(t)
# or 
# b(t) = 0.2727476 + .7303*x(t-1) + v(t).
# Note that 0.2727476 = 1.0113*(1-.7303), 
# see page 572, the equation before 12.13. R presents the "mean" of the series in the ARIMA output,
# not the intercept.


# It is possible to have the algorithm find the optimal model specification
fit=auto.arima(b)
fit
#tsdiag(fit)
plot(forecast(fit, h = 50))
abline(h=mean(b), col="red")

t <- 1:length(b)
set.seed(1234)
c=arima.sim(list(order=c(1,0,0), ar=.7), n=500) + 1 + 0.01*t

plot(c, main=expression(paste("(c) ",y[t],"=","1+0.01t+0.7",y[t-1]+v[t])))

arima(c, order = c(1,0,0), xreg=1:length(t)) 
mean(c)

Arima(c, order = c(1, 0, 0), xreg=t)  # Estimation of AR(1) model
auto.arima(c)

set.seed(3234)
d=ts(cumsum(rnorm(500)))

plot(d, main=expression(paste("(d) ",y[t],"=",y[t-1]+v[t])))
mean(d)
Arima(d, order = c(1,0,0))

set.seed(1234)
e=ts(cumsum(rnorm(500))+0.1)

plot(e, main=expression(paste("(e) ",y[t],"=","0.1+",y[t-1]+v[t])))

Arima(e, order = c(1,0,0))


set.seed(1234)
f=ts(cumsum(rnorm(500)+0.1+ 0.01*t))

plot(f, main=expression(paste("(f) ",y[t],"=","0.1+0.01t+",y[t-1]+v[t])))
Arima(f, order = c(1,0,0), xreg=1:length(t)) 
mean(f)

# Copy Figure 12.4
par(mfrow=c(3,2))
plot(a, main=expression(paste("(a) ",y[t],"=","0.7",y[t-1]+v[t])))
plot(b, main=expression(paste("(b) ",y[t],"=","1+0.7",y[t-1]+v[t])))
plot(c, main=expression(paste("(c) ",y[t],"=","1+0.01t+0.7",y[t-1]+v[t])))
plot(d, main=expression(paste("(d) ",y[t],"=",y[t-1]+v[t])))
plot(e, main=expression(paste("(e) ",y[t],"=","0.1+",y[t-1]+v[t])))
plot(f, main=expression(paste("(f) ",y[t],"=","0.1+0.01t+",y[t-1]+v[t])))
par(mfrow=c(1,1))



#################################
######## Spurious Regression 
#######################################

#Example 12.3:  Regression with two random walks

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/spurious.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/spurious.rdata"))

head(spurious)

summary(lm(rw1~rw2, data = spurious))

library(lattice)
Data.ts <- ts(spurious)
xyplot(Data.ts[, 1:2], superpose = TRUE)
names(spurious)
plot(spurious$rw2,spurious$rw1, xlab="rw2", ylab="rw1") #scatter plot 

require(lmtest)
mod1=lm(rw1~rw2, data = spurious)
summary(mod1)

#' This result suggests that the simple regression model fits the data well (look at the R^2)
#' and the estimated slope is significantly different from zero.In fact, the t-statistic is huge!
#' These results are, however, completely meaningless, or spurious. The apparent significance of the 
#' relationship is false. Because the two series are have nothing in common, nor are they causally related in any way.
#' look at the serial correlation of the model(very high correlation), which is an indication that
#' something is wrong with the regression.

bgtest(mod1)
require(car)
durbinWatsonTest(lm(rw1~rw2, data = spurious))

#-----------------------------------------
require(astsa)
rw1 <- ts(spurious$rw1)
rw2 <- ts(spurious$rw2)

acf2(rw1)
Arima(rw1, order = c(1,0,0), xreg=rw2) 

#--------------------------------------------



############################################
########## Unit Root Tests for Stationarity 
############################################

#Ho: unit root (non-stationary) vs H1: stationary

rm(list = ls())
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usdata5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usdata5.rdata"))
head(usdata5)
str(usdata5)
tail(usdata5)
# Obs: 749   Monthly U.S. Data from 1954M8 to 2016M12
# 
# infn=	Annual inflation rate for each month (obtained using infn = 100*(ln(cpi)-ln(cpi(-12)))
# where CPI is the consumer price index from FRED series CPIAUCSL
# br=	3-year bond rate, percent (3-Year Treasury Constant Maturity Rate, FRED series G3)
# ffr= Federal funds rate, percent (FRED series FEDFUNDS) 

br.ts <- ts(usdata5$br, start = c(1954,8), frequency = 12) 
ffr.ts <- ts(usdata5$ffr, start = c(1954,8), frequency = 12)
infn.ts <- ts(usdata5$infn, start = c(1954,8), frequency = 12)


library(dynlm)

#' Example 12.4:  PP. 579 

#' Augmented Dickey-Fuller test with intercept, No trend 

#' H0:unit root (non-stationary).
#' For checking stationary, the usual t-critical values and p-values cannot be used
#' Instead we compare the t-critical value for the first lag of the dep.var. with the critical value from Table 12.2. (see the Text) 
#' Reject Ho if tau(t-value) <= t_Cv, (t_Cv=-2.86 at 5% level)
#' Notice here two augmentation terms have been included for both variables, to account for serial correlation. 

summary(dynlm(d(ffr.ts) ~ L(ffr.ts)+d(L(ffr.ts,1:2)))) 
summary(dynlm(d(br.ts) ~ L(br.ts)+d(L(br.ts,1:2))))


# Example 12.5: Is GDP trend stationary?

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/gdp5.rdata"))
# Obs: 132 quarterly observations, U.S. data from 1984Q1 to 2016Q4
head(gdp5)
gdp.ts <- ts(gdp5$gdp, start = c(1984,1), frequency = 4)


t <- 0:length(gdp.ts)
t <- ts(t, start = c(1984,1), frequency = 4)
summary(dynlm(d(gdp.ts) ~ t + L(gdp.ts)+d(L(gdp.ts,1:2))))

#' two augmentation terms minimized the SC, eliminated major autocorrelation in the residuals.

#' H0:unit root, Reject Ho if tau(t-value) <= t_Cv, (t_Cv=-3.41 at 5% level). 
#' conclusion: tau=-1.999 <=-3.41, 
#' False. Hence, GDP follows a non-stationary random walk. 
#' Thus,there is insufficient evidence to conclude that GDP is trend stationary. 


#' Example 12.6. Is wheat yield trend stationary? 

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/toody5.rdata"))
head(toody5)

yield.ts <- ts(toody5$y, start = 1950, frequency = 1)
t <- ts(toody5$t, start = 1950, frequency = 1)
summary(dynlm(d(log(yield.ts)) ~ t + L(log(yield.ts))))



#' Thus, we reject the null hypothesis of non-stationarity and
#' conclude that ln(yield) is trend stationary.



#' Example 12.7: Order of Integration of variables

#' Above, we showed that ffr.ts, and br.ts are non-stationary.
#' To find their order of integration,
#' we ask the next question: Are their first differences stationary?
#' Their plots fluctuate around zero.
plot(diff(ffr.ts))
plot(diff(br.ts))
#' Given their plots fluctuate around zero, we use Dickey-Fuller test equation with no intercept and no trend.

#' Manually (on levels), p. 581
summary(dynlm(d(d(ffr.ts)) ~ 0 + L(d(ffr.ts))+d(d(L(ffr.ts)))))

summary(dynlm(d(d(br.ts)) ~ 0 + L(d(br.ts))+d(d(L(br.ts)))))


#' Ho:unit root(non-stationary). 
#' The null is rejected in either series. 
#' and conclude that both series are stationary at their first difference
#' Hence, we say that the series ffr.ts and br.ts are I(1) because they had to be 
#' differences once to make them stationary. 


#' Stationarity test directly using R packages  

#' install.packages("urca")
library(urca)
#' helpful link 
#' https://stats.stackexchange.com/questions/24072/interpreting-rs-ur-df-dickey-fuller-unit-root-test-results

??ur.df

summary(ur.df(ffr.ts, type = "drift", lags = 2)) # do not reject H0 on Nonstationary at 1%
summary(ur.df(br.ts, type = "drift", lags = 2)) # do not reject H0 on Nonstationary at 5%


# The null hypothesis is nonstationarity, it now rejected on both series
# The series are stationary on first diff, hence they are I(1).
summary(ur.df(diff(ffr.ts), type = "none", lags = 0)) #none=neither an intercept nor a trend is included in the test regression 
summary(ur.df(diff(br.ts), type = "none", lags = 0))

#install.packages("tseries")
library(tseries)

# Automatic ADF test
adf.test(ffr.ts)
adf.test(diff(ffr.ts))

?adf.test
#k- is the optimal lag of the differenced term on the right side of the equation, added to account serial correlation 

adf.test(br.ts)
adf.test(diff(br.ts))

# Phillips-Perron test
# A nonparametric correction for autocorrelation (essentially employing a HAC
# estimate of the long-run variance in a Dickey-Fuller-type test
?pp.test
pp.test(ffr.ts, type = "Z(t_alpha)")
pp.test(diff(ffr.ts), type = "Z(t_alpha)")

pp.test(br.ts, type = "Z(t_alpha)")
pp.test(diff(br.ts), type = "Z(t_alpha)")


############################################
##### Cointegration test 
###########################################

#' Co-integration:- the relationship between I(1) variables 
#' such as the residuas are I(0)
#' 
#' H0: no cointegration vs cointegration 

#' Two step procedure by Engle and Granger
summary(dynlm(br.ts~ffr.ts))
# Extract the residuals from the model 
e=resid(dynlm(br.ts~ffr.ts)) 

# Stationarity test of the residuals 
summary(dynlm(d(e) ~ 0+L(e,1)+L(d(e),1:2)))
# Compare the t-value of first lag of e with the 5% critical value
# T_c = 3.37 (See Table 12.4 in the text book).
# Reject H0: no-cointegration when t-value <= t_c


#' Alternatively, just check whether the error is stationary or not 
summary(ur.df(e, type = "none", lags = 2))
adf.test(e)
pp.test(e, type = "Z(t_alpha)")

# Still another method, direct method, br is column 2, and ffr is column 3 of usa data
head(usdata5)

library(tseries)
po.test(usdata5[,3:2])  #Phillips-Ouliaris Cointegration Test, H0: no cointegration vs H1: Cointegration


#' Another cointegration approach, Johansen test for co-integration, 
#' H0: no co-integration vs H1: Co-integration
library(urca)
?ca.jo
johansen <- ca.jo(usdata5[,3:2], ecdet = "const", type = "trace")
summary(johansen)


###########################################
#####Error Correction Model
#################################################

# A relationship between I(1) variables (or co-integration) is often referred to as 
# long-run relationship while a relationship between I(0) variables is often referred to as a short-run relationship.
# Error correction model is a dynamic relationship between I(0) variables, which embeds a cointegrating relationship


#' Several ways to estimate Error correction model 

B <- br.ts
F <- ffr.ts

# Error correction model, when B is the dep.variable 
Error_corr_B=dynlm(diff(B)~0+L(e,1)+L(diff(B),1:2)+L(diff(F),0:4))
summary(Error_corr_B)
# Error correction model, when F is the dep.variable 
Error_corr_F=dynlm(diff(F)~0+L(e,1)+L(diff(F),1:2)+L(diff(B),0:4))
summary(Error_corr_F)

#Using VECM function 
library(tsDyn)
Vector_Error <- VECM(cbind(B,F), lag = 4, include = "none")
summary(Vector_Error)

#' Alternatively, Estimate directly using non-linear least squares or 
#' use OLS to estimate the modified model and then retrieve the parameters  

Data <- ts.intersect(dB=diff(B),lagB=stats::lag(B,-1),lagF=stats::lag(F,-1),dF=diff(F),lagdF=stats::lag(diff(F),-1), dframe=TRUE)

require(mosaic)

#' options(scipen=999)
beta2 <- coef(lm(dB~lagB+lagF+dF+lagdF, data=Data))  #see page 585 and 584 
beta2
#
g <- fitModel(dB~A*lagB+A*B1+A*B2*lagF+D*dF+D1*lagdF, data=Data, start=list(A=beta2[2],B1=beta2[1]/beta2[2],B2=beta2[3],D=beta2[4],D1=beta2[5]))
summary(g)





