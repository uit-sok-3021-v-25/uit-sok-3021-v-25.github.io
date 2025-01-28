

# Q 12.7

rm(list=ls())

library(tidyverse)


#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

str(usmacro)

head(usmacro)

# change the variables into ts() object

U <- ts(usmacro$u, start = c(1948,1), frequency = 4)
G <- ts(usmacro$g, start = c(1948,1), frequency = 4)
INF <- ts(usmacro$inf, start = c(1948,1), frequency = 4)

class(U)

# Unit root test

# H0: the series/variable is non-stationary/unit root 
# H1: the series is stationary

# criteria: Tau <= t_CV Reject H0

# Is U non-stationary or stationary?

plot.ts(U)
# the data does not show any clear trend,
# but has a mean which is different from zero.

# Manuall
library(dynlm)
fit <- dynlm(d(U) ~ L(U,1)+L(d(U),1:2))
summary(fit)

# The tau-statistic is equal to -3.901.
# The crtical value (at 5%) is equal to -2.86
# conlusion: Reject H0
# That means , U is stationary variable. 

# If you decide to include trend variable in the ADF model

t <- ts(1:length(U),start = c(1948,1), frequency = 4)
fit <- dynlm(d(U) ~ L(U,1)+L(d(U),1:2)+t)
summary(fit)
# Same conclusion 

library(urca)
?ur.df

summary(ur.df(U, type = "drift",selectlags = "BIC" ))


# the USA economic growth rate (G)

plot.ts(G)
summary(ur.df(G, type = "drift",selectlags = "AIC" ))
# Reject Ho

# Inf
plot.ts(INF)
summary(ur.df(INF, type = "drift",selectlags = "AIC" ))
# Reject H0



# conclusion:
# U,G,and INF are stationary in level.
# U ~ I(0), G ~ I(0), INF ~ I(0)
# That means each of them have a order of one.






# !2.8

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/okun5_aus.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/okun5_aus.rdata"))

str(okun5_aus)

head(okun5_aus)

# ts()

G <- ts(okun5_aus$g, start = c(1978,1), frequency = 4)
U <- ts(okun5_aus$u, start = c(1978,1), frequency = 4)


plot.ts(G)
plot.ts(U)

# unit root test
# H0: the series is non-stationary or the series has a unit root 
# H1: the series is stationary 

summary(ur.df(G, type = "drift",selectlags = "BIC" ))
# Reject Ho, and conclude that G is stationary, I(0)

# Unit root test in level 
summary(ur.df(U, type = "drift",selectlags = "BIC" ))
# Keep Ho, i.e., U is non-stationary 

# Unit root test in first difference 
plot.ts(diff(U)) # no need to include a drift term 
summary(ur.df(diff(U), type = "none",selectlags = "BIC" ))
# Reject Ho, i.e, first diff of U is stationary

# Conclusion: The order of integration of U is one.
# That is U ~ I(1). 
# that means, U is non-stationary in level, but stationary after 
# taking the first difference. 



# Q 12.10

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/oil5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/oil5.rdata"))

str(oil5)

head(oil5)


# a
dev.off()

oil5 %>% 
  ggplot(aes( x= dateid01, y= oil))+
  geom_line()

# define the data only before 2016Q1, including 
tail(oil5)
oil5M <- oil5 %>% filter(dateid01 <="2015-04-01")
tail(oil5M)

oil5M <- ts(oil5M$oil, start = c(1980,1), frequency = 4)

# Unit roo test 
summary(ur.df(oil5M, type = "trend",selectlags = "BIC" ))

# The tau-statistic is -3.397
# The critical value is (at 5%) is -3.43

# conclusion: Keep Ho: non-stationary or unit root 


# Unit root test in first difference 

summary(ur.df(diff(oil5M), type = "none",selectlags = "BIC" ))

# Conclusion: Reject Ho 

# Finally conclude that the variable oil is non-stationary in level, 
# but it is stationary in its first difference.
# So, oil ~ I(1) ---integrated of order one. 


# c). AR model 
library(dynlm)

# this is not a good model 
#AR_fit <- dynlm(oil5M ~ L(oil5M, 1:2)) 

# this is the good model 
AR_fit <- dynlm(d(oil5M) ~ L(d(oil5M), 1:2))

summary(AR_fit)

# Using the arima function 
library(forecast)
 AR_fit2 <- Arima(oil5M, order = c(2,1,0)) # notice d= 1 b/c the oil5M is stationary
# at first difference 
summary(AR_fit2)

# Alternatively 
AR_fit3 <- Arima(diff(oil5M), order = c(2,0,0))
summary(AR_fit3)

# Forecasting 
fit <- Arima(oil5M, order = c(2,1,0)) %>% forecast(h=3)

names(fit)

fit
df <- fit %>% data.frame()

class(df)

df

# include the actual values in the df

df$actual_value <- tail(oil5$oil,3)

df %>% 
  mutate(pfe = 100*(actual_value-Point.Forecast)/actual_value)

# positive percentage forecast error , pfe
df %>% 
  mutate(pfe = 100*(Point.Forecast-actual_value)/actual_value)














