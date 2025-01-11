
# lags of time series data and regression 

rm(list=ls())
library(mosaic)

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

str(usmacro)

head(usmacro)


ggplot(usmacro, aes(x=dateid01, y=g)) + geom_line() + ylab("Growth Rate") + xlab("Year") +
  labs(title = "Figure 1: U.S. Quarterly GDP growth rate 1948:Q1 to 2016:Q1")

usmacro %>% ggplot(aes(x=dateid01, y=u)) + geom_line() + ylab("Unemployment Rate") + xlab("Year") +
  labs(title = "Figure 2: U.S. Quarterly unemployment rate 1948:Q1 to 2016:Q1")



usmacro %>% select(dateid01,u) %>% head()

usmacro %>% select(dateid01,u) %>%  
  mutate(u1 = dplyr::lag(u,1), # first lag of u
         u2 = dplyr::lag(u,2)) %>% head() # second lag u


# regress u against ints two lags 
df <- usmacro %>% select(dateid01,u) %>%  
  mutate(u1 = dplyr::lag(u,1),  
         u2 = dplyr::lag(u,2)) %>%  
  na.omit()  # drop NA's 


head(df)

fit <- lm(u~ u1+u2, data = df)  # The model: u_t = a0+a1*u_(t-1)+a2*u_(t-2)
summary(fit)

# Interpretation of results: the same interpretation as in the case of multiple linear regression model

# Alternatively use dynlm 
library(dynlm)
fit2 <- dynlm(u ~ L(u,1)+L(u,2), data = ts(usmacro)) #note ts()  in the data
summary(fit2)


# the advantage of using dynlm package is that 
fit3 <- dynlm(u ~ L(u,1:2)+L(g,0:1), data = ts(usmacro))
summary(fit3)

#' Note that in `base R`lag(x,-1) is the lag operator, lag(x) is a lead (t+1) operator.

# Lag: This is when you're looking at past data points to understand the current or future situation. In simple terms, a "lag" is like looking back in time. For example, if you’re studying sales this month, you might look at sales from last month to predict current trends. So, the "lag" is the previous month’s sales data compared to this month.
# 
# Lead: This is when you're looking at future data points to predict what will happen. It's like trying to look forward in time. For example, if you’re studying how current air traffic might affect passenger numbers next month, the "lead" would be the future data (next month’s data) based on the current month's figures.
# 
# So, in short:
# 
# Lag = looking back in time (past data)
# Lead = looking forward in time (future data)

# Both are used to understand how things that happened earlier (lag) or might happen later (lead) impact what is happening right now.


t=ts(1:5)
t
cbind(t,stats::lag(t,-1))
cbind(t,stats::lag(t))

df <- cbind(t,stats::lag(t),stats::lag(t,-1))
df
ts.intersect(df)

ts.intersect(t,stats::lag(t),stats::lag(t,-1))



##############################
# Forecasting
#######################
library(forecast)
#Forecasting using an AR(2) model using arima()
u <- ts(usmacro$u)
fit_ar <- arima(u, order = c(2, 0, 0))  # AR(2)
fit_ar 

?arima
summary(fit_ar)

# Forecast next 10 time points
ar1_forecast <- forecast(fit_ar , h = 10)
ar1_forecast
plot(ar1_forecast, main = "AR(1) Model Forecast")

# OR more easily
usmacro %>% select(u) %>% Arima(., order=c(2,0,0)) %>%  forecast(h=20) %>% autoplot()


# #Forecasting using an MA(2) model using arima() 
fit_ma <- arima(u, order = c(0, 0, 2))  # MA(2)
fit_ma
summary(fit_ma )

# Forecast next 10 time points
ma1_forecast <- forecast(fit_ma , h = 10)
ma1_forecast

# Plot forecast
plot(ma1_forecast, main = "MA(1) Model Forecast")
usmacro %>% select(u) %>% Arima(., order=c(0,0,2)) %>%  forecast(h=20) %>% autoplot()

