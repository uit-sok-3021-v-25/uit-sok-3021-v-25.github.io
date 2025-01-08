
rm(list = ls())
library(tidyverse)
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

View(usmacro)

usmacro %>% 
  ggplot(aes(x=dateid01,y=g))+
  geom_line()+
  ylab("GDP growth(g)")+
  xlab("Time")


g <- usmacro$g
g

g <- ts(usmacro$g, frequency = 4, start = c(1948,1),end = C(2016,1))
g

plot.ts(g)



usmacro %>% select(dateid01,u) %>% head()

usmacro %>% select(dateid01,u) %>% 
  mutate(u1 = dplyr::lag(u,1) , # first lag of u
         u2 = dplyr::lag(u,2),
         u3 = dplyr::lag(u,3)
         
         ) %>% head()



df <- usmacro %>% select(dateid01,u) %>% 
  mutate(u1 = dplyr::lag(u,1) , # first lag of u
         u2 = dplyr::lag(u,2),
         u3 = dplyr::lag(u,3)) %>% na.omit()
head(df)


fit <- lm(u ~ u1+u2+u3, data = df)
summary(fit)



