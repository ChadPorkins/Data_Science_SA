attacks = read.csv("attacks.csv")
library(tidyverse)
library(tseries)
library(car)
library(moments)

attacks = as_tibble(attacks) %>%
  select(event_id,wounded_low,wounded_high,killed_low,killed_high) %>%
  filter(wounded_low >= 0) %>%
  filter(wounded_high >= 0) %>%
  filter(killed_low >= 0) %>%
  filter(killed_high >= 0) %>%
  mutate(wounded_average = ((wounded_low + wounded_high)/2) ) %>%
  mutate(killed_average = ((killed_low + killed_high)/2) ) %>% 
  select(event_id,wounded_average,killed_average)

attacks_test = apply(attacks, 2, jarque.bera.test)
attacks_test = do.call(rbind.data.frame, attacks_test)
attacks_test = subset(attacks_test, select = -c(1,2,4,5) )
print(attacks_test > 0.05)

qqPlot(attacks$wounded_average, id= FALSE)
qqPlot(attacks$killed_average, id= FALSE)

skewness(attacks$wounded_average,na.rm = TRUE)
skewness(attacks$killed_average,na.rm = TRUE)

attacks_normal = attacks %>%
  mutate(wounded_average = log(wounded_average + 1, base = 1000)) %>%
  mutate(killed_average = log(killed_average + 1, base = 1000)) 

qqPlot(attacks_normal$wounded_average, id= FALSE)
qqPlot(attacks_normal$killed_average, id= FALSE)

apply(attacks_normal, 2, jarque.bera.test)
