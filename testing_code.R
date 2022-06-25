library(tidyverse)
library(mapview)
library(sf)
library(ggplot2)
library(ggmap)
library(tseries)
library(car)
library(moments)
library(writexl)
library(AID)

attacks = read.csv("attacks.csv")

attacks = as_tibble(attacks) %>%
  select(event_id,wounded_low,wounded_high,killed_low,killed_high) %>%
  filter(wounded_low >= 0) %>%
  filter(wounded_high >= 0) %>%
  filter(killed_low >= 0) %>%
  filter(killed_high >= 0) %>%
  mutate(wounded_average = ceiling(((wounded_low + wounded_high)/2) )) %>%
  mutate(killed_average = ceiling(((killed_low + killed_high)/2) )) %>% 
  mutate(casualties = wounded_average + killed_average) %>%
  filter(casualties < 500) %>%
  select(event_id,wounded_average,killed_average,casualties)

attackers = read.csv("attackers.csv") 

attackers = attackers %>%
  as_tibble() %>%
  select(event_id,attacker_id,age_time_death,gender,birth_admin0_txt) %>%
  filter(age_time_death > 0) 

male_attackers = attackers %>%
  filter(gender == "Male") %>%
  inner_join(attacks, by =  "event_id") %>%
  select(age_time_death,wounded_average,killed_average,casualties,gender)

female_attackers = attackers %>%
  filter(gender == "Female") %>%
  inner_join(attacks, by =  "event_id") %>%
  select(age_time_death,wounded_average,killed_average,casualties,gender)

both_gender1 = bind_rows(male_attackers,female_attackers) %>%
  select(gender,wounded_average )
test <- wilcox.test(both_gender1$wounded_average ~ both_gender1$gender)
test

ggplot(both_gender1, aes(x= wounded_average, color=gender)) +
  geom_histogram(fill="white")

both_gender2 = bind_rows(male_attackers,female_attackers) %>%
  select(gender,killed_average )
test2 <- wilcox.test(both_gender2$killed_average ~ both_gender2$gender)
test2

both_gender3 = bind_rows(male_attackers,female_attackers) %>%
  select(gender,casualties )
test3 <- wilcox.test(both_gender3$casualties ~ both_gender3$gender)
test3$p.value


hist(both_gender1$wounded_average)
