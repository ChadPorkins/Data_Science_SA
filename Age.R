attacks = read.csv("attacks.csv")
library(tidyverse)
library(tseries)
library(car)
library(moments)
library(writexl)
library(AID)



  attacks = as_tibble(attacks) %>%
  select(event_id,wounded_low,wounded_high,killed_low,killed_high) %>%
  filter(wounded_low >= 0) %>%
  filter(wounded_high >= 0) %>%
  filter(killed_low >= 0) %>%
  filter(killed_high >= 0) %>%
  mutate(wounded_average = round(((wounded_low + wounded_high)/2) )) %>%
  mutate(killed_average = round(((killed_low + killed_high)/2) )) %>% 
  select(event_id,wounded_average,killed_average)
 
   attacks = attacks %>%
    mutate(sk = row_number())

attacks_test = apply(attacks, 2, jarque.bera.test)
attacks_test = do.call(rbind.data.frame, attacks_test)
attacks_test = subset(attacks_test, select = -c(1,2,4,5) )
print(attacks_test > 0.05)

qqPlot(attacks$wounded_average, id= FALSE)
#top 5 are extreme
qqPlot(attacks$killed_average, id= FALSE)
#top 5 are extreme

skewness(attacks$wounded_average,na.rm = TRUE)
skewness(attacks$killed_average,na.rm = TRUE)

attacks_normal = attacks %>%
  mutate(wounded_average = -1 /(wounded_average +1 )^2) %>%
 mutate(killed_average = log(killed_average + 1, base = exp(10))) 

#qqPlot(attacks_normal$wounded_average, id= FALSE)
#qqPlot(attacks_normal$killed_average, id= FALSE)
#apply(attacks_normal, 2, jarque.bera.test)

attacks_fix = attacks %>%
  select(wounded_average, killed_average, sk )


########################
top5_wounded_average =  attacks %>%                                      
  arrange(desc(wounded_average)) %>% 
  head(5)
top5_wounded_average
#########################
top5_killed_average =  attacks %>%                                      
  arrange(desc(killed_average)) %>% 
  head(5)
top5_killed_average
###########################
attacks2 = attacks %>%
filter(!event_id %in% top5_wounded_average$event_id & top5_killed_average$event_id ) %>%
  arrange(desc(wounded_average))
###########################
qqPlot(attacks2$wounded_average, id= FALSE)
qqPlot(attacks2$killed_average, id= FALSE)
#############################
attacks_normal = attacks2 %>%
  mutate(wounded_average2 = log(wounded_average + 1))
  #mutate(killed_average2 = log(killed_average + 1, base = exp(10))) 
############################
  qqPlot(attacks_normal$wounded_average2, id= FALSE)
  qqPlot(attacks_normal$killed_average2, id= FALSE)  
  
  write_xlsx(attacks2,"pop")
  ###################################################33
x = attacks$wounded_average 
x2 = x[!x %in% boxplot.stats(x)$out] 
length(x) - length(x2)   
boxplot(x2)  
qqPlot(x2, id= FALSE)
######################################################
attacks2 = attacks %>%
  mutate(sk = row_number()) %>%
  mutate(casualties = wounded_average + killed_average ) %>%
  filter(casualties >= 1)
######################################################
y = attacks2$casualties
y2 = y[!y %in% boxplot.stats(y)$out] 
length(y) - length(y2)   
qqPlot(y2, id= FALSE)
#########################################################
attacks3 = attacks %>%
  mutate(casualties = wounded_average + killed_average) %>%
  filter(casualties >= 1) 

attacks_count =  count(attacks3,casualties, sort = TRUE)
hist(attacks_count$n)

###################################################

qqPlot(attacks3$casualties, id= FALSE)
jarque.bera.test(attacks3$casualties2)
hist(attacks3$casualties, breaks = 1000)
#################################################
attacks4 = attacks3 %>%
  filter( casualties %in% 1:3)
hist(attacks4$casualties, breaks = 3)

qqPlot(attacks4$casualties, id= FALSE)
shapiro.test(attacks4$casualties)
qqPlot(sqrt(attacks4$casualties), id= FALSE)
qqPlot(attacks4$zscore, id= FALSE)

library(AID)
data = very_common1$casualties
out <- boxcoxnc(data, method = "sw", lambda = seq(-2,2,0.0001), verbose = F, plot = F)
out$lambda.hat
qqPlot(out$tf.data)
out <- boxcoxnc(data, method = "mle", lambda = seq(-2,2,0.0001))
################################################################################
very_common1  =  filter(attacks3, casualties %in% c(2, 4,24,22,18,20,17,16,15,13,14,11,12,9,1,7,10,8,3,5,6))
very_common1  =  filter(attacks3, casualties %in% c(2, 4))
hist(very_common1$casualties)
hist(sqrt(very_common1$casualties))
shapiro.test(very_common1$casualties)
shapiro.test(very_common1$casualties)

agostino.test(c(x,y))
agostino.test(very_common1$casualties)
shapiro.test(attacks_count$n)

attacks_test = apply(attacks, 2, jarque.bera.test)
attacks_test = do.call(rbind.data.frame, attacks_test)
attacks_test = subset(attacks_test, select = -c(1,2,4,5) )
print(attacks_test > 0.05)