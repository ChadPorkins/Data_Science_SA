library(tidyverse)
library(tseries)
library(car)
library(moments)
library(AID)
library(leaflet)
library(hrbrthemes)
library(ggpubr)
library(ggpmisc)

attacks = read.csv("attacks.csv")
head(as_tibble(attacks))

attacks_new = as_tibble(attacks) %>%
  select(event_id,wounded_low,wounded_high,killed_low,killed_high,admin0_txt,date_year,weapon_txt) %>%
  filter(wounded_low >= 0) %>%
  filter(wounded_high >= 0) %>%
  filter(killed_low >= 0) %>%
  filter(killed_high >= 0) %>%
  mutate(wounded_average = ceiling(((wounded_low + wounded_high)/2) )) %>%
  mutate(killed_average = ceiling(((killed_low + killed_high)/2) )) %>% 
  mutate(casualties = wounded_average + killed_average) %>%
  filter(casualties < 500) %>%
  rename( country = admin0_txt) %>%
  select(event_id,wounded_average,killed_average,casualties,country,date_year,weapon_txt)


attackers = read.csv("attackers.csv") 
as.tibble(attackers)

attackers_new = attackers %>%
  as_tibble() %>%
  select(event_id,attacker_id,age_time_death,gender,birth_admin0_txt) %>%
  rename(birth_country = birth_admin0_txt) %>%
  filter(age_time_death > 0) %>%
  filter(gender %in% c("Male" , "Female") )

attackers_new

attacks_new %>%
  ggplot( aes(x=casualties)) +
  geom_histogram( binwidth=10, fill="#0afa4a", color="#fa0a0a", alpha=0.9) +
  ggtitle("Histogram of the number of casualties ") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

i = attacks_new %>%
  ggplot( aes(x=sqrt(casualties + 1 ))) +
  geom_histogram( binwidth = 1 , fill="#0afa4a", color="#fa0a0a", alpha=0.9) +
  ggtitle("Square-root transformation") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


o = attacks_new %>%
  ggplot( aes(x=log(casualties + 1 ))) +
  geom_histogram( binwidth = 1 , fill="#0afa4a", color="#fa0a0a", alpha=0.9) +
  ggtitle("Log transformation ") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


p = attacks_new %>%
  ggplot( aes(x=1/(casualties + 1 ))) +
  geom_histogram( binwidth = 0.5 , fill="#0afa4a", color="#fa0a0a", alpha=0.9) +
  ggtitle("Inverse transformation ") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

q = attacks_new %>%
  ggplot( aes(x=log10(casualties + 1 ))) +
  geom_histogram( binwidth = 1 , fill="#0afa4a", color="#fa0a0a", alpha=0.9) +
  ggtitle("Log10 transformation ") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

figure <- ggarrange(i, o, q, p,
                    labels = c("A", "B", "C","D"),
                    ncol = 2, nrow = 2)
figure

agostino.test(log10(attacks_new$casualties+1))
agostino.test(log(attacks_new$casualties+1))

qqPlot(log10(attacks_new$casualties+1),id= FALSE, ylab ="log10 values", main = "Q–Q plot log10 casualties ")
qqPlot(log(attacks_new$casualties+1),id= FALSE, ylab ="log values", main = "Q–Q plot log casualties")

mean(attacks_new$casualties) 
var(attacks_new$casualties)

a = attacks_new %>%
  filter(casualties < 100)
b = attacks_new %>%
  filter(casualties < 200 & casualties > 100 )
c = attacks_new %>%
  filter(casualties < 300 & casualties > 200 )
d = attacks_new %>%
  filter(casualties < 400 & casualties > 300 )
e = attacks_new %>%
  filter(casualties < 500 & casualties > 400)

mean(a$casualties)
var(a$casualties)
mean(b$casualties)
var(b$casualties)
mean(c$casualties)
var(c$casualties)
mean(d$casualties) 
var(d$casualties)
mean(e$casualties)
var(e$casualties)

mean1 = c(17.4095,
          137.9321,
          237.1207,
          338,
          452.8
)

var1  = c(379.039,
          749.7151,
          799.3711,
          974.4286,
          1298.622
) 

df <- data.frame(mean1, var1)


ggplot(data = df, aes(x = mean1, y = var1)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label))) +
  stat_poly_eq(label.y = 0.9) +
  xlab("Mean of each group") + ylab("Variance of each group") +
  geom_point()


attacks_graph = attacks_new %>%
  left_join(attackers_new, by =  "event_id") %>%
  select(date_year,gender,country,weapon_txt) %>%
  arrange(desc(date_year)) %>%
  add_count(date_year) 

attacks_graph_male = attacks_graph %>%
  filter(gender == "Male") %>%
  select(date_year,gender,country,weapon_txt) %>%
  add_count(date_year) 

attacks_graph_female = attacks_graph %>%
  filter(gender == "Female") %>%
  select(date_year,gender,country,weapon_txt) %>%
  add_count(date_year) 

ggplot(data = attacks_graph) + 
  geom_point(mapping = aes(x = date_year, y = n)) +
  geom_line(mapping = aes(x = date_year, y = n)) +
  xlab("Year") + ylab("Number of attacks") + ggtitle("Suicide attacks 1982 - 2019") +
  theme_bw()

ggplot(NULL, aes(date_year, n)) + 
  geom_line(data=attacks_graph_male, col="blue")+
  geom_line(data=attacks_graph_female, col="red")+
  xlab("Year") + ylab("Number of attacks") + ggtitle("Suicide attacks 1982 - 2019 lines (Male - Blue, Female - Red)") +
  theme_bw()

ggplot(NULL, aes(date_year, n)) + 
  geom_smooth(data=attacks_graph_male, col="blue",se = F)+
  geom_smooth(data=attacks_graph_female, col="red",se = F)+
  xlab("Year") + ylab("Number of attacks") + ggtitle("Suicide attacks 1982 - 2019 smooth (Male - Blue, Female - Red)") +
  theme_bw()

###########################3

attacks_wepon = attacks_graph %>%
  count(weapon_txt) %>%


attacks_wepon_male = attacks_graph_male %>%
  count(weapon_txt)  %>%
  rename(n_male =n) 


attacks_wepon_female = attacks_graph_female %>%
  count(weapon_txt)  %>%
  rename(n_female =n)


comb_wepons = left_join(attacks_wepon_male,attacks_wepon_female, by = "weapon_txt")
sum(comb_wepons$n_male, na.rm = TRUE)
sum(comb_wepons$n_female, na.rm = TRUE)

comb_wepons

belt_bomb = prop.test(x = c(84, 314), n = c(155, 674), alternative = "greater")
backpack_bomb = prop.test(x = c(4, 17), n = c(155, 674), alternative = "greater")


male_attackers = attackers_new %>%
  filter(gender == "Male") %>%
  inner_join(attacks_new, by =  "event_id") %>%
  select(age_time_death,casualties,gender,birth_country)

female_attackers = attackers_new %>%
  filter(gender == "Female") %>%
  inner_join(attacks_new, by =  "event_id") %>%
  select(age_time_death,casualties,gender,birth_country)

both_gender1 = bind_rows(male_attackers,female_attackers) %>%
  select(gender,casualties,birth_country  )
test <- wilcox.test(both_gender1$casualties ~ both_gender1$gender)
test 
ggplot(both_gender1, aes(x= casualties, color=gender)) +
  ggtitle(" World suicide attacks histogram - Male vs Female" ) +
  geom_histogram(fill="white")

both_gender2 =  both_gender1 %>%
  filter(birth_country ==  "Sri Lanka") %>%
  select(gender, casualties)
test2 <- wilcox.test(both_gender2$casualties ~ both_gender2$gender)
test2 
ggplot(both_gender2, aes(x= casualties, color=gender)) +
  ggtitle(" Sri Lankan suicide attacks  histogram - Male vs Female" ) +
  geom_histogram(fill="white")

both_gender3 =  both_gender1 %>%
  filter(birth_country ==  "Russia") %>%
  select(gender, casualties)
test3 <- wilcox.test(both_gender3$casualties ~ both_gender3$gender)
test3 
ggplot(both_gender3, aes(x= casualties, color=gender)) +
  ggtitle(" Russian suicide attacks histogram - Male vs Female" ) +
  geom_histogram(fill="white")

sri_lanka_male = read.csv("Sri_Lanka_Male.csv")
sri_lanka_female = read.csv("Sri_Lanka_Female.csv")
attacks_sri_lanka_man =  attackers_new %>%
  filter(gender == "Male") %>%
  filter(birth_country == "Sri Lanka") %>%
  inner_join(attacks_new, by = "event_id") %>%
  filter(country == birth_country) %>%
  rename(Year = date_year) %>%
  inner_join(sri_lanka_male, by = "Year")

t1= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Male, data = attacks_sri_lanka_man , family = quasipoisson(link = "log"))
summary(t1)


cor.test(attacks_sri_lanka_man$casualties,attacks_sri_lanka_man$age_time_death, method="kendall")
cor.test(attacks_sri_lanka_man$casualties,attacks_sri_lanka_man$Education_Index, method="kendall")
cor.test(attacks_sri_lanka_man$casualties,attacks_sri_lanka_man$GDP, method="kendall")
cor.test(attacks_sri_lanka_man$casualties,attacks_sri_lanka_man$Unemployment_Male, method="kendall")

attacks_sri_lanka_female =  attackers_new %>%
  filter(gender == "Female") %>%
  filter(birth_country == "Sri Lanka") %>%
  inner_join(attacks_new, by = "event_id") %>%
  filter(country == birth_country) %>%
  rename(Year = date_year) %>%
  inner_join(sri_lanka_female, by = "Year")
t2= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Female, data = attacks_sri_lanka_female , quasipoisson(link = "log"))
summary(t2)

cor.test(attacks_sri_lanka_female$casualties,attacks_sri_lanka_female$age_time_death, method="kendall")
cor.test(attacks_sri_lanka_female$casualties,attacks_sri_lanka_female$Education_Index, method="kendall")
cor.test(attacks_sri_lanka_female$casualties,attacks_sri_lanka_female$GDP, method="kendall")
cor.test(attacks_sri_lanka_female$casualties,attacks_sri_lanka_female$Unemployment_Female, method="kendall")


russia_male = read.csv("Russia_Male.csv")
russia_female = read.csv("Russia_Female.csv")
attacks_russia_male =  attackers_new %>%
  filter(gender == "Male") %>%
  filter(birth_country == "Russia") %>%
  inner_join(attacks_new, by = "event_id") %>%
  filter(country == birth_country) %>%
  rename(Year = date_year) %>%
  inner_join(russia_male, by = "Year")
t3= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Male, data = attacks_russia_male , family = quasipoisson(link = "log"))
summary(t3)

cor.test(attacks_russia_male$casualties,attacks_russia_male$age_time_death, method="kendall")
cor.test(attacks_russia_male$casualties,attacks_russia_male$Education_Index, method="kendall")
cor.test(attacks_russia_male$casualties,attacks_russia_male$GDP, method="kendall")
cor.test(attacks_russia_male$casualties,attacks_russia_male$Unemployment_Male, method="kendall")


attacks_russia_female =  attackers_new %>%
  filter(gender == "Female") %>%
  filter(birth_country == "Russia") %>%
  inner_join(attacks_new, by = "event_id") %>%
  filter(country == birth_country) %>%
  rename(Year = date_year) %>%
  inner_join(russia_female, by = "Year")
t4= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Female, data = attacks_russia_female , family = quasipoisson(link = "log"))	
summary(t4)

cor.test(attacks_russia_female$casualties,attacks_russia_female$age_time_death, method="kendall")
cor.test(attacks_russia_female$casualties,attacks_russia_female$Education_Index, method="kendall")
cor.test(attacks_russia_female$casualties,attacks_russia_female$GDP, method="kendall")
cor.test(attacks_russia_female$casualties,attacks_russia_female$Unemployment_Female, method="kendall")

world_man = read.csv("World_Male.csv")
attacks_world_male =  attackers_new %>%
  filter(gender == "Male") %>%
  inner_join(attacks_new, by = "event_id") %>%
  filter(country == birth_country) %>%
  rename(Year = date_year) %>%
  inner_join(world_man, by = "Year")

t5= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Male, data = attacks_world_male , family = quasipoisson(link = "log"))
summary(t5)

cor.test(attacks_world_male$casualties,attacks_world_male$age_time_death, method="kendall")
cor.test(attacks_world_male$casualties,attacks_world_male$Education_Index, method="kendall")
cor.test(attacks_world_male$casualties,attacks_world_male$GDP, method="kendall")
cor.test(attacks_world_male$casualties,attacks_world_male$Unemployment_Male, method="kendall")


world_female = read.csv("World_Female.csv")
attacks_world_female =  attackers_new %>%
  filter(gender == "Female") %>%
  inner_join(attacks_new, by = "event_id") %>%
  filter(country == birth_country) %>%
  rename(Year = date_year) %>%
  inner_join(world_female, by = "Year")

t6= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Female, data = attacks_world_female , family = quasipoisson(link = "log"))	

summary(t6)

cor.test(attacks_world_female$casualties,attacks_world_female$age_time_death, method="kendall")
cor.test(attacks_world_female$casualties,attacks_world_female$Education_Index, method="kendall")
cor.test(attacks_world_female$casualties,attacks_world_female$GDP, method="kendall")
cor.test(attacks_world_female$casualties,attacks_world_female$Unemployment_Female, method="kendall")

