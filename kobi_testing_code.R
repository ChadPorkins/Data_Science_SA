attacks = read.csv("attacks.csv")
attacks = as_tibble(attacks) %>%
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
attackers_new = attackers %>%
  as_tibble() %>%
  select(event_id,attacker_id,age_time_death,gender,birth_admin0_txt) %>%
  rename(birth_city = birth_admin0_txt) %>%
  filter(age_time_death > 0) %>%
  filter(gender %in% c("Male" , "Female") )

attacks_graph = attacks %>%
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
  geom_point(mapping = aes(x = date_year, y = global_count)) +
  geom_line(mapping = aes(x = date_year, y = global_count))

ggplot(data = attacks_graph_male) + 
  geom_point(mapping = aes(x = date_year, y = male_count)) + 
geom_line(mapping = aes(x = date_year, y = male_count))

ggplot(data = attacks_graph_female) + 
  geom_point(mapping = aes(x = date_year, y = female_count)) + 
  geom_line(mapping = aes(x = date_year, y = female_count))

ggplot(NULL, aes(date_year, n)) + 
  #geom_line(data=attacks_graph, col="green") +
  geom_line(data=attacks_graph_male, col="blue")+
  geom_line(data=attacks_graph_female, col="red")+
geom_smooth(data=attacks_graph_male, col="blue")+
  geom_smooth(data=attacks_graph_female, col="red")



attacks_wepon = attacks_graph %>%
  count(weapon_txt) %>%
  mutate(precentage = (n/sum(n)*100))
attacks_wepon

attacks_wepon_male = attacks_graph_male %>%
  count(weapon_txt)  %>%
rename(n_male =n) 


attacks_wepon_female = attacks_graph_female %>%
  count(weapon_txt)  %>%
  rename(n_female =n)

ggplot(attacks_wepon, aes(x=weapon_txt, y=n)) + 
  geom_bar(stat = "identity") +
  coord_flip()
 
comb_wepons = left_join(attacks_wepon_male,attacks_wepon_female, by = "weapon_txt")
sum(comb_wepons$n_male, na.rm = TRUE)
sum(comb_wepons$n_female, na.rm = TRUE)



prop.test(x = c(84, 314), n = c(155, 674))
prop.test(x = c(4, 17), n = c(155, 674))
prop.test(x = c(18, 60), n = c(155, 674))
prop.test(x = c(10, 151), n = c(155, 674))

#new_graph = group_by(attacks_graph,date_year,gender,country) %>%
#  summarise(
#    count = n()
#)
#
#new_graph_male = new_graph %>%
#  filter(gender == "Male")
#ggplot(data = new_graph_male) + 
#  geom_point(mapping = aes(x =  date_year, y = count)) + 
# facet_wrap(~ country, nrow = 2)

z = count(attackers_new, birth_city)
sri_lanka_male = read.csv("Sri_Lanka_Male.csv")
sri_lanka_female = read.csv("Sri_Lanka_Female.csv")
attacks_sri_lanka_man =  attackers_new %>%
filter(gender == "Male") %>%
  filter(birth_city == "Sri Lanka") %>%
  inner_join(attacks, by = "event_id") %>%
filter(country == birth_city) %>%
  rename(Year = date_year) %>%
  inner_join(sri_lanka_male, by = "Year")

t1= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Male, data = attacks_sri_lanka_man , family = "quasipoisson"	)
summary(t1)

cor.test(attacks_sri_lanka_man$casualties,attacks_sri_lanka_man$age_time_death, method="kendall")
cor.test(attacks_sri_lanka_man$casualties,attacks_sri_lanka_man$Education_Index, method="kendall")
cor.test(attacks_sri_lanka_man$casualties,attacks_sri_lanka_man$GDP, method="kendall")
cor.test(attacks_sri_lanka_man$casualties,attacks_sri_lanka_man$Unemployment_Male, method="kendall")

attacks_sri_lanka_female =  attackers_new %>%
  filter(gender == "Female") %>%
  filter(birth_city == "Sri Lanka") %>%
  inner_join(attacks, by = "event_id") %>%
  filter(country == birth_city) %>%
  rename(Year = date_year) %>%
  inner_join(sri_lanka_female, by = "Year")
t2= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Female, data = attacks_sri_lanka_female , family = "quasipoisson"	)
summary(t2)

cor.test(attacks_sri_lanka_female$casualties,attacks_sri_lanka_female$age_time_death, method="kendall")
cor.test(attacks_sri_lanka_female$casualties,attacks_sri_lanka_female$Education_Index, method="kendall")
cor.test(attacks_sri_lanka_female$casualties,attacks_sri_lanka_female$GDP, method="kendall")
cor.test(attacks_sri_lanka_female$casualties,attacks_sri_lanka_female$Unemployment_Female, method="kendall")

russia_male = read.csv("Russia_Male.csv")
russia_female = read.csv("Russia_Female.csv")
attacks_russia_male =  attackers_new %>%
  filter(gender == "Male") %>%
  filter(birth_city == "Russia") %>%
  inner_join(attacks, by = "event_id") %>%
  filter(country == birth_city) %>%
  rename(Year = date_year) %>%
  inner_join(russia_male, by = "Year")
t3= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Male, data = attacks_russia_male , family = "quasipoisson"	)
summary(t3)

cor.test(attacks_russia_male$casualties,attacks_russia_male$age_time_death, method="kendall")
cor.test(attacks_russia_male$casualties,attacks_russia_male$Education_Index, method="kendall")
cor.test(attacks_russia_male$casualties,attacks_russia_male$GDP, method="kendall")
cor.test(attacks_russia_male$casualties,attacks_russia_male$Unemployment_Male, method="kendall")

attacks_russia_female =  attackers_new %>%
  filter(gender == "Female") %>%
  filter(birth_city == "Russia") %>%
  inner_join(attacks, by = "event_id") %>%
  filter(country == birth_city) %>%
  rename(Year = date_year) %>%
  inner_join(russia_female, by = "Year")
t4= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Female, data = attacks_russia_female , family = "quasipoisson"	)
summary(t4)

cor.test(attacks_russia_female$casualties,attacks_russia_female$age_time_death, method="kendall")
cor.test(attacks_russia_female$casualties,attacks_russia_female$Education_Index, method="kendall")
cor.test(attacks_russia_female$casualties,attacks_russia_female$GDP, method="kendall")

pakistan_male = read.csv("Pakistan_Male.csv")
pakistan_female = read.csv("Pakistan_Female.csv")
attacks_pakistan_male =  attackers_new %>%
  filter(gender == "Male") %>%
  filter(birth_city == "Pakistan") %>%
  inner_join(attacks, by = "event_id") %>%
  filter(country == birth_city) %>%
  rename(Year = date_year) %>%
  inner_join(pakistan_male, by = "Year")
t5= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Male, data = attacks_pakistan_male , family = "quasipoisson"	)
summary(t5)

attacks_pakistan_female =  attackers_new %>%
  filter(gender == "Female") %>%
  filter(birth_city == "Pakistan") %>%
  inner_join(attacks, by = "event_id") %>%
  filter(country == birth_city) %>%
  rename(Year = date_year) %>%
  inner_join(pakistan_male, by = "Year")
t6= glm(casualties ~  age_time_death + Education_Index + GDP  + Unemployment_Male, data = attacks_pakistan_male , family = "quasipoisson"	)
summary(t5)

z = attackers %>% 
  filter(birth_admin0_txt == "Palestine")
