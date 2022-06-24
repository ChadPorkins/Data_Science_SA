
## We need to Split our data
```{r}
attacks2 = attacks %>%
  count(casualties, sort = TRUE) #%>%
#filter(casualties < 800)
head(attacks2)

```

## creating new tibblies
```{r}
x = filter(attacks2, n>=400 ) %>%
  select(casualties) %>%
  pull()
```

```{r}

very_common_400  =  filter(attacks, casualties %in% x )

test1 = very_common_400 %>%
  select(wounded_average ,killed_average , casualties) %>%
  apply(2, agostino.test)

test1

```

```{r}
y = filter(attacks2, n<=400 & n>=300 ) %>%
  select(casualties) %>%
  pull()
```


```{r}
very_common_300 = filter(attacks, casualties %in% y )

test2 = very_common_300 %>%
  select(wounded_average ,killed_average , casualties) %>%
  apply(2, agostino.test)

test2

```


```{r}
z = filter(attacks2, n<=300 & n>=100 ) %>%
  select(casualties) %>%
  pull()
```


```{r}
very_common_200 = filter(attacks, casualties %in% z )

test3 = very_common_200 %>%
  select(wounded_average ,killed_average , casualties) %>%
  apply(2, agostino.test)

test3

```


```{r}
q = filter(attacks2, n<=100 & n>=80 ) %>%
  select(casualties) %>%
  pull()
```


```{r}
very_common_100 = filter(attacks, casualties %in% q )

test4 = very_common_100 %>%
  select(wounded_average ,killed_average , casualties) %>%
  apply(2, agostino.test)

test4

```

```{r}
p = filter(attacks2, n<=80 & n>50 ) %>%
  select(casualties) %>%
  pull()
```


```{r}
very_common_0 = filter(attacks, casualties %in% p )

test5 = very_common_0 %>%
  select(wounded_average ,killed_average , casualties) %>%
  apply(2, agostino.test)

test5

```


```{r}
k = filter(attacks2, n<=50 & n>30 ) %>%
  select(casualties) %>%
  pull()
```


```{r}
very_common_20 = filter(attacks, casualties %in% k )

test6 = very_common_20 %>%
  select(wounded_average ,killed_average , casualties) %>%
  apply(2, agostino.test)

test6

```


```{r}
e = filter(attacks2, n<=30 & n>21 ) %>%
  select(casualties) %>%
  pull()
```


```{r}
very_common_e = filter(attacks, casualties %in% e )

test7 = very_common_e %>%
  select(wounded_average ,killed_average , casualties) %>%
  apply(2, agostino.test)

test7

```


```{r}
r = filter(attacks2, n<=21 & n>16 ) %>%
  select(casualties) %>%
  pull()
```


```{r}
very_common_r = filter(attacks, casualties %in% r )

test8 = very_common_r %>%
  select(wounded_average ,killed_average , casualties) %>%
  apply(2, agostino.test)

test8

```


```{r}
t = filter(attacks2, n<=16 & n>0 ) %>%
  select(casualties) %>%
  pull()
```


```{r}
very_common_t = filter(attacks, casualties %in% t )

agostino.test(1/(very_common_t$casualties))


```

attackers = read.csv("attackers.csv") 
head( as_tibble(attackers) )

attackers = attackers %>%
  as_tibble() %>%
  select(event_id,attacker_id,age_time_death,gender,birth_admin0_txt) %>%
  filter(age_time_death > 0) 

