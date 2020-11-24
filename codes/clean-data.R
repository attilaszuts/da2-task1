library(tidyverse)
rm(list = ls())

# read data 
covid_raw <- read_csv('data/raw/covid_raw.csv')
pop_raw <- read_csv('data/raw/pop_raw.csv')

# drop unnecessary columns
covid_clean <- covid_raw %>% 
  select(c(3,4, 8:11)) %>% 
  rename(country = Country_Region)%>% 
  group_by(country) %>% 
  summarise(
    confirmed = sum(Confirmed),
    deaths = sum(Deaths),
    recovered = sum(Recovered),
    active = sum(Active)
  ) 

# merge data and drop unnecessary columns
