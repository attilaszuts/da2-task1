library(tidyverse)
rm(list = ls())

# read data 
covid_raw <- read_csv('data/raw/covid_raw.csv')
pop_raw <- read_csv('data/raw/pop_raw.csv')

# drop unnecessary columns, and clean data
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

# cleaning non-entities
pop_clean <- pop_raw %>% 
  filter(!grepl("[[:digit:]]", pop_raw$iso2c))


drop_id <- c("EU","HK","OE")
pop_clean <- pop_clean %>% 
  filter(!grepl(paste(drop_id, collapse="|"), pop_clean$iso2c))


# 2nd drop values with certain starting char
# Get the first letter from iso2c
fl_iso2c <- substr(pop_clean$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW")

# Save observations which are the opposite (use of !)
pop_clean <- pop_clean %>% 
  filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & !grepl( paste( retain_id , collapse="|"), pop_clean$iso2c ) ) ) 

# Clear non-needed variables
rm( drop_id, fl_iso2c , retain_id )

# Check for missing observations
pop_clean %>% filter( !complete.cases( pop_clean ) )
# Drop if life-expectancy, gdp or total population missing -> if not complete case except iso2c
pop_clean <- pop_clean %>% filter( complete.cases( pop_clean ) | is.na( pop_clean$iso2c ) )

# rename variables
pop_clean <- pop_clean %>% transmute( country = country,
                        population = SP.POP.TOTL / 1000000)


# merge data and drop unnecessary columns
df <- covid_clean %>% 
  full_join(pop_clean, by = 'country')




