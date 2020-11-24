library(tidyverse)
rm(list = ls())

# read in data 
df <- read_csv('data/clean/covid_pop_clean.csv')


# check histograms and extreme values
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

df %>% 
  filter(active > 200000)

df %>% 
  filter(confirmed > 2000000)

df %>% 
  filter(death > 50000)

df %>% 
  filter(population > 1000000)

df %>% 
  filter(recovered > 2000000)
