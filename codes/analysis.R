library(tidyverse); library(estimatr); library(scales)
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

# summary
summary(df)

# there are countries with death = 0 -> this would cause problems for log - * regressions

df0 <- df 

df <- df %>% 
  filter(death != 0)

# scatterplots

# loess 
df %>% 
  ggplot(aes(confirmed, death)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = 'Number of confirmed COVID-19 cases', 
       y = 'Number of COVID-19 deaths',
       title = 'COVID-19 cases and deaths') +
  theme_bw()

df %>% 
  ggplot(aes(confirmed, death)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths',
       title = 'COVID-19 cases and deaths') +
  scale_x_continuous( trans = log_trans(), breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)) + 
  theme_bw() 

df %>% 
  ggplot(aes(confirmed, death)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = 'Number of confirmed COVID-19 cases', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths') +
  scale_y_continuous( trans = log_trans(), breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)) + 
  theme_bw() 

df %>% 
  ggplot(aes(confirmed, death)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths') +
  scale_x_continuous( trans = log_trans(), breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)) + 
  scale_y_continuous( trans = log_trans(), breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)) + 
  theme_bw() 

# based on these plots, I need to transform both variables logarithmically
df <- df %>% 
  mutate(ln_cases = log(confirmed),
         ln_death = log(death),
         ln_cases_sq = ln_cases ^ 2)

# analysis
# models:
#   reg1: ln_deaths = alpha + beta * ln_cases
#   reg2: ln_deaths = alpha + beta_1 * ln_cases + beta_2 * ln_cases_sq
#   reg3: ln_deaths = alpha + beta * ln_cases , weights : population

# reg1 - simple linear regression
reg1 <- lm_robust(ln_death ~ ln_cases, data = df)
summary(reg1)

df %>% 
  ggplot(aes(ln_cases, ln_death)) + 
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(method = "lm", color = 'green') +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths') +
  theme_bw() 


# reg2 - quadratic linear regression
reg2 <- lm_robust(ln_death ~ ln_cases + ln_cases_sq, data = df)
summary(reg2)

df %>% 
  ggplot(aes(ln_cases, ln_death)) + 
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) + 
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color = 'green') +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths') +
  theme_bw() 

# reg3 - weighted OLS
reg3 <- lm_robust(ln_death ~ ln_cases, data = df, weights = population)
summary(reg3)

df %>% 
  ggplot(aes(ln_cases, ln_death)) + 
  geom_point(aes(size = population), color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(aes(weight = population), method = "lm", formula = y ~ x, color = 'green') +
  scale_size(range = c(1, 15)) +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths') +
  theme_bw() 

reg4 <- lm_robust(death ~ ln_cases + ln_cases_sq, data = df)
summary(reg4)

df %>% 
  ggplot(aes(ln_cases, death)) + 
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = 'green') +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths') +
  theme_bw() 
