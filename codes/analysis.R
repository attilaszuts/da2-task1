library(tidyverse); library(estimatr); library(scales); library(lspline); library(texreg); library(car)
rm(list = ls())

# EDA ---------------------------------------------------------------------


# read in data 
df <- read_csv('data/clean/covid_pop_clean.csv')


# check histograms and extreme values
df %>%
  select(confirmed, death, population) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free", nrow = 1) +
  geom_histogram() +
  labs(x = 'Variable values', y = 'Absolute Frequency', title = 'Histogram of numeric variables in dataset') + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 

df %>% 
  filter(active > 200000)

df %>% 
  filter(confirmed > 2000000)

df %>% 
  filter(death > 50000)

df %>% 
  filter(population > 1000000000)

df %>% 
  filter(recovered > 2000000)

# summary
summary(df)

# there are countries with death = 0 -> this would cause problems for log - * regressions

df0 <- df 

# countries without deaths
df0 %>% 
  filter(confirmed == 0 | death == 0) %>% 
  arrange(desc(confirmed))
  

df <- df %>% 
  filter(death != 0 & confirmed != 0)

# scatterplots

# loess 
df %>% 
  ggplot(aes(confirmed, death)) + 
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(method = "loess", color = 'green') +
  labs(x = 'Number of confirmed COVID-19 cases', 
       y = 'Number of COVID-19 deaths',
       title = 'COVID-19 cases and deaths') +
  theme_bw()

df %>% 
  ggplot(aes(confirmed, death)) + 
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(method = "loess", color = 'green') +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths',
       title = 'COVID-19 cases and deaths') +
  scale_x_continuous( trans = log_trans(), breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)) + 
  theme_bw() 

df %>% 
  ggplot(aes(confirmed, death)) + 
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(method = "loess", color = 'green') +
  labs(x = 'Number of confirmed COVID-19 cases', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths') +
  scale_y_continuous( trans = log_trans(), breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)) + 
  theme_bw() 

df %>% 
  ggplot(aes(confirmed, death)) + 
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(method = "loess", color = 'green') +
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


# analysis ----------------------------------------------------------------


# models:
#   reg1: ln_deaths = alpha + beta * ln_cases
#   reg2: ln_deaths = alpha + beta_1 * ln_cases + beta_2 * ln_cases_sq
#   reg3: ln_deaths = alpha + beta_1 * ln_cases * 1(cases < ???) + beta_2 * ln_cases * 1(cases >= ???)
#   reg4: ln_deaths = alpha + beta * ln_cases , weights : population

# reg1 - simple linear regression
reg1 <- lm_robust(ln_death ~ ln_cases, data = df)
summary(reg1)

df %>% 
  ggplot(aes(ln_cases, ln_death)) + 
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(method = "lm", color = 'green') +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths',
       subtitle = 'Simple linear regression') +
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
       title = 'COVID-19 cases and deaths',
       subtitle = 'Quadratic linear regression') +
  theme_bw() 

# reg3 - PLS
cutoff <- quantile(df$confirmed, .90)
ln_cutoff <- log(cutoff)
reg3 <- lm_robust(ln_death ~ lspline(ln_cases, ln_cutoff), data = df)
summary(reg3)

df %>% 
  ggplot(aes(ln_cases, ln_death)) +
  geom_point(color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(formula = y ~ lspline(x, ln_cutoff), method = "lm", color = 'green') +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths',
       subtitle = 'PLS') +
  geom_vline(xintercept = ln_cutoff, linetype = 'dotted') + 
  theme_bw() 

# reg4 - weighted OLS
reg4 <- lm_robust(ln_death ~ ln_cases, data = df, weights = population)
summary(reg4)

df %>% 
  ggplot(aes(ln_cases, ln_death)) + 
  geom_point(aes(size = population), color = 'purple', shape = 16, show.legend = F, alpha = 0.6) +
  geom_smooth(aes(weight = population), method = "lm", formula = y ~ x, color = 'green') +
  scale_size(range = c(1, 15)) +
  labs(x = 'Number of confirmed COVID-19 cases, log transformed', 
       y = 'Number of COVID-19 deaths, log transformed',
       title = 'COVID-19 cases and deaths',
       subtitle = 'Weighted OLS') +
  theme_bw() 



# create model comparison table -----------------------------------------------------------

data_out <- 'out/'
htmlreg(list(reg1 , reg2 , reg3 , reg4),
        type = 'html',
        custom.model.names = c("Simple linear","Quadratic linear","PLS", "Weighted OLS"),
        custom.coef.names = c('Intercept', 'ln(cases)', 'ln(cases)^2', 'ln(cases<339062.1)', 'ln(cases>=339062.1)'),
        caption = "Modelling case fatality and confirmed COVID-19 cases",
        file = paste0( data_out ,'model_comparison.html'), 
        include.ci = FALSE)

## based on these results, I chose simple linear regression, because even though it has a smaller R^2 than 
## the weighted OLS model, it's SE is half, and the R^2 difference is not that big.

# hypothesis testing ------------------------------------------------------

linearHypothesis( reg1 , "ln_cases = 0")

# residual analysis -------------------------------------------------------

# Get the predicted y values from the model
df$reg1_y_pred <- reg1$fitted.values
# Calculate the errors of the model
df$reg1_res <- df$ln_death - df$reg1_y_pred 
# calculate the predicted number of deaths (inverse log transformation)
df$reg1_y_pred_exp <- exp(df$reg1_y_pred)

# Find countries with largest negative errors
df %>% top_n( -5 , reg1_res ) %>% 
  select( country , confirmed, death , reg1_y_pred_exp, reg1_y_pred , ln_death , reg1_res ) %>% 
  arrange(reg1_res)

# Find countries with largest positive errors
df %>% top_n( 5 , reg1_res ) %>% 
  select( country , confirmed, death , reg1_y_pred_exp, reg1_y_pred , ln_death , reg1_res ) %>% 
  arrange(desc(reg1_res))
