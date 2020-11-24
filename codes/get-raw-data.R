library(tidyverse); library(WDI)
rm(list=ls())

# download covid data -----------------------------------------------------

covid_raw <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/10-09-2020.csv')

# download population data ------------------------------------------------

pop_raw <- WDI(indicator=c('SP.POP.TOTL'), country="all", start=2019, end=2019)

# save raw data -----------------------------------------------------------

write_csv(covid_raw, 'data/raw/covid_raw.csv')

write_csv(pop_raw, 'data/raw/pop_raw.csv')
