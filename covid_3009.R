library(tidyverse)
# dane pobierane bezposrednio z github
covid <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"),
                  sep = ",", header = T)
head(covid)

covid %>% 
  distinct(location)
# 210 krajow + world + international 
# pytanie czy brac pod uwage jakies male kraiki, wysepki itd

# co z total cases==NA, czy mozna zastapic wartoscia z poprzedniego dnia?

# dla kazdego kraju, wykasowac dni zanim pojawil sie pierwszy przypadek

# zamiast dat numery kolejnych dni trwania pandemii
covid <- covid %>% 
  select(location, date, total_cases, new_cases,
         total_deaths, new_deaths, total_cases_per_million, new_cases_per_million,
         new_tests, total_tests, total_tests_per_thousand, tests_per_case, stringency_index,
         population, population_density, life_expectancy)

covid <- covid %>% 
  fill(total_cases_per_million)



covid <- covid[covid$total_cases_per_million!=0,]


covid$time <- with(covid, ave(rep(1, nrow(covid)), location, FUN = seq_along))

library(lattice)
xyplot(total_cases_per_million~time|location,
       data = covid)

library(nlme)
mod <- lme(total_cases_per_million~time, 
           random = ~1|location,
           data = covid)
summary(mod)
# oba wspolczynniki efektow stalych sa istotne (intercept i wsp przy time)
# wyjaśniona wariancja przez efekt losowy 2943.93 - porównywalne z Residuals 2538.357
AIC(mod) # miara utraconej informacji - im mniej tym lepiej


polska <- covid[covid$location=="Poland",]

# dla krajow gdzie istnieja takie dane
#mod1 <- lme(total_cases_per_million~time+total_tests_per_thousand,
#           random = ~1|location,
#           data = covid)
