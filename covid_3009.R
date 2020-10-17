library(tidyverse)
# dane pobierane bezposrednio z github
covid <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"),
                  sep = ",", header = T)
head(covid)

kraje <- covid %>% 
  distinct(location) %>% 
  as.data.frame()
kraje_id <- c(1,2,3,4,5,8,9,11,12,13,15,16,18,19,20,21,23,24,26,27,28,31,32,34,
              35,36,39,40,41,42,43,45,46,47,48,49,51,52,53,54,56,57,58,59,
              63,64,68,69,71,72,73,74,75,77,81,89,90,91,92,93,94,95,97,98,100,
              103,104,109,110,113,114,115,116,117,120,126,127,128,129, 130,
              132,133,137,139,140,141,142,144,145,146,147,148,150,151,153,154,156,
              157,158,159,165,166,167,170,172,173,175,176,177,178,179,180,181,183,
              184,185,186,189,190,194,197,198,199,200,201,203,204,205,206,207,209,210,211)
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

library(nlme) #lmer4
# population density, liczba testow, PKB, extreme poverty, odsetek osob z problemami sercowymi, life expectancy (przecietny czas zycia)

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
