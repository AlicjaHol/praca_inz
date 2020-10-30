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

#covid <- covid[(covid$location!="International"& covid$location!="World"& covid$location!="Aruba"),]
#covid <- drop_na(covid, location)
covid <- covid[!covid$location %in% c("International", "World", "Kosovo", 
                                      "South Sudan", "Syria", "Taiwan"),]
covid <- covid[covid$population>1000000,]
# co z total cases==NA, czy mozna zastapic wartoscia z poprzedniego dnia?

# dla kazdego kraju, wykasowac dni zanim pojawil sie pierwszy przypadek

# zamiast dat numery kolejnych dni trwania pandemii
#covid <- covid %>% 
#  select(location, date, total_cases, new_cases,
#         total_deaths, new_deaths, total_cases_per_million, new_cases_per_million,
#         new_tests, total_tests, total_tests_per_thousand, tests_per_case, stringency_index,
#         population, population_density, life_expectancy)

covid <- drop_na(covid, total_cases_per_million)



covid <- covid[covid$total_cases_per_million!=0,]


covid$time <- with(covid, ave(rep(1, nrow(covid)), location, FUN = seq_along))

library(lattice)
xyplot(total_cases_per_million~time|location,
       data = covid)

library(nlme) #lmer4
# population density, liczba testow, PKB, extreme poverty, odsetek osob z problemami sercowymi, life expectancy (przecietny czas zycia)
#library(lmer4)
lmeControl(maxIter=1000, msMaxIter = 1000, niterEM = 1000)
mod <- lme(total_cases_per_million~time, 
           random = ~1|location,
           data = covid, method="ML")
#mod <- lmer(total_cases_per_million~time+(1~location), data = covid)
summary(mod)
# oba wspolczynniki efektow stalych sa istotne (intercept i wsp przy time)
# wyjaśniona wariancja przez efekt losowy 2943.93 - porównywalne z Residuals 2538.357
AIC(mod) # miara utraconej informacji - im mniej tym lepiej

kraje <- covid$location %>% unique()

Beta <- vector(length = 152)
for (i in 1:152){
  mod_i <- summary(lm(total_cases_per_million ~ time,
                        subset = (location==kraje[i]), data=covid))
  Beta[i] <- mod_i$coefficients[2]
}
Beta
cbind(kraje, Beta)

aggregate(is.na(covid$total_tests_per_thousand), list(covid$location), FUN=sum)




covid_na <- drop_na(covid, total_tests_per_thousand)


# TRZEBA BEDZIE NAJPIERW WYBRAĆ KRAJE GDZIE JEST MALO BRAKOW
mod1 <- lme(total_cases_per_million~time+total_tests_per_thousand,
           random = ~1|location,
           data = covid_na)
summary(mod1)


covid$age <- 0
covid[covid$life_expectancy<55,]$age <- "below 55"
covid[covid$life_expectancy>=55&covid$life_expectancy<60,]$age <- "55-59"
covid[covid$life_expectancy>=60&covid$life_expectancy<65,]$age <- "60-64"
covid[covid$life_expectancy>=65&covid$life_expectancy<70,]$age <- "65-69"
covid[covid$life_expectancy>=70&covid$life_expectancy<75,]$age <- "70-74"
covid[covid$life_expectancy>=75&covid$life_expectancy<80,]$age <- "75-79"
covid[covid$life_expectancy>=80,]$age<- "80 and above"
covid$age <- as.factor(covid$age)







mod2 <- lme(total_cases_per_million~time+age,
            random=~1|location,
            data=covid)
summary(mod2)


mod3 <- lme(total_cases_per_million~time+population_density,
            random=~1|location,
            data = covid)
summary(mod3)


covid_si <- drop_na(covid, stringency_index)
mod4 <- lme(total_cases_per_million~time+stringency_index,
            random=~1|location,
            data = covid_si)
summary(mod4)

covid_hdi <- drop_na(covid, human_development_index)
mod5 <- lme(total_cases_per_million~time+human_development_index,
            random=~1|location,
            covid_hdi)
summary(mod5)


mod6 <- lme(total_cases_per_million~time+cardiovasc_death_rate,
            random=~1|location,
            data= covid)
summary(mod6)

mod7 <- lme(total_cases_per_million~time+diabetes_prevalence,
            random=~1|location,
            data= covid)
summary(mod7)


covid_ep <- drop_na(covid, extreme_poverty)
mod8 <- lme(total_cases_per_million~time+extreme_poverty,
            random=~1|location,
            data= covid_ep)
summary(mod8)


covid_gdp <- drop_na(covid, gdp_per_capita)
mod9 <- lme(total_cases_per_million~time+gdp_per_capita,
            random=~1|location,
            data= covid_gdp)
summary(mod9)


mod10 <- lme(total_cases_per_million~time+age+stringency_index,
             random=~1|location,
             data=covid_si)
summary(mod10)

mod11 <- lme(total_tests_per_thousand~time+total_cases_per_million,
             random=~1|location,
             data=covid_na)
summary(mod11)


# random intercept and slope

mod12 <- lme(total_cases_per_million~time,
             random= ~time|location,
             data = covid)
summary(mod12)

mod13 <- lme(total_cases_per_million~time+age,
             random= ~time|location,
             data = covid)
summary(mod13)

akaike <- rbind(cbind('mod', AIC(mod)),
      cbind('mod1', AIC(mod1)),
      cbind('mod2', AIC(mod2)),
      cbind('mod3', AIC(mod3)),
      cbind('mod4', AIC(mod4)),
      cbind('mod5', AIC(mod5)),
      cbind('mod6', AIC(mod6)),
      cbind('mod7', AIC(mod7)),
      cbind('mod8', AIC(mod8)),
      cbind('mod9', AIC(mod9)))
akaike <- as.data.frame(akaike)
colnames(akaike) <- c('Model', 'AIC')

akaike$AIC <- as.numeric(as.character(akaike$AIC))
akaike
