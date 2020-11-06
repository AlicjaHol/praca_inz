library(tidyverse)
# dane pobierane bezposrednio z github
covid <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"),
                  sep = ",", header = T)
head(covid)

kraje <- covid %>% 
  distinct(location) %>% 
  as.data.frame()



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
library(lme4)
library(lmerTest)
library(stargazer)
library(texreg)
library(gghighlight)

covid %>% 
  group_by(location) %>% 
  summarize(m=mean(total_cases_per_million)) %>% 
  arrange(-m)

lmeControl(maxIter=1000, msMaxIter = 1000, niterEM = 1000)
#mod <- lme(total_cases_per_million~time, 
#           random = ~1|location,
#           data = covid, method="ML")
mod_lme <- lmer(total_cases_per_million~time+(1|location), covid)
mod_lme_slope <- lmer(total_cases_per_million~time+(time|location), covid)
summary(mod_lme_slope)
summary(mod_lme)
anova(mod_lme)
stargazer(mod, type="latex")
texreg(mod_lme)
texreg(mod_lme_slope)
#mod <- lmer(total_cases_per_million~time+(1~location), data = covid)
#summary(mod)
# oba wspolczynniki efektow stalych sa istotne (intercept i wsp przy time)
# wyjaśniona wariancja przez efekt losowy 2943.93 - porównywalne z Residuals 2538.357
AIC(mod_lme) # miara utraconej informacji - im mniej tym lepiej
AIC(mod_lme_slope)
kraje <- covid$location %>% unique()

# Beta <- vector(length = 152)
# for (i in 1:152){
#   mod_i <- summary(lm(total_cases_per_million ~ time,
#                         subset = (location==kraje[i]), data=covid))
#   Beta[i] <- mod_i$coefficients[2]
# }
# Beta
# wsp_kraje <- cbind(as.character(kraje), Beta) %>% as.data.frame()
# wsp_kraje$Beta <- as.numeric(wsp_kraje$Beta)

covid %>% 
  ggplot(aes(time, total_cases_per_million, col=location))+
  geom_line()+
  gghighlight(location %in% c('Poland', 'China', 'United States', 'Italy',
                              'Germany', 'Norway', 'Spain', 'United Kingdom',
                              'Qatar', 'Bahrain','Brazil'), label_key=location)
fit <- predict(mod_lme)
cbind(covid, fit) %>% 
  ggplot(aes(time, fit, col=location))+
  geom_line()+
  gghighlight(location %in% c('Poland', 'China', 'United States', 'Italy',
                              'Germany', 'Norway', 'Spain', 'United Kingdom',
                              'Qatar', 'Bahrain','Brazil'), label_key=location)
fit_slope <- predict(mod_lme_slope)
cbind(covid, fit_slope) %>% 
  ggplot(aes(time, fit_slope, col=location))+
  geom_line()+
  gghighlight(location %in% c('Poland', 'China', 'United States', 'Italy',
                              'Germany', 'Norway', 'Spain', 'United Kingdom',
                              'Qatar', 'Bahrain','Brazil'), label_key=location)

#aggregate(is.na(covid$total_tests_per_thousand), list(covid$location), FUN=sum)




covid_na <- drop_na(covid, total_tests_per_thousand)
mod1 <- lmer(total_cases_per_million~total_tests_per_thousand+(1|location),
           data = covid_na)
summary(mod1)
texreg(mod1)

covid$age <- 0
covid[covid$life_expectancy<55,]$age <- "below 55"
covid[covid$life_expectancy>=55&covid$life_expectancy<60,]$age <- "55-59"
covid[covid$life_expectancy>=60&covid$life_expectancy<65,]$age <- "60-64"
covid[covid$life_expectancy>=65&covid$life_expectancy<70,]$age <- "65-69"
covid[covid$life_expectancy>=70&covid$life_expectancy<75,]$age <- "70-74"
covid[covid$life_expectancy>=75&covid$life_expectancy<80,]$age <- "75-79"
covid[covid$life_expectancy>=80,]$age<- "80 and above"
covid$age <- as.factor(covid$age)
covid$age <- relevel(covid$age, 'below 55')





library(RColorBrewer)

mod2 <- lmer(total_cases_per_million~+age+(1|location),
            data=covid)
summary(mod2)
texreg(mod2)
covid %>% 
  ggplot(aes(time, total_cases_per_million, group=location, colour=age))+
  geom_line()+
  scale_colour_brewer('Set1')
  


mod3 <- lmer(total_cases_per_million~population_density+(1|location),
            data = covid)
summary(mod3)
texreg(mod3)

covid_si <- drop_na(covid, stringency_index)
mod4 <- lmer(total_cases_per_million~stringency_index+(1|location),
            data = covid_si)
summary(mod4)
texreg(mod4)


covid_hdi <- drop_na(covid, human_development_index)
mod5 <- lmer(total_cases_per_million~human_development_index+(1|location),
            covid_hdi)
summary(mod5)
texreg(mod5)

mod6 <- lmer(total_cases_per_million~cardiovasc_death_rate+(1|location),
            data= covid)
summary(mod6)
texreg(mod6)

mod7 <- lmer(total_cases_per_million~diabetes_prevalence+(1|location),
            data= covid)
summary(mod7)
texreg(mod7)

covid_ep <- drop_na(covid, extreme_poverty)
mod8 <- lmer(total_cases_per_million~extreme_poverty+(1|location),
            data= covid_ep)
summary(mod8)
texreg(mod8)

covid_gdp <- drop_na(covid, gdp_per_capita)
mod9 <- lmer(total_cases_per_million~gdp_per_capita+(1|location),
            data= covid_gdp)
summary(mod9)
texreg(mod9)


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




# czas trwania epidemii w poszczegolnych krajach
czas_kraj <- covid %>% select(location, time)%>% group_by(location) %>% top_n(1, time)
min(czas_kraj$time)


