library(faraway)
library(lme4)
library(kableExtra)
library(tidyverse)
library(texreg)
data(pulp)

lmod <- aov(bright~operator, pulp)
summary(lmod)
mean(pulp$bright)
coef(lmod)
#sigma^2=0.1062=MSE
#MSA=0.4467
#wariancja efektu losowego (MSA-MSE)/n, n-liczba poziomow zm grup
(0.4467-0.1062)/5

lmemod <- lmer(bright~(1|operator), pulp)
summary(lmemod)
texreg(lmemod)
pulp %>% 
  kable("latex")

ranef(lmemod) %>% 
  kable("latex")

random <- c(-0.1219403, -0.2591231, 0.1676679,0.2133955)
mean(random)


# recznie
overallMean <- mean(pulp$bright)
n <- 5
a <- 4
aMean <- mean(pulp[pulp$operator=='a',]$bright)
bMean <- mean(pulp[pulp$operator=='b',]$bright)
cMean <- mean(pulp[pulp$operator=='c',]$bright)
dMean <- mean(pulp[pulp$operator=='d',]$bright)
SST <- sum((pulp$bright-overallMean)^2)
SSA <- ((aMean-overallMean)^2+(bMean-overallMean)^2+(cMean-overallMean)^2+(dMean-overallMean)^2)*n
SSE <- sum((pulp[pulp$operator=='a',]$bright-aMean)^2)+sum((pulp[pulp$operator=='b',]$bright-bMean)^2)+sum((pulp[pulp$operator=='c',]$bright-cMean)^2)+sum((pulp[pulp$operator=='d',]$bright-dMean)^2)
MSE <- SSE/(a*(n-1)) # sigma epsilon
MSA <- SSA/(a-1)
sigma <- (MSA-MSE)/n
sigma # sigma alfa
ro <- sigma/(sigma+MSE)
ro #ICC


