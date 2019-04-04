# Tue Mar 12 20:50:17 2019 ------------------------------
# ZIPoisson model from rethinking; cross-classified multilevel models
library(rethinking)
library(tidyverse)
library(magrittr)
library(readxl)

# Download data set from Riehl et al. 2019
# dataURL <- "https://datadryad.org/bitstream/handle/10255/dryad.204922/Riehl%20and%20Strong_Social%20Parasitism%20Data_2007-2017_DRYAD.xlsx"
# download.file(dataURL, destfile = "data.xlsx")

(allTabs <- excel_sheets("data.xlsx")) # list tabs

# Read female reproductive output
fro <- read_xlsx("data.xlsx", sheet = allTabs[2])
# pairwDist <- read_xlsx("data.xlsx", sheet = allTabs[3])

# Assess missingness
sum(complete.cases(fro)) / nrow(fro)
# only 0.57 complete records; which vars have at least one NA?
names(which(apply(fro, 2, function(x){any(is.na(x))})))

# Filter out missingness in fledged eggs, the model does not cope with it
fro %<>% slice(which(!is.na(Eggs_fledged))) %>% 
      as.data.frame()

fro %<>% mutate(female_id = as.integer(factor(Female_ID_coded)),
                year_id = as.integer(factor(Year)),
                group_id = as.integer(factor(Group_ID_coded)))

eggsFMod <- map2stan(alist(
      Eggs_fledged ~ dzipois(p, lambda),
      logit(p) <- ap,
      log(lambda) <- a + a_fem[female_id] + a_year[year_id] + a_group[group_id] +
            Parasite*bP + Min_age*bA + Group_size*bGS + Mean_eggsize*bES,
      Group_size ~ dnorm(nuGS, 5), # remove this, the hist is bimodal!
      Mean_eggsize ~ dnorm(nuES, 5), # 215 NAs!
      nuGS ~ dnorm(4.5, 1),
      nuES ~ dnorm(30, 2),
      a_fem[female_id] ~ dnorm(0, sigma1),
      a_year[year_id] ~ dnorm(0, sigma2),
      a_group[group_id] ~ dnorm(0, sigma3),
      c(sigma1, sigma2, sigma3) ~ dcauchy(0, 1),
      c(ap, a) ~ dnorm(0, 3),
      c(bP, bA, bGS, bES) ~ dnorm(0, 2)),
      data = fro,
      iter = 5e3, warmup = 1e3, chains = 4, cores = 4)

# Try Eggs_laid ~ dzipois
froReduced <- slice(fro, which(!is.na(Eggs_laid))) %>% 
      as.data.frame()

froReduced %<>% mutate(female_id = as.integer(factor(Female_ID_coded)),
                year_id = as.integer(factor(Year)),
                group_id = as.integer(factor(Group_ID_coded)))

eggsLMod <- map2stan(alist(
      Eggs_laid ~ dzipois(p, lambda),
      logit(p) <- ap,
      log(lambda) <- a + a_fem[female_id] + a_year[year_id] + a_group[group_id] +
            Parasite*bP + Min_age*bA + Group_size*bGS + Mean_eggsize*bES,
      Group_size ~ dnorm(nuGS, 5), # remove this, the hist is bimodal!
      Mean_eggsize ~ dnorm(nuES, 5), # 215 NAs!
      nuGS ~ dnorm(4.5, 1),
      nuES ~ dnorm(30, 2),
      a_fem[female_id] ~ dnorm(0, sigma1),
      a_year[year_id] ~ dnorm(0, sigma2),
      a_group[group_id] ~ dnorm(0, sigma3),
      c(sigma1, sigma2, sigma3) ~ dcauchy(0, 1),
      c(ap, a) ~ dnorm(0, 3),
      c(bP, bA, bGS, bES) ~ dnorm(0, 2)),
      data = froReduced,
      iter = 5e3, warmup = 1e3, chains = 4, cores = 4)

# Try predicting Eggs_laid later, produce eggsLMod
precis(eggsFMod, prob = .95) # use depth = 2 for varying intercepts
precis(eggsLMod, prob = .95)

# Sample posterior
post <- extract.samples(eggsFMod)
# PI of P(no clutch at all)
dens(logistic(post$ap), show.HPDI = T, xlab = "ZIP Bernoulli(p)")

# Run simulations (predictive posterior?)
lambdaP <- exp(post$a + 1*post$bP + 3*post$bA + 4*post$bGS + 30*post$bES)
simFledgePar <- rpois(n = length(postPars), lambda = lambdaP)
lambdaNoP <- exp(post$a + 0*post$bP + 3*post$bA + 4*post$bGS + 30*post$bES)
simFledgeNoPar <- rpois(n = length(postPars), lambda = lambdaNoP)
table(simFledgePar)
table(simFledgeNoPar)

# Sim with varying group size
rangeGS <- seq(1, 10, length.out = 100)
# No parasite
predictions <- sapply(rangeGS, function(x){
      exp(post$a + 0*post$bP + 6*post$bA + x*post$bGS + 30*post$bES)
})
hdpiPois <- apply(predictions, 2, HPDI, prob = .95)
meanPois <- colMeans(predictions)
plot(rangeGS, meanPois, type = "l", ylim = c(0, 2),
     xlab = "Group size", ylab = expression(lambda))
shade(hdpiPois, rangeGS)
# Parasite
predictionsP <- sapply(rangeGS, function(x){
      exp(post$a + 1*post$bP + 6*post$bA + x*post$bGS + 30*post$bES)
})
hdpiPoisP <- apply(predictionsP, 2, HPDI, prob = .95)
meanPoisP <- colMeans(predictionsP)
lines(rangeGS, meanPoisP, lty = 2)
shade(hdpiPoisP, rangeGS)
