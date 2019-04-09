# Tue Mar 12 20:50:17 2019 ------------------------------
# ZIPoisson model from rethinking; cross-classified multilevel models
library(rethinking)
library(tidyverse)
library(magrittr)
library(readxl)

# Download data set from Riehl et al. 2019
dataURL <- "https://datadryad.org/bitstream/handle/10255/dryad.204922/Riehl%20and%20Strong_Social%20Parasitism%20Data_2007-2017_DRYAD.xlsx"
download.file(dataURL, destfile = "data.xlsx")

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
                group_id = as.integer(factor(Group_ID_coded)),
                Min_age_Z = scale(Min_age),
                Group_size_Z = scale(Group_size),
                Mean_eggsize_Z = scale(Mean_eggsize))

eggsFMod <- map2stan(alist(
      Eggs_fledged ~ dzipois(p, lambda),
      logit(p) <- ap,
      log(lambda) <- a + a_fem[female_id] + a_year[year_id] + a_group[group_id] +
            Parasite*bP + Min_age_Z*bA + Group_size_Z*bGS + Mean_eggsize_Z*bES +
            Parasite*Min_age_Z*bPA,
      Group_size_Z ~ dnorm(0, 3), # remove this, the hist is bimodal!
      Mean_eggsize_Z ~ dnorm(0, 3),
      a_fem[female_id] ~ dnorm(0, sigma1),
      a_year[year_id] ~ dnorm(0, sigma2),
      a_group[group_id] ~ dnorm(0, sigma3),
      c(sigma1, sigma2, sigma3) ~ dcauchy(0, 1),
      c(ap, a) ~ dnorm(0, 3),
      c(bP, bA, bGS, bES, bPA) ~ dnorm(0, 2)),
      data = fro,
      iter = 5e3, warmup = 1e3, chains = 4, cores = 4)

# Try Eggs_laid ~ dzipois
froReduced <- slice(fro, which(!is.na(Eggs_laid))) %>% 
      as.data.frame()

# I need to redo the variable scaling, otherwise the sampling throws an error
froReduced %<>% mutate(female_id = as.integer(factor(Female_ID_coded)),
                year_id = as.integer(factor(Year)),
                group_id = as.integer(factor(Group_ID_coded)),
                Min_age_Z = scale(Min_age),
                Group_size_Z = scale(Group_size),
                Mean_eggsize_Z = scale(Mean_eggsize))

eggsLMod <- map2stan(alist(
      Eggs_laid ~ dzipois(p, lambda),
      logit(p) <- ap,
      log(lambda) <- a + a_fem[female_id] + a_year[year_id] + a_group[group_id] +
            Parasite*bP + Min_age_Z*bA + Group_size_Z*bGS + Mean_eggsize_Z*bES +
            Parasite*Min_age_Z*bPA,
      Group_size_Z ~ dnorm(0, 3),
      Mean_eggsize_Z ~ dnorm(0, 3),
      a_fem[female_id] ~ dnorm(0, sigma1),
      a_year[year_id] ~ dnorm(0, sigma2),
      a_group[group_id] ~ dnorm(0, sigma3),
      c(sigma1, sigma2, sigma3) ~ dcauchy(0, 1),
      c(ap, a) ~ dnorm(0, 3),
      c(bP, bA, bGS, bES, bPA) ~ dnorm(0, 2)),
      data = froReduced,
      iter = 5e3, warmup = 1e3, chains = 4, cores = 4)

# Try predicting Eggs_laid later, produce eggsLMod
precis(eggsFMod, prob = .95) # use depth = 2 for varying intercepts
precis(eggsLMod, prob = .95)

# INFERENCE IN THE Eggs_fledged ~ dzipois MODEL
# Sample posterior
post <- extract.samples(eggsFMod)
# PI of P(no clutch at all)
dens(logistic(post$ap), show.HPDI = T, xlab = "ZIP Bernoulli(p)")

# Run simulations
lambdaNoP <- exp(post$a + 0*post$bP + 0*post$bA +
                       0*post$bGS + 0*post$bES + 0*0*post$bPA)
simFledgeNoPar <- rpois(n = length(lambdaNoP), lambda = lambdaNoP)

lambdaP <- exp(post$a + 1*post$bP + 0*post$bA +
                     0*post$bGS + 0*post$bES + 1*0*post$bPA)
simFledgePar <- rpois(n = length(lambdaP), lambda = lambdaP)
table(simFledgeNoPar)
table(simFledgePar)

# Sim with varying group size
rangeA <- seq(-3, 3, length.out = 100)
# No parasite
predictions <- sapply(rangeA, function(x){
      exp(post$a + 0*post$bP + x*post$bA + 0*post$bGS +
                0*post$bES + 0*x*post$bPA)
})
hdpiPois <- apply(predictions, 2, HPDI, prob = .95)
meanPois <- colMeans(predictions)
plot(rangeA, meanPois, type = "l", ylim = c(0, 3), yaxp = c(0, 3, 3),
     xlab = "Min Age (standardized)", ylab = expression(lambda))
shade(hdpiPois, rangeA)
# Parasite
predictionsP <- sapply(rangeA, function(x){
      exp(post$a + 1*post$bP + x*post$bA + 0*post$bGS +
                0*post$bES + x*post$bPA)
})
hdpiPoisP <- apply(predictionsP, 2, HPDI, prob = .95)
meanPoisP <- colMeans(predictionsP)
lines(rangeA, meanPoisP, lty = 2, col = "red")
shade(hdpiPoisP, rangeA, col = rgb(1,0,0,.25))

# INFERENCE IN THE Eggs_laid ~ dzipois MODEL
# Sample posterior
post <- extract.samples(eggsLMod)
# PI of P(no clutch at all)
dens(logistic(post$ap), show.HPDI = T, xlab = "ZIP Bernoulli(p)")

# Run simulations
lambdaNoP <- exp(post$a + 0*post$bP + 0*post$bA +
                       0*post$bGS + 0*post$bES + 0*0*post$bPA)
simFledgeNoPar <- rpois(n = length(lambdaNoP), lambda = lambdaNoP)

lambdaP <- exp(post$a + 1*post$bP + 0*post$bA +
                     0*post$bGS + 0*post$bES + 1*0*post$bPA)
simFledgePar <- rpois(n = length(lambdaP), lambda = lambdaP)
table(simFledgeNoPar)
table(simFledgePar)

# Sim with varying group size
# No parasite
predictions <- sapply(rangeA, function(x){
      exp(post$a + 0*post$bP + x*post$bA + 0*post$bGS +
                0*post$bES + 0*x*post$bPA)
})
hdpiPois <- apply(predictions, 2, HPDI, prob = .95)
meanPois <- colMeans(predictions)
plot(rangeA, meanPois, type = "l", ylim = c(0, 7), yaxp = c(0, 7, 7),
     xlab = "Min Age (standardized)", ylab = expression(lambda))
shade(hdpiPois, rangeA)
# Parasite
predictionsP <- sapply(rangeA, function(x){
      exp(post$a + 1*post$bP + x*post$bA + 0*post$bGS +
                0*post$bES + x*post$bPA)
})
hdpiPoisP <- apply(predictionsP, 2, HPDI, prob = .95)
meanPoisP <- colMeans(predictionsP)
lines(rangeA, meanPoisP, lty = 2, col = "red")
shade(hdpiPoisP, rangeA, col = rgb(1,0,0,.25))

# Bonus: sample counts from predictionsP, take 95% HDPI
hdpiPoisP <- apply(predictionsP, 2, HPDI, prob = .95)
meanPoisP <- colMeans(predictionsP)
plot(rangeA, meanPoisP, type = "l", ylim = c(0, 15),
     yaxp = c(0, 15, 5), xlab = "Min Age (standardized)",
     ylab = expression(paste(lambda, " / no. eggs laid")))
shade(hdpiPoisP, rangeA)
poisample <- sapply(1:100, function(k){
      rpois(nrow(predictionsP), predictionsP[,k])
})
hdpiSample <- apply(poisample, 2, HPDI, prob = .95)
shade(hdpiSample, rangeA)
