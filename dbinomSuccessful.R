# Sun Mar 24 08:24:59 2019 ------------------------------
# Logistic regression model from greta w/ varying intercepts
library(tensorflow)
use_condaenv("greta")
library(greta)
library(tidyverse)
library(bayesplot)
library(readxl)

# Download data set from Riehl et al. 2019
dataURL <- "https://datadryad.org/bitstream/handle/10255/dryad.204922/Riehl%20and%20Strong_Social%20Parasitism%20Data_2007-2017_DRYAD.xlsx"
download.file(dataURL, destfile = "data.xlsx")

(allTabs <- excel_sheets("data.xlsx")) # list tabs

# Read female reproductive output and discard records w/ NAs
fro <- read_xlsx("data.xlsx", sheet = allTabs[2])
# pairwDist <- read_xlsx("data.xlsx", sheet = allTabs[3])
fro <- fro[complete.cases(fro),]

# Create the termination stage var the authors used
# fro$TermStage <- colSums(t(select(fro,starts_with("Eggs")) != 0))

# Use cross-classified varying intercepts for year, female ID and group ID
female_id <- as.integer(factor(fro$Female_ID_coded))
year <- as.integer(factor(fro$Year))
group_id <- as.integer(factor(fro$Group_ID_coded))

# Define and standardize model vars
Age <- as_data(scale(fro$Min_age))
Eggs_laid <- as_data(scale(fro$Eggs_laid))
Mean_eggsize <- as_data(scale(fro$Mean_eggsize))
Group_size <- as_data(scale(fro$Group_size))
Parasite <- as_data(fro$Parasite)
# TermStage <- as_data(fro$TermStage)

# Define model effects
sigmaML <- cauchy(0, 1, truncation = c(0, Inf), dim = 3)
a_fem <- normal(0, sigmaML[1], dim = max(female_id))
a_year <- normal(0, sigmaML[2], dim = max(year))
a_group <- normal(0, sigmaML[3], dim = max(group_id))

a <- normal(0, 5)
bA <- normal(0, 3)
bEL <- normal(0, 3)
bES <- normal(0, 3)
bGS <- normal(0, 3)
bP <- normal(0, 3)
bPA <- normal(0, 3)

# Model setup
mu <- a + a_fem[female_id] + a_year[year] + a_group[group_id] +
      Age*bA + Eggs_laid*bEL + Mean_eggsize*bES + Parasite*bP +
      Group_size*bGS + Parasite*Age*bPA

p <- ilogit(mu)
distribution(fro$Successful) <- bernoulli(p)
cuckooModel <- model(a, bA, bEL, bES, bP, bGS, bPA)

# Plot
plot(cuckooModel)

# HMC sampling
draws <- mcmc(cuckooModel, n_samples = 4000,
              warmup = 1000, chains = 4, n_cores = 10)
# Trace plots
mcmc_trace(draws)
# Parameter posterior
mcmc_intervals(draws, prob = .95)

# Simulation with average eggs laid, egg size and group size, w/ and w/o parasitism
seqX <- seq(-3, 3, length.out = 100)
probsNoPar <- sapply(seqX, function(x){
      scenario <- ilogit(a + x*bA)
      probs <- calculate(scenario, draws)
      return(unlist(probs))
})
probsPar <- sapply(seqX, function(x){
      scenario <- ilogit(a + x*bA + bP + x*bPA)
      probs <- calculate(scenario, draws)
      return(unlist(probs))
})

plot(seqX, apply(probsNoPar, 2, mean), type = "l", ylim = 0:1,
     xlab = "Min age (standardized)", ylab = "P(Successful)",
     yaxp = c(0, 1, 2))
rethinking::shade(apply(probsNoPar, 2, rethinking::HPDI, prob = .95),
                  seqX)
lines(seqX, apply(probsPar, 2, mean), lty = 2, col = "red")
rethinking::shade(apply(probsPar, 2, rethinking::HPDI, prob = .95),
                  seqX, col = rgb(1,0,0,.25))

# Write sessioninfo
writeLines(capture.output(sessionInfo()), "sessionInfo")