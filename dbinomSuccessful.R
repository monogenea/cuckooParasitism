# Sun Mar 24 08:24:59 2019 ------------------------------
# Logistic regression model from greta w/ varying intercepts
library(tensorflow)
use_condaenv("greta")
library(greta)
#library(tidyverse)
#library(rethinking)
library(bayesplot)
library(readxl)

# Download data set from Riehl et al. 2019
dataURL <- "https://datadryad.org/bitstream/handle/10255/dryad.204922/Riehl%20and%20Strong_Social%20Parasitism%20Data_2007-2017_DRYAD.xlsx"
download.file(dataURL, destfile = "data.xlsx")

(allTabs <- excel_sheets("data.xlsx")) # list tabs

# Read female reproductive output & Pairwise distances
femRoutput <- read_xlsx("data.xlsx", sheet = allTabs[2])
femRoutput <- femRoutput[complete.cases(femRoutput),]
# pairwDist <- read_xlsx("data.xlsx", sheet = allTabs[3])

# Use cross-classified varying intercepts for year, female ID and group ID
female_id <- as.integer(factor(femRoutput$Female_ID_coded))
year <- as.integer(factor(femRoutput$Year))
group_id <- as.integer(factor(femRoutput$Group_ID_coded))

# Define model vars
Age <- as_data(femRoutput$Min_age)
Eggs_laid <- as_data(femRoutput$Eggs_laid)
Mean_eggsize <- as_data(femRoutput$Mean_eggsize)
Group_size <- as_data(femRoutput$Group_size)
Parasite <- as_data(femRoutput$Parasite)

# Define model effects
a <- normal(0, 5)
a_fem <- normal(0, 1, dim = max(female_id))
a_year <- normal(0, 1, dim = max(year))
a_group <- normal(0, 1, dim = max(group_id))
bA <- normal(0, 5)
bEL <- normal(0, 5)
bES <- normal(0, 5)
bGS <- normal(0, 5)
bP <- normal(0, 5)

# Model setup
mu <- a + a_fem[female_id] + a_year[year] + a_group[group_id] +
      Age * bA + Eggs_laid * bEL + Mean_eggsize * bES +
      Group_size * bGS + Parasite * bP

p <- ilogit(mu)
distribution(femRoutput$Successful) <- bernoulli(p)
cuckooModel <- model(a, bA, bEL, bES, bGS, bP)

# Plot
plot(cuckooModel)

# HMC sampling
draws <- mcmc(cuckooModel, n_samples = 4000,
              warmup = 1000, chains = 4, n_cores = 10)
# Trace plots
mcmc_trace(draws)
mcmc_intervals(draws)
# Large eggs, young bird, small group, no fledged birds
scenario1 <- ilogit(a + mean(femRoutput$Min_age) * bA + mean(femRoutput$Eggs_fledged) * bEL +
                          mean(femRoutput$Mean_eggsize) * bES + mean(femRoutput$Group_size) * bGS)
probs1 <- calculate(scenario1, draws)

# Small eggs, old bird, large group, many fledged birds
# scenario2 <- ilogit(a + 12 * bA + 4 * bEF + 26 * bES + 7 * bGS)
# probs2 <- calculate(scenario2, draws)
# boxplot(unlist(probs1), unlist(probs2),
#         names = c("Scenario1", "Scenario2"),
#         ylab = "P(Parasitism)")

