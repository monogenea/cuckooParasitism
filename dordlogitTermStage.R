# Sat Mar 30 09:53:20 2019 ------------------------------
# Logistic regression model from greta w/ varying intercepts
library(tensorflow)
use_condaenv("greta")
library(greta)
library(tidyverse)
library(bayesplot)
library(readxl)
source("dordlogitFun.R")

# Download data set from Riehl et al. 2019
dataURL <- "https://datadryad.org/bitstream/handle/10255/dryad.204922/Riehl%20and%20Strong_Social%20Parasitism%20Data_2007-2017_DRYAD.xlsx"
download.file(dataURL, destfile = "data.xlsx")

(allTabs <- excel_sheets("data.xlsx")) # list tabs

# Read female reproductive output and discard records w/ NAs
fro <- read_xlsx("data.xlsx", sheet = allTabs[2])
# pairwDist <- read_xlsx("data.xlsx", sheet = allTabs[3])
fro <- fro[complete.cases(fro),]

# Create the termination stage var the authors used
fro$TermStage <- colSums(t(select(fro,starts_with("Eggs")) != 0))

# Use cross-classified varying intercepts for year, female ID and group ID
female_id <- as.integer(factor(fro$Female_ID_coded))
year <- as.integer(factor(fro$Year))
group_id <- as.integer(factor(fro$Group_ID_coded))

# Define model vars
Age <- as_data(fro$Min_age)
Eggs_laid <- as_data(fro$Eggs_laid)
Mean_eggsize <- as_data(fro$Mean_eggsize)
Group_size <- as_data(fro$Group_size)
Parasite <- as_data(fro$Parasite)

# Define model effects
sigmaML <- cauchy(0, 1, truncation = c(0, Inf), dim = 3)
a_group <- normal(0, sigmaML[3], dim = max(group_id))
a_fem <- normal(0, sigmaML[1], dim = max(female_id))
a_year <- normal(0, sigmaML[2], dim = max(year))

bA <- normal(0, 3)
bEL <- normal(0, 3)
bES <- normal(0, 3)
bGS <- normal(0, 3)
bP <- normal(0, 3)

# make one-hot-encoded response vector
resp <- matrix(0, nrow(fro), 4)
resp[cbind(1:nrow(fro), fro$TermStage)] <- 1

# unordered vector of cutpoints (bit flaky and needs increasing starting values
# to be provided)
# cutpoints <- normal(0, 10, dim = k - 1)

# make an ordered vector of cutpoints (the priors become a bit tricky here though)
cutpoints_raw <- c(normal(0, 10),
                   normal(0, 1, truncation = c(0, Inf), dim = 4 - 2))
cutpoints <- cumsum(cutpoints_raw)
phi <- a_fem[female_id] + a_year[year] + a_group[group_id] +
      Age * bA + Eggs_laid * bEL + Mean_eggsize * bES +
      Group_size * bGS + Parasite * bP

distribution(resp) <- ordered_logit(phi, cutpoints,
                                    n_realisations = nrow(fro))

m <- model(bA, bEL, bES, bGS, bP)
draws <- mcmc(m, warmup = 1e3, n_samples = 5e3)
mcmc_intervals(draws)
