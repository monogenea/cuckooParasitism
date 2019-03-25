# Tue Mar 12 20:50:17 2019 ------------------------------
# ZIPoisson model from rethinking; cross-classified varying intercepts
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

# femRoutput <- femRoutput[complete.cases(femRoutput),]

fro %<>% mutate(female_id = as.integer(factor(Female_ID_coded)),
                year_id = as.integer(factor(Year)),
                group_id = as.integer(factor(Group_ID_coded)))

# Filter out missingness in fledged eggs, the model does not cope with it
fro %<>% slice(which(!is.na(Eggs_fledged)))

eggsMod <- map2stan(alist(
      Eggs_fledged ~ dzipois(p, lambda),
      logit(p) <- aP,
      log(lambda) <- a + a_fem[female_id] + a_year[year_id] + a_group[group_id] +
            Group_size*bE + Parasite * bP + Min_age * bA,
      Group_size ~ dnorm(4.5, 2),
      # APPARENTLY IMPUTATION ONLY WORKS WITH DNORM!
      a_fem[female_id] ~ dnorm(0, sigma1),
      a_year[year_id] ~ dnorm(0, sigma2),
      a_group[group_id] ~ dnorm(0, sigma3),
      c(sigma1, sigma2, sigma3) ~ dcauchy(0, 1),
      c(a, aP) ~ dnorm(0, 3),
      c(bE, bP, bA, bE) ~ dnorm(0, 2)),
      data = as.data.frame(fro), iter = 5e3, warmup = 1e3, chains = 4, cores = 4)

precis(eggsMod)

# Sample posterior
post <- extract.samples(eggsMod)

# PI of P(no clutch at all)
dens(logistic(post$aP))
