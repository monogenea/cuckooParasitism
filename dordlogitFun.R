# Sat Mar 30 08:26:20 2019 ------------------------------
# ordered logit model in greta by Nick Golding
library(greta)

# First pass at an ordered logit distribution.
# phi is a column vector of logit-effects
# a is a k-1, increasing column vector of logit-cutoffs
# the output is a one-hot-encoded matrix, with each row an observation 
ordered_logit <- function(phi, a, n_realisations = NULL) {
      
      as.greta_array <- .internals$greta_arrays$as.greta_array
      
      # coerce to greta arrays
      a <- as.greta_array(a)
      phi <- as.greta_array(phi)
      
      # get number of realisations (check_multivariate_dims doesn't work here
      # because both are vectors)
      if (!is.null(n_realisations)) {
            n <- n_realisations
      } else {
            n <- nrow(phi)
      }
      
      # replicate phi if it's a scalar
      if (nrow(phi) == 1 & n != 1) {
            phi <- rep(phi, n)
      }
      km1 <- length(a)
      phi <- do.call(cbind, replicate(km1, phi, simplify = FALSE))
      eta <- sweep(phi, 2, a, "-")
      
      # convert to logits
      p <- ilogit(eta)
      
      # get cumulative probabilities
      probs <- cbind(rep(1, n), p) - cbind(p, rep(0, n))
      
      # return categorical distribution
      categorical(probs)
      
}