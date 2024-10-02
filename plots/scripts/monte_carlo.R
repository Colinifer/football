library(tidyverse)

library(MASS)

# Load the data

data <- read_csv(“marketing_data.csv”)

# Set the number of simulations

n_simulations <- 5

# Generate random samples from the prior distribution

samples <- mvrnorm(n = n_simulations, mu = c(0, 0, 0), Sigma = diag(3))

# Use the samples as input for the model and run the simulations

results <- vector("numeric", length = n_simulations)

for (i in 1:n_simulations) {
  
  x1 <- samples[i, 1]
  
  x2 <- samples[i, 2]
  
  x3 <- samples[i, 3]
  
  y <- 2 * x1 + 3 * x2 + 4 * x3 + rnorm(1, 0, 1)
  
  results[i] <- y
  
}

# Plot the histogram of the results

ggplot(data.frame(results), aes(x = results)) +
  geom_histogram(bins = 50)
