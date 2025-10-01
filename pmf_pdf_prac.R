# Learning how to plot PMFs and PDFs
# Week 1, Lecture 2

library(tidyverse)

## Normal Distribution

#1. Define the possible outcomes
foo <- tibble(
  x = seq(0, 100, length.out = 100)
)

#2. Choose our parameters
mu <- 45
sigma <- 16

#3. Calculate density
foo <- mutate(foo, density = dnorm(x, mean = mu, sd = sigma))

#4. Plot it
ggplot(foo, aes(x, density)) +
  geom_line()


## Attempting a Beta distribution

#1. Define the possible outcomes
pos <- tibble(
  x = seq(0, 1, length.out = 50) #sequence has to be between 0 and 1 in a Beta distribution!
)

#2. Choose our parameters.
# k = successes
k = 12
# n = number of trials
n = 20
# calculating alpha and beta
alpha = k + 1
beta = n - k + 1

#3. Calculate density
den <- mutate(pos, density = dbeta(x, shape1 = alpha, shape2 = beta))

#4. Plot it
ggplot(den, aes(x, density)) +
  geom_line()