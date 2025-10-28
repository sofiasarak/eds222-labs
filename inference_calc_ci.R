library(tidyverse)
theme_set(theme_classic(14))
set.seed(123)

# 1. Define your population
p1 <- 0.42
p2 <- 0.55
n1 <- 60
n2 <- 40

# 2. Simulate many samples
n_samples <- 1e4
x1 <- rbinom(n = n_samples,
             size = n1,
             prob = p1) # calculating the # of successes for each of garden staple plots

x2 <- rbinom(n = n_samples,
             size = n2,
             prob = p2) 

# 3. Calculate statistic for each sample
p1_hat <- x1 / n1
p2_hat <- x2 / n2
diff_prop <- p2_hat - p1_hat

# Here's a distribution of our samples
tibble(p1_hat,
       p2_hat) %>% 
  pivot_longer(c(p1_hat, p2_hat), 
             names_to = "treatment", 
             values_to = "prop_success") %>% 
  ggplot(aes(prop_success)) +
  geom_histogram() +
  facet_wrap(~ treatment)
# in the real world, we would be getting 10000 of these

# 4. Calculate the SE for the CI (following formula)
se_diff_prop <- sqrt(p1_hat * (1 - p1_hat) / n1 +
                       p2_hat * (1-p2_hat) /n2)

# 5. Calculate the CI for each sample
ci_95_lower <-  qnorm(0.025, 
                      mean = diff_prop, # mean = actual sample mean
                      sd = se_diff_prop) 
ci_95_upper <-  qnorm(0.975, 
                      mean = diff_prop, 
                      sd = se_diff_prop) 
# we now have 10,000 confidence intervals

# 6. Calculate coverage
is_covered <- ci_95_lower <= p2 - p1 & p2 - p1 <= ci_95_upper # does pop parameter fall within
coverage = mean(is_covered) # tells us the fraction that actually is covered (bc is_covered is TRUE/FALSEs)

coverage # got 0.9474, so close to 95%

# Visualize the CIs
tibble(sample = 1:n_samples,
       diff_prop,
       ci_95_lower,
       ci_95_upper,
       is_covered) %>% 
  sample_n(100) %>% 
  ggplot(aes(diff_prop, sample, 
             xmin = ci_95_lower,
             xmax = ci_95_upper,
             color = is_covered)) +
  geom_pointrange()

# where blue: CI covered sample
# where red: CI didn't cover

