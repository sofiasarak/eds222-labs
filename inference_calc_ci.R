
#              Constructing a Confidence Interval             ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


#            Hypothesis testing, p_hats and pnorm()           ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Hypothesis test --> starting from the null distribution (H0)
# H0 assumes NO DIFFERENCE between p_hat1 and p_hat2
# If there's no difference, toss them all in the same bag

# Let's simulate what a null distribution looks like
p <- 0.5 # regardless of treatment, the probability of success is always 50%
n1 <-  60
n2 <- 40

# Simulate samples from that null distribution
n_samples <- 1e4
x1 <- rbinom(n = n_samples, # not used for resampling
             size = n1,
             prob = p)

x2 <- rbinom(n = n_samples,
             size = n2,
             prob = p)
# generating numbers from the exact same underlying probability

# Calculate the statistic from each sample
p1_hat <- x1/n1
p2_hat <- x2/n2
diff_prop <- p2_hat - p1_hat

# Standard error for the null hypothesis
# SE(p_hat)
p_hat <- (x1 + x2) / (n1 + n2)
hist(p_hat)

se_null <- sqrt(p_hat * (1 - p_hat) * (1 / n1 + 1/n2))

# pnorm() is the area under the curve
# why is mean 0?
#.    because we are talking about the null hypothesis (no difference at all)
# why is sd se_null?
#.    we want the standard deviation of the sample statistic 
# why is the first argument diff_prop?
#.    that's the sample statistic vertical line on the plot

# we're asking: by random chance, what percent of the time would we get diff_prop in the null distribution?

pval <- pnorm(diff_prop, mean = 0, sd = se_null, lower.tail = FALSE)
# "what is the probability of getting diff_prop, given this mean and this standard error?"

mean(pval <= 0.05) # how often are our observations 

# the larger the sample, the narrower our null distribution is (the odds of getting an extreme value are really low)


#          Differences between distribution functions         ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# What's the difference between ...

pnorm() # area under the curve that's to the left or right of a value
# What's the probability of getting a random number LESS than -0.6?
pnorm(-.06, mean = 0, sd = 1)
# Associate p-values with pnorm() applied to the null distribution

dnorm() # what's the density right at that value; tells you the shape of the PDF (the actual line itself)
x <- seq(-4, 4, length.out = 100)
density_x <- dnorm(x, mean = 0, sd = 1)
tibble(x, density_x) %>% 
  ggplot(aes(x, density_x)) +
  geom_line()

rnorm() # generates random numbers following a normal distribution 

random_numbers <- rnorm(100, mean = 0, sd =1) # 100 = # of obsvs
hist(random_numbers)

qnorm() # tell the function what you want the area under the curve to be, and qnorm() tells you what values that corresponds to
# What x value corresponds to 2.5% area under the curve?
qnorm(0.025, mean = 0, sd = 1)
qnorm(0.975, mean = 0, sd = 1)
# Associate confidence intervals (CIs) with qnorm() applied to the sampling distribution (not under H0)
