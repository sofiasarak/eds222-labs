library(tidyverse)
library(palmerpenguins)

penguins %>%
  count(species, island) %>%
  group_by(island) %>%
  mutate(n = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = island,
              values_from = n,
              values_fill = 0)

penguins %>%
  count(species, island) %>%
  group_by(species) %>%
  mutate(n = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = species,
              values_from = n,
              values_fill = 0)

penguins %>%
  count(species, island) %>%
  group_by(species, island) %>%
  mutate(n = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = island,
              values_from = n,
              values_fill = 0)