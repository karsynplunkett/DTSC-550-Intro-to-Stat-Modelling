library(tidyverse)
library(openintro)
library(infer)

us_adults <- tibble(
  climate_change_affects = c(rep("Yes", 62000), rep("No", 38000))
)
ggplot(us_adults, aes(x = climate_change_affects)) +
  geom_bar() +
  labs(
    x = "", y = "",
    title = "Do you think climate change is affecting your local community?"
  ) +
  coord_flip() 
#summary stats
us_adults %>%
  count(climate_change_affects) %>%
  mutate(p = n /sum(n))
#rabdom sample size
n <- 60
samp <- us_adults %>%
  sample_n(size = n)
#95% Confidence Intervals
samp %>%
  specify(response = climate_change_affects, success = "Yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci(level = 0.95)
