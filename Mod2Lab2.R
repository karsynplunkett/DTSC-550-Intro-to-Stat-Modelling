#probability lab
library(tidyverse)
library(openintro)
glimpse(kobe_basket)
kobe_streak <- calc_streak(kobe_basket$shot)
ggplot(data = kobe_streak, aes(x = length)) +
  geom_bar()

coin_outcomes <- c("heads", "tails")
sample(coin_outcomes, size = 1, replace = TRUE)

sim_fair_coin <- sample(coin_outcomes, size = 100, replace = TRUE)

sim_fair_coin
table(sim_fair_coin)

set.seed(112)
sim_unfair_coin <- sample(coin_outcomes, size = 100, replace = TRUE, 
                          prob = c(0.2, 0.8))

sim_unfair_coin
table(sim_unfair_coin)

table(unlist(sim_unfair_coin))

shot_outcomes <- c("H", "M")
sim_basket <- sample(shot_outcomes, size = 1, replace = TRUE)

sim_basket <- sample(shot_outcomes, size = 133, replace = TRUE, 
                          prob = c(0.45, 0.55))

sim_streak <- calc_streak(sim_basket)
table(sim_streak)
ggplot(data = sim_streak, aes(x = length)) +
  geom_bar()
