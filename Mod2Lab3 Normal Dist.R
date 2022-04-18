library(tidyverse)
library(openintro)

head(fastfood)
mcdonalds <- fastfood %>%
  filter(restaurant == "Mcdonalds")
dairy_queen <- fastfood %>%
  filter(restaurant == "Dairy Queen")
summary(mcdonalds$cal_fat)
hist(mcdonalds$cal_fat)

summary(dairy_queen$cal_fat)
hist(dairy_queen$cal_fat)

dqmean <- mean(dairy_queen$cal_fat)
dqsd <- sd(dairy_queen$cal_fat)

ggplot(data = dairy_queen, aes(x = cal_fat)) +
  geom_blank() +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = c(mean = dqmean, sd = dqsd), col = "tomato")

ggplot(data = dairy_queen, aes(sample = cal_fat)) +
  geom_line(stat = "qq")

sim_norm <- rnorm(n = nrow(dairy_queen), mean = dqmean, sd = dqsd)
ggplot(data = NULL, aes(sample = sim_norm)) +
  geom_line(stat = "qq")

qqnormsim(sample = cal_fat, data = dairy_queen)

qqnormsim(sample = cal_fat, data = mcdonalds)

#Arby's >30gs protein calculations: 
arbys <- fastfood %>%
  filter(restaurant == "Arbys")

a_mean <- mean(arbys$protein)
a_sd <- sd(arbys$protein)

1 - pnorm(q = 30, mean = a_mean, sd = a_sd)

arbys %>% 
  filter(protein > 30) %>%
  summarise(percent = n() / nrow(arbys))

ff_mean <- mean(fastfood$calories)
ff_sd <- sd(fastfood$calories)

pnorm(q = 300, mean = ff_mean, sd = ff_sd)

fastfood %>% 
  filter(calories < 300) %>%
  summarise(percent = n() / nrow(fastfood))

#Chick Fil-A sodium plot **
cfa <- fastfood %>%
  filter(restaurant == "Chick Fil-A")

qqnorm(cfa$sodium, main = "Chick Fil-A")


qqnorm(dq$total_carb, main = "Dairy Queen Carbs")
qqline(dq$total_carb)
