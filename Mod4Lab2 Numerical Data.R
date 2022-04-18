library(tidyverse)
library(openintro)
library(infer)
install.packages("skimr")
library(skimr)
library(statsr)
install.packages("statsr")

data(yrbss)

?yrbss

glimpse(yrbss)
yrbss %>% 
  skim()

skim(yrbss)

yrbss <- yrbss %>% 
  mutate(physical_3plus = if_else(physically_active_7d > 2, "yes", "no"))

ggplot(yrbss, aes(x=physical_3plus, y=weight)) + 
  geom_violin(trim=FALSE)

yrbss %>%
  group_by(physical_3plus) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE))

obs_diff <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))

null_dist <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("yes", "no"))

ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()
visualize(null_dist) + 
  shade_p_value(obs_stat = obs_diff, direction = "two_sided")

inference(data = yrbss, y = weight, x = physical_3plus,
          statistic = "mean",
          type = "ci", 
          null = NULL, 
          alternative = "twosided", 
          method = "theoretical")

t.test(data = yrbss, weight ~ physical_3plus)

yrbss %>% 
  group_by(physical_3plus) %>% 
  summarise(sd_weight = sd(weight, na.rm = TRUE))

yrbss %>% 
  group_by(physical_3plus) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE))

yrbss %>% 
  group_by(physical_3plus) %>% 
  summarise(freq = table(weight)) %>%
  summarise(n = sum(freq))
x_not3plus <- 66.67389
n_not3plus <- 4022
s_not3plus <- 17.63805
x_3plus <- 68.44847
n_3plus <- 8342
s_3plus <- 16.47832
# At 95% confidence level where n is so large it is ~= z* of
# normal distribution
t = 1.96

# Not physically active 3 plus days per week
upper_ci_not <- x_not3plus + t*(s_not3plus/sqrt(n_not3plus))
lower_ci_not <- x_not3plus - t*(s_not3plus/sqrt(n_not3plus))

# physically active 3 plus days per week
upper_ci <- x_3plus + t*(s_3plus/sqrt(n_3plus))
lower_ci <- x_3plus - t*(s_3plus/sqrt(n_3plus))

upper_ci_not

table_height <- as.data.frame(table(yrbss$height))
freq_height <- sum(table_height$Freq)

x_height <- mean(yrbss$height, na.rm = TRUE)
sd_height <- sd(yrbss$height, na.rm = TRUE)
n_height <- yrbss %>% 
  summarise(freq = table(height)) %>%
  summarise(n = sum(freq, na.rm = TRUE))

upper_ci_height <- x_height + t*(sd_height/sqrt(n_height))
lower_ci_height <- x_height - t*(sd_height/sqrt(n_height))
upper_ci_height


obs_diff_hgt <- yrbss %>%
  specify(height ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))

set.seed(10152020)
null_dist_hgt <- yrbss %>%
  specify(height ~ physical_3plus) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("yes", "no"))

