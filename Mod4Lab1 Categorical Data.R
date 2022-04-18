library(tidyverse)
library(openintro)
library(infer)

data("yrbss")
count(yrbss, text_while_driving_30d)
#my way
yrbss_prac <- yrbss %>% 
  mutate(texts = text_while_driving_30d!=0
         & text_while_driving_30d!='did not drive'
         & text_while_driving_30d!='')
table(yrbss_prac$texts)
text_nohelmet <- length(which(yrbss_prac$texts==TRUE
                          & yrbss_prac$helmet_12m=='never'))
proportion <-text_nohelmet/13583

#solved way
no_helmet <- yrbss %>%
  filter(helmet_12m == "never")
no_helmet <- no_helmet %>%
  mutate(text_ind = ifelse(text_while_driving_30d == "30", "yes", "no"))

no_helmet %>%
  specify(response = text_ind, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci(level = 0.95)

n <- 1000
p <- seq(from = 0, to = 1, by = 0.01)
me <- 2 * sqrt(p * (1 - p)/n)

dd <- data.frame(p = p, me = me)
ggplot(data = dd, aes(x = p, y = me)) + 
  geom_line() +
  labs(x = "Population Proportion", y = "Margin of Error")


n <- 300
p <- seq(from = 0, to = 1, by = 0.1)
me <- 2 * sqrt(p * (1 - p)/n)

dd <- data.frame(p = p, me = me)
ggplot(data = dd, aes(x = p, y = me)) + 
  geom_line() +
  labs(x = "Population Proportion", y = "Margin of Error")


sleep10 <- yrbss %>%
  filter(school_night_hours_sleep == "10+")
sleep10 <- sleep10 %>%
  mutate(strength = ifelse(strength_training_7d == "7", "yes", "no"))

sleep10 %>%
  specify(response = strength, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci(level = 0.95)
