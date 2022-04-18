library(tidyverse)
library(openintro)

install.packages("infer")
library(infer)

global_monitor <- tibble(
  scientist_work = c(rep("Benefits", 80000), rep("Doesn't benefit", 20000))
)
ggplot(global_monitor, aes(x = scientist_work)) +
  geom_bar() +
  labs(
    x = "", y = "",
    title = "Do you believe that the work scientists do benefit people like you?"
  ) +
  coord_flip() 
global_monitor %>%
  count(scientist_work) %>%
  mutate(p = n /sum(n))
samp1 <- global_monitor %>%
  sample_n(50)
ggplot(samp1, aes(x = scientist_work)) +
  geom_bar() +
  labs(
    x = "", y = "",
    title = "Do you believe that the work scientists do benefit people like you?"
  ) +
  coord_flip() 
samp1 %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n))
samp2 <- global_monitor %>%
  sample_n(50)
samp2 %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n))
samp3 <- global_monitor %>%
  sample_n(100)
samp3 %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n))

sample_props50 <- global_monitor %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")
ggplot(data = sample_props50, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.02) +
  labs(
    x = "p_hat (Doesn't benefit)",
    title = "Sampling distribution of p_hat",
    subtitle = "Sample size = 50, Number of samples = 15000"
  )

sample_props_small <- global_monitor %>%
  rep_sample_n(size = 10, reps = 25, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")
ggplot(data = sample_props_small, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.1) +
  labs(
    x = "p_hat (Doesn't benefit)",
    title = "Sampling distribution of p_hat",
    subtitle = "Sample size = 10, Number of samples = 25"
  )

ggplot(data = sample_props50, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.02)

sample_prac <- global_monitor %>%
  sample_n(100)
sample_prac %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n))
sample_prac <- global_monitor %>%
  rep_sample_n(size = 100, reps = 5000, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")
ggplot(data = sample_prac, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.02) +
  labs(
    x = "p_hat (Doesn't benefit)",
    title = "Sampling distribution of p_hat",
    subtitle = "Sample size = 10, Number of samples = 25"
  )


#extra practice
global_monitor %>%
  count(scientist_work) %>%
  mutate(p = n /sum(n))
samp11 <- global_monitor %>%
  sample_n(15)
samp11 %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n))

sample_props15 <- global_monitor %>%
  rep_sample_n(size = 15, reps = 2000, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Benefits")
ggplot(data = sample_props15, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.06) +
  labs(
    x = "p_hat (Benefits)",
    title = "Sampling distribution of p_hat",
    subtitle = "Sample size = 10, Number of samples = 25"
  )

sample_props150 <- global_monitor %>%
  rep_sample_n(size = 150, reps = 2000, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Benefits")
ggplot(data = sample_props150, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.02) +
  labs(
    x = "p_hat (Benefits)",
    title = "Sampling distribution of p_hat",
    subtitle = "Sample size = 10, Number of samples = 25"
  )
