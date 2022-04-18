library(tidyverse)
library(openintro)
library(statsr)
library(broom)

dim(hfi)


hfi_2016 <- hfi %>%
  filter(year == "2016")

plot(hfi_2016$pf_expression_control,hfi_2016$pf_score,xlab="pf_expression_control",ylab="pf_score",pch=3)

hfi_2016 %>%
  summarise(cor(pf_expression_control, pf_score))

plot_ss(x = pf_expression_control, y = pf_score, data = hfi_2016)

plot_ss(x = pf_expression_control, y = pf_score, data = hfi_2016, showSquares = TRUE)

m1 <- lm(pf_score ~ pf_expression_control, data = hfi_2016)

tidy(m1)

glance(m1)
# new model to predict hf_score
plot(hfi_2016$pf_expression_control,hfi_2016$hf_score,xlab="pf_expression_control",ylab="hf_score",pch=3)
hfi_2016 %>%
  summarise(cor(pf_expression_control, hf_score))

m2 <- lm(hf_score ~ pf_expression_control, data = hfi_2016)

tidy(m2)
glance(m2)


ggplot(data = hfi_2016, aes(x = pf_expression_control, y = pf_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

m1_aug <- augment(m1)

ggplot(data = m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals")


ggplot(data = m1_aug, aes(x = .resid)) +
  geom_histogram(binwidth = 0.25) +
  xlab("Residuals")
