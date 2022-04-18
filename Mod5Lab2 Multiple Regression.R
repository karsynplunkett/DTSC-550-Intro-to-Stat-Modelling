library(tidyverse)
library(openintro)
install.packages("GGally")
library(GGally)
library(broom)

glimpse(evals)

?evals

hist(evals$score)

plot(evals$age,evals$score,xlab="age",ylab="score",pch=3)


plot(evals$language,evals$score,xlab="language",ylab="score",pch=3)

plot(evals$pic_outfit,evals$score,xlab="pic_outfit",ylab="score",pch=3)
plot(evals$pic_color,evals$score,xlab="pic_color",ylab="score",pch=3)
plot(evals$rank,evals$score,xlab="rank",ylab="score",pch=3)
plot(evals$gender,evals$score,xlab="gender",ylab="score",pch=3)
plot(evals$bty_avg,evals$score,xlab="bty_avg",ylab="score",pch=3)


m_bty <- lm(evals$score~evals$bty_avg)
plot(evals$bty_avg~jitter(evals$score))
abline(m_bty)

summary(m_bty)

plot(m_bty$residuals ~ evals$bty_avg, ylab="Residuals", xlab="Average Beauty", 
     main="Rating and Beauty") 
abline(h = 0, lty = 3) 

hist(m_bty$residuals)

qqnorm(m_bty$residuals)
qqline(m_bty$residuals)

plot(evals$bty_avg ~ evals$bty_f1lower)

cor(evals$bty_avg, evals$bty_f1lower)

plot(evals[,13:19])

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)
library(ggplot2)
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
qqnorm(m_bty_gen$residuals)
qqline(m_bty_gen$residuals)

plot(m_bty_gen$residuals ~ evals$bty_avg)
abline(h = 0, lty = 3)
ggplot(evals,aes(y=m_bty_gen$residuals,x=evals$gender))+geom_boxplot()+geom_point()

summary(m_bty_gen)

summary(m_bty)

multiLines(m_bty_gen)

m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)

m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + 
               cls_students + cls_level + cls_profs + cls_credits + bty_avg + pic_outfit + 
               pic_color, data = evals)
summary(m_full)
#backward selection
m_full.drop <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
                  + cls_students + cls_level + cls_credits + bty_avg 
                  + pic_outfit + pic_color, data = evals)
summary(m_full.drop)

m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students  + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)

m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)

m_full <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval 
             + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)

m_full <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval 
             + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)

m_full <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval 
             + cls_credits + bty_avg 
             + pic_color, data = evals)
summary(m_full)

m_bty_final <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval 
                  + cls_credits + bty_avg 
                  + pic_color, data = evals)
qqnorm(m_bty_final$residuals)
qqline(m_bty_final$residuals)


plot(m_bty_final$residuals ~ evals$bty_avg)
abline(h = 0, lty = 3)
