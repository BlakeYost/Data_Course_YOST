library(tidyverse)
library(easystats)
library(janitor)
library(modelr)
library(GGally)

#Use the data set “/Data/GradSchool_Admissions.csv”
df <- read_csv("GradSchool_Admissions.csv") %>% 
  janitor::clean_names()

#You will explore and model the predictors of graduate school admission
df %>% 
  ggpairs()

df %>% 
  ggplot(aes(x=gpa, y=gre, color = admit))+
  geom_point()+
  facet_wrap(~rank)

m1 <- glm(data=df,
            formula = admit ~ gpa, family = "binomial")
summary(m1)

m2 <- glm(data=df,
          formula = admit ~ gpa + gre, family = "binomial")
summary(m2)

m3 <- glm(data=df,
          formula = admit ~ gpa+gre+rank, family = "binomial")
summary(m3)

m4 <- glm(data=df,
          formula = admit ~ gpa*gre*rank, family = "binomial")
summary(m4)

compare_performance(m1,m2,m3,m4) %>% plot

df$pred <- predict(m4, type = "response")

df$pred_v2 <- ifelse(df$pred > 0.5, 1, 0)

df %>% 
  ggplot(aes(x = gpa, y = pred_v2)) +
  geom_point(aes(color = factor(admit)),size=3) +
  geom_point(aes(x=gpa, y = admit), color = "black", alpha = 0.5)+
  scale_color_manual(values = c("blue", "red"),labels = c("Not Admitted", "Admitted")) +
  labs(x = "GPA", y = "Prediction of Admission")+
  ggtitle("Actual(Black) vs Predicted Admission Chance")+
  guides(color = guide_legend(title = "Admission")) +
  facet_wrap(~rank)

m4 %>% model_parameters()
