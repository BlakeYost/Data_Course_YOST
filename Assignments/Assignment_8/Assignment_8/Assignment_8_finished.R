library(tidyverse)
library(easystats)
library(janitor)
library(modelr)

#loads the “/Data/mushroom_growth.csv” data set
df <- read_csv("mushroom_growth.csv") %>% 
  janitor::clean_names()

#creates several plots exploring relationships between the response and predictors

df %>% 
  ggplot(aes(x=light, y=growth_rate, color = species ))+
  geom_point()+
  facet_wrap(~species)

df %>% 
  ggplot(aes(x=nitrogen, y=growth_rate, color = species ))+
  geom_point()+
  facet_wrap(~species)

df %>% 
  ggplot(aes(x=humidity, y=growth_rate, color = species ))+
  geom_point()+
  facet_wrap(~species)

df %>% 
  ggplot(aes(x=temperature, y=growth_rate, color = species ))+
  geom_point()+
  facet_wrap(~species)

#defines at least 4 models that explain the dependent variable “GrowthRate”
m1 <- glm(data=df,
          formula = growth_rate ~ nitrogen * factor(species))
summary(m1)

m2 <- glm(data=df,
          formula = growth_rate ~ nitrogen*light* factor(species))
summary(m2)

m3 <- glm(data=df,
          formula = growth_rate ~ nitrogen*light*humidity* factor(species))
summary(m3)

m4 <- glm(data=df,
          formula = growth_rate ~ nitrogen*light*humidity*temperature* factor(species))
summary(m4)

#calculates the mean sq. error of each model
pred1 <- add_predictions(df, m1)

mse1 <- mean((1/nrow(pred1))*((pred1$growth_rate - pred1$pred)^2))
mse1

pred2 <- add_predictions(df, m2)
mse2 <- mean((1/nrow(pred2))*((pred2$growth_rate - pred2$pred)^2))
mse2

pred3 <- add_predictions(df, m3)
mse3 <- mean((1/nrow(pred3))*((pred3$growth_rate - pred3$pred)^2))
mse3

pred4 <- add_predictions(df, m4)
mse4 <- mean((1/nrow(pred4))*((pred4$growth_rate - pred4$pred)^2))
mse4

#selects the best model you tried
mods <- list(m1=m1,m2=m2,m3=m3,m4=m4)
map(mods,performance) %>% reduce(full_join)

compare_performance(m1,m2,m3,m4) %>% plot
# Model 4 seems to be the best model because it has the smallest AIC and also
# covers more of the data (R2).


#adds predictions based on new hypothetical values for the independent variables used in your model
# I used my first model, to predict nitrogen and growth_rate
newdf <- data.frame(nitrogen = c(0,3,7,9,22,34,42,50,55,5,10,15,20,25,30,35,40,45), species = c("P.ostreotus","P.cornucopiae"))
pred <-  predict(m1, newdata = newdf)
hyp_preds <- data.frame(nitrogen = newdf$nitrogen,
                        pred = pred)
df$predictiontype <- "Real"
hyp_preds$predictiontype <- "Hypothetical"
fullpreds <- full_join(df,hyp_preds)

combined_plot <- ggplot() +
  geom_point(data = fullpreds, aes(x = nitrogen, y = growth_rate, color = predictiontype)) +
  geom_line(data = fullpreds, aes(x = nitrogen, y = pred, color = predictiontype))
combined_plot

#plots these predictions alongside the real data
# Switching my variable to light
df %>% 
  gather_predictions(m1,m2,m3,m4) %>% 
  ggplot(aes(x=light,y=growth_rate)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal()

report(m4)


