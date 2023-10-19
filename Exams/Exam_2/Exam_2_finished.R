library(tidyverse)
library(janitor)
library(modelr)
library(gridExtra)


# Read in unicef data and get into tidy format
df <- read_csv("Exams/Exam_2/unicef-u5mr.csv") %>% 
  clean_names()

df1 <- 
  df %>% 
  gather(key = "year", value = "u5mr", u5mr_1950:u5mr_2015)
df1$year <- as.numeric(gsub("u5mr_", "", df1$year))


# Plot of each country's U5MR over time
plot_1 <- 
df1 %>% 
  ggplot(aes(x=year, y=u5mr, group = country_name))+
  geom_line()+
  facet_wrap(~continent)+
  labs(x = "Year", y = "U5MR")
plot_1

# Save plot as png
file_path <- "Exams/Exam_2/YOST_Plot_1.png"
ggsave(file_path, plot_1)

# Plot of mean U5MR for all countries within a given continent at each year
df1_mean <- 
  df1 %>% 
  group_by(year, continent) %>% 
  summarize(mean_u5mr = mean(u5mr, na.rm = TRUE))

plot_2 <- 
df1_mean %>% 
  ggplot(aes(x=year, y=mean_u5mr, color = continent))+
  geom_line(size = 1.5)
plot_2

# Save plot as png
file_path_2 <- "Exams/Exam_2/YOST_Plot_2.png"
ggsave(file_path_2, plot_2)

# Model 1 accounts for only year on U5MR
mod1 <- glm(data=df1,
        formula = u5mr ~ year)
mod1

# Model 2 accounts for Year and Continent
mod2 <- glm(data=df1,
            formula = u5mr ~ year + continent)
mod2

# Model 3 accounts for Year, Continent, and their interaction term
mod3 <- glm(data=df1,
            formula = u5mr ~ year * continent)
mod3

# Comparing the three different models
# As you can see by the plot, Model 3 covers the most variance and has the highest AIC score so it is the best.  
compare_performance(mod1,mod2,mod3) 
compare_performance(mod1,mod2,mod3) %>% plot 


# Predictions based on the models:

pred1 <- add_predictions(df1, mod1)
pred1

pred2 <- add_predictions(df1, mod2)
pred2

pred3 <- add_predictions(df1, mod3)
pred3

# Plotting the three different models
pred_plot_1 <- 
df1 %>% 
  ggplot(aes(x=year, y=pred1$pred, color = continent))+
  geom_line(size = 1.5)+
  labs(x = "Year", y = "Predicted U5MR",title = "mod1")+
  theme(legend.position = "none")

pred_plot_2 <- 
df1 %>% 
  ggplot(aes(x=year, y=pred2$pred, color = continent))+
  geom_line(size = 1.5)+
  theme(legend.position = "none")+
  labs(x = "Year", y = NULL ,title = "mod2")

pred_plot_3 <- 
df1 %>% 
  ggplot(aes(x=year, y=pred3$pred, color = continent))+
  geom_line(size = 1.5)+
  labs(x = "Year" , y = NULL, title = "mod3")

combined_plots <- grid.arrange(pred_plot_1, pred_plot_2, pred_plot_3, ncol = 3)

# Predicting what U5MR would be for Ecuador in the year 2020. (Not Finished)
# Only has created a new model based on trained and tested data, does not predict Ecuador in the year 2020.
df2 <- 
  df1 %>% 
  mutate(newcolumn = rbinom(nrow(df1),1, .8))

train <- df2 %>% filter(newcolumn == 1)
test <- df2 %>% filter(newcolumn == 0)

step <- stepAIC(mod3)
step$formula

mod_best <- glm(data = train, formula = step$formula)
compare_performance(mod3, mod_best) %>% plot
compare_performance(mod3, mod_best)
