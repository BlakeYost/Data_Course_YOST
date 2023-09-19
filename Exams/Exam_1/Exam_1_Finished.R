library(tidyverse)
library(dplyr)
"I.
Read the cleaned_covid_data.csv file into an R data frame. (20 pts)"

df <- read_csv("Exams/Exam_1/cleaned_covid_data.csv")

"II.
Subset the data set to just show states that begin with A 
and save this as an object called A_states. (20 pts)"

A_states <- filter(df,str_detect(Province_State, "^A"))
print(A_states)

"III.
Create a plot _of that subset_ showing Deaths over time, with a separate facet for each state. (20 pts)"
  
ggplot(A_states, aes(x=Last_Update, y=Deaths, color=Province_State))+
  geom_point()+
  geom_smooth(method = "loess", color = "black", se=FALSE)+
  facet_wrap(~Province_State, scales = "free")

"IV. (Back to the full dataset)
Find the peak of Case_Fatality_Ratio for each state and save this as a new data frame object 
called state_max_fatality_rate. (20 pts)"

state_max_fatality_rate <- df %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm=TRUE))
print(state_max_fatality_rate)

state_max_fatality_rate_sorted <- state_max_fatality_rate %>%
  arrange(desc(Maximum_Fatality_Ratio))
print(state_max_fatality_rate_sorted)

"V.
Use that new data frame from task IV to create another plot. (20 pts)"

ggplot(state_max_fatality_rate_sorted, aes(x=reorder(Province_State, -Maximum_Fatality_Ratio),y = Maximum_Fatality_Ratio))+
  geom_bar(stat= "identity", fill = "firebrick")+
  labs(x = "Province_State", y = "Maximum_Fatality_Ratio")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


"VI. (BONUS 10 pts)
Using the FULL data set, plot cumulative deaths for the entire US over time"

covid_data <- df
covid_data$Last_Update <- as.Date((covid_data$Last_Update))
cumulative_deaths <- aggregate(Deaths ~ Last_Update, data = covid_data, sum)
ggplot(cumulative_deaths, aes(x = Last_Update, y = Deaths)) +
  geom_line()

