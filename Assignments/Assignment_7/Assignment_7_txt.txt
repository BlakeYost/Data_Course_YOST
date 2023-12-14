library(tidyverse)
library(janitor)
library(easystats)
library(stats)
library(skimr)


#tidy dataset
df <- read_csv("Assignments/Assignment_7/Utah_Religions_by_County.csv") %>% 
  janitor::clean_names()

df1 <- df %>%
  pivot_longer(cols = -c(county, pop_2010, religious, non_religious),
    names_to = "religion",
    values_to = "count")
#Exploring the dataset:

summary(df1)

#looking at some different plots and making models
df1 %>% 
  ggplot(aes(x=pop_2010,y=count))+
  geom_point(data = filter(df1, religion == "assemblies_of_god"))
m1 <- glm(data=df1,
          formula = religion == "assemblies_of_god" ~ pop_2010)
m1$aic

df1 %>% 
  ggplot(aes(x=pop_2010,y=count))+
  geom_point(data = filter(df1, religion == "catholic"))
m2 <- glm(data=df1,
          formula = religion == "catholic" ~ pop_2010)
m2$aic

df1 %>% 
  ggplot(aes(x=pop_2010,y=count))+
  geom_point(data = filter(df1, religion == "muslim"))
m3 <- glm(data=df1,
          formula = religion == "muslim" ~ pop_2010)
m3$aic

df1 %>% 
  ggplot(aes(x=pop_2010,y=count))+
  geom_point(data = filter(df1, religion == "lds"))
m4 <- glm(data=df1,
          formula = religion == "lds" ~ pop_2010)
m4$aic

df1 %>% 
  ggplot(aes(x=pop_2010,y=count, color=county))+
  geom_point()+
  facet_wrap(~religion)
m5 <- glm(data=df1,
          formula = religious ~ pop_2010)
m5$aic

m6 <- glm(count ~ pop_2010, data = df1, family = poisson(link = log))


compare_performance(m1,m2,m3,m4,m5,m6) %>% plot


# Answering "Does population of a county correlate with the proportion of any specific religious group in that county?"
religious_groups <- unique(df1$religion)
correlation_results <- data.frame(religion = character(0), correlation = numeric(0))

for (religion1 in religious_groups) {
  specific_religion1 <- df1 %>%
    filter(religion == religion1) %>%
    mutate(proportion = count / pop_2010)
  correlation <- cor(specific_religion1$pop_2010, specific_religion1$proportion)
  result_row <- data.frame(religion = religion1, correlation = correlation)
  correlation_results <- rbind(correlation_results, result_row)
}
print(correlation_results)
summary(correlation_results)

# I would say not necessarily because there is a wide spread from -0.26 all the way to 0.18,
# and normally when an interval crosses 0, usually it is not significant.

# Answering “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
df2 <- 
  df1 %>% 
  mutate(non_religious_proportion = non_religious / pop_2010)

religious_groups1 <- unique(df1$religion)

proportion_results <- data.frame(county = character(0), religion = character(0), proportion = numeric(0))

for (religion3 in religious_groups1) {
  specific_religion3 <- df2 %>%
    filter(religion == religion3) %>% 
    mutate(proportion = count / pop_2010)
  proportion <- specific_religion3$proportion / specific_religion3$non_religious_proportion
  result_df3 <- data.frame(county = specific_religion3$county, religion = religion3, proportion = proportion)
  proportion_results <- rbind(proportion_results, result_df3)
}

print(proportion_results)
summary(proportion_results)

# Then I plot my results for proportion of non_religious and religous by each religion to get a good visual/ idea. 
ggplot(proportion_results, aes(x = religion, y = proportion)) +
  geom_boxplot()
# I am still not sure if it is significant so then I do a t.test on each combination of county and religion to find a significant pvalue.
t_test_results <- data.frame(county = character(0), religion = character(0), p_value = numeric(0))

for (religion4 in religious_groups1) {
  religion_data <- proportion_results %>%
    filter(religion == religion4)
  
  counties <- unique(religion_data$county)
  for (county in counties) {
    county_data <- religion_data %>%
      filter(county == county)
    t_test_result <- t.test(county_data$proportion, proportion_results$proportion)
    
    result_row <- data.frame(county = county, religion = religion4, p_value = t_test_result$p.value)
    t_test_results <- rbind(t_test_results, result_row)
  }
}
print(t_test_results)
summary(t_test_results)
# As we can see from the summary, every result is significant, which gives the conclusion that population and religion/non_religion by county correlate. 

