library(tidyverse)
library(gganimate)
library(janitor)

#read data into R
dat <- read_csv("Data/BioLog_Plate_Data.csv")

#clean data set, make new column that specifies if its soil or water
hi <- 
dat %>% 
  clean_names() %>%
  pivot_longer(starts_with("hr_"), values_to = "absorbance") %>% 
  mutate(hour = case_when(name == "hr_24" ~ 24,
                          name == "hr_48" ~ 48,
                          name == "hr_144" ~ 144)) %>% 
  select(-name) %>% 
  mutate(type = ifelse(grepl("Soil", sample_id), "soil", "water"))

#make faceted plot for dilution 0.1
tada <- 
hi %>% 
  filter(dilution == .1) %>% 
  ggplot(aes(x=hour, y=absorbance, color=type))+ 
  geom_smooth(method = "loess", se = FALSE)+
  facet_wrap(~substrate)+
  theme_minimal()
tada

#make animated plot for just Itaconic Acid faceted by dilution
p2 <- 
  hi %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(dilution, sample_id, hour) %>% 
  summarize(mean_abs = mean(absorbance)) %>% 
  ggplot(aes(x=hour, y=mean_abs, color = sample_id))+
  geom_path() +
  facet_wrap(~dilution)+
  theme_minimal()+
  transition_reveal(hour)
p2
