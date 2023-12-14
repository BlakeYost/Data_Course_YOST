library(tidyverse)
library(GGally)
library(easystats)
library(stringr)

# GOAL: Predicting when an NBA player will peak


# Tidying all the Data, filtered from 1997 to 2023, minutes played greater than 1500, and games played greater than 30 (for the per game dataset).
# I filtered it this way because before 1997 stats were not as descriptive/incomplete
# and our 2024 stats are only from the first 20 or so games, this will be the data we 
# will use to do our modeling and predictions. Minutes played greater than 1500 because
# there are 82 games and 48 minutes a game, this leads to players averaging around 18-24 minutes 
# a game to give a buffer for injuries and such to get active players. Games played greater than 30 because
# we will get data that skews our predictions, you know that the greater the sample size, the more accuracy. 
advanced <- read_csv("Advanced.csv") %>% 
  filter(season >= 1997 & season <= 2023, mp >=1500)

replacement_rules <- c("C-PF" = "C", "PF-C" = "PF", "PF-SF" = "PF", "PG-SF" = "PG",
                       "PG-SG" = "PG", "SF-PF" = "SF", "SF-SG" = "SF", "SG-PF" = "SG",
                       "SG-PG" = "SG", "SG-SF" = "SG", "C" = "C", "PF" = "PF", "PG" = "PG", "SF" = "SF", "SG" = "SG")

advanced <- mutate(advanced, 
                   new_pos = ifelse(pos == "C-PF", "C",
                                    ifelse(pos == "PF-C", "PF",
                                           ifelse(pos == "PF-SF", "PF",
                                                  ifelse(pos == "PG-SF", "PG",
                                                         ifelse(pos == "PG-SG", "PG",
                                                                ifelse(pos == "SF-PF", "SF",
                                                                       ifelse(pos == "SF-SG", "SF",
                                                                              ifelse(pos == "SG-PF", "SG",
                                                                                     ifelse(pos == "SG-PG", "SG",
                                                                                            ifelse(pos == "SG-SF", "SG", pos)))))))))))
# Percentage of attempts from three point range
advanced %>% 
  ggplot(aes(x=per, y = x3p_ar))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~new_pos)+
  labs(x="PER", y = "Percentage of Attempts from Three Point Range")+
  theme_minimal()

advanced %>% 
  ggplot(aes(x=ts_percent, y = orb_percent))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~new_pos)+
  labs(x="True Shooting Percentage", y = "Offensive Rebound Percent")+
  theme_minimal()

advanced %>% 
  ggplot(aes(x=season, y = per))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~new_pos)+
  labs(x="Season", y = "PER")+
  theme_minimal()

#Can see that age does not affect per
m1 <- glm(data=advanced,
          formula = per ~ age)
summary(m1)
# age, tm, 
# experience, -g, mp(makes sense, more time to get stats), ts_percent,
#x3p_ar(is negative, but centers positive), f_tr, 
m2 <- glm(data=advanced,
          formula = per ~ experience + g + mp + ts_percent + x3p_ar + f_tr)
summary(m2)

m3 <- glm(data=advanced,
          formula = per ~ experience * g * mp * ts_percent * x3p_ar * f_tr)
summary(m3)

m4 <- glm(data=advanced,
          formula = per ~ experience + g + mp + ts_percent + x3p_ar + f_tr + orb_percent + drb_percent + trb_percent + ast_percent + stl_percent + blk_percent+ tov_percent + usg_percent + ows + dws+ ws+ ws_48 + obpm + dbpm + bpm + vorp)
summary(m4)

m5 <- glm(data=advanced,
          formula = per ~ (experience * ts_percent) + g + x3p_ar + f_tr + orb_percent + drb_percent + trb_percent + ast_percent + stl_percent + blk_percent+ tov_percent + usg_percent + ows + ws_48 + obpm + dbpm + bpm + vorp)
summary(m5)
compare_performance(m1,m2,m3,m4,m5) %>% plot
