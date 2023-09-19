"Example Data"
data <- data.frame(player_height <- c(72,80,78,73,75,82,84,70,75,76),
           player_weight <- c(215,280,230,210,215,290,315,220,245,230),
           season_rank <- c(1,1,2,2,3,3,4,5,1,2))

ggplot(data, aes(x=player_height, y=player_weight, color=season_rank))+
  geom_point()+
  geom_smooth()
