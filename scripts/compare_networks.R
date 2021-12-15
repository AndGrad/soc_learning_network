rm(list = ls())

#packages
library(tidyverse)
library(igraph)
library(correlation)

# import data

# import ego-network data and select only variables related to SELF
full_data_ego <- read.csv('clean_data/full_dataset_egonets.csv') %>% 
  select(-c(IDother, distance))
full_data_ego <- unique(full_data_ego)

# import friendship network data select only variables related to SELF
full_data <- read.csv('clean_data/full_dataset.csv')  %>% 
select(c(IDself, friendsIn, stdFriendsIn, EigenvectorCentrality, Betweenness))
full_data <- unique(full_data)

# merge datasets
new <- left_join(full_data_ego, full_data, by = 'IDself') %>% 
  select(IDself, EigenvectorCentrality.x, EigenvectorCentrality.y, stdFriendsIn.x , stdFriendsIn.y, Betweenness.x ,Betweenness.y )

#### check correlations ####

#  1. eigenvector centrality

ggplot() +
  geom_point(data = new, aes(x=EigenvectorCentrality.x, y = EigenvectorCentrality.y), alpha = 0.7) +
  theme_classic()

cor.test(new$EigenvectorCentrality.x, new$EigenvectorCentrality.y, method = "pearson")


# 2. in degree
ggplot() +
  geom_point(data = new, aes(x=stdFriendsIn.x, y = stdFriendsIn.y), alpha = 0.7)+
  theme_classic()

cor.test(new$stdFriendsIn.x , new$stdFriendsIn.y, method = "pearson")

# 3. betweenness centrality

ggplot() +
  geom_point(data = new, aes(x=Betweenness.x, y = Betweenness.y), alpha = 0.7)+
  theme_classic()

cor.test(new$Betweenness.x , new$Betweenness.y, method = "pearson")

