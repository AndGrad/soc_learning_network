#### Prepare environment for analyses------------------------------------------

#### IMPORT PACKAGES ####

library(lme4)
library(correlation)
library(stargazer)
library(dplyr)
library(tidyr)
library(apaTables)
library(plotrix)

### IMPORT DATASETS

# full dataset 
data_complete <- read.csv('clean_data/BEAST_data_175.csv', header=TRUE) 

# dataset of participants who meet experimental requirement for the endogenous
# experimental treatment of seeing  with 2 different peers
 
data <- read.csv('clean_data/BEAST_data_156.csv', header=TRUE) 

# dataset of BEAST with two different peers, only mean values
data_means <- read.csv('clean_data/BEAST_data_meanS_n156.csv', header=TRUE)  

# same as abovem but with only one row per participant per treatment
# (contains identifying characteristics of each participant)
data_means_ind <- data_means %>% 
  select(ID, age,  participantNr, gender,self_popularity, std_self_popularity, classR, self_indegree, std_self_indegree, self_smart, std_self_smart, eigenCentralitySelf) %>% 
  distinct()

# data from social network questionnaires
data_questionnaires <- 
  read.csv('clean_data/full_dataset_school_BEAST.csv') 


