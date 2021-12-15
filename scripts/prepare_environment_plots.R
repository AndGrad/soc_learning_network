#### SET UP ####
rm(list = ls())

# import packages
library(tidyverse)
library(gridExtra)
library(scales)
library(lme4)
library(lmerTest)
library(patchwork)
library(cowplot)
library(ggsignif)
library(gghalves)

#### READ DATA ####

# each row is one round
data <-
  read.csv('clean_data/BEAST_data.csv',
           header = T,
           stringsAsFactors = TRUE)

data_complete <- read.csv('clean_data/BEAST_data_175.csv', header=TRUE) # full dataset 

# each row is the average of the treatments (friend vs. non friend)
data_mean <-
  read.csv(
    'clean_data/BEAST_data_meanS_n156.csv',
    header = T,
    stringsAsFactors = TRUE
  ) %>% 
  mutate(distance = ordered(
    distance,
    levels = c("1", "2", "3", "4", "5", "0"),
    labels = c("1", "2", "3", "4", "5", "unconnected")))

# load data for plot
data_mean_t_test <-
  read.csv(
    'clean_data/BEAST_data_meanS.csv',
    header = T,
    stringsAsFactors = TRUE
  )

# filter dataset for extreme values of s
data_filter1 <- data %>%
  filter(norme1 > .5 & norme1 < 1.5)

data_filter2 <- data %>%
  filter(norme2 > .5 & norme2 < 1.5)

data_filter3 <- data %>%
  filter(improvementnorm > -.2 & improvementnorm < .2)

data_filter4 <- data %>%
  filter(s >= -0 & s <= 1)

data_filter5 <- data %>%
  filter(s >= -0.5 & s <= 1.5)
