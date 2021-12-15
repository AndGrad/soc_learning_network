#### Prepare environment--------------------------------------------------------

source('scripts/prepare_environment.R')

### Step 1: Tables
### TABLE WITH DESCRIPTIVE STATISTICS AND CORRELATIONS

# make a dataset with only relevant variables from complete dataset 
#(every participant who completed the questionnaires)

descriptives <- data_complete %>%
  select(
    participantNr,
    self_smart,
    self_popularity,
    self_indegree,
    eigenCentralitySelf,
    BetweenesSelf
  ) %>%
  distinct() %>%
  select(-c(participantNr)) %>% 
  rename(
    "Friendship Nominations" = self_indegree,
    "Popularity Nominations" = self_popularity,
    "Smartness Nominations" = self_smart,
    "Eigenvector Centrality" =    eigenCentralitySelf,
    "Betweenness" = BetweenesSelf)

# make table
apa.cor.table(descriptives, filename="Descripives_APA.doc", table.number=1)


#### ANALYSIS ------------------------------------------------------------------

#### Friendship and Social Information Use--------------------------------------

# t-test between mean_S in friend condition and mean_S in
# non friend condition #

# subset dataset to have 2 columns, one for mean_S friend (friend1) and one
# for mean_S non-friends (friend0)

t_test_data <-  data_means %>%
  select(friend, mean_s, participantNr) %>%
  pivot_wider(
    names_from = friend,
    values_from = mean_s,
    names_prefix = "friend",
    values_fill = NA,
    values_fn = mean
  ) %>%
  rowwise() %>%
  mutate(b = mean(c(friend0, friend1), na.rm = TRUE))

# run paired t-test:
# H1 is that the difference in mean S between conditions !=0,
# H0 is that difference in mean S bewteen conditions = 0 

t.test(t_test_data$friend1, t_test_data$friend0, paired = TRUE)

# calculate mean and SD of the 2 groups
mean(t_test_data$friend1, na.rm = TRUE)
sd(t_test_data$friend1, na.rm = TRUE)

mean(t_test_data$friend0, na.rm = TRUE)
sd(t_test_data$friend0, na.rm = TRUE)


#### Difference in IOS between friend condition and non-friend condition--------

# -> run a t-test between S_friend and S_nonfriend

# subset dataset to have 2 columns, one for IOS friend (friend1) and one
# for IOS non-friends (friend0)
t_IOS <-  data_means %>%
  select(friend, IOS, participantNr) %>%
  pivot_wider(
    names_from = friend,
    values_from = IOS,
    names_prefix = "friend",
    values_fill = NA,
    values_fn = mean
  ) %>%
  rowwise()

# run paired t-test:
# H1 is that the difference in mean IOS between conditions !=0,
# H0 is that difference in mean IOS bewteen conditions = 0

t.test(t_IOS$friend1, t_IOS$friend0, paired = TRUE)

# calculate mean and SD of the 2 groups

#friend
mean(t_IOS$friend1, na.rm = TRUE)
sd(t_IOS$friend1, na.rm = TRUE)

# non-friend
mean(t_IOS$friend0, na.rm = TRUE)
sd(t_IOS$friend0, na.rm = TRUE)
