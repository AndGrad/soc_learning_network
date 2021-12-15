# prepare environment
source('scripts/prepare_environment_plots.R')

#### FIGURE 1: basic behavioral results ####

# panel A: distribution of normalized E1 to show underestimations > overestimations
f
fig1_B <- ggplot() +
  geom_rect(
    aes(xmin = -Inf, xmax = 1),
    fill = 'yellow',
    alpha = 0.2,
    ymin = 0,
    ymax = Inf
  ) +
  geom_vline(xintercept = 1) +
  geom_histogram(
    aes(x = norme1, y = stat(y = ..count.. / nrow(data_filter1))),
    data = data_filter1,
    fill = "#22407A",
    color = "#e9ecef",
    binwidth = 0.1,
    position = 'identity'
  ) +
  labs(title = '',
       x = "E1 / Actual # of animals",
       y = "frequency") +
  theme_classic() +
  theme(text = element_text(size = 17)) +
  annotate(
    "text",
    x = 0.62,
    y = .2,
    label = "underestimate",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.38,
    y = .2,
    label = "overestimate",
    size = 5
  )


fig1_B

#

# figure 1C: Boxplots that show improvement from estimate 1 to estimate 2

# filter data & calculate mean error
f <-  data_filter4 %>%
  select(percent_error1, percent_error2, ID, nAnimals, std_self_smart) %>%
  group_by(ID) %>%
  mutate(mean_error1 = mean(percent_error1)) %>%
  mutate(mean_error2 = mean(percent_error2)) %>%
  pivot_longer(
    cols = c("mean_error1", "mean_error2"),
    names_to = "part",
    values_to = "error",
  )  %>%
  select(part, error, ID)

f <- unique(f)

model <- lmer(error ~ part + (1 | ID), data = error_test)
summary(model)

fig1_C <- ggplot(data = f, aes(x = part, y = error, fill = part)) +
  geom_boxplot(outlier.shape = NA,
               notch = TRUE,
               width = .5) +
  geom_jitter(position=position_jitter(0.2), alpha = 0.2 ) +
   stat_summary(fun=mean, geom="point", shape=4, size=1, stroke = 1, color="red", fill="red") +
  labs(title = '',
       x = '',
       y = "Mean error (% of animals)"
       ) +
  scale_fill_brewer(type = 'qual',
                    palette = 1,
                    name = '') +
  scale_x_discrete(labels = c('estimate 1', 'estimate 2')) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 17),
        axis.text.x = element_text(size=17)
        ) +
  ylim(c(0,40))


fig1_C

# figure 1D: histogram of S

fig1_D <- ggplot() +
  geom_histogram(
    aes(x = mean_s, y = stat(y = ..count.. / nrow(data_mean))),
    data = data_mean,
    fill = "#e6550d",
    color = "black",
    binwidth = 0.1,
    position = 'identity'
  ) +
  labs(title = '',
       x = "Mean adjustment (S)",
       y = "frequency") +
  theme_classic() +
  theme(text = element_text(size = 17))
fig1_D

#### FIGURE 2: main results ####

# panel C: boxplot showing S_friend > s_nonfriend

data_mean_t_test <- data_mean_t_test %>%
  mutate(friend = ordered(
    friend,
    levels = c("1", "0"),
    labels = c("friend", "non-friend")
  ))

# default
fig2_B <- ggplot(data_mean_t_test, aes(x = friend, y = mean_s, fill = friend)) +
  geom_half_boxplot(nudge = 0,
                    errorbar.draw = FALSE,
                    outlier.shape = NA,
                    width = 0.5) +
  geom_half_point(alpha =.2,width = 0.5
                    ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape =23,
    size = 2,
    stroke = 1,
    color = "black",
    fill = "white"
  ) +
  labs(title = '',
       x = 'Treatment',
       y = "Mean adjustment (S)",
       tag = "") +
  #scale_fill_brewer(type = 'qual',
  #                 palette = 2
  #                ,
  #               name = '') +
  scale_fill_manual(values = c("#a6cee3","#33a02c"))+
  scale_x_discrete(labels = c('friend', 'non-friend')) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_signif(
    comparisons = list(c("friend", "non-friend")),
    annotations = '*',
    y_position = .91,
    tip_length = 0,
    vjust = 0.5,
    textsize = 7
  ) +
  theme(text = element_text(size = 17))

fig2_B

# figure 2C: figure plot relationship between S and IOS

data_mean %>% 
  group_by(IOS) %>% 
  summarise(sd(mean_s))

fig2_C <-
  ggplot(data = data_mean, aes(
    x = factor(IOS),
    y = mean_s,
    fill = as.factor(IOS)
  )) +
  geom_boxplot(outlier.shape = NA) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    aes(group = 1),
    color = 'black',
    lty = 3
  ) +
  geom_jitter(position = position_jitter(0.2), alpha = 0.3) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    shape =23,
    size = 2,
    stroke = 1,
    color = "black",
    fill = "white"
  ) +
  labs(title = '',
       x = "Subjective Closeness",
       y = 'Mean adjustment (S)',
       tag = "") +
  scale_fill_brewer(type = 'qual',
                    palette = 'Oranges',
                    name = '') +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 17)) +
  ylim(c(0,.6))

fig2_C

# plot relationship between IOS and distance

data_mean_no0 <- data_mean %>%
  filter(distance != "unconnected")

fig2_D <-
  ggplot(data = data_mean_no0, aes(
    x = distance,
    y = IOS,
    fill = as.factor(distance)
  )) +
  geom_boxplot(outlier.shape = NA) +
 
  geom_jitter(position = position_jitter(0.2), alpha = 0.3) +
    labs(title = '',
       x = "Network Distance",
       y = 'Subjective Closeness',
       tag = "") +
  stat_summary(
    fun = mean,
    geom = "point",
    shape =23,
    size = 2,
    stroke = 1,
    color = "black",
    fill = "white"
  ) +
  scale_fill_brewer(
    type = 'qual',
    palette = 'Purples',
    name = '',
    direction = -1
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  theme(text = element_text(size = 17))

fig2_D

#### SI Figures ####

### plot distribution of distances in experimental rounds

figS_1 <- ggplot() +
  geom_bar(data = data_mean,
    aes(x = distance, y = stat(y = ..count.. / nrow(data_mean)), fill = factor(friend)),
    color = "black",
    position = 'identity',
    stat = 'count'
  ) +
  labs(title = '',
       x = "Network Distance",
       y = "frequency") +
  scale_fill_manual(values=c("#1b9e77","#d95f02" ),
                    name = 'Treatment') +
  theme_classic() +
  theme(text = element_text(size = 17))
figS_1


# figure S2: smart X accuracy

fig_S2_cortest <- data_filter4 %>%
  select(percent_error1, ID, std_self_smart) %>%
  group_by(ID) %>%
  mutate(mean_error1 = mean(percent_error1)) %>% 
  select(-c(percent_error1)) %>% 
  distinct

cor.test(fig_S2_cortest$mean_error1, fig_S2_cortest$std_self_smart)


figS_2 <- data_filter4 %>%
  select(percent_error1, ID, std_self_smart) %>%
  group_by(ID) %>%
  mutate(mean_error1 = mean(percent_error1)) %>% 
  select(-c(percent_error1)) %>% 
  distinct() %>% 
  ggplot() +
  geom_point(aes(
    x = std_self_smart,
    y = mean_error1)) + 
  labs(x = "Smart nominations", y = 'Error in estimate 1 (% animals off)') +
  geom_smooth(aes(
    x = std_self_smart,
    y = mean_error1), method = lm, color = 'black')+
  theme_classic() +
  theme(text = element_text(size = 17)) +
  annotate("text",
           x = 2,
           y = 32,
           label = 'Pearson\'s r = -.028',
           size = 5)

figS_2 

# histogram of s - Figure S3

percentages <-
  c(as.character(round(
    length(data_filter5$s[data_filter5$s < 0]) / nrow(data_filter5) * 100, 1
  )),
  as.character(round(
    length(data_filter5$s[data_filter5$s >= 0 &
                            data_filter5$s <= 1]) / nrow(data_filter5) * 100, 1
  )),
  as.character(round(
    length(data_filter5$s[data_filter5$s > 1]) / nrow(data_filter5) * 100, 1
  )))


figS_3 <- ggplot() +
  geom_rect(
    aes(xmin = -Inf, xmax = 0),
    fill = 'grey',
    alpha = 0.5,
    ymin = 0,
    ymax = Inf
  ) +
  geom_rect(
    aes(xmin = 1, xmax = Inf),
    fill = 'grey',
    alpha = 0.5,
    ymin = 0,
    ymax = Inf
  ) +
  geom_histogram(
    aes(x = s, y = stat(y=..count../nrow(data_filter5))),
    data = data_filter5,
    binwidth = 0.05,
    fill = 'orange',
    color = 'black'
  ) +
  labs(x = "Adjustment in individual rounds (s)",
       y = "frequency") +
  theme_classic() +
  theme(text = element_text(size = 17))+
  annotate(
    "text",
    x = -0.25,
    y = .3,
    label = paste(percentages[1], '%', sep = ""),
    size = 5
  ) +
  annotate(
    "text",
    x = 0.5,
    y = .3,
    label = paste(percentages[2], '%', sep = ""),
    size = 5
  )  +
  annotate(
    "text",
    x = 1.25,
    y = .3,
    label = paste(percentages[3], '%', sep = ""),
    size = 5
  )
  
figS_3

# age distribution - figure S4

data_mean %>% 
  group_by(distance) %>% 
  summarise(mean(mean_s))


fig_S4 <- data_complete %>% 
  select(participantNr, age) %>% 
  distinct() %>% 
  filter(!is.na(age)) %>% 
ggplot()+
  geom_bar( aes(x = factor(age))) +
  labs(title = 'Age distribution across sample',
       x = 'Age')+
  theme_classic() +
  theme(text = element_text(size = 17)) 

fig_S4

