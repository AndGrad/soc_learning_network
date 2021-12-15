#### Table S4 - Linear mixed regression with random intercept for participants fitted to % errors----------
# Question: did participants improve from Estimate 1 to Estimate 2? 

# pivot dataset to have all errors in 1 column
error_test <-  data %>%
  filter(s >= -0 & s <= 1) %>% 
  select(percent_error1, percent_error2, ID, nAnimals) %>%
  pivot_longer(
    cols = c("percent_error1", "percent_error2"),
    names_to = "error_estimate",
    values_to = "error"
  ) %>% 
  mutate(error_estimate = ifelse(error_estimate == "percent_error1", "1", "2"))

# fit Linear mixed model to Error, to check if percent_error2 < percent_error1
model_error <- lme4::lmer(error ~ error_estimate + (1 | ID), data = error_test)
summary(model_error)

# save table with results
stargazer(model_error, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
          out="Table Error Model.html")

#### TABLE S5 - Demographics of classrooms -------------------------------------
data_ind_questionnaires <- 
  data_questionnaires %>% 
  select(IDself, 
         gender,
         age,
         classNr,
         sample) %>% 
  filter(!is.na(classNr)) %>% 
  distinct()


demographics_classroom <- data_ind_questionnaires %>% 
  group_by(classNr) %>% 
  summarise(mean_age = mean(age),
            sd = sd(age, na.rm = TRUE),
            n = n(),
            perc = n(), 
            nmales =   sum(as.numeric(factor(gender)) -1)) 


# Table of correlations bewteen characteristics of demonstrators
# select variables
data_cor <- data_complete %>%
  select(
    participantNr,
    otherNr,
    distance,
    IOS,
    other_p_indegree,
    other_p_popularity,
    other_p_smart,
    eigenCentralityOther,
    BetweenesOther
  ) %>%
  distinct() %>%
  select(-c(participantNr, otherNr)) %>% 
  rename(
    "Network Distance" = distance,
    "Friendship Nominations" = other_p_indegree,
    "Popularity Nominations" = other_p_popularity,
    "Smartness Nominations" = other_p_smart,
    "Eigenvector Centrality" =    eigenCentralityOther,
    "Betweenness" = BetweenesOther)

# save table
apa.cor.table(data_cor, filename="Correlation_Matrix_APA.doc", table.number=1)



#Charactheristics of excluded participants: anything special about them?

'%!in%' <- function(x,y)!('%in%'(x,y))

# remove participants who did not see friend vs non friend
att<-subset(data_complete, data_complete$friendANDnonFriend!=1) 

additional <- data_complete
additional$s <-ifelse(additional$s < 0, NA, additional$s)
additional$s <-ifelse(additional$s > 1, NA, additional$s)

additional <- additional%>% 
  group_by(participantNr, friend, otherNr) %>% 
  mutate(non_na_count = sum(!is.na(s)))

to_rm <- additional$participantNr[which(additional$non_na_count<3)] 

additional <- additional %>% 
  filter(participantNr %in% to_rm)

excluded <- bind_rows(att, additional )

ex_ID <- unique(excluded$participantNr)


descriptives_complete <-  data_complete %>% 
  select(
    age,
    participantNr,
    self_smart,
    self_popularity,
    self_indegree,
    eigenCentralitySelf,
    BetweenesSelf
  ) %>%
  distinct() %>%  
  filter(participantNr %!in% ex_ID) %>% 
  select(-c(participantNr)) %>% 
  rename(
    "Friendship Nominations" = self_indegree,
    "Popularity Nominations" = self_popularity,
    "Smartness Nominations" = self_smart,
    "Eigenvector Centrality" =    eigenCentralitySelf,
    "Betweenness" = BetweenesSelf)


descriptives_excluded <- excluded %>%
  select(
    participantNr,
    age,
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


apa.cor.table(descriptives_excluded, filename="Descripives_excluded_APA.doc", table.number=1)

# were excluded participants different from main sample?

# - in smartness nominations

smart1 <- descriptives_complete$`Smartness Nominations`
smart2 <- descriptives_excluded$`Smartness Nominations`

var.test(smart1, smart2)
t.test(smart1, smart2,var.equal = TRUE)

f1 <- descriptives_complete$`Friendship Nominations`
f2 <- descriptives_excluded$`Friendship Nominations`

sd(f1)
sd(f2, na.rm = TRUE)
var.test(f1, f2)
t.test(f1, f2,var.equal = TRUE)

hist(f1)
hist(f2)

std.error(f1)
std.error(f2)

# - in popularity nominations

p1 <- descriptives_complete$`Popularity Nominations`
p2 <- descriptives_excluded$`Popularity Nominations`
sd(p1)
sd(p2, na.rm = TRUE)
var.test(p1, p2)
t.test(p1, p2, var.equal = FALSE)

a1 <- descriptives_complete$age
a2 <- descriptives_excluded$age

var.test(a1, a2)
t.test(a1, a2,var.equal = TRUE)

# - in gender nominations

gender_test <- data.frame(males = c(84, 17),
                          females = c(88, 24))
chisq.test(gender_test)

excluded %>% 
  select(ID, gender) %>%
  distinct() %>% 
  summarise(nmales =   sum(as.numeric(factor(gender)) -1))


