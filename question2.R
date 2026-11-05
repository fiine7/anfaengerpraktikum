# Question 2:
# Which disorders frequently occur together, such as ADHD and learning disabilities?

child24 <- child24
library(ggplot2)
library(dplyr)
library(purrr)

disorderData <- data.frame(
  "everADHD" = child24$ADHDEV_C,
  "currentlyADHD" = child24$ADHDNW_C,
  "everIntDisab" = child24$IDEV1_C,
  "currentlyIntDisab" = child24$IDNW1_C,
  "everAutism" = child24$ASDEV_C,
  "currentlyAutism" = child24$ASDNW_C,
  "everDevDelay" = child24$DDEV_C,
  "currentlyDevDelay" = child24$DDNW_C,
  "everLearningDisab" = child24$LDEV_C,
  "currentlyLearningDisab" = child24$LDNW_C
)

disorderData <- disorderData %>% mutate(
  ADHD = factor(everADHD,
                levels = c(1, 2),
                labels = c("ADHD", "No ADHD")
                ),
  LearningDisability = factor(everLearningDisab,
                         levels = c(1, 2),
                         labels = c("Learning Disability", "No Learning Disability")
                         ),
  Autism = factor(everAutism,
                  levels = c(1, 2),
                  labels = c("Autism", "No Autism")
                  ),
  IntellectualDisability = factor(everIntDisab,
                                  levels = c(1, 2),
                                  labels = c("Intellectual Disability", "No Intellectual Disability")
                                  ),
  DevelopmentalDelay = factor(everDevDelay,
                              levels = c(1, 2),
                              labels = c("Developmental Delay", "No Developmental Delay")
                              )
)

compareConditions <- function(data, cond1, cond2, label) {
  data %>%
    filter(!is.na(.data[[cond1]]), !is.na(.data[[cond2]])) %>%
    group_by(.data[[cond1]], .data[[cond2]]) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(.data[[cond1]]) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    filter(.data[[cond2]] == label) %>%
    rename(
      FirstCondition = all_of(cond1),
      SecondCondition = all_of(cond2)
    )
}

conditions <- c(
  "ADHD",
  "Autism",
  "LearningDisability",
  "IntellectualDisability",
  "DevelopmentalDelay"
)

compareADHDandLD <- disorderData %>% compareConditions("ADHD",
                                                       "LearningDisability",
                                                       "Learning Disability")

compareADHDandAutism <- disorderData %>% compareConditions("ADHD",
                                                           "Autism",
                                                           "Autism")

compareADHDandID <- disorderData %>% compareConditions("ADHD",
                                                       "IntellectualDisability",
                                                       "Intellectual Disability")

compareADHDandDD <- disorderData %>% compareConditions("ADHD",
                                                       "DevelopmentalDelay",
                                                       "Developmental Delay")

completeADHDComparisons <- compareADHDandLD %>% bind_rows(compareADHDandAutism, compareADHDandID, compareADHDandDD)

completeADHDComparisons %>% ggplot(aes(x=FirstCondition, y=percentage)) +
  geom_col(fill = "#DD6C97") +
  labs(
    x = "Children with ADHD",
    y = "Percentage",
    title = "Prevalence of different conditions among children with ADHD"
  ) +
  facet_wrap(~SecondCondition) +
  theme_minimal()

compareAutismandLD <- disorderData %>% compareConditions("Autism",
                                                       "LearningDisability",
                                                       "Learning Disability")

compareAutismandADHD <- disorderData %>% compareConditions("Autism",
                                                           "ADHD",
                                                           "ADHD")

compareAutismandID <- disorderData %>% compareConditions("Autism",
                                                       "IntellectualDisability",
                                                       "Intellectual Disability")

compareAutismandDD <- disorderData %>% compareConditions("Autism",
                                                       "DevelopmentalDelay",
                                                       "Developmental Delay")

completeAutismComparisons <- compareAutismandLD %>% bind_rows(compareAutismandADHD,
                                                              compareAutismandID,
                                                              compareAutismandDD)

completeAutismComparisons %>% ggplot(aes(x=FirstCondition, y=percentage)) +
  geom_col(fill = "#19A6B6") +
  labs(
    x = "Children with Autism",
    y = "Percentage",
    title = "Prevalence of different conditions among children with Autism"
  ) +
  facet_wrap(~SecondCondition) +
  theme_minimal()

compareLDandAutism <- disorderData %>% compareConditions("LearningDisability",
                                                         "Autism",
                                                         "Autism")

compareLDandADHD <- disorderData %>% compareConditions("LearningDisability",
                                                           "ADHD",
                                                           "ADHD")

compareLDandID <- disorderData %>% compareConditions("LearningDisability",
                                                         "IntellectualDisability",
                                                         "Intellectual Disability")

compareLDandDD <- disorderData %>% compareConditions("LearningDisability",
                                                         "DevelopmentalDelay",
                                                         "Developmental Delay")

completeLDComparisons <- compareLDandAutism %>% bind_rows(compareLDandADHD,
                                                              compareLDandID,
                                                              compareLDandDD)

completeLDComparisons %>% ggplot(aes(x=FirstCondition, y=percentage)) +
  geom_col(fill = "#98DCFE") +
  labs(
    x = "Children with Learning Disabilities",
    y = "Percentage",
    title = "Prevalence of different conditions among children with Learning Disabilities"
  ) +
  facet_wrap(~SecondCondition) +
  theme_minimal()
