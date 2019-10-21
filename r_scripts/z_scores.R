## Adjust raw exam scores based on z-scores
## Shifts scores to new mean with same standard deviation.

library(tidyverse)

desired_mean = 70

e1 <- read_csv("exam1.csv", col_names = "raw")

e1 <- e1 %>% 
  mutate(zscore = scale(raw, center = TRUE, scale = TRUE)) %>% 
  mutate(final = desired_mean + (sd(raw) * zscore))

e1_results <- e1 %>%
  mutate(letter = case_when(
    final >= 90.0                ~ "A",
    final >= 80.0 & final < 90.0 ~ "B",
    final >= 70.0 & final < 80.0 ~ "C",
    final >= 60.0 & final < 70.0 ~ "D",
    TRUE ~ "F"
  )) %>% 
  group_by(letter) %>% 
  summarize(n = n())

ggplot(e1_results) +
  geom_point(aes(x = reorder(letter, desc(letter)), 
                 y = n),
             size = 3) +
  coord_flip() +
  labs(x = NULL,
       y = "Number of exams")



# Used to compare multiple semesters --------------------------------------

e1 <- read_csv("exam1.csv") %>% 
  gather(key = semester, value = score) %>% 
  drop_na()

e1_by_semester <- e1 %>% 
  group_by(semester) %>% 
  summarise(mean = mean(score, na.rm = TRUE),
            stdev = sd(score, na.rm = TRUE),
            N = n())

e1_overall <- e1 %>% 
  summarise(mean = mean(score, na.rm = TRUE),
            stdev = sd(score, na.rm = TRUE),
            N = n())


e1x <- e1 %>% mutate(score = case_when())


