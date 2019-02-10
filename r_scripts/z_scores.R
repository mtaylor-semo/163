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
