setwd('~/teach/163/r_scripts')

library("tidyverse")
library("ggthemes")

group <- c("Supernormal", "Longtailed", "Shorttailed", "Control")
active_nests <- c(5.2, 1.9, 0.7, 0.5)
se <- c(1.3, 0.7, 0.5, 0.3)

group_levels <- c("Control", "Shorttailed", "Longtailed", "Supernormal")

group <- parse_factor(group, group_levels)

nest <- tibble(group, active_nests, se)

p1 <- nest %>% ggplot(
  aes(x = group,
      y = active_nests)) + 
  geom_point() +
  geom_linerange(aes(ymin = active_nests - se, 
                     ymax = active_nests + se)) +
  theme(text = element_text(family = "serif",
                            size = 24)) +
  labs(x = "Treatment",
       y = "Mean number of active nests") +
  scale_x_discrete(labels = c("Control", 
                              "Short-tailed", 
                              "Long-tailed", 
                              "Supernormal")) +
  ggsave("nest_success.png", p1)
