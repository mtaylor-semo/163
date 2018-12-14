# Brief script to convert lecture grades to lab grades.
# X = X (Did not take final, receives F)
# I = I (Incomplete)
# F = F (Took final, failed lecture)
# All else gets CR. 

library(tidyverse)

# Exported from Excel file. 
grades <- read_csv("test2.csv", na = "-")

# Select just the columns needed,
# Rename 'Course total (Letter)' to 'grades 
# per Grade Reporter requirement, replace
# grades with the appropriate credit.
new_grades <- grades %>% 
  select(`First name`, 
         Surname, 
         `ID number`, 
         `Email address`, 
         `Course total (Letter)`) %>% 
  rename(Grade = `Course total (Letter)`) %>% 
  filter(!is.na(Grade)) %>% 
  mutate(Grade = case_when(Grade == 'F' ~ 'F',
                            Grade == 'X' ~ 'X',
                            Grade == 'I' ~ 'I',
                            TRUE ~ 'CR'))

# Export the csv file for upload to Grade Reporter.
write_csv(new_grades,"063_grades.csv")
