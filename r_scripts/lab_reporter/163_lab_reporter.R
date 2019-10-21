# Merge the mutate line from this into the function in the 
# "lab_grade_converter.R" version. 

# TO USE:
#  Add BI 163 lecture grades to this directory.
#
#  The files must be csv files.
#
#  Each file should have the following columns:
#    First name, 
#    Surname, 
#    ID number, 
#    Institution, 
#    Department,
#    Email address,
#    Course total (letter),
#    Last downloaded from this course
#  
#  Rename each file as 163-X_grades.csv,
#  where X is the section number (usuall 1 or 2).

library(tidyverse)

files <- dir(pattern = "163-[12]_grades.csv")

df <- files %>%      
  map(read_csv) %>%    # read each file separately
  reduce(rbind)        # reduce with rbind into one dataframe

df <- select(df,       # Select only the columns needed to upload
             `First name`,
             Surname,
             `ID number`,
             `Email address`,
             grades = `Course total (Letter)`)

# Change A, B, C, and D letter grades to CR (credit).
# F, X, and I remain as is.
df <- df %>% mutate(grades =
                      if_else(grades %in% c("A", "B", "C", "D"), "CR", grades))

# Export the csv file for upload to Moodle grade reporter.
write_csv(df,"lab_grades.csv")
