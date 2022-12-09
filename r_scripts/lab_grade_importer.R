# Brief script to put BI 063 lab grades from Canvas
# into file for import into BI 163 Canvas grade book

library(tidyverse)

# Read the exported grade book file so that student
# names and required fields are present.

## Required columns and order
# * Student Name
# * Student ID
# * SIS User ID (only required if you use SIS)
# * SIS Login ID (only required if you use SIS)
# * Section
# * Assignment (this can be for an existing assignment or 
#     a new assignment; retain IDs for existing assignments)

message('Open the exported data file for BI 163.')
grades163 <- read_csv(file.choose()) %>% trim_fields()

message('Open the lab grades file for BI 063.')
lab063 <- read_csv(file.choose()) %>% 
  rename(`BI063 Grade (132025)` = `Grade (%)`)

# Merge the lab grade INTO the lecture file
merged <- left_join(grades163, lab063)

# Total points for the lab grade
merged[1,6] <- 100


# Write the csv file to import into Canvas
write_csv(merged,"grades-bi_163.csv", na = "")


