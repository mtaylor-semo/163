setwd("~/Documents/r/ok_earthquakes/")

library(lubridate)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)

## Destination file to be used and read.
##
destination_file <- paste("ok_m3_quakes",today(),".csv",sep="")

## Download the data file for each new year
## that you run this exercise.
##
## Downloads csv file of Oklahoma earthquakes of
## magnitude 3 or higher.
##
download.file("http://wichita.ogs.ou.edu/eq/catalog/m3/m3.csv",
              destfile=destination_file)

## Read in the destination file. Keep only wanted columns.
## Add columns for year and month from origintime string.
## Month not used, at least for now.
## Rename the columns that are kept.
##
okq <- read.csv(destination_file)[,2:12]
okq <- subset(okq, select = c(origintime, latitude, longitude, prefmag))

okq <- okq %>%
  mutate(year = year(origintime), month = month(origintime, label=TRUE))

okq <- okq[,-1]
names(okq) = c('latitude', 'longitude', 'magnitude', 'year', 'month')

min_year <- 1980
max_year <- max(okq$year)

## Create a dataframe with all years between 1980 and current year.
## Remove last row for current year as it is not included in
## the final data used for graphing.
years <- year(seq.POSIXt(as.POSIXct("1980-01-01 0:00"), now(),by="year"))
years_df <- data.frame(year=years)
years_df <- years_df %>%
  filter(row_number() < n())

## Count the number of earthquakes for each year.
##
okq_write <- okq %>%
  filter(year >= min_year & year < max_year) %>%
  count(year)

## Merge in all years to account for years with no quakes.
## Write the data to a csv file for import into an Excel file
## for use in the graphing lab.
##
full_join(years_df, okq_write) %>%
  mutate_each(funs(ifelse(is.na(.),0,.))) %>%
  write.table('ok_quakes.csv', row.names=FALSE, col.names=c("Year", "Earthquakes"), sep=",")

# okq %>%
#   filter(year >= min_year & year < max_year) %>%
#   count(year) %>%
#   write.csv('ok_quakes.csv')


## Write a png file for reference
##
png(filename = paste("ok_quakes_1980-",year(today())-1,".png",sep=""),width=800, height=600)
okq %>% 
  filter(year >= min_year & year < max_year) %>%
  ggplot(aes(year)) +
  geom_bar() +
  scale_x_continuous(name='Year', breaks=seq(min_year,max_year,5)) +
  scale_y_continuous(name="Number of Earthquakes", breaks = seq(0,900,100)) +
  theme_tufte() +
  geom_hline(yintercept = seq(0,900,100), color="white", size=0.1)
dev.off()

## End ###
##########

#########################
## Not yet working    ###
## Probably won't use ###
uic <- read.csv("all_uic_wells.csv", stringsAsFactors=FALSE)[,c(2,8)]

uic <- uic %>% 
  mutate(year = year(mdy(Approval.Date))) %>%
  filter(year >= min_year & year < max_year) 

uic %>%
  filter(year >= min_year ) %>%
  count(year) %>%
  write.csv('ok_wells.csv')

ok_quakes <- okq %>%
  filter(year >= min_year & year < max_year) %>%
  count(year) %>%
  as.data.frame()
names(ok_quakes)=c("Year", "num_quakes")

ok_wells <- uic %>%
  filter(year >= min_year & year < max_year) %>%
  count(year) %>%
  as.data.frame()
names(ok_wells)=c("Year", "num_wells")

df <- inner_join(ok_quakes,ok_wells,by="Year") %>%
  gather(key=type,value=count, -Year)

df %>%  ggplot(aes(type)) +
  geom_bar() +
  theme_tufte()



###############
## Not used ###


## Is the number of earthquakes for the
## first three months of 2017 the same as
## first three months 0f 2016?

okq %>% 
  filter(year > 2015, month <= 'Mar') %>%
  ggplot(aes(x=month, fill=as.factor(year))) +
  geom_bar(position=position_dodge()) +
  theme_tufte()

## Plot of Injection wells.
## Not used right now, but might
## Try to find a way to calculate
## Number of recent wells.

uic %>% 
  ggplot(aes(year)) +
  geom_bar() +
  theme_tufte()

uic %>%
  count(year) %>%
  ggplot(aes(year, y=cumsum(n))) +
  geom_line() +
  theme_tufte() +
  scale_y_continuous(name="Cumulative Number of Wells", breaks=seq(0,12000,1000)) +
  scale_x_continuous(name="Year", breaks=seq(1936,2016,10))




