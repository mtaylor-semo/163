# Downloads lower troposphere data from
# University of Alabama Huntsville

library(tidyverse)
library(lubridate)
library(ggthemes)
library(RColorBrewer)

## Uncomment the following lines to get the latest data file.
if (file.exists('lower_troposphere.txt')){
 file.rename('lower_troposphere.txt','lower_troposphere.BAK')
}

# First three files included for completeness but
# the lower troposphere file is the file of importance
# for this exercise.

# Lower Stratosphere
uah_tls_dat <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt"
uah_tls_raw <- readLines(uah_tls_dat)
write.table(uah_tls_raw, file="lower_stratosphere.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)

# Tropopause 
uah_ttp_dat <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt"
uah_ttp_raw <- readLines(uah_ttp_dat)
write.table(uah_ttp_raw, file="tropopause.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)

# Middle Troposphere
uah_tmt_dat <- "http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt"
uah_tmt_raw <- readLines(uah_tmt_dat)
write.table(uah_tmt_raw, file="middle_troposphere.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)

# Lower Troposphere
# This is the file often used by climate deniers 
# to show no change since the 1997 El Nino.
uah_tlt_dat <- "http://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"
uah_tlt_raw <- readLines(uah_tlt_dat)
write.table(uah_tlt_raw, file="lower_troposphere.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)



uah_dat <- read_table("lower_troposphere.txt", n_max = 467)
uah <- uah_dat %>% 
  select(Year, Mo, Globe) %>% 
  mutate(the_date = make_date(Year, Mo))

## Cherry-picked from January 1997 to August 2015
uah_short_plot <- uah %>%
  filter(the_date >= "1997-01-01" & the_date <= "2015-08-01") %>% 
  ggplot(aes(the_date, Globe)) + 
  geom_line() +
  geom_smooth(method='lm', aes(group=1), fill = NA) +
  theme_tufte() +
  labs(x = "Year", y = "Global temperature anomaly (°C)") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")
ggsave(filename = "uah_short_plot.png", uah_short_plot, device = "png")

one_color=brewer.pal(3,'Dark2')[2]
uah_short_trend <- filter(uah, the_date >= "1997-01-01" & the_date <= "2015-08-01")

uah_long_plot <- uah %>%
  ggplot(aes(the_date, Globe)) + 
  geom_line() +
  geom_smooth(method='lm', aes(group=1), fill = NA, color=one_color) +
  theme_tufte() +
  labs(x = "Year", y = "Global temperature anomaly (°C)") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  geom_smooth(data=uah_short_trend, aes(group=1), method='lm', fill=NA)
  
ggsave(filename = "uah_long_plot.png", uah_long_plot, device = "png")


