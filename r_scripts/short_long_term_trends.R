# Code based on
# https://data-steve.github.io/global-temp-pt2/
# This uses only adjusted global temps.

library(tidyverse)
library(ggthemes)
library(RColorBrewer)

## Uncomment the following lines to get the latest data file.
# if (file.exists('HadSST.3.1.1.0_monthly_globe_ts.txt')){
#   file.rename('HadSST.3.1.1.0_monthly_globe_ts.txt','HadSST.3.1.1.0_monthly_globe_ts.BAK')
# }
# mon_gl <- "http://hadobs.metoffice.com/hadsst3/data/HadSST.3.1.1.0/diagnostics/HadSST.3.1.1.0_monthly_globe_ts.txt"
# had_m_avg <- readLines(mon_gl)
# write.table(had_m_avg, file="HadSST.3.1.1.0_monthly_globe_ts.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)

sst_m_avg <- readLines("HadSST.3.1.1.0_monthly_globe_ts.txt")

sst_long <- lapply(sst_m_avg, function(x) {#browser()
  v <-  strsplit(x,"\\s+")[[1]]
  data_frame(ym=v[1], avg=as.numeric(v[2]))
}) %>% bind_rows()

one_color=brewer.pal(3,'Dark2')[2]

sst_short <- filter(sst_long, ym>="1997/01", ym<="2015/08")
ticks_short <- sst_short$ym[seq(1,300,by=12)]
sst_short_plot <- sst_short %>%
  mutate(ym=factor(ym,levels=ym, ordered=T)) %>%
  ggplot(aes(x=ym, y=avg)) + 
  geom_point(color=one_color) +
  ylab("Anomalies (°C) from 1961-90 avg.") + xlab("Date (y/m)") +
  ggtitle("Global Sea-Surface Temperature Trends (HadSST3)") +
  scale_x_discrete(breaks = ticks_short) +
  theme_tufte() + 
  geom_smooth(method='lm', aes(group=1), fill=NA, color='black') +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))
sst_short_plot
ggsave('sst_short_plot.png', sst_short_plot, device = 'png')

sst_long[["sign"]] = ifelse(sst_long[["avg"]] >= 0, "positive", "negative")

ticks_long <- sst_long$ym[seq(1,2000,by=96)]

# Long-term, overall trend
sst_long_plot <- 
sst_long %>%
  mutate(ym=factor(ym,levels=ym, ordered=T)) %>%
  ggplot(aes(x=ym, y=avg)) + 
  geom_abline(intercept = 0, slope = 0, color='lightgray',size=0.4) +
  geom_point(aes(colour = sign, shape = sign)) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Anomalies (°C) from 1961-90 avg.") + xlab("Date (y/m)") +
  ggtitle("Global Sea-Surface Temperature Trends (HadSST3)") +
  scale_x_discrete(breaks = ticks_long) +
  geom_smooth(method='lm', aes(group=1), colour='black', fill=NA) +
  theme_tufte() + 
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))

# Use the ggsave based on the trendline used.
ggsave('sst_overall_trend.png', sst_long_plot, device = 'png')

# Long-term trend with 15 year trend line from sst-short added.
sst_long_plot <- 
  sst_long %>%
  mutate(ym=factor(ym,levels=ym, ordered=T)) %>%
  ggplot(aes(x=ym, y=avg)) + 
  geom_abline(intercept = 0, slope = 0, color='lightgray',size=0.4) +
  geom_point(aes(colour = sign, shape = sign)) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Anomalies (°C) from 1961-90 avg.") + xlab("Date (y/m)") +
  ggtitle("Global Sea-Surface Temperature Trends (HadSST3)") +
  scale_x_discrete(breaks = ticks_long) +
  geom_smooth(data=sst_short, aes(group=1), method='lm', color='black', fill=NA) +
  theme_tufte() + 
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('sst_long_plus_15yr_trend.png', sst_long_plot, device = 'png')

# Long-term trend: Two trends, with a break 50 years ago.
# Highlights recent rapid increase.
sst_long_plot <- 
  sst_long %>%
  mutate(ym=factor(ym,levels=ym, ordered=T)) %>%
  ggplot(aes(x=ym, y=avg)) + 
  geom_abline(intercept = 0, slope = 0, color='lightgray',size=0.4) +
  geom_point(aes(colour = sign, shape = sign)) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Anomalies (°C) from 1961-90 avg.") + xlab("Date (y/m)") +
  ggtitle("Global Sea-Surface Temperature Trends (HadSST3)") +
  scale_x_discrete(breaks = ticks_long) +
  geom_smooth(data=subset(sst_long,ym>="1967/01"), aes(group=1), method='lm', color='black', fill=NA) +
  geom_smooth(data=subset(sst_long,ym<"1967/01"), aes(group=1), method='lm', color='black', fill=NA) +
  theme_tufte() + 
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('sst_long_50yr_trend.png', sst_long_plot, device = 'png')

