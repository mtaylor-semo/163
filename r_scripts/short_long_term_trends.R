library(tidyverse)
library(ggthemes)
library(RColorBrewer)
sst_m_avg <- readLines("HadSST.3.1.1.0_monthly_globe_ts.txt")

sst_long <- lapply(sst_m_avg, function(x) {#browser()
  v <-  strsplit(x,"\\s+")[[1]]
  data_frame(ym=v[1], avg=as.numeric(v[2]))
}) %>% bind_rows()

one_color=brewer.pal(3,'Dark2')[2]

sst_short <- filter(sst_long, ym>="1997/06", ym<="2013/06")
ticks_short <- sst_short$ym[seq(1,200,by=12)]
sst_short_plot <- sst_short %>%
  mutate(ym=factor(ym,levels=ym, ordered=T)) %>%
  ggplot(aes(x=ym, y=avg)) + geom_point(color=one_color) +
  ylab("Anomalies (°C) from 1961-90 avg.") + xlab("Date (y/m)") +
  ggtitle("Global Sea-Surface Temperature Trends (HadSST3)") +
  scale_x_discrete(breaks = ticks_short) +
  theme_tufte() + 
  geom_smooth(method='lm', aes(group=1), fill=NA, color='black') +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('sst_short_plot.png', sst_short_plot, device = 'png')

sst_long[["sign"]] = ifelse(sst_long[["avg"]] >= 0, "positive", "negative")

ticks_long <- sst_long$ym[seq(1,2000,by=96)]

sst_long_plot <- 
sst_long %>%
  mutate(ym=factor(ym,levels=ym, ordered=T)) %>%
  ggplot(aes(x=ym, y=avg)) + 
  geom_abline(intercept = 0, slope = 0, color='lightgray',size=0.2) +
  geom_point(aes(colour = sign)) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Anomalies (°C) from 1961-90 avg.") + xlab("Date (y/m)") +
  ggtitle("Global Sea-Surface Temperature Trends (HadSST3)") +
  scale_x_discrete(breaks = ticks_long) +
  #
  # Overall trend
  # geom_smooth(method='lm', aes(group=1), colour='black', fill=NA) +
  #
  # Short term trend from sst_short
  # geom_smooth(data=sst_short, aes(group=1), method='lm', color='black', fill=NA) +
  #
  # Two trends, with a 50 year ago break.
  geom_smooth(data=subset(sst_long,ym>="1967/01"), aes(group=1), method='lm', color='black', fill=NA) +
  geom_smooth(data=subset(sst_long,ym<"1967/01"), aes(group=1), method='lm', color='black', fill=NA) +
  theme_tufte() + 
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))

# Use the ggsave based on the trendline used.
ggsave('sst_long_long_trend.png', sst_long_plot, device = 'png')
ggsave('sst_long_short_trend.png', sst_long_plot, device = 'png')
ggsave('sst_long_50yr_trend.png', sst_long_plot, device = 'png')

