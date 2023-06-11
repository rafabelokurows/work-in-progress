install.packages("ggHoriPlot")
library(tidyverse)
library(ggHoriPlot) 
library(ggthemes)
?climate_CPH
utils::data(climate_CPH)

climate_CPH
cutpoints <- climate_CPH  %>% 
  mutate(
    outlier = between(
      AvgTemperature, 
      quantile(AvgTemperature, 0.25, na.rm=T)-
        1.5*IQR(AvgTemperature, na.rm=T),
      quantile(AvgTemperature, 0.75, na.rm=T)+
        1.5*IQR(AvgTemperature, na.rm=T))) %>% 
  filter(outlier)

ori <- sum(range(cutpoints$AvgTemperature))/2
sca <- seq(range(cutpoints$AvgTemperature)[1], 
           range(cutpoints$AvgTemperature)[2], 
           length.out = 7)[-4]

round(ori, 2) # The origin
#> [1] 6.58

round(sca, 2) # The horizon scale cutpoints
#> [1] -12.11  -5.88   0.35  12.81  19.05  25.28

climate_CPH %>% ggplot() +
  geom_horizon(aes(date_mine, 
                   AvgTemperature,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl(palette = 'RdBu', reverse = T) +
  facet_grid(Year~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) +
  scale_x_date(expand=c(0,0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  xlab('Date') +
  ggtitle('Average daily temperature in Copenhagen', 
          'from 1995 to 2019')


load(system.file("extdata", "isd_history.rda", package = "GSODR"))
Oz <- subset(isd_history, COUNTRY_NAME == "PORTUGAL") %>%
  filter(str_detect(END, "2021"))
Oz
"085450-99999"
library(GSODR)
italia = get_GSOD(years = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),station = "085450-99999")
porto = italia %>% select(MONTH,DAY,YEAR,TEMP) 

cutpoints = porto %>% 
  mutate(
    outlier = between(
      TEMP, 
      quantile(TEMP, 0.25, na.rm=T)-
        1.5*IQR(TEMP, na.rm=T),
      quantile(TEMP, 0.75, na.rm=T)+
        1.5*IQR(TEMP, na.rm=T))) %>% 
  filter(outlier)

ori <- sum(range(cutpoints$TEMP))/2
sca <- seq(range(cutpoints$TEMP)[1], 
           range(cutpoints$TEMP)[2], 
           length.out = 7)[-4]

round(ori, 2) # The origin
#> [1] 6.58

round(sca, 2) # The horizon scale cutpoints
#> [1] -12.11  -5.88   0.35  12.81  19.05  25.28

climate_CPH %>% ggplot() +
  geom_horizon(aes(date_mine, 
                   AvgTemperature,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl(palette = 'RdBu', reverse = T) +
  facet_grid(Year~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) +
  scale_x_date(expand=c(0,0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  xlab('Date') +
  ggtitle('Average daily temperature in Copenhagen', 
          'from 1995 to 2019')
?lubridate
climate_CPH %>% filter(Year == 2019)
?str_glue
#porto$date_mine = str_glue("My name is {name}, ",
porto %>% 
  mutate(date_mine = as.Date(str_glue("2021-{MONTH}-{DAY}"))) %>% 
  ggplot() +
  geom_horizon(aes(date_mine, 
                   TEMP,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl(palette = 'RdBu', reverse = T) +
  facet_grid(YEAR~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) +
  scale_x_date(expand=c(0,0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  xlab('Date') +
  ggtitle('Average daily temperature in Porto', 
          'from 2010 to 2021')
