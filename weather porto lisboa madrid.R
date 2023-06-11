pacman::p_load(ggridges,GSODR,ggthemes,leaflet,dplyr,ggplot2,tidyverse)

load(system.file("extdata", "isd_history.rda", package = "GSODR"))
Oz <- subset(isd_history, COUNTRY_NAME == "ITALY")
  Oz
# m <- leaflet() %>% setView(lng = -8, lat = 40, zoom = 6)
# m %>% addProviderTiles(providers$CartoDB.Positron)

leaflet(data = Oz) %>% addTiles() %>%
  addCircleMarkers(~LON, ~LAT, popup = ~as.character(NAME), label = ~as.character(NAME))
leaflet(data = Oz[c(1:57),]) %>% addTiles() %>%
  addMarkers(~LON, ~LAT, popup = ~as.character(NAME), label = ~as.character(NAME))
leaflet(data = Oz[c(19:57),]) %>% addTiles() %>%
  addMarkers(~LON, ~LAT, popup = ~as.character(NAME), label = ~as.character(STNID))

stations = Oz[c(19:57),] %>% select(STNID) %>% pull()

# subset(isd_history, STNID %in% c("085350-99999"))
# 085450-99999
weatherpt <- get_GSOD(years = c(2020), station = stations)
head(weatherpt)
str(weatherpt)
weatherpt %>% count(STNID)


#### Estações Lisboa, Porto e Madrid
tbar <- get_GSOD(years = c(2020,2021), station = "085350-99999")
str(tbar)
nearest_stations(LAT = 40.438,LON = -3.63,distance = 10)
nearest_stations(LAT = 38.720038,LON = -9.151966,distance = 50)
stations = c("085350-99999","085450-99999","082220-99999")
tbar <- get_GSOD(years = c(2020,2021), station = stations)
str(tbar)

tbar_temps <- tbar[, c("NAME","YEARMODA", "TEMP", "MAX", "MIN")]
tbar_temps <-
  pivot_longer(tbar_temps, cols = TEMP:MIN, names_to = "Measurement")

ggplot(data = tbar_temps, aes(x = YEARMODA,
                              y = value,
                              colour = Measurement)) +
  geom_line() +
  scale_color_brewer(type = "qual", na.value = "black") +
  scale_y_continuous(name = "Temperature") +
  scale_x_date(name = "Date") +
  ggtitle(label = "Max, min and mean temperatures for Toowoomba, Qld, AU",
          subtitle = "Data: U.S. NCEI GSOD") +
  theme_classic() + facet_grid(~NAME)


tbar %>% arrange(desc(MAX)) %>% select(YEARMODA,NAME,TEMP,MIN,MAX,PRCP,I_SNOW_ICE)
# Variação de temperatura entre as cidades
ggplot(tbar, aes(x = YEARMODA, y = MAX,color=NAME)) + 
  geom_smooth(se = FALSE) 
ggplot(tbar, aes(y = MAX,x=NAME)) + geom_boxplot()
ggplot(tbar, aes(x = NAME, y = TEMP)) + 
  geom_violin(aes(fill = NAME), color = "blue", alpha = .5) + 
  stat_summary(fun.y = median, geom = "point", color = "blue", size = 4)
ggplot(tbar, aes(x = MAX, y = NAME)) + 
  geom_density_ridges(scale = .85)
ggplot(tbar,aes(x = YEARMODA,y = TEMP,group=NAME)) + 
  geom_point(aes(colour = TEMP)) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 16) + 
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Daily average temperature") +
  xlab("Date") +  ylab ("Average Temperature ( ºC )")

ggplot(tbar, aes(x = YEARMODA, y = MAX, color = NAME)) + 
  geom_point(aes(size = PRCP), alpha = .5) +
  geom_smooth(se = FALSE) + 
  facet_grid(. ~ NAME)


#https://www.r-bloggers.com/2015/03/part-3a-plotting-with-ggplot2/
#https://www.neonscience.org/resources/learning-hub/tutorials/dc-time-series-plot-ggplot-r
#https://rstudio-pubs-static.s3.amazonaws.com/124829_5425d99025a046e5a13e4a038b744aa5.html
ggplot(tbar, aes(YEARMODA, PRCP)) +
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("Daily Precipitation\n") +
  xlab("Date") + ylab("Precipitation (mm)") +
  scale_x_date(labels=scales::date_format ("%b %y"), breaks=scales::date_breaks("1 year")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18))


tbar_temps2 = filter(tbar_temps,Measurement %in% c("MIN","MAX"))
tbar_temps2 %>% filter(NAME == "LISBOA/GEOF")
ggplot(tbar_temps2, aes(x=YEARMODA, y=value, color=Measurement, linetype=Measurement)) + geom_point() + scale_colour_brewer(palette="Set1") +
  geom_smooth() +
  ggtitle("2020/2021 \n") + theme(plot.title=element_text(size=18, face="bold", vjust=-5.5)) + 
  xlab("Day of year") + theme(axis.title.x=element_text(size=16)) + 
  ylab(expression(paste("Temperature (",degree,"C)"))) + theme(axis.title.y=element_text(size=16)) + #scale_y_continuous(limits=c(-30, 50)) + scale_x_continuous(limits=c(0, 200)) +
  theme(legend.text=element_text(size=16)) + theme(legend.title=element_text(size=16))+
  facet_wrap(~ NAME)

tbar$rained = ifelse(tbar$PRCP == 0 | is.na(tbar$PRCP),"0","1")

ggplot(tbar, aes(NAME)) +
  geom_bar(aes(fill=rained), position="fill") +
  xlab("Season") + ylab ("Proportion") +
  ggtitle("Proportion of days without and with rain, by city")

ggplot(tbar, aes(YEARMODA, PRCP,group=NAME)) +
  geom_point(aes(color=PRCP)) +
  geom_smooth(color="blue", size=1) +
  scale_colour_gradient() +
  xlab("Date") + ylab("Rain (mm)") +
  ggtitle("Daily rain amount")
#https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
toSeason <- function(dat) {
  
  stopifnot(class(dat) == "Date")
  
  scalarCheck <- function(dat) {
    m <- as.POSIXlt(dat)$mon+ 1        # correct for 0:11 range
    d <- as.POSIXlt(dat)$mday           # correct for 0:11 range
    if ((m == 3 & d >= 21) | (m == 4) | (m == 5) | (m == 6 & d < 21)) {
      r <- 1
    } else if ((m == 6 & d >= 21) | (m == 7) | (m == 8) | (m == 9 & d < 21)) {
      r <- 2
    } else if ((m == 9 & d >= 21) | (m == 10) | (m == 11) | (m == 12 & d < 21)) {
      r <- 3
    } else {
      r <- 4
    }
    r
  }
  
  res <- sapply(dat, scalarCheck)
  res <- ordered(res, labels=c("Spring", "Summer", "Fall", "Winter"))
  invisible(res)
}
tbar_porto$season = toSeason(tbar_porto$YEARMODA)
tbar_porto %>% filter(YEAR == 2020) %>%
ggplot(aes(x = season,fill=rained)) +
  geom_bar( position="fill") +
  #geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  #geom_text(aes(y = label_y, label = Weight), colour = "white") 
  #geom_text(size = 3, position = position_fill(vjust = 0.5)) +
  stat_count(geom = "text", 
             aes(label = ..count..),
             position=position_fill(vjust=0.5), colour="white")+
  xlab("Season") + ylab ("Proportion") +
  ggtitle("Proportion of days without and with rain, by season") +
  facet_grid(~ NAME)

tbar_porto %>% filter(YEAR == 2020) %>% count(season)
library(lubridate)
monthweek <- function(d, w) ceiling((d - w) / 7) + 1
tbar = mutate(tbar, wd = wday(tbar$YEARMODA, label = TRUE))
tbar = mutate(tbar, wd = factor(tbar$wd))
tbar = mutate(tbar, mw = factor(monthweek(DAY, wday(YEARMODA))))
tbar = mutate(tbar, mw = factor(mw, rev(levels(mw))))

tbar_porto = tbar %>% filter(NAME == "PORTO")
ggplot(tbar_porto, aes(x = wd, y = mw, fill = TEMP)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colours = rev(colorsporto[1:7])) +
  #scale_fill_gradient2(low = "blue",mid = "white",high="red",midpoint=12) +
  facet_wrap(~ month(YEARMODA, TRUE)) +
  ylab("") + xlab("Month") + labs(fill='Temperature') +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


library(RColorBrewer)
display.brewer.all()
display.brewer.pal(n = 8, name = 'RdBu')
brewer.pal(n = 8,name = 'RdBu')
colorsporto = c(brewer.pal(n = 8,name = 'RdBu'))
colorsporto[1:6]

ggplot(tbar_porto, aes(x=MIN, y=MAX)) +
  geom_point(color="firebrick", alpha=0.5) + 
  geom_smooth(aes(color=season), se=FALSE, size=1.1) +
  ggtitle ("Daily low and high temperatures") +
  xlab("Daily low temperature ( ºC )") +  ylab ("Daily high temperature ( ºC )") 


ggplot(tbar_porto, aes(YEARMODA, PRCP)) +
  geom_point(aes(color=PRCP)) +
  geom_smooth(color="blue", size=1) +
  scale_colour_gradient() +
  xlab("Date") + ylab("Rain (mm)") +
  ggtitle("Daily rain amount")

ggplot(tbar_porto, aes(season, PRCP)) +
  geom_boxplot() +
  ylab("Season") + xlab("Rain (mm)") +
  ggtitle("Daily rain amount by season")

ggplot(tbar_porto, aes(MXSPD, PRCP)) +
  geom_point(color="firebrick") +
  geom_smooth(size=0.75, se=FALSE) +
  facet_wrap(~season) +
  xlab("Maximum wind speed (km/h)") +
  ylab ("Rain (mm)") +
  ggtitle("Amount of rain vs. maximum wind speed, by season")

library(viridis)
ggplot(data = tbar_porto, aes(x = day(YEARMODA),y = month(YEARMODA, label=T, abbr=T))) + 
  geom_tile(aes(fill = MAX)) + 
  scale_x_continuous(breaks=c(1:31), expand=c(0,0)) + coord_equal(ratio = 1) + 
  scale_fill_viridis(option="magma") + theme_tufte(base_family="Helvetica")

pacman::p_load(gganimate)
theme_set(theme_bw())
p = ggplot(
  tbar_porto,
  aes(YEARMODA, TEMP, group = NAME, color = factor(NAME))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
p + 
  geom_point() +
  transition_reveal(YEARMODA)

library(plyr)
library(reshape)
library(ggplot2)
# Using ggplot2 0.9.2.1

nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
nba$Name <- with(nba, reorder(Name, PTS))
nba.m <- melt(nba)
nba.m <- ddply(nba.m, .(variable), transform, value = scale(value))

# Convert the factor levels to numeric + quanity to determine size of hole.
nba.m$var2 = as.numeric(nba.m$variable) + 15

# Labels and breaks need to be added with scale_y_discrete.
y_labels = levels(nba.m$variable)
y_breaks = seq_along(y_labels) + 15

p2 = ggplot(nba.m, aes(x=Name, y=var2, fill=value)) +
  geom_tile(colour="white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylim(c(0, max(nba.m$var2) + 0.5)) +
  scale_y_discrete(breaks=y_breaks, labels=y_labels) +
  coord_polar(theta="x") +
  theme(panel.background=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_text(size=5))
p2

ggsave(filename="plot_2.png", plot=p2, height=7, width=7)

#olhar também:
https://learnr.wordpress.com/2009/04/29/ggplot2-labelling-data-series-and-adding-a-data-table/
#criar artigo com equivalente disso em R:
https://towardsdatascience.com/creating-a-simple-map-with-folium-and-python-4c083abfff94
#plot aqui:
https://github.com/taraskaduk/weather/blob/master/R/old%20and%20drafts/new%20graphs.R