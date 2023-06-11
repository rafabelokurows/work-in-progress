#### itlaia ####

options(repos = getOption("repos")["CRAN"])
pacman::p_load(stringr,rnaturalearth,rnaturalearthhires,sf,tidyverse,leaflet,mapIT,sf,ggplot2)
library(devtools)
install_github("nicolasturaro/mapIT")

load(system.file("extdata", "isd_history.rda", package = "GSODR"))
Oz <- subset(isd_history, COUNTRY_NAME == "ITALY")
Oz

teste = subset(isd_history, COUNTRY_NAME == "ITALY")
teste = teste %>% 
  filter(str_detect(END, "2021"))# %>% select(NAME,LAT,LON) %>% pull(STNID)
teste


teste$indicator <- st_within(teste,sardinia) %>% lengths > 0
teste123 = teste %>% st_as_sf(coords = c("LON", "LAT")) %>% st_set_crs(4326) %>% mutate(indicator = st_within(geometry,sardinia))
teste123
str(sardinia)

italy %>% as.data.frame() %>%  count(region)
italy %>%  
  filter(region %in% c("Sardegna")) %>% 
  ggplot() + 
  geom_sf()


italy %>% 
  group_by(region) %>% filter(region %in% c("Sardegna"))


italyregions[-c(14),] %>%  
  #filter(region %in% c("Sardegna")) %>% 
  ggplot() + 
  geom_sf() 


italyregions = italy %>% 
  mutate(region = as.character(region),
         region = ifelse(is.na(region), "NA", region)) %>% 
  group_by(region) %>% 
  summarize(geometry = st_union(geometry))

teste[st_within(teste123$geometry,italyregions[-c(14),]),]

continentalitaly = teste[st_within(teste123$geometry,italyregions[14,]) %>% lengths == 0,]
exclude = c("CAPE CARBONARA","USTICA ISLAND","CAPE CACCIA","LAMPEDUSA","PANTELLERIA","ISOLA SALINA","ISOLA STROMBOLI","MONTE CALAMITA")
continentalitaly = continentalitaly %>% filter(!NAME %in% c(exclude)) # %>% st_as_sf(coords = c("LON", "LAT"))
options(scipen=999)
stations = continentalitaly[121:130]  %>%
  #sample_n(50) %>%
  pull(STNID)
stations2 = continentalitaly[101:110]  %>%
  #sample_n(50) %>%
  pull(STNID)
stations3 = continentalitaly[111:120]  %>%
  #sample_n(50) %>%
  pull(STNID)
italia = get_GSOD(years = c(2020),station = stations)
italia2 = get_GSOD(years = c(2020),station = stations2)
italia3 = get_GSOD(years = c(2020),station = stations3)
#italiatutto = bind_rows(italia,italia2,italia3)
italiatutto = bind_rows(italiatutto,italia)

italiatutto %>% distinct()

melhores = italiatutto %>% distinct() %>% mutate(month = month(YEARMODA)) %>% filter(month == 10) %>% group_by(STNID,NAME) %>% 
summarise(norain = sum(PRCP == 0,na.rm=T),goodtemp = sum(MIN>15 & MAX<30,na.rm=T),
          perfectweather=sum(PRCP == 0 &MIN>15 & MAX<30,na.rm=T))  %>% arrange(-perfectweather) %>% ungroup() %>% slice(1:10) %>% pull(STNID)

save(italiatutto,file="italiatutto.rdata")

load("italiatutto.rdata")
pacman::p_load(gt)
italiatutto
italiatutto %>% distinct()  %>% filter(MONTH == 10)%>% select(NAME,YEARMODA,PRCP,I_FOG,MIN,MAX,LATITUDE,LONGITUDE) 
#%>% gt()

str(italiatutto %>% select(PRCP,I_FOG,MIN,MAX,LATITUDE,LONGITUDE))

porcidade = italiatutto %>% distinct() %>% filter(MONTH == 10) %>% group_by(STNID,NAME) %>% 
  summarise(norain = sum(PRCP == 0,na.rm=T),goodtemp = sum(MIN>15 & MAX<30,na.rm=T),
            perfectweather=sum(PRCP == 0 &MIN>15 & MAX<30,na.rm=T))  %>% arrange(-perfectweather)

porcidade = italiatutto %>% distinct() %>% filter(!NAME %in% c(exclude)) %>% filter((MONTH == 10)|(MONTH == 9 & DAY >20)) %>% group_by(STNID,NAME) %>% 
  #summarise(fogdays = sum(I_FOG,na.rm=T)) %>% arrange(desc(fogdays))
  summarise(norain = sum(PRCP == 0,na.rm=T),goodtemp = sum(MIN>15 & MAX<30&(I_FOG==0|is.na(I_FOG)),na.rm=T),
            perfectweatherhelo=sum(PRCP == 0 &MIN>15 & MAX<30&(I_FOG==0|is.na(I_FOG)),na.rm=T),
            perfectweatherrafael=sum(PRCP == 0 &MIN>10 & MAX<22&(I_FOG==0|is.na(I_FOG)),na.rm=T),
           # perfectweatherboth=sum(PRCP == 0 &MIN>15 & MAX<22&(I_FOG==0|is.na(I_FOG)),na.rm=T)
           )  %>% 
  arrange(-perfectweatherrafael)

porcidade = italiatutto2 %>% distinct() %>% filter(!NAME %in% c(exclude)) %>% filter(MONTH == 10) %>% group_by(STNID,NAME) %>% 
  summarise(nofog = floor(sum(I_FOG==0|is.na(I_FOG),na.rm=T)/3),
            norain = floor(sum(PRCP == 0,na.rm=T)/3),
            goodtemp = floor(sum(MIN>10 & MAX<=25&(I_FOG==0|is.na(I_FOG)),na.rm=T)/3),
            perfectweather=floor(sum(PRCP == 0 &MIN>10 & MAX<=25&(I_FOG==0|is.na(I_FOG)),na.rm=T)/3))  %>% 
  arrange(-perfectweather) 
#%>% ungroup() %>% slice(1:3,5:11) %>% gt()

#italiatutto %>% distinct() %>% filter((MONTH == 10 & DAY < 15)|(MONTH == 9 & DAY >20)) 
  
  
exclude = c("CAPE CARBONARA","USTICA ISLAND","CAPE CACCIA","LAMPEDUSA","PANTELLERIA","ISOLA SALINA","ISOLA STROMBOLI",
            "MONTE CALAMITA","OLBIA COSTA SMERALDA","ALGHERO","CAPE BELLAVISTA","PERDASDEFOGU","CAPE S. LORENZO",
            "DECIMOMANNU","ELMAS","CAPE FRASCA","PONZA ISLAND")
teste = teste %>% filter(!NAME %in% c(exclude))

pacman::p_load(GSODR)
stationstofetch1 = teste[1:20,] %>% pull(STNID)
stationstofetch2 = teste[21:60,] %>% pull(STNID)
stationstofetch3 = teste[61:70,] %>% pull(STNID)
stationstofetch4 = teste[71:90,] %>% pull(STNID)
stationstofetch5 = teste[91:110,] %>% pull(STNID)
stationstofetch6 = teste[111:128,] %>% pull(STNID)

teste1 = get_GSOD(years = c(2018,2019,2020),station = stationstofetch1)
teste2 = get_GSOD(years = c(2018,2019,2020),station = stationstofetch2)
teste3 = get_GSOD(years = c(2018,2019,2020),station = stationstofetch3)
teste4 = get_GSOD(years = c(2018,2019,2020),station = stationstofetch4)
teste5 = get_GSOD(years = c(2018,2019,2020),station = stationstofetch5)
teste6 = get_GSOD(years = c(2018,2019,2020),station = stationstofetch6)
italiatutto2 = bind_rows(teste1,teste2,teste3,teste4,teste5,teste6)

porcidade = italiatutto2 %>% distinct() %>% filter(!NAME %in% c(exclude)) %>% filter((MONTH == 10)|(MONTH == 9 & DAY >20)) %>% group_by(STNID,NAME) %>% 
  #summarise(fogdays = sum(I_FOG,na.rm=T)) %>% arrange(desc(fogdays))
  summarise(norain = ceiling(sum((PRCP==0|is.na(PRCP)),na.rm=T)/3),goodtemp = ceiling((sum(MIN>15 & MAX<30&(I_FOG==0|is.na(I_FOG)),na.rm=T))/3),
            perfectweatherhelo=ceiling((sum((PRCP==0|is.na(PRCP)) &MIN>15 & MAX<30&(I_FOG==0|is.na(I_FOG)),na.rm=T))/3),
            perfectweatherrafael=ceiling((sum((PRCP==0|is.na(PRCP)) &MIN>9 & MAX<24&(I_FOG==0|is.na(I_FOG)),na.rm=T))/3),
            # perfectweatherboth=sum(PRCP == 0 &MIN>15 & MAX<22&(I_FOG==0|is.na(I_FOG)),na.rm=T)
  )  %>% 
  arrange(-perfectweatherhelo)

top20helo = porcidade %>% 
  arrange(-perfectweatherhelo)%>% ungroup() %>% slice(1:20) %>% pull(STNID)
top20rafa = porcidade %>% 
  arrange(-perfectweather)%>% ungroup() %>% slice(1:3,5:11) %>% pull(STNID)
melhoreshelo= teste %>% filter(STNID %in% top20helo)
melhoresrafa= teste %>% filter(STNID %in% top20rafa)
#italy <- ne_states(country = "italy", returnclass = "sf")
leaflet(data = italy) %>% addTiles() %>%
  addPolygons(fillColor = c("red","green","white"), stroke = FALSE) %>%
  addCircleMarkers(color="blue",radius=2,~melhoresrafa$LON, ~melhoresrafa$LAT, popup = ~as.character(paste("rafa",melhoresrafa$NAME)), 
                                      label = ~as.character(melhoresrafa$NAME)) 


leaflet(data = italy) %>% addTiles() %>%
  addPolygons(fillColor = c("red","green","white"), stroke = FALSE) %>%
  addCircleMarkers(color=~pal(cidades$tipo),radius=5,~cidades$LON, ~cidades$LAT, popup = ~as.character(paste(cidades$NAME,"<br>",cidades$tipo)), 
                   label = ~as.character(paste(cidades$NAME,"-",cidades$tipo))) 
#%>%   addCircleMarkers(color="blue",radius=2,~melhoresrafa$LON, ~melhoresrafa$LAT, popup = ~as.character(paste("rafa",melhoresrafa$NAME)), 
#                   label = ~as.character(melhoresrafa$NAME))
bind_rows(melhoresrafa,melhoreshelo,.id="tipo") %>% mutate(tipo = as.factor(tipo)) %>% distinct() 

cidades = bind_rows(merge(melhoresrafa,melhoreshelo) %>% mutate(tipo = "ambos"),
          anti_join(melhoreshelo,melhoresrafa) %>% mutate(tipo = "helo"),
          anti_join(melhoresrafa,melhoreshelo) %>% mutate(tipo = "rafa"))

pal <- 
  colorFactor(palette = c("blue", "red", "black"), 
              levels = c("ambos", "helo", "rafa"))

save(porcidade,italy,teste,italiatutto,italiatutto2,file="arquivos.rdata")


top10 = porcidade %>% arrange(-perfectweather)%>% ungroup() %>% slice(1,3,6,9,11) %>% pull(STNID)
top5 = porcidade %>% arrange(-perfectweather)%>% ungroup() %>% slice(1,3,6,9,11) %>% pull(STNID)
top10cities = italiatutto2 %>% filter(STNID %in% c(top5))
top5cities = italiatutto2 %>% filter(STNID %in% c(top5))

tbar <- top5cities[, c("MONTH","NAME","YEARMODA", "TEMP", "MAX", "MIN","PRCP")]
tbar_temps <-
  pivot_longer(tbar, cols = TEMP:MIN, names_to = "Measurement")

tbar_temps
ggplot(tbar, aes(x = NAME, y = TEMP)) + 
  geom_violin(aes(fill = NAME), color = "blue", alpha = .5) + 
  stat_summary(fun.y = median, geom = "point", color = "blue", size = 4)
ggplot(tbar,aes(x = YEARMODA,y = TEMP,group=NAME)) + 
  geom_point(aes(colour = TEMP)) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 16) + 
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Daily average temperature") +
  xlab("Date") +  ylab ("Average Temperature ( ºC )")

extremeprcp = tbar_temps2 %>% filter(MONTH==10) %>% slice_max(PRCP,n=1) %>% slice(2)
extrememax = tbar_temps2%>% filter(MONTH==10) %>% filter(Measurement == "MAX") %>% slice_max(value)
extrememin = tbar_temps2%>% filter(MONTH==10) %>% filter(Measurement == "MIN") %>% slice_min(value)
tbar %>% filter(MONTH==10) %>% 
ggplot( aes(x = YEARMODA, y = MAX, color = NAME)) + 
  geom_point(aes(size = PRCP), alpha = .5) +
  geom_smooth(se = FALSE) + 
  facet_grid(. ~ NAME) +
  geom_text(data = extrememax,label=paste(extrememax$YEARMODA,"\n",extrememax$MAX)) +
  geom_text(data = extrememin,label=paste(extrememin$YEARMODA,"\n",extrememin$MIN))+
  geom_text(data = extremeprcp,label=paste(extremeprcp$YEARMODA,"\n",extremeprcp$PRCP))

devtools::install_github('Mikata-Project/ggthemr')
ggthemr('greyscale',layout = "clear")
library(ggthemr)
ggthemr_reset()

"DESENZANO","BARI","PALERMO"
tbar_temps2 %>% filter(NAME %in% c("CIVITAVECCHIA","CAPRI ISLAND")) %>%
    mutate(YEARMODA = as.Date(YEARMODA)) %>% filter(MONTH==10) %>% 
  ggplot(aes(x=YEARMODA, y=value, color=Measurement,linetype=Measurement)) + 
  geom_point(aes(size = PRCP),alpha = .5) +
  scale_x_date(breaks = breaks_pretty(10),date_labels = "%y",date_breaks = "1 year") + 
  labs (y= "Temperature",x="Years(October)",title="2 best cities") +
  facet_grid(. ~ NAME) +
  #geom_text(data = extrememax,label=paste("Max.Temp.\n",extrememax$value,"ºC"), color="coral2",size=5,hjust = "inward",show.legend = FALSE) +
  geom_smooth(se = FALSE) +
  theme_few()
#+  geom_text(data = extrememin,label=paste("Min.Temp.\n",extrememin$value,"ºC"), size=5,hjust = "inward",show.legend = FALSE)
?year
# +
#   geom_smooth(se = FALSE) +
#   geom_text(data = extrememax,label=paste("Max.Temp.\n",extrememax$value,"ºC"), color="coral2",size=5,hjust = "inward",show.legend = FALSE) +
#   geom_text(data = extrememin,label=paste("Min.Temp.\n",extrememin$value,"ºC"), size=5,hjust = "inward",show.legend = FALSE)+
#   geom_text(data = extremeprcp,label=paste("Max.Precip.\n",extremeprcp$PRCP,"mm"), size=5,  hjust = "inward",show.legend = FALSE)



tbar_temps2 = filter(tbar_temps,Measurement %in% c("MIN","MAX"))
tbar_temps2 %>% filter(NAME == "LISBOA/GEOF")
tbar_temps2 %>% filter(MONTH==10) %>% 
ggplot(aes(x=YEARMODA, y=value, color=Measurement, linetype=Measurement)) + geom_point() + scale_colour_brewer(palette="Set1") +
  geom_smooth() +
  ggtitle("2020/2021 \n") + theme(plot.title=element_text(size=18, face="bold", vjust=-5.5)) + 
  xlab("Day of year") + theme(axis.title.x=element_text(size=16)) + 
  ylab(expression(paste("Temperature (",degree,"C)"))) + theme(axis.title.y=element_text(size=16)) + #scale_y_continuous(limits=c(-30, 50)) + scale_x_continuous(limits=c(0, 200)) +
  theme(legend.text=element_text(size=16)) + theme(legend.title=element_text(size=16))+
  facet_wrap(~ NAME)







#### outras coisas ####
library(mapIT)
mapIT::shapedata_istat_regioni
?mapIT
values = runif(20, min = 0, max = 1000)
mapIT(values = values)
mapIT::shapedata_istat_regioni

remotes::install_github("ropensci/rnaturalearthhires")
library("rnaturalearth")
library("ggplot2")
italy <- ne_states(country = "italy", returnclass = "sf")

ggplot(data = italy) +
  geom_sf()


it <- ggplot2::map_data("italy")

ggplot(it, aes(long, lat, group = group)) +
  geom_polygon() +
  coord_fixed()
it

italy

ggplot(teste, aes(x=LON, y=LAT)) + geom_point()

italy_sf = teste %>%
  st_as_sf(coords = c("LON", "LAT"), crs=4326)

turbine_sf_t <- st_transform(italy_sf, crs=2163)

ggplot() +
  geom_sf(data = italy_sf) + 
  geom_sf(data = italy, fill = NA, color = "black", size = 0.15, alpha = 0) +
  #coord_sf(datum = st_crs(2163)) +   
  labs(fill  = "", 
       title = "",
       caption='') + 
  theme_bw()
library(leaflet)

#stations
leaflet(data = italy) %>% addTiles() %>%
  addPolygons(fillColor = c("red","green","white"), stroke = FALSE) %>%
  addCircleMarkers(radius=2,~teste$LON, ~teste$LAT, popup = ~as.character(teste$NAME), label = ~as.character(teste$NAME))

addCircleMarkers(
  radius = ~ifelse(type == "ship", 6, 10),
  color = ~pal(type),
  stroke = FALSE, fillOpacity = 0.5
)

stations = teste %>% select(STNID) %>% pull()
(teste2 <- get_GSOD(years = 2020, country = "Italy"))
teste3 <- get_GSOD(years = 2020,station = stations)
str(teste3)

teste3 %>% filter(NAME=="BOLZANO")
tbar <- teste3
str(tbar)

tbar_temps <- teste3[, c("NAME","YEARMODA", "TEMP", "MAX", "MIN")]
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
