if(!require("tidyverse")) install.packages("tidyverse")
if(!require("feedeR")) install.packages("feedeR")
if(!require("fs")) install.packages("fs")
if(!require("lubridate")) install.packages("lubridate")
if(!require("fs")) install.packages("fs")
if(!require("tmap")) install.packages("tmap")
if(!require("classInt")) install.packages("classInt")
if(!require("showtext")) install.packages("showtext")
if(!require("sysfonts")) install.packages("sysfonts")
if(!require("rvest")) install.packages("rvest")
devtools::install_github("datawookie/feedeR")
# load packages
library(feedeR)
library(sf) 
library(fs)
library(tidyverse)
library(lubridate)
library(classInt)
library(tmap)
library(rvest)
url <- "http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.bu.atom.xml"

# import RSS feed with provincial links
prov_enlaces <- feed.extract(url)
str(prov_enlaces)
prov_enlaces_tab <- as_tibble(prov_enlaces$items) %>% 
  mutate(title = repair_encoding(title))

val_atom <- filter(prov_enlaces_tab, str_detect(title, "Barcelona")) %>% pull(link)

# import the RSS
val_enlaces <- feed.extract(val_atom)

# get the table with the download links
val_enlaces_tab <- val_enlaces$items
val_enlaces_tab <- mutate(val_enlaces_tab, title = repair_encoding(title),
                          link = repair_encoding(link)) 
val_link <- filter(val_enlaces_tab, str_detect(title, "BARCELONA")) %>% pull(link)
val_link
temp <- tempfile()

# download the data
download.file(URLencode(val_link), temp)

# unzip to a folder called buildings
unzip(temp, exdir = "buildings_barcelona")

file_val <- dir_ls("buildings_barcelona", regexp = "building.gml") # change the folder if needed

# import the data
library(sf)
buildings_val <- st_read(file_val)

buildings_val <- mutate(buildings_val, 
                        beginning = str_replace(beginning, "^-", "0000") %>% 
                          ymd_hms() %>% as_date()
)
sysfonts::font_add_google("Montserrat", "Montserrat")

#use showtext for fonts
showtext::showtext_auto() 

filter(buildings_val, beginning >= "1750-01-01") %>%
  ggplot(aes(beginning)) + 
  geom_density(fill = "#2166ac", alpha = 0.7) +
  scale_x_date(date_breaks = "20 year", 
               date_labels = "%Y") +
  theme_minimal(base_family = "Montserrat") +
  labs(y = "",x = "", title = "Evolution of urban development")
ciudad_point <- tmaptools::geocode_OSM("Barcelona", 
                                       as.sf = TRUE)

#  project the points
ciudad_point <- st_transform(ciudad_point, st_crs(buildings_val))

# create the buffer
point_bf <- st_buffer(ciudad_point, 2500) # radius of 2500 m


# get the intersection between the buffer and the building
buildings_val25 <- st_intersection(buildings_val, point_bf)
br <- classIntervals(year(buildings_val25$beginning), 10,style = "fixed")

br$var

br$bks = br$brks[-c(3)]

lab <- names(print(br, under = "<", over = ">", cutlabels = FALSE))



buildings_val25 <-buildings_val25 %>% 
  mutate(yr_cl = cut(year(beginning), 
                     br$brks[-c(3)], 
                     labels = lab[-c(3)], 
                     include.lowest = TRUE))

col_spec <- RColorBrewer::brewer.pal(11, "Spectral")

# colour ramp function
col_spec_fun <- colorRampPalette(col_spec)


# create the final map
tm_shape(buildings_val25) +
  tm_polygons("yr_cl", 
              border.col = "transparent",
              palette = col_spec_fun(15), # adapt to the number of classes
              textNA = "Without data",
              title = "") +
  tm_layout(bg.color = "black",
            outer.bg.color = "black",
            legend.outside = TRUE,
            legend.text.color = "white",
            legend.text.fontfamily = "Montserrat", 
            panel.label.fontfamily = "Montserrat",
            panel.label.color = "white",
            panel.label.bg.color = "black",
            panel.label.size = 5,
            panel.label.fontface = "bold")
