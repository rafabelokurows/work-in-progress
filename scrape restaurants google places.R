#https://stackoverflow.com/questions/50260867/next-page-token-in-google-places-googleway-package
library(googleway)
library(tidyverse)
#api_key <- 'AIzaSyAVVLjn0zgY8RFhFla9cAqDHnvOk6vsA2k'

res <- google_places(search_string = "Restaurants, Porto, Portugal",
                     key = api_key,
                     radius = 1500)

oc <- c(33.685494, -117.812070)

df <- google_places(search_string = "Restaurants, Porto, Portugal",
                    radius = 1500, key = api_key)

dftotal = bind_rows(df$results,aux$results)
for (i in 1:5){  
  print(i)
  aux <- google_places(search_string = "Restaurants, Porto, Portugal",
                radius = 1500, key = api_key, page_token = token)
  token = aux$next_page_token
  dftotal = bind_rows(dftotal,aux$results)
}



dftotal %>%  
  mutate(rating = as.numeric(rating),
         user_ratings_total = as.numeric(user_ratings_total)) %>% 
  filter(rating > 4.6 & user_ratings_total> 200)


## use the 'next_page_token' from the previous search to get the next 20 results
res_next <- google_places(search_string = "Restaurants in Melbourne, Australia",
                          page_token = res$next_page_token,
                          key = api_key)

## search for a specific place type
google_places(location = c(-37.817839,144.9673254),
              place_type = "restaurants",
              radius = 20000,
              key = api_key)



google_places(search_string = "Bicycle shop, Melbourne, Australia",
              open_now = TRUE,
              key = api_key)