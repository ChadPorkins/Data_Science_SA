library(tidyverse)
library(leaflet)
library(htmlwidgets)

points = read.csv("attacks.csv")
points = points[!is.na(points$latitude), ]
points = points[!is.na(points$longitude), ]

x = leaflet(data = points) %>%
  addTiles() %>%
  addMarkers(lat=points$latitude, lng=points$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", points$date_day,"/",points$date_month,"/", points$date_year,
                          "<br><br><strong>Place: </strong>", points$city_txt,"-",points$admin0_txt,
                          "<br><strong>Killed_low_estimate: </strong>", points$killed_low,
                          "<br><strong>Killed_high_estimate: </strong>", points$killed_high,
                          "<br><strong>wounded_low_estimate: </strong>", points$wounded_low,
                          "<br><strong>wounded_high_estimate: </strong>", points$wounded_high,
                          "<br><strong>wepon: </strong>", points$weapon_txt
                          
                          
             ))

saveWidget(x, file="Interactive_map_finale.html")
