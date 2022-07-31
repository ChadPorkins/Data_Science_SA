library(tidyverse)
library(mapview)
library(sf)
library(ggplot2)
library(ggmap)
kaboom2 = read.csv("kaboom2.csv")
kaboom2 = kaboom2[!is.na(kaboom2$latitude), ]
kaboom2 = kaboom2[!is.na(kaboom2$longitude), ]
kaboom3 = st_as_sf(kaboom2, coords = c("longitude", "latitude"),crs  = 4326)
mapview(kaboom3)


kaboom4 = ggplot(kaboom2, aes(x = longitude, y = latitude)) + 
  geom_point() + 
  coord_equal() + 
  xlab('longitude') + 
  ylab('latitude')



 kaboom5 = ggplot(kaboom2, aes(x = longitude, y = latitude)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('longitude') + 
  ylab('latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = 100,
                 geom = "polygon", data = kaboom2) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none') 
  
 
   
   
   
##################
   world <- map_data("world")
   ggplot() +
     geom_map(
       data = world, map = world,
       aes(long, lat, map_id = region),
       color = "black", fill = "lightgray", size = 0.1
     ) +
     geom_point(
       data = kaboom2,
       aes(longitude, latitude),
       alpha = 0.7
     ) 
   ######################3
   ggplot(kaboom2, aes(x = longitude, y = latitude)) + 
     geom_point() + 
     coord_equal() + 
     xlab('longitude') + 
     ylab('latitude') + 
     stat_density2d(aes(fill = ..level..), alpha = 100,
                    geom = "polygon", data = kaboom2) + 
     scale_fill_viridis_c() + 
     theme(legend.position = 'none') 
     
   ##############################
   ggplot(kaboom2, aes(x = longitude, y = latitude)) + 
     geom_point() + 
     coord_equal() + 
     xlab('longitude') + 
     ylab('latitude')
   