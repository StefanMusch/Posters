library(ggplot2)
library(ggmap)
library(tidyverse)
library(dplyr)


### Create a list of maps to create plot outputs
toner_map <- get_map(c(-4.18265, 56.816922), zoom = 7,
                                   source='stamen',maptype="toner-background", color = "bw", crop = TRUE)

## Inverted toner map
# invert colors in raster
invert <- function(x) rgb(t(255-col2rgb(x))/255)    
toner_map_inv <- as.raster(apply(toner_map, 2, invert))

# copy attributes from original object
class(toner_map_inv) <- class(toner_map)
attr(toner_map_inv, "bb") <- attr(toner_map, "bb")

#Terrain map
terrain_map <- get_map(c(-4.18265, 56.816922), zoom = 7,
                       source='stamen',maptype="terrain", color = "color", crop = TRUE)

#Terrain map BG
terrain_map_bg <- get_map(c(-4.18265, 56.816922), zoom = 7,
                       source='stamen',maptype="terrain-background", color = "color", crop = TRUE)

#List of all the maps
maplist <- list(terrain_map, terrain_map_bg, toner_map, toner_map_inv)
names(maplist)  <- c("terrain_map", "terrain_bg", "toner", "toner_inv")
  
### cities data ----------------------------------------------------------------

# cities to geolocate
city_name <- c("Newcastle UK", "St. Abbs UK", "Edinburgh", "Ballachulish UK", "Glencoe UK", "Fort William UK","Ben Nevis UK", "Kyle of Lochalsh UK", "Broadford UK", "Portree UK" , "Old Man of Storr UK", "Portree UK", "Fort Augustus UK", "Dalwhinnie UK", "Perth UK", "Edinburgh", "Melrose UK", "Newcastle UK") 

# initialize dataframe  
tibble(    
  city = city_name,    
  lon = rep(NA, length(city_name)),    
  lat = rep(NA, length(city_name))  
) ->
  cities

# loop cities through API to overcome SQ limit
for(c in city_name){
  temp <- tibble(lon = NA)
  # geolocate until found
  while(is.na(temp$lon)) {
    temp <- geocode(c)
  } 
  # write to dataframe
  cities[cities$city == c, -1] <- temp
}

###=============track route==================
cities_trek <- cities %>%
  mutate(from = lag(city),
         to = city ) %>%
  select(from, to) %>%
  filter(from != "NA")

treklist <- list()
for (i in 1:nrow(cities_trek)){
  trek_df <- trek(from = cities_trek$from[i], to = cities_trek$to[i], structure = "route", mode = "driving", output = "simple")
  treklist[[i]] <- trek_df
}

tracks <- do.call(rbind, treklist)


###==============Map creation====================


for (i in 1: length(maplist)){
 p <-  ggmap(maplist[[i]]) + 
  geom_point(data=cities, aes(lon, lat), alpha = 1, color = "DarkRed", size = 2)+
  geom_point(data=cities, aes(lon, lat), alpha = 0.7, color = "DarkRed", size = 2.5)+
  geom_point(data=cities, aes(lon, lat), alpha = 0.4, color = "DarkRed", size = 4.5)+
  geom_path(data=tracks, aes(lon,lat), color="red", size=0.6, alpha = 0.5, linetype = "dotted" )+
  labs(caption = "Scotland '17")+
  guides(alpha = FALSE, 
         size = FALSE)+
  scale_size_continuous(range = c(1,3))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.caption = element_text(hjust=0.5, size=rel(1.5), vjust = -0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=2))
 
  ggsave(filename = paste0("Images/", names(maplist[i]), "_map.png"), plot = p,
         width = 6.7, height = 7, units = 'cm',
         scale = 2, dpi = 600)
}
