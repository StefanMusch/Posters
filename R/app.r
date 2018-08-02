###==============libraries
library(ggplot2)
library(ggmap)
library(tidyverse)
library(shiny)
library(shinythemes)



#==========UI=============
ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Map Poster Creator"),
  sidebarLayout(
    sidebarPanel(
      h2("Customizer"),
      actionButton("button", "Create your poster!"),
      helpText("Customize your map by adjusting the values."),
      h3("Map Type"),
      selectInput(inputId = "maptype",
                  label = NULL,
                  choices = c("Terrain" ="terrain",
                              "Terrain Background" = "terrain-background",
                              "Toner" = "toner",
                              "Toner Background" = "toner-background",
                              "Watercolour" = "watercolor"),
                  selected = "Toner"),
      h3("Color or Black & White"),
      radioButtons("colortype", NULL, 
                   choices = c("Color" = "color",
                               "Black & White" = "bw"),
                   selected = "bw"),
      h3("Inverted Colors?"),
      radioButtons("inverted", NULL,
                   choices = c("Yes" = "inverted",
                               "No" = "not-inverted"),
                   selected = "not-inverted"),
      h3("Map Title"),
      textInput("map_title", NULL, width = "300px", value = "Amsterdam"),
      h3("Destination Color"),
      textInput("dest_color", NULL, width = "200px", value = "DarkRed"),
      h3("Path Color"),
      textInput("path_color", NULL, width = "200px", value = "DarkRed"),
      h3("Location Dot Size"),
      sliderInput("dot_size", NULL, value = 3, min = 1, max = 10, ticks = F),
      h3("Zoom level"),
      sliderInput("zoom", NULL, value = 7, min = 1, max = 18, ticks = F),
      h3("Central Location"),
      textInput("central_loc", NULL, width = "200px", value = "Amsterdam NL"),
      h3("Route"),
      helpText("Your Route in steps. Divide with comma's."),
      textInput("route", NULL, width = "400px", value = "Amsterdam NL, Rotterdam NL, Breda NL, Antwerp, Tilburg NL, Amsterdam NL")
      
    ),
    mainPanel(
      plotOutput("plot", width = "1000px", height = "1000px")
    )
  )
)


#==========Server=============
server <- function(input, output) {
  
    observeEvent(input$button, {
        output$plot <- renderPlot({
        isolate({
            #make sure the call to google's API works
            loc <- NA
            
            while(is.na(loc)) {
              loc <- geocode(input$central_loc)
            } 
            
            map <-get_map(c(loc$lon, loc$lat), zoom = input$zoom,
                                     source='stamen',maptype= input$maptype, color = input$colortype, crop = TRUE)
            
            if(input$inverted == "inverted"){
              invert <- function(x) rgb(t(255-col2rgb(x))/255)    
              new_map <- as.raster(apply(map, 2, invert))
              
              # copy attributes from original object
              class(new_map) <- class(map)
              attr(new_map, "bb") <- attr(map, "bb")
              
              map <- new_map
            }else{}
        
            city_names <- 
              str_split(input$route, ",")[[1]] %>%
              str_trim(.,side = "left")
            
            
            cities <- tibble(    
                city = city_names,    
                lon = rep(NA, length(city_names)),    
                lat = rep(NA, length(city_names))  
              )
            
            # loop cities through API to overcome SQ limit
            for(c in city_names){
              temp <- tibble(lon = NA)
              # geolocate until found
              while(is.na(temp$lon)) {
                temp <- geocode(c)
              } 
              # write to dataframe
              cities[cities$city == c, -1] <- temp
            }
            
          
            cities_trek <- cities %>%
              mutate(from = lag(city),
                     to = city ) %>%
              select(from, to)
            
            treklist <- list()
            for (i in 1:nrow(cities_trek)){
              trek_df <- trek(from = cities_trek$from[i], to = cities_trek$to[i], structure = "route")
              treklist[[i]] <- trek_df
            }
            
            tracks <- do.call(rbind, treklist)
            
            
            ggmap(map) + 
              geom_point(data=cities, aes(lon, lat), alpha = 1, fill = input$dest_color, size = input$dot_size)+
              geom_point(data=cities, aes(lon, lat), alpha = 0.4, fill = input$dest_color, size = input$dot_size+3)+
              geom_point(data=cities, aes(lon, lat), alpha = 0.2, fill = input$dest_color, size = input$dot_size+5)+
              geom_path(data=tracks, aes(lon,lat), color=input$path_color, size= 1.5, alpha = 0.5, linetype = "dotted" )+
              labs(caption = input$map_title)+
              guides(alpha = FALSE, 
                     size = FALSE)+
              scale_size_continuous(range = c(1,3))+
              theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),
                    plot.caption = element_text(hjust=0.5, size=rel(2)),
                    panel.border = element_rect(colour = "black", fill=NA, size=2))
        })
      })
    })
}

#==========RUN=============
shinyApp(ui = ui, server = server)