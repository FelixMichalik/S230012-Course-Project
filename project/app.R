#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about buildig applications with Shiny here:
#
#    http://shiny.rstudio.com/
#jnjk

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinydashboard)
library(tmap)
library(sf)   
library(raster)
library(spData)
library(ggmap)
library(xml2)
library(rvest)

register_google(key = "AIzaSyA4D9aPj-qv-E2uJOZftIks39gfKV8hT4g")

team_cities <- c("Madrid, Spain", "Barcelona, Spain", "Madrid, Spain", "Bilbao, Spain", "Valencia, Spain")

#cities_coord <- geocode(team_cities, source = "google")

team <- c("Real Madrid", "FC Barcelona", "Atlético Madrid", "Athletic Bilbao", "Valencia")

n_champions <- c(33, 25, 10, 8, 6)

#cities_coord$Teams <- team
#cities_coord$Champions <- n_champions
#cities_coord$City <- team_cities
#is.data.frame(cities_coord)

#Webscrape music festival data
music_festivals = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/")

festivals <- music_festivals %>% 
  html_nodes(".mobile-one-whole") %>%
  html_text()

n = length(festivals)
festival_names = rep("", n)
festival_locations = rep("", n)
festival_dates = rep("", n)
festival_end_dates = rep("", n)
festival_cities_coord = rep(NA, n)

for (i in 1:n){ 
  festival_names[i] = gsub("\\,", "", (sub(".*\"\n  }\n},\n  \"name\": \"*(.*?) *\",\n  \"description\":.*", "\\1", festivals[i])))
  
  festival_locations[i] = paste(gsub("\\,", "", (sub(".*\"addressLocality\": \"*(.*?) *\",\n.*", "\\1", festivals[i]))), gsub("\\,", "", (sub(".*\",\n    \"addressRegion\": \" *(.*?) *\"\n  }\n},\n  \"name\":.*", "\\1", festivals[i]))))
  
  
  festival_dates[i] = gsub("\\,", "", (sub(".*\n \"startDate\": \"*(.*?) *\",\n \"endDate\":.*", "\\1", festivals[i])))
  
  festival_end_dates[i] = gsub("\\,", "", (sub(".*\"endDate\": \"*(.*?) *\"\n.*", "\\1", festivals[i])))
}

data_festival = geocode(festival_locations, source = "google")

data_festival$names =  festival_names
data_festival$locations = festival_locations
data_festival$dates = festival_dates

is.data.frame(data_festival)


#Get data on airports
data1 = read.csv("largeairports.csv")




# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    title = "Google Location Map",
                    dashboardHeader(title = "Google Location Map", titleWidth = 300),
                    
                    # interactive sidebar with menu and widgets
                    dashboardSidebar(width = 300,
                                     
                                     tags$div(
                                       tags$blockquote("Use this app to see where to party next!"),
                                       
                                       tags$p(checkboxInput("airports", "Airports", FALSE)),
                                       tags$p(checkboxInput("festivals", "Festivals", FALSE)),
                                       #tags$p(checkboxInput("country", "Switzerland", FALSE)),
                                       textInput(inputId = "country", label = "Country", value = "Type country here", width = NULL,
                                                 placeholder = NULL),
                                       tags$h4("Here we can explain what we did:"),
                                       tags$p("Visit ", tags$a(href="https://takeout.google.com/", "Google Takeout")," to see and download any of the data Google holds on you."),
                                       tags$p("Click on SELECT NONE, then scroll down to Location History and click on the slider to select it."),
                                       tags$p("Scroll to the bottom and click NEXT, then CREATE ARCHIVE, and finally DOWNLOAD when it is ready. You will need to verify by logging into your Google account."),
                                       tags$p("This will download a ZIP file to your downloads directory. Extract this ZIP file which will create a directory called Takeout"),
                                       tags$p("Upload the JSON file found in Takeout/Location History using the selector below..."),
                                       style = "padding: 10px;"
                                       
                                     )
                                     
                                     
                                     
                                     
                                     
                    ),
                    
                    # Main panel for displaying outputs ----
                    dashboardBody(
                      
                      tags$head(tags$style("#mymap{height:90vh !important;}")),
                      
                      leafletOutput(outputId = "mymap")
                      
                      
                      
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Default Maptile") %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
      setView(24, 27, zoom = 2) %>% 
      addLayersControl(
        baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
  
  observe({
    proxy <- leafletProxy("mymap", data = data)
    if (input$airports) {
      proxy %>% addMarkers(data = data1, popup=data1$name, clusterOptions = markerClusterOptions(), icon = makeIcon(iconUrl = "https://cdn4.iconfinder.com/data/icons/city-elements-colored-lineal-style/512/airportbuildingtravellingtransportaion-512.png", iconWidth = 35, iconHeight = 35))
      }
    else {
     proxy  %>% clearMarkerClusters()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data)
    if (input$festivals) {
      proxy %>%  addMarkers(data = data_festival, popup=data_festival$names, icon = makeIcon(iconUrl = "https://cdn2.iconfinder.com/data/icons/new-year-s-hand-drawn-basic/64/dancer_3-512.png", iconWidth = 35, iconHeight = 35))
    }
    else {
      proxy %>% clearMarkers()
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

