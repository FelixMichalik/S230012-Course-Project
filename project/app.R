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
library(htmltools)
library(spData)
library(tidyverse)
register_google(key = "AIzaSyA4D9aPj-qv-E2uJOZftIks39gfKV8hT4g")



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
data_festival$dates = as.Date(festival_dates, "%B %d %Y")

is.data.frame(data_festival)


#Get data on airports
data1 = read.csv("largeairports.csv")

#Get data on beer prices
data_beer = read.csv("beerprices.csv")
pal = colorNumeric(palette = "RdYlBu", domain = c(max(data_beer[,3]), min(data_beer[,3])))

#Get data on STI
sti_data = c("United Kingdom", "Denmark", "Sweden", "Iceland", "Norway")
sti_coord = geocode(sti_data, source = "google")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    title = "Party Index Map",
                    dashboardHeader(title = "Party Index Map", titleWidth = 300),
                    
                    # interactive sidebar with menu and widgets
                    dashboardSidebar(width = 300,
                                     tags$head(
                                       tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                    }"
                                       ) # close HTML       
                                       )            # close tags$style
                                     ),
                                     
                                     tags$div(
                                       tags$blockquote("Use this app to see where to party next!"),
                                       tags$p("Transportation"),
                                       tags$p(checkboxInput("airports", "Airports", FALSE)),
                                       tags$p(checkboxInput("uber", "Countries with Uber", FALSE)),
                                       tags$p("What's going on?"),
                                       tags$p(checkboxInput("festivals", "Festivals", FALSE)),
                                       
                                       #tags$p(checkboxInput("time", "Use time?", FALSE)),
                                       #tags$p(sliderInput(inputId = "dateRange", 
                                        #           label = "Date & time:", 
                                         #          min = min(data_festival$dates), max = max(data_festival$dates),
                                          #         value= c(min(data_festival$dates), max = max(data_festival$dates))
                                     #  )),
                                     tags$p(dateInput('minDate',
                                                      label = 'From:',
                                                      value = min(data_festival$dates)
                                     )),
                                       tags$p(dateInput('maxDate',
                                                  label = 'To:',
                                                     value = max(data_festival$dates)
                                       )),
                                       #tags$p(checkboxInput("country", "Switzerland", FALSE)),
                                     tags$p("Compare by"),
                                     tags$p(checkboxInput("prostitution", "Prostitution", FALSE)),
                                     tags$p(checkboxInput("weed", "Weed", FALSE)),
                                     tags$p(checkboxInput("sti", "Countries with highest propensity (at least 10% risk) of STI infection", FALSE)),
                                     tags$p("Compare beer prices:"),
                                     tags$p(checkboxInput("checkBeer", "Display beer index", FALSE)),
                                     sliderInput(inputId = "beer", "Beer price", min(data_beer$price)+0.9, max(data_beer$price), value = max(data_beer$price)),
                                       #textInput(inputId = "country", label = "Country", value = "Switzerland", width = NULL,
                                        #         placeholder = NULL),
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
  
  filteredData = reactive({
   
   a = filter(data_beer, price <= input$beer)
   qpal <- colorQuantile("Blues", a[,3], n = 7)
    a[,]
 })
 # filteredDatacol = reactive({
  #  a = filter(data_beer, price <= input$beer)
   # a
#  })
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Default Maptile") %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
      setView(24, 27, zoom = 2) %>% 
      #addPolygons(fill = TRUE)%>% 
      addLayersControl(
        baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
  
  observe({
    proxy <- leafletProxy("mymap", data = data1)
    if (input$airports) {
      proxy %>% addMarkers(data = data1, label = htmlEscape(data1$name), clusterOptions = markerClusterOptions(), icon = makeIcon(iconUrl = "https://cdn4.iconfinder.com/data/icons/city-elements-colored-lineal-style/512/airportbuildingtravellingtransportaion-512.png", iconWidth = 35, iconHeight = 35))
      }
    else {
     proxy  %>% clearMarkerClusters()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data_festival)
    if (input$festivals) {
      #fdata = data_festival[data_festival$dates %in% input$dateRange),]
      proxy %>% clearMarkers() %>%  addMarkers(data = data_festival[data_festival$dates %in% as.Date(input$minDate, "yyyy-mm-dd"):as.Date(input$maxDate, "yyyy-mm-dd"),], popup=paste(sep = "<br/>",
                                                                                                                                                                   data_festival$names,
                                                                                                                                                                   gsub(" ", ", ", data_festival$locations, fixed = TRUE),
                                                                                                                                                                   data_festival$dates), icon = makeIcon(iconUrl = "https://cdn2.iconfinder.com/data/icons/new-year-s-hand-drawn-basic/64/dancer_3-512.png", iconWidth = 35, iconHeight = 35))
    }
    
    if(input$sti){
      proxy %>% clearMarkers() %>%  addMarkers(data = sti_coord, icon = makeIcon(iconUrl = "https://cdn4.iconfinder.com/data/icons/happy-valentine-s-day-sex-shop/64/condom_sex_safety_contraception-512.png", iconWidth = 35, iconHeight = 35), label = htmlEscape("Stay safe ;)"))
    }
    
    if(!input$festivals && !input$sti) {
      proxy %>% clearMarkers()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = filteredData()) 
    if (input$checkBeer) {
    proxy %>% clearShapes() %>% clearControls() %>% 
      addPolygons(data = world[world$name_long %in% filteredData()[,1],][order(world[world$name_long %in% filteredData()[,1],]$name_long),], stroke = FALSE, fillColor = ~pal(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][order(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][,1]),3]) , label = htmlEscape(paste(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][order(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][,1]),3], "$US")))%>%  
      addLegend(position = "bottomright", title = "Beer Prices",
                pal = pal, values = ~filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][order(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][,1]),3])
      }else {
      proxy %>% clearShapes()}
  })
  

  #    for(i in 1:nrow(data_beer)){
   #     if(input$beer >= data_beer[i,3]){
 #   proxy %>% addPolygons(data = world[world$name_long == data_beer[i,1], ], fill = TRUE)
#  }}})
  
  observe({
    proxy <- leafletProxy("mymap", data = data) 
    if (input$prostitution) {
     # proxy %>% clearShapes() %>% addPolygons(data = world[world$name_long %in% c("Denmark", "Finland", "Belgium", "Germany", "Greece", "Italy", "Switzerland", "Netherlands", "Spain"),], fillColor = "green", label = htmlEscape("Prostitution is legal here"), stroke = FALSE) %>% 
      #  addPolygons(data = world[world$name_long %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Bosnia", "Bulgaria", "Croatia", "Cyprus", "Czechia",  "Estonia", "France", "Georgia", "Greece", "Hungary", "Iceland", "Ireland", "Kazakhstan", "Kosovo", "Latvia", " Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", " Montenegro", "Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Sweden", "Turkey", "Ukraine", "United Kingdom"),], fillColor = "red", label = htmlEscape("Prostitution is illegal here"), stroke = FALSE)
      proxy %>% clearShapes() %>%  
        addPolygons(data = world[((world$continent %in% c("Europe")) & !(world$name_long %in% c("Denmark", "Finland", "Belgium", "Germany", "Greece", "Italy", "Switzerland", "Netherlands", "Spain"))),], fillColor = "red", label = htmlEscape("Prostitution is legal here"), stroke = FALSE) %>%
        addPolygons(data = world[world$name_long %in% c("Denmark", "Finland", "Belgium", "Germany", "Greece", "Italy", "Switzerland", "Netherlands", "Spain"),], fillColor = "green", label = htmlEscape("Prostitution is legal here"), stroke = FALSE) 
         } 
    if(input$weed) {
      proxy %>% clearShapes() %>% addPolygons(data = world[((world$continent %in% c("Europe")) & !(world$name_long %in% c("Croatia", "Sweden", "Portugal", "Turkey", "Macedonia", "Finland", "Norway", "Poland", "Denmark", "Estonia", "Greece", "Switzerland", "Austria", "Italy", "Germany", "Belgium", "Netherlands"))),], fillColor = "red", label = htmlEscape("Weed is legal here"), stroke = FALSE) %>%
        addPolygons(data = world[world$name_long %in% c("Croatia", "Sweden", "Portugal", "Turkey", "Macedonia", "Finland", "Norway", "Poland", "Denmark", "Estonia", "Greece", "Switzerland", "Austria", "Italy", "Germany", "Belgium", "Netherlands"),], fillColor = "green", label = htmlEscape("Weed is legal here"), stroke = FALSE) 
    }
    if(input$uber) {
      proxy %>% clearShapes() %>% addPolygons(data = world[((world$continent %in% c("Europe")) & !(world$name_long %in% c("Austria", "Belgium", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Spain", "Sweden", "Switzerland", "United Kingdom"))),], fillColor = "red", label = htmlEscape("Consider taking a cab, cause there is no Uber here"), stroke = FALSE) %>%
        addPolygons(data = world[world$name_long %in% c("Austria", "Belgium", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Spain", "Sweden", "Switzerland", "United Kingdom"),], fillColor = "green", label = htmlEscape("There is Uber here!"), stroke = FALSE) 
    }
    if(input$sti){
      proxy %>% clearShapes() %>% addPolygons(data = world[world$name_long %in% sti_data,], fillColor = "red", stroke = FALSE) 
    }
    if(!input$prostitution && !input$weed && !input$uber && !input$sti) {
      proxy %>% clearShapes()}
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

