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
music_festivals2 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/2/")
music_festivals2children <- xml_children(music_festivals2)
for (child in music_festivals2children) {
  xml_add_child(music_festivals, child)
}

music_festivals3 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/3/")
music_festivals3children <- xml_children(music_festivals3)
for (child in music_festivals3children) {
  xml_add_child(music_festivals, child)
}

music_festivals4 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/4/")
music_festivals4children <- xml_children(music_festivals3)
for (child in music_festivals4children) {
  xml_add_child(music_festivals, child)
}

music_festivals5 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/5/")
music_festivals5children <- xml_children(music_festivals3)
for (child in music_festivals5children) {
  xml_add_child(music_festivals, child)
}

music_festivals6 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/6/")
music_festivals6children <- xml_children(music_festivals3)
for (child in music_festivals6children) {
  xml_add_child(music_festivals, child)
}

music_festivals7 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/7/")
music_festivals7children <- xml_children(music_festivals3)
for (child in music_festivals7children) {
  xml_add_child(music_festivals, child)
}

music_festivals8 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/8/")
music_festivals8children <- xml_children(music_festivals3)
for (child in music_festivals8children) {
  xml_add_child(music_festivals, child)
}
music_festivals9 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/9/")
music_festivals9children <- xml_children(music_festivals3)
for (child in music_festivals9children) {
  xml_add_child(music_festivals, child)
}
music_festivals10 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/10/")
music_festivals10children <- xml_children(music_festivals3)
for (child in music_festivals10children) {
  xml_add_child(music_festivals, child)
}

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


#create Data Frame

f_names <- music_festivals %>%
  html_nodes("h2") %>%
  html_text()
f_locations <- music_festivals %>%
  html_nodes(".search-meta") %>%
  html_text()

f_countries <- rep(NA, length(f_names))
for (i in 1:length(f_names)) {
  f_locations2 = strsplit(f_locations, split = "\n           ", fixed = T)[[i]]
  f_country = strsplit(f_locations2, split = ", ", fixed = T)[[2]]
  f_countries[i] = f_country[2]
}
rm(f_locations, f_locations2, f_country, f_country2)

f_prostitution <- rep(NA, length(f_names))
prostitution_legal <- c("Denmark", "Finland", "Belgium", "Germany", "Greece", "Italy", "Switzerland", "Netherlands", "The Netherlands", "Spain")
for (j in 1:length(f_names)) {
  if (f_countries[j] %in% prostitution_legal ) {
    f_prostitution[j] = "Yes"
  }else{
    f_prostitution[j] = "No"
  } 
} 
rm(prostitution_legal)

f_weed <- rep(NA, length(f_names))
weed_legal <- c("Croatia", "Sweden", "Portugal", "Turkey", "Macedonia", "Finland", "Norway", "Poland", "Denmark", "Estonia", "Greece", "Switzerland", "Austria", "Italy", "Germany", "Belgium", "Netherlands", "The Netherlands")
for (k in 1:length(f_names)) {
  if (f_countries[k] %in% weed_legal ) {
    f_weed[k] = "Yes"
  }else{
    f_weed[k] = "No"
  } 
}  
rm(weed_legal)

f_uber <- rep(NA, length(f_names))
is_there_uber <- c("Austria", "Belgium", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Netherlands", "The Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Spain", "Sweden", "Switzerland", "United Kingdom")
for (l in 1:length(f_names)) {
  if (f_countries[l] %in% is_there_uber ) {
    f_uber[l] = "Yes"
  }else{
    f_uber[l] = "No"
  } 
}  
rm(is_there_uber)

f_sti <- rep(NA, length(f_names))
high_risk <- c("United Kingdom", "Denmark", "Sweden", "Iceland", "Norway")
for (m in 1:length(f_names)) {
  if (f_countries[m] %in% high_risk ) {
    f_sti[m] = "High risk! (>10%)"
  }else{
    f_sti[m] = "All good (<10%)"
  } 
}  
rm(high_risk)

f_beer <- rep(5.67, length(f_names))
prices_beer <- data_beer
for (n in 1:length(f_names)) {
  for(z in 1:177){
    if (f_countries[n] == prices_beer[,1][z]) {
      f_beer[n] = prices_beer[,3][z]
    }
  }
} 

compiled <- data.frame("Country" = f_countries, "Festival" = f_names, "Average price of beer" = f_beer, "Is weed legal?" = f_weed, "Is prostitution legal?" = f_prostitution, "Do they have Uber?" = f_uber, "Propensity of STI infection" = f_sti)


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
                                     #sidebarMenu(
                                     
                                     tags$div(
                                      tags$blockquote("Use this app to see where to party next!"),
                                       tags$b("Display:"),
                                       style = "padding-left: 10px;"),
                                       checkboxInput("airports", "Airports", FALSE),
                                       checkboxInput("sti", "Countries with highest propensity (at least 10% risk) of STI infection", FALSE),
                                       
                                       tags$div(tags$b("What's going on?"),style = "padding-left: 10px;"),
                                       
                                       checkboxInput("festivals", "Festivals", FALSE), 
                                     tags$div(
                                       tags$p(dateInput('minDate',
                                                      label = 'From:',
                                                      value = min(data_festival$dates)
                                     ),
                                       dateInput('maxDate',
                                                  label = 'To:',
                                                     value = max(data_festival$dates)
                                       )), style = "padding-left: 30px;"),
                                       #tags$p(checkboxInput("country", "Switzerland", FALSE)),
                                     
                                     tags$div(tags$b("Compare countries by"), style = "padding-left: 10px;"),
                                     tabsetPanel(
                                       tabPanel("Other", checkboxInput("uber", "Is Uber available?", FALSE), checkboxInput("prostitution", "Is prostitution legal?", FALSE), checkboxInput("weed", "Is weed legal?", FALSE)),
                                       tabPanel("Beer Prices",checkboxInput("checkBeer", "Display beer prices", FALSE), sliderInput(inputId = "beer", "Select price range", min(data_beer$price)+0.9, max(data_beer$price), value = max(data_beer$price)))
                                     ),
                                     #tags$hr(),
                                     
                                     #tags$p(checkboxInput("prostitution", "Prostitution", FALSE)),
                                     #tags$p(checkboxInput("weed", "Weed", FALSE)),
                                     
                                     #selectInput("abc", "ABC", c("a", "b","c"), selected = NULL),
                                     
                                     
                                     #tags$p("Compare beer prices:"),
                                     #tags$p(checkboxInput("checkBeer", "Display beer index", FALSE)),
                                    
                                       #textInput(inputId = "country", label = "Country", value = "Switzerland", width = NULL,
                                        #         placeholder = NULL),
                                     selectInput("inCountry", "Select a country", choices = compiled$Country),
                                     
                                     tags$div(
                                       tags$h4("Here we can explain what we did:"),
                                       tags$p("For a full overview over our data please see our", tags$a(href="https://github.com/FelixMichalik/project/blob/master/README.md", "README file"),"."),
                                       tags$p("Click on SELECT NONE, then scroll down to Location History and click on the slider to select it."),
                                       tags$p("Scroll to the bottom and click NEXT, then CREATE ARCHIVE, and finally DOWNLOAD when it is ready. You will need to verify by logging into your Google account."),
                                       tags$p("This will download a ZIP file to your downloads directory. Extract this ZIP file which will create a directory called Takeout"),
                                       tags$p("Upload the JSON file found in Takeout/Location History using the selector below..."),
                                     style = "padding: 10px;")
                                   #  ) 
                               
                    ),
                    
                    # Main panel for displaying outputs ----
                    dashboardBody(
                      
                      tags$head(tags$style("#mymap{height:90vh !important;}")),
                      
                      leafletOutput(outputId = "mymap"),
                      tableOutput("countryData")
                      
                      
                      
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filteredData = reactive({
   
   a = filter(data_beer, price <= input$beer)
   
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
  #create the Table
  output$countryData <- renderTable({
    stateFilter <-subset(compiled, compiled$Country == input$inCountry)
  })
  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.
  
  observe({
    proxy <- leafletProxy("mymap", data = data1)
    if (input$airports) {
      proxy %>% 
        addMarkers(data = data1, label = htmlEscape(data1$name), clusterOptions = markerClusterOptions(), icon = makeIcon(iconUrl = "https://cdn4.iconfinder.com/data/icons/city-elements-colored-lineal-style/512/airportbuildingtravellingtransportaion-512.png", iconWidth = 35, iconHeight = 35))
      }
    else {
     proxy  %>% clearMarkerClusters()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data_festival)
   proxy %>% clearMarkers()
   if(!input$sti || !input$sti){
     proxy %>% clearMarkers()
   }
    if (input$festivals){
      #fdata = data_festival[data_festival$dates %in% input$dateRange),]
      proxy %>%  addMarkers(data = data_festival[data_festival$dates %in% as.Date(input$minDate, "yyyy-mm-dd"):as.Date(input$maxDate, "yyyy-mm-dd"),], popup=paste(sep = "<br/>",
                                                                                                                                                                   data_festival$names,
                                                                                                                                                                   gsub(" ", ", ", data_festival$locations, fixed = TRUE),
                                                                                                                                                                   data_festival$dates), icon = makeIcon(iconUrl = "https://cdn2.iconfinder.com/data/icons/new-year-s-hand-drawn-basic/64/dancer_3-512.png", iconWidth = 35, iconHeight = 35))
    }
   if(input$sti){
     proxy %>% addMarkers(data = sti_coord, icon = makeIcon(iconUrl = "https://cdn4.iconfinder.com/data/icons/happy-valentine-s-day-sex-shop/64/condom_sex_safety_contraception-512.png", iconWidth = 45, iconHeight = 45), label = htmlEscape("Stay safe ;)"))
   }
     
 
     
     # if(!input$festivals && !input$sti) {
   
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = filteredData()) 
    if (input$checkBeer) {
    proxy %>% clearShapes() %>% clearControls() %>% 
      addPolygons(data = world[world$name_long %in% filteredData()[,1],][order(world[world$name_long %in% filteredData()[,1],]$name_long),], stroke = FALSE, fillColor = ~pal(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][order(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][,1]),3]) , label = htmlEscape(paste(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][order(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][,1]),3], "$US")))%>%  
      addLegend(position = "bottomright", title = "Beer Prices",
                pal = pal, values = ~filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][order(filteredData()[filteredData()[,1] %in% world[world$name_long %in% filteredData()[,1],]$name_long,][,1]),3])
      }else {
      proxy %>% clearShapes() %>% clearControls()}
  })
  

  #    for(i in 1:nrow(data_beer)){
   #     if(input$beer >= data_beer[i,3]){
 #   proxy %>% addPolygons(data = world[world$name_long == data_beer[i,1], ], fill = TRUE)
#  }}})
  
  observe({
    proxy <- leafletProxy("mymap", data = data) 
    if(!input$prostitution || !input$weed || !input$uber || !input$sti) {
      proxy %>% clearShapes() %>% clearControls()}
    if (input$prostitution) {
       proxy %>%  
        addPolygons(data = world[((world$continent %in% c("Europe")) & !(world$name_long %in% c("Denmark", "Finland", "Belgium", "Germany", "Greece", "Italy", "Switzerland", "Netherlands", "Spain"))),], fillColor = "red", label = htmlEscape("Prostitution is illegal here"), stroke = FALSE) %>%
        addPolygons(data = world[world$name_long %in% c("Denmark", "Finland", "Belgium", "Germany", "Greece", "Italy", "Switzerland", "Netherlands", "Spain"),], fillColor = "green", label = htmlEscape("Prostitution is legal here"), stroke = FALSE) 
         } 
    if(input$weed) {
      proxy %>%   addPolygons(data = world[((world$continent %in% c("Europe")) & !(world$name_long %in% c("Croatia", "Sweden", "Portugal", "Turkey", "Macedonia", "Finland", "Norway", "Poland", "Denmark", "Estonia", "Greece", "Switzerland", "Austria", "Italy", "Germany", "Belgium", "Netherlands"))),], fillColor = "red", label = htmlEscape("Weed is illegal here"), stroke = FALSE) %>%
        addPolygons(data = world[world$name_long %in% c("Croatia", "Sweden", "Portugal", "Turkey", "Macedonia", "Finland", "Norway", "Poland", "Denmark", "Estonia", "Greece", "Switzerland", "Austria", "Italy", "Germany", "Belgium", "Netherlands"),], fillColor = "green", label = htmlEscape("Weed is legal here"), stroke = FALSE) 
    }
    if(input$uber) {
      proxy %>%   addPolygons(data = world[((world$continent %in% c("Europe")) & !(world$name_long %in% c("Austria", "Belgium", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Spain", "Sweden", "Switzerland", "United Kingdom"))),], fillColor = "red", label = htmlEscape("Consider taking a cab, cause there is no Uber here"), stroke = FALSE) %>%
        addPolygons(data = world[world$name_long %in% c("Austria", "Belgium", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Spain", "Sweden", "Switzerland", "United Kingdom"),], fillColor = "green", label = htmlEscape("There is Uber here!"), stroke = FALSE) 
    }
    if(input$sti){
      proxy %>%   addPolygons(data = world[world$name_long %in% sti_data,], fillColor = "grey", stroke = FALSE) 
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
