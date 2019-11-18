#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about buildig applications with Shiny here:
#
#    http://shiny.rstudio.com/
#jnjk

#Needed packages (ATTENTION:This part of the code would only need to be run once, we leave it here so a new user can run easily the code)
install.packages("shiny")
install.packages("leaflet")
install.packages("dplyr")
install.packages("leaflet.extras")
install.packages("shinydashboard")
install.packages("tmap")
install.packages("sf")   
install.packages("raster")
install.packages("spData")
install.packages("ggmap")
install.packages("xml2")
install.packages("rvest")
install.packages("htmltools")
install.packages("spData")
install.packages("tidyverse")

#libraries used
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


register_google(key = [instert key])


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
music_festivals4children <- xml_children(music_festivals4)
for (child in music_festivals4children) {
  xml_add_child(music_festivals, child)
}

music_festivals5 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/5/")
music_festivals5children <- xml_children(music_festivals5)
for (child in music_festivals5children) {
  xml_add_child(music_festivals, child)
}

music_festivals6 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/6/")
music_festivals6children <- xml_children(music_festivals6)
for (child in music_festivals6children) {
  xml_add_child(music_festivals, child)
}

music_festivals7 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/7/")
music_festivals7children <- xml_children(music_festivals7)
for (child in music_festivals7children) {
  xml_add_child(music_festivals, child)
}

music_festivals8 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/8/")
music_festivals8children <- xml_children(music_festivals8)
for (child in music_festivals8children) {
  xml_add_child(music_festivals, child)
}
music_festivals9 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/9/")
music_festivals9children <- xml_children(music_festivals9)
for (child in music_festivals9children) {
  xml_add_child(music_festivals, child)
}
music_festivals10 = read_html("https://www.musicfestivalwizard.com/festival-guide/europe-festivals/page/10/")
music_festivals10children <- xml_children(music_festivals10)
for (child in music_festivals10children) {
  xml_add_child(music_festivals, child)
}



festivals <- music_festivals %>% 
  html_nodes(".mobile-one-whole") %>%
  html_text()

n = length(festivals)
festival_names = rep("", n)
festival_locations = rep("", n)
festival_country = rep("", n)
festival_dates = rep("", n)
festival_end_dates = rep("", n)
festival_cities_coord = rep(NA, n)

for (i in 1:n){ 
  festival_names[i] = gsub("\\,", "", (sub(".*\"\n  }\n},\n  \"name\": \"*(.*?) *\",\n  \"description\":.*", "\\1", festivals[i])))
  
  festival_locations[i] = paste(gsub("\\,", "", (sub(".*\"addressLocality\": \"*(.*?) *\",\n.*", "\\1", festivals[i]))), gsub("\\,", "", (sub(".*\",\n    \"addressRegion\": \" *(.*?) *\"\n  }\n},\n  \"name\":.*", "\\1", festivals[i]))))
  
  festival_country[i] = gsub("\\,", "", (sub(".*\",\n    \"addressRegion\": \" *(.*?) *\"\n  }\n},\n  \"name\":.*", "\\1", festivals[i])))
  
  festival_dates[i] = gsub("\\,", "", (sub(".*\n \"startDate\": \"*(.*?) *\",\n \"endDate\":.*", "\\1", festivals[i])))
  
  festival_end_dates[i] = gsub("\\,", "", (sub(".*\"endDate\": \"*(.*?) *\"\n.*", "\\1", festivals[i])))
}

data_festival = geocode(festival_locations, source = "google")

data_festival$names =  festival_names
data_festival$locations = festival_locations
data_festival$country = festival_country
data_festival$dates = as.Date(festival_dates, "%B %d %Y")

is.data.frame(data_festival)

#Get data on airports
data1 = read.csv("largeairports.csv") #when our app goes online we would upload this dataset

#Get data on beer prices
data_beer = read.csv("beerprices.csv") #when our app goes online we would upload this dataset
pal = colorNumeric(palette = "RdYlBu", domain = c(max(data_beer[,3]), min(data_beer[,3])))  #set colors for beer prices

#Get data on STI
sti_data = c("United Kingdom", "Denmark", "Sweden", "Iceland", "Norway")
sti_coord = geocode(sti_data, source = "google")


#create Data Frame
#including categories listed 

f_names <- data_festival$names
f_locations <- data_festival$country

f_dates <- rep(NA, length(f_names))
for (u in 1:length(f_names)) {
  f_dates[u] = toString(data_festival$dates[u])
}

f_prostitution <- rep(NA, length(f_names))
prostitution_legal <- c("Denmark", "Finland", "Belgium", "Germany", "Greece", "Italy", "Switzerland", "Netherlands", "The Netherlands", "Spain")
for (j in 1:length(f_names)) {
  if (f_locations[j] %in% prostitution_legal) {
    f_prostitution[j] = "Yes"
  }else{
    f_prostitution[j] = "No"
  } 
} 
rm(prostitution_legal)

f_weed <- rep(NA, length(f_names))
weed_legal <- c("Croatia", "Sweden", "Portugal", "Turkey", "Macedonia", "Finland", "Norway", "Poland", "Denmark", "Estonia", "Greece", "Switzerland", "Austria", "Italy", "Germany", "Belgium", "Netherlands", "The Netherlands")
for (k in 1:length(f_names)) {
  if (f_locations[k] %in% weed_legal ) {
    f_weed[k] = "Yes"
  }else{
    f_weed[k] = "No"
  } 
}  
rm(weed_legal)

f_uber <- rep(NA, length(f_names))
is_there_uber <- c("Austria", "Belgium", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Netherlands", "The Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Spain", "Sweden", "Switzerland", "United Kingdom")
for (l in 1:length(f_names)) {
  if (f_locations[l] %in% is_there_uber ) {
    f_uber[l] = "Yes"
  }else{
    f_uber[l] = "No"
  } 
}  
rm(is_there_uber)

f_sti <- rep(NA, length(f_names))
high_risk <- c("United Kingdom", "Denmark", "Sweden", "Iceland", "Norway")
for (m in 1:length(f_names)) {
  if (f_locations[m] %in% high_risk ) {
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
    if (f_locations[n] == prices_beer[z,1]) {
      f_beer[n] = prices_beer[z,3]
    }
  }
} 

compiled2 <- data.frame(Country = f_locations, Festival = f_names, "When" = f_dates, "Average price of beer" = f_beer, "Is weed legal?" = f_weed, "Is prostitution legal?" = f_prostitution, "Do they have Uber?" = f_uber, "Propensity of STI" = f_sti)
compiled <- compiled2[order(compiled2$Country),]

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    title = "Party Index Map",
                    dashboardHeader(title = "Party Index Map", titleWidth = 300),
                    
                    # interactive sidebar with menu and widgets
                    dashboardSidebar(width = 300,
                                     tags$head(
                                       tags$style(HTML(".sidebar {
                      height: 90vh; width: 300px; overflow-y: auto; position: fixed;
                    }"
                                       )      
                                       )    
                                     ),
                                    
                                     #sidebarMenu(
                                     tags$div(
                                      tags$blockquote("Use this app to see where to party next!"),
                                      tags$div(
                                        tags$h4("WELCOME PARTY TRAVELERS, TO YOUR ULTIMATE GUIDE IN NAVIGATING THE WORLD OF SEX, DRUGS, AND ROCK AND ROLL..ALL OVER EUROPE!"),
                                        tags$p("THE PARTY INDEX WAS CREATED TO SATISFY THAT CURIOSITY MANY OF US HAVE, BUT ARE TOO SHY TO ASK, WHEN TRYING TO DECIDE WHERE TO GO AND WHAT TO DO FOR THE NEXT BIG TRIP."),
                                        tags$p("ALL OF THE INFORMATION WE PROVIDE IS OPEN SOURCED AND PUBLICLY DISCLOSED ONLINE. FOR A FULL OVERVIEW AND SOURCES SEE OUR", tags$a(href="https://github.com/FelixMichalik/project/blob/master/README.md", "README FILE")),
                                        tags$p("THROUGH OUR APP, WE GIVE YOU THE POWER TO MAKE THE SAFEST AND MOST RESPONSIBLE DECISIONS WHEN IT COMES TO HOW TO SPEND YOUR TIME IN ANOTHER COUNTRY."),
                                        tags$p("BE SURE TO CHECK OUT ALL OF THE CATEGORIES DISPLAYED, HOVER YOUR MOUSE OVER OUR INTERACTIVE MAP."),
                                        tags$p("PARTY ON!"),
                                        style = "padding: 10px;"),
                                      tags$hr(),
                                      tags$div(tags$blockquote("Already know where you will party next?")),
                                      tags$div(tags$b("Select your destination and get a summary of all the relevant information underneath the map"),style = "padding-left: 10px;"),
                                      selectInput("inCountry", "Select a country", choices = compiled$Country),
                                      tags$br(),
                                      
                                      tags$div(tags$blockquote("If not, why not explore our map?")),
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
                                      tags$br(),
                                      tags$b("Not sure how to get there?"), style = "padding-left: 10px;"),
                                     checkboxInput("airports", "Show Airports", FALSE),
                                     tags$br(),
                                      tags$div(tags$b("Compare countries by:"),tags$br(), style = "padding-left: 10px;"),
                                      tabsetPanel(
                                        tabPanel("Other", checkboxInput("uber", "Is Uber available?", FALSE), checkboxInput("prostitution", "Is prostitution legal?", FALSE), checkboxInput("weed", "Is weed legal?", FALSE), checkboxInput("sti", "Countries with highest propensity (at least 10% risk) of STI infection", FALSE)),
                                        tabPanel("Beer Prices",checkboxInput("checkBeer", "Display beer prices", FALSE), sliderInput(inputId = "beer", "Select price range", min(data_beer$price)+0.9, max(data_beer$price), value = max(data_beer$price)))
                                      )
                     ),
                    
                    
                    # Main panel for displaying outputs 
                    dashboardBody(
                      
                      tags$head(tags$style("#mymap{height:90vh !important;}")),
                      
                      leafletOutput(outputId = "mymap"),
                      tableOutput("countryData")
                      
                      
                      
                    )
)

# Define server
server <- function(input, output, session) {
  
  # make data imputs dynamic
  filteredData = reactive({
   
   a = filter(data_beer, price <= input$beer)
   
    a[,]
 })
  
  table_data = reactive({
    
    compiled$Country == input$inCountry
  })
 
  
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
  
  #create the Table
  output$countryData <- renderTable({
    stateFilter <-subset(compiled, table_data())
  })
  
  #use the observe function to make the inputs dynamic. 
  
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
    proxy %>%  addMarkers(data = data_festival[as.Date(data_festival$dates, "%Y-%m-%d") %in% seq(from = as.Date(input$minDate, "%Y-%m-%d"), to = as.Date(input$maxDate, "%Y-%m-%d"), by = "day"),], popup=paste(sep = "<br/>",
                                                                                                                                                                                                                data_festival[as.Date(data_festival$dates, "%Y-%m-%d") %in% seq(from = as.Date(input$minDate, "%Y-%m-%d"), to = as.Date(input$maxDate, "%Y-%m-%d"), by = "day"),]$names,
                                                                                                                                                                 gsub(" ", ", ", data_festival[as.Date(data_festival$dates, "%Y-%m-%d") %in% seq(from = as.Date(input$minDate, "%Y-%m-%d"), to = as.Date(input$maxDate, "%Y-%m-%d"), by = "day"),]$locations, fixed = TRUE),
                                                                                                                                                                 data_festival[as.Date(data_festival$dates, "%Y-%m-%d") %in% seq(from = as.Date(input$minDate, "%Y-%m-%d"), to = as.Date(input$maxDate, "%Y-%m-%d"), by = "day"),]$dates), icon = makeIcon(iconUrl = "https://cdn2.iconfinder.com/data/icons/new-year-s-hand-drawn-basic/64/dancer_3-512.png", iconWidth = 35, iconHeight = 35))
  }
   if(input$sti){
     proxy %>% addMarkers(data = sti_coord, icon = makeIcon(iconUrl = "https://cdn4.iconfinder.com/data/icons/happy-valentine-s-day-sex-shop/64/condom_sex_safety_contraception-512.png", iconWidth = 45, iconHeight = 45), label = htmlEscape("Stay safe ;)"))
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
      proxy %>% clearShapes() %>% clearControls()}
  })
  

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
