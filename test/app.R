#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
rm(f_locations, f_locations2, f_country)

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
prices_beer <- read.csv("beerprices.csv")
for (n in 1:length(f_names)) {
  for(z in 1:177){
    if (f_countries[n] == prices_beer$country[z]) {
      f_beer[n] = prices_beer$price[z]
    }
  }
} 

compiled <- data.frame("Country" = f_countries, "Festival" = f_names, "Average price of beer" = f_beer, "Is weed legal?" = f_weed, "Is prostitution legal?" = f_prostitution, "Do they have Uber?" = f_uber, "Propensity of STI infection" = f_sti)

#ui
ui <- fluidPage(
  titlePanel("Search for info by country here:"),
  sidebarLayout(
    sidebarPanel(
      selectInput("inCountry", "Select a country", choices = compiled$Country)
    ),
    mainPanel(
      tableOutput("countryData")
    )
  )
)

#server
server <- shinyServer(function(input, output){
  output$countryData <- renderTable({
    stateFilter <-subset(compiled, compiled$Country == input$inCountry)
  })
})


# Run the application 
shinyApp(ui = ui, server = server)

