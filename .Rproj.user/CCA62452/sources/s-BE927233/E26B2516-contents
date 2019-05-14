#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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

