#install.packages("shiny")
#packageVersion("shiny")
 
getwd()
setwd("C:/Users/HP/Desktop/app")
dir()

library(shiny)


ui <- fluidPage(
  textInput("name", "Cual es tu nombre?"),
  textOutput("greeting")
)
server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hola ", input$name, "!")
  })
}


shinyApp(ui, server)


