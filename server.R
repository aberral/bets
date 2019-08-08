# Server
pacman::p_load(shiny, shinyWidgets, DT)

function(input, output) {
  
  # You can access the value of the widget with input$text, e.g.
  output$value <- renderPrint( {input$text })
  source('scrap.R')
  data <- scrapper(input$text)
  output$tbl = renderDT(
    data, options = list(lengthChange = FALSE)
  
}