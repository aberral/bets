# Server
pacman::p_load(shiny, shinyWidgets, DT)
source('scrap.R')

function(input, output) {
  # You can access the value of the widget with input$text, e.g.
  output$value <- reactive( {
    renderPrint( {input$text })
  })
  output$tbl <- renderTable({
    data <- scrapper(as.character(input$text))
    browser()
    return(data)}
  )
}