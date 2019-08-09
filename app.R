# Server
pacman::p_load(shiny, shinyWidgets, DT)
source('scrapper.R')

# UI
ui <- fluidPage(
  # Titulo de la app. Strong lo pone en negrita
  titlePanel("GAUGE"),
  sidebarLayout(
    sidebarPanel(
      img(height = 100, width = 100, 
          src = "gauge.png", class = "pull-right"),
      br(),
      p("Gauge is a bet-comparison app that uses scrapped data from elcomparador.com pages."),
      br(),
      br(),
      br()
      ),
  mainPanel(
    textInput("url", 
              label = "Enter an Url:",
              value = ""),
    column(12, 
           p("When exporting data only what is displayed will be exported. Select the desired amount of entries to export."),
           DT::dataTableOutput("tbl")
    )
  )
),
div(class = "footer", includeHTML("footer.html"))
)


server <- function(input, output) {
  # You can access the value of the widget with input$text, e.g.
  reactive_df <- reactive({
      url <- input$url
      if (url == "") {
        return(NULL)
      }
      else {
        return(scrapper(url))
      }
  })
  output$tbl <- DT::renderDataTable(reactive_df(), 
extensions = 'Buttons',
options = list(
  dom = 'lBfrtip',
  buttons = c('colvis', 'csv', 'excel', 'pdf'),
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}")
))
}

shinyApp(ui, server)
#http://www.elcomparador.com


