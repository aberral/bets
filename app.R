# Server
pacman::p_load(shiny, shinyWidgets, DT)
source('scrapper.R')

# UI
ui <- fluidPage(
  # Titulo de la app. Strong lo pone en negrita
  titlePanel(h2("GAUGE")),
  sidebarLayout(
    sidebarPanel(
      img(height = 100, width = 100, 
          src = "gauge.png", class = "pull-right"),
      
      br(),
      p("Gauge is a bet-comparison app that uses scrapped data 
        from elcomparador.com pages."),
      br(),
      br(),
      br(),
      # Cuota de la apuesta
      textInput("cuota", 
                label = "Enter the desired share:",
                value = "25"),
      # Cuantas columnas se seleccionan
      pickerInput(
        inputId = "casapuesta", 
        label = "Select/deselect betting houses (at least 2)",
        selected = 1:12,
        choices = 1:12, 
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 12"
        ), 
        multiple = TRUE
      )
      ),
  mainPanel(
    textInput("url", 
              label = "Enter an Url:",
              value = "http://www.elcomparador.com"),
    column(12, 
           p("When exporting data only what is displayed will be exported. 
             Select the desired amount of entries to export."),
           DT::dataTableOutput("tbl")
    )
  )
),
div(class = "footer", includeHTML("footer.html"))
)


server <- function(input, output) {
  # You can access the value of the widget with input$text, e.g.
  # Data Matrix
  reactive_df <- reactive({
      url <- input$url
      if (url == "") {
        return(NULL)
      }
      else {
        return(scrapper(url))
      }
  })
  observeEvent(input$casapuesta,{
    cols <- as.numeric(input$casapuesta)
    output$tbl <- DT::renderDataTable(reactive_df()[,cols],
                                      extensions = c("Buttons"),
                                      options = list(dom = 'lBfrtip',
                                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                     initComplete = JS(
                                                       "function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                       "}")))}
)
observeEvent(input$cuota,{
  cols <- as.numeric(input$casapuesta)
  cuota <- as.numeric(input$cuota)
  data <- as.matrix(reactive_df()[,cols])
  # MAX where is it
  max <- apply(data, 1, FUN = function(x) max(x))
  max <- unname(max)
  max <- as.numeric(max)
  # Earnings
  casa <- rownames(data)
  casa <- strsplit(casa, '__')
  casa2 <- lapply(casa, FUN = function(x) tail(x, 1))
  casa2 <- unlist(casa2)
  
  i = 1
  earns <- NULL
  # We iterate through MAX vector
  while (i <= length(casa2)) {
    if (casa2[i] == '1') {
      val <- max[i]
      p1 <- max[i] * cuota - 25
      p2 <- p1 / max[i + 1]
      p3 <- p1 / max[i + 2]
      tot <- p1 - p2 - p3
      earns <- c(earns, tot)
      i = i + 1
      # browser()
    }
    if (casa2[i] == 'X') {
      val <- max[i]
      p1 <- max[i] * cuota - 25
      p2 <- p1 / max[i - 1]
      p3 <- p1 / max[i + 1]
      tot <- p1 - p2 - p3
      earns <- c(earns, tot)
      i = i + 1
    }
    else {
      val <- max[i]
      p1 <- max[i] * cuota - 25
      p2 <- p1 / max[i - 2]
      p3 <- p1 / max[i - 1]
      tot <- p1 - p2 - p3
      earns <- c(earns, tot)
      i = i + 1
    }
  }
  rm(i, p1, p2, p3, tot)
  #
  output$tbl <- DT::renderDataTable(cbind(reactive_df()[,cols], max, earns), 
                                    extensions = c("Buttons"),
                                    options = list(dom = 'lBfrtip',
                                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                   initComplete = JS(
                                                     "function(settings, json) {",
                                                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                     "}")))

})
}

shinyApp(ui, server)
#http://www.elcomparador.com


