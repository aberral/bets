# UI
fluidPage(
  
  # Copy the line below to make a text input box
  textInput("Url", label = h3("Introduce una url:"), 
            value = ""),
  hr(),
  fluidRow(
    column(3, 
           uiOutput("tbl")
           )
    )
)