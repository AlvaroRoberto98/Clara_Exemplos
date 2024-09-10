library(shiny)
library(shinyWidgets)

# UI part
ui <- fluidPage(
  titlePanel("Reactive vs Observe Example"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num", "Enter a number:", value = 1, min = 1, max = 10),
      actionButton("multiply", "Multiply by 2"),
      textOutput("reactiveText"),
      textOutput("observeText")
    ),
    mainPanel(
      h3("Outputs will update based on the button click")
    )
  )
)

# Server part
server <- function(input, output, session) {
  
  # Reactive expression that tracks the input number
  reactiveNumber <- reactive({
    input$num
  })
  
  # Observer that triggers a side effect (printing to the console)
  observe({
    cat("Reactive number is:", reactiveNumber(), "\n")
  })
  
  # Reactive expression that multiplies the number by 2
  multipliedValue <- reactive({
    input$multiply # Triggered by button press
    isolate(reactiveNumber() * 2) # Isolate to prevent reactivity before button click
  })
  
  # Output reactive result to UI
  output$reactiveText <- renderText({
    paste("Reactive value (multiplied):", multipliedValue())
  })
  
  # Observer that outputs a message when the button is clicked
  observeEvent(input$multiply, {
    output$observeText <- renderText({
      paste("Observer: Button clicked, number is", reactiveNumber())
    })
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
