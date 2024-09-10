# Load the shiny package
library(shiny)

# Define the UI
ui <- fluidPage(
  # App title
  titlePanel("Basic Shiny App Example"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Text input for user's name
      textInput("name", "Enter your name:", "Shiny User"),
      
      # Input: Slider for the number of bins
      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = 30)
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: A personalized greeting
      textOutput("greeting"),
      
      # Output: Histogram
      plotOutput("distPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Greeting based on user input
  output$greeting <- renderText({
    paste("Hello,", input$name, "!")
  })
  
  # Generate a plot of the requested number of bins
  output$distPlot <- renderPlot({
    # Generate some random data
    x <- faithful$eruptions
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # Draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         main = "Histogram of Old Faithful Eruptions",
         xlab = "Eruption duration (minutes)")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
