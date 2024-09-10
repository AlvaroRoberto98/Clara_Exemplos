library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Reactive Example with Table and Buttons"),
  sidebarLayout(
    sidebarPanel(
      actionButton("increment", "Increment Count"),
      actionButton("decrement", "Decrement Count")
    ),
    mainPanel(
      tableOutput("item_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Create a reactiveVal to store the counts
  counts <- reactiveVal(c(5, 10, 15))  # Initial counts for 3 items
  
  # Create a reactive expression for the items data frame
  items <- reactive({
    data.frame(
      Item = c("Item A", "Item B", "Item C"),
      Count = counts(),
      stringsAsFactors = FALSE
    )
  })
  
  # Observe the counts and update the table when they change
  output$item_table <- renderTable({
    items()
  })
  
  # Observe button clicks and increment or decrement the count
  observeEvent(input$increment, {
    current_counts <- counts()
    current_counts[1] <- current_counts[1] + 1  # Increment first item as an example
    counts(current_counts)
  })
  
  observeEvent(input$decrement, {
    current_counts <- counts()
    current_counts[1] <- max(current_counts[1] - 1, 0)  # Decrement first item, ensure no negative values
    counts(current_counts)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
