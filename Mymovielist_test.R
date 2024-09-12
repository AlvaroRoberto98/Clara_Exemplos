library(shiny)
library(bslib)
library(DT)

# Define UI
ui <- fluidPage(
  # Apply a theme using bslib
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # Title and layout
  titlePanel("MyMovieList"),
  sidebarLayout(
    sidebarPanel(
      textInput("movie_name", "Movie Name", ""),
      textInput("movie_genre", "Genre", ""),
      numericInput("movie_rating", "Rating (out of 10)", 5, min = 1, max = 10),
      textInput("movie_image", "Image URL", ""),
      actionButton("add_btn", "Add Movie"),
      actionButton("edit_btn", "Edit Selected Movie"),
      br(),
      br(),
      actionButton("remove_btn", "Remove Selected Movie")
    ),
    
    # Main panel for displaying the movie list
    mainPanel(
      DTOutput("movie_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the movie list
  movies <- reactiveVal(data.frame(
    Name = character(),
    Genre = character(),
    Rating = numeric(),
    Image = character(),
    stringsAsFactors = FALSE
  ))
  
  # Add a movie when the 'Add Movie' button is clicked
  observeEvent(input$add_btn, {
    new_movie <- data.frame(
      Name = input$movie_name,
      Genre = input$movie_genre,
      Rating = input$movie_rating,
      # OBs.: caso Queira mudar a image padrão (jpg ou gif), trocar o link abaixo.
      Image = ifelse(input$movie_image != "", 
                     paste0('<img src="', input$movie_image, '" height="100">'), 
                     '<img src="https://media1.tenor.com/images/99ad5c42627f7733aaf5af3dc700c7f7/tenor.gif?itemid=15963112" height="100">'),
      stringsAsFactors = FALSE
    )
    movies(rbind(movies(), new_movie))
    
    # Clear inputs
    updateTextInput(session, "movie_name", value = "")
    updateTextInput(session, "movie_genre", value = "")
    updateNumericInput(session, "movie_rating", value = 5)
    updateTextInput(session, "movie_image", value = "")
  })
  
  # Edit the selected movie
  observeEvent(input$edit_btn, {
    selected_row <- input$movie_table_rows_selected
    if (length(selected_row) == 1){
      movie_list <- movies()
      movie_list[selected_row, ] <- data.frame(
        Name = input$movie_name,
        Genre = input$movie_genre,
        Rating = input$movie_rating,
        Image = ifelse(input$movie_image != "", 
                       paste0('<img src="', input$movie_image, '" height="100">'), 
                       '<img src="https://media1.tenor.com/images/99ad5c42627f7733aaf5af3dc700c7f7/tenor.gif?itemid=15963112" height="100">'),
        stringsAsFactors = FALSE
      )
      movies(movie_list)  # Update reactive value with edited movie
    }
  })
  
  # Remove selected movie when the 'Remove Movie' button is clicked
  observeEvent(input$remove_btn, {
    selected_row <- input$movie_table_rows_selected
    if (length(selected_row) > 0) {
      movie_list <- movies()
      movie_list <- movie_list[-selected_row,]
      movies(movie_list)
    }
  })
  
  # Render the movie list as a DataTable
  output$movie_table <- renderDT({
    datatable(movies(), selection = 'single', escape = FALSE, 
              options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # Populate the input fields when a row is selected
  observeEvent(input$movie_table_rows_selected, {
    selected_row <- input$movie_table_rows_selected
    if (length(selected_row) == 1) {
      movie <- movies()[selected_row, ]
      updateTextInput(session, "movie_name", value = movie$Name)
      updateTextInput(session, "movie_genre", value = movie$Genre)
      updateNumericInput(session, "movie_rating", value = movie$Rating)
      # Extract the URL from the img tag for editing
      image_url <- gsub('.*src="([^"]*)".*', '\\1', movie$Image)
      updateTextInput(session, "movie_image", value = image_url)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
