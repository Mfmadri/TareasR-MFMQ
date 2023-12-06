library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(openxlsx)
library(readr)
library(waiter)



spotify_data <- read_delim("DATOS/spotify_2000_2023.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


unique_years <- sort(unique(spotify_data$year))  
unique_genres <- unique(spotify_data$`top genre`)


ui <- dashboardPage(
  dashboardHeader(title = "La cosa de Spotify"),
  dashboardSidebar(
    autoWaiter(),
    selectInput("year_filter", "Filtrar por Año:", choices = unique_years, selected = unique_years),
    selectInput("genre_filter", "Filtrar por Género:", choices = unique_genres, selected = NULL),
    downloadButton("download_btn", "Descargar Información Filtrada")
  ),
  dashboardBody(
    box(
      title = "La cosa de Canciones",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("scatter_plot")
    ),
    box(
      title = "La cosa de la cosa de Canciones",
      status = "primary",
      solidHeader = TRUE,
      DTOutput("filtered_table")
    )
  )
)


server <- function(input, output) {
  output$scatter_plot <- renderPlotly({
    filtered_data <- spotify_data |> 
      filter(year == as.numeric(input$year_filter)) |> 
      filter(`top genre` == input$genre_filter | is.null(input$genre_filter))
    
    
    plot_ly(filtered_data, 
            x = ~danceability, 
            y = ~popularity, 
            z = ~energy,  
            text = ~title,
            color = ~popularity,  
            colors = c("red", "orange", "green"),  
            type = "scatter3d", 
            mode = "markers") |> 
      layout(title = paste("Danceability, Popularity and Energy", input$year_filter, ")"),
             scene = list(
               xaxis = list(title = "Danceability"),
               yaxis = list(title = "Popularity"),
               zaxis = list(title = "Energy")
             ))
  })
  
  output$filtered_table <- renderDT({
    filtered_data <- spotify_data |> 
      filter(year == as.numeric(input$year_filter)) |> 
      filter(`top genre` == input$genre_filter | is.null(input$genre_filter))
    
    datatable(filtered_data, options = list(pageLength = 5))
  })
  
  output$download_btn <- downloadHandler(
    filename = function() {
      paste("filtered_data_", input$year_filter, ".xlsx", sep = "")
    },
    content = function(file) {
      filtered_data <- spotify_data |> 
        filter(year == as.numeric(input$year_filter)) |> 
        filter(`top genre` == input$genre_filter | is.null(input$genre_filter))
      
      write.xlsx(filtered_data, file, rowNames = FALSE)
    }
  )
}


shinyApp(ui, server)
