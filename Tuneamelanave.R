library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(openxlsx)
library(waiter)
library(sf)
library(leaflet)


historicos <- readxl::read_excel("DATOS/ProyectofinalR.xlsx", sheet = "Historicos")
Hogares <- read_excel("DATOS/ProyectofinalR.xlsx", sheet = "Hogares")
Proyecciones <- read_excel("DATOS/ProyectofinalR.xlsx", sheet = "proyecciones")
TamanosHogar <- read_excel("DATOS/ProyectofinalR.xlsx", sheet = "tamaño del hogar")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Radiografía ADM"),
  dashboardSidebar(
    autoWaiter(),
    sidebarMenu(
      menuItem("Historicos", tabName = "historicos", icon = icon("history")),
      menuItem("Hogares LE & CD", tabName = "hogares", icon = icon("home")),
      menuItem("Tamaño del Hogar", tabName = "tamano", icon = icon("arrows-h")),
      menuItem("Proyecciones de Población", tabName = "proyecciones", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet",
                        type = "text/css", 
                        href = "styles.css")),
    tabItems(
      tabItem(tabName = "historicos",
              fluidRow(
                box(
                  title = "Esperanza de Vida",
                  plotlyOutput("esperanza_plot")
                ),
                box(
                  title = "Tasa de Mortalidad",
                  plotlyOutput("mortalidad_plot")
                ),
                box(
                  title = "Tasa de Fertilidad",
                  plotlyOutput("fertilidad_plot")
                ),
                box(
                  title = "Población Total",
                  plotlyOutput("poblacion_plot")
                )
              )
      ),
      tabItem(tabName = "hogares",
              fluidRow(
                box(
                  title = "Mapa de Costa Rica",
                  leafletOutput("mapa_costa_rica", width = "100%", height = 500)
                ),
                box(
                  title = "Resumen de Hogares Diurnos",
                  dataTableOutput("tabla_resumen_hogares_Diurnos")
                ),
                box(
                  title = "Resumen de Hogares Larga Estancia",
                  dataTableOutput("tabla_resumen_hogares_Larga_Estancia")
                )
              )
      ),
      tabItem(tabName = "tamano",
              fluidRow(
                box(
                  title = "Personas por Tamaño del Hogar",
                  plotlyOutput("tamano_hogar_plot")
                ),
                box(
                  title = "Personas por Grupo de Edad",
                  plotlyOutput("edad_por_tamano_plot"),
                  selectInput("tamano_filtro", "Filtrar por tamaño del hogar:", choices = unique(TamanosHogar$`Tamaño del hogar`))
                )
              )
      ),
      tabItem(tabName = "proyecciones",
              fluidRow(
                box(
                  title = "Distribución de la Población por Grupos de Edad",
                  plotlyOutput("proyecciones_pie_chart")
                ),
                box(
                  title = "Razón de Dependencia (65+ / 15-64)",
                  valueBoxOutput("razon_dependencia_valuebox"),
                  selectInput("proyecciones_anio_filtro", "Filtrar por año:", choices = unique(Proyecciones$Años))
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$esperanza_plot <- renderPlotly({
    plot_ly(historicos, x = ~Años, y = ~`esperanza de vida`, type = 'scatter', mode = 'lines', name = 'Esperanza de Vida') %>%
      layout(
        showlegend = TRUE,
        colorway = "#A094C3"  # Color morado
      )
  })
  
  output$mortalidad_plot <- renderPlotly({
    plot_ly(historicos, x = ~Años, y = ~`Tasa de mortalidad (por cada 1000)`, type = 'scatter', mode = 'lines', name = 'Tasa de Mortalidad') %>%
      layout(
        showlegend = TRUE,
        colorway = "#A094C3"  # Color morado
      )
  })
  
  output$fertilidad_plot <- renderPlotly({
    plot_ly(historicos, x = ~Años, y = ~`Tasa de fertilidad (nacimientos por mujer)`, type = 'scatter', mode = 'lines', name = 'Tasa de Fertilidad') %>%
      layout(
        showlegend = TRUE,
        colorway = "#A094C3"  # Color morado
      )
  })
  
  output$poblacion_plot <- renderPlotly({
    plot_ly(historicos, x = ~Años, y = ~`Poblacion Total`, type = 'scatter', mode = 'lines', name = 'Población Total') %>%
      layout(
        showlegend = TRUE,
        colorway = "#A094C3"  # Color morado
      )
  })
  
  output$mapa_costa_rica <- renderLeaflet({
    leaflet(data = Hogares) |> 
      addTiles() |> 
      addMarkers(
        lng = ~as.numeric(Longitud),
        lat = ~as.numeric(Latitud),
        popup = ~paste("Provincia: ", Provincia, "<br>",
                       "Cantón: ", Cantón, "<br>",
                       "Hogares LE: ", `Hogares de larga estancia`, "<br>",
                       "Población LE: ", Ocupantes, "<br>",
                       "CD Público: ", `CD Público`, "<br>",
                       "CD Privado o semiprivado: ", `CD Privado o semiprivado`),
      )
  })
  
  output$tabla_resumen_hogares_Diurnos <- renderDataTable({
    resumen_provincia_CD <- Hogares %>%
      group_by(Provincia) %>%
      summarize(
        Total_CD_Publico = sum(`CD Público`),
        Total_CD_Privado = sum(`CD Privado o semiprivado`)
      )
    datatable(resumen_provincia_CD, options = list(lengthMenu = c(5, 10, 15), pageLength = 7))
  })
  
  output$tabla_resumen_hogares_Larga_Estancia <- renderDataTable({
    resumen_provincia_otros <- Hogares %>%
      group_by(Provincia) %>%
      summarize(
        Total_Hogares_LE = sum(as.numeric(`Hogares de larga estancia`)),
        Total_Poblacion = sum(`Poblacion +65`)
      )
    datatable(resumen_provincia_otros, options = list(lengthMenu = c(5, 10, 15), pageLength = 7))
  })
  
  output$tamano_hogar_plot <- renderPlotly({
    plot_ly(TamanosHogar, x = ~Año, y = ~`de 65 a 69 años` + `de 70 a 74 años` + `de 75 a 79 años` + `de 80 a 84 años` + `de 85 a 89 años` + `de 90 a 94 años` + `de 95 a 97 años`,
            color = ~`Tamaño del hogar`, type = 'scatter', mode = 'stack', fill = 'tonexty',
            hoverinfo = 'text',
            text = ~paste('Tamaño del hogar: ', `Tamaño del hogar`, '<br>Cantidad de adultos mayores: ', sum(`de 65 a 69 años` + `de 70 a 74 años` + `de 75 a 79 años` + `de 80 a 84 años` + `de 85 a 89 años` + `de 90 a 94 años` + `de 95 a 97 años`)))
  })
  
  output$edad_por_tamano_plot <- renderPlotly({
    filtered_data <- TamanosHogar %>%
      filter(`Tamaño del hogar` == input$tamano_filtro)
    
    plot_ly(filtered_data, x = ~Año,
            y = ~`de 65 a 69 años`, color = ~`de 65 a 69 años`,
            type = 'scatter', mode = 'lines', line = list(width = 2),
            hoverinfo = 'text',
            text = ~paste('Tamaño del hogar: ', `Tamaño del hogar`, '<br>Grupo de edad: de 65 a 69 años')) %>%
      add_trace(y = ~`de 70 a 74 años`, color = ~`de 70 a 74 años`,
                type = 'scatter', mode = 'lines', line = list(width = 2),
                hoverinfo = 'text',
                text = ~paste('Tamaño del hogar: ', `Tamaño del hogar`, '<br>Grupo de edad: de 70 a 74 años')) %>%
      add_trace(y = ~`de 75 a 79 años`, color = ~`de 75 a 79 años`,
                type = 'scatter', mode = 'lines', line = list(width = 2),
                hoverinfo = 'text',
                text = ~paste('Tamaño del hogar: ', `Tamaño del hogar`, '<br>Grupo de edad: de 75 a 79 años')) %>%
      add_trace(y = ~`de 80 a 84 años`, color = ~`de 80 a 84 años`,
                type = 'scatter', mode = 'lines', line = list(width = 2),
                hoverinfo = 'text',
                text = ~paste('Tamaño del hogar: ', `Tamaño del hogar`, '<br>Grupo de edad: de 80 a 84 años')) %>%
      add_trace(y = ~`de 85 a 89 años`, color = ~`de 85 a 89 años`,
                type = 'scatter', mode = 'lines', line = list(width = 2),
                hoverinfo = 'text',
                text = ~paste('Tamaño del hogar: ', `Tamaño del hogar`, '<br>Grupo de edad: de 85 a 89 años')) %>%
      add_trace(y = ~`de 90 a 94 años`, color = ~`de 90 a 94 años`,
                type = 'scatter', mode = 'lines', line = list(width = 2),
                hoverinfo = 'text',
                text = ~paste('Tamaño del hogar: ', `Tamaño del hogar`, '<br>Grupo de edad: de 90 a 94 años')) %>%
      add_trace(y = ~`de 95 a 97 años`, color = ~`de 95 a 97 años`,
                type = 'scatter', mode = 'lines', line = list(width = 2),
                hoverinfo = 'text',
                text = ~paste('Tamaño del hogar: ', `Tamaño del hogar`, '<br>Grupo de edad: de 95 a 97 años'))
  })
  
  output$proyecciones_pie_chart <- renderPlotly({
    filtered_proyecciones <- Proyecciones %>%
      filter(Años == input$proyecciones_anio_filtro)
    
    labels <- c("0 a 14", "15 a 64", "65 a más de 100")
    values <- c(filtered_proyecciones$`0 a 14`, filtered_proyecciones$`15 a 64`, filtered_proyecciones$`65 a 100 y más`)
    
    # Paleta de colores en tonos de morado
    colores_morados <- c("#5E4FA2", "#807DBA", "#A094C3")
    
    # Crear el gráfico de pie con la nueva paleta de colores
    plot_ly(labels = labels, values = values, type = "pie", hole = 0.6) %>%
      layout(
        title = "Distribución de la Población por Grupos de Edad",
        showlegend = TRUE,
        legend = list(x = 0.5, y = 1),
        margin = list(l = 0, r = 0, b = 0, t = 30),
        colorway = colores_morados  # Establecer la paleta de colores
      )
  })
  
  output$razon_dependencia_valuebox <- renderValueBox({
    filtered_proyecciones <- Proyecciones %>%
      filter(Años == input$proyecciones_anio_filtro)
    
    razon_dependencia <- (filtered_proyecciones$`65 a 100 y más` / filtered_proyecciones$`15 a 64`) * 100
    
    valueBox(
      value = sprintf("%.2f%%", razon_dependencia),
      subtitle = "Razón de Dependencia",
      icon = icon("users"),
      color = "purple"
    )
  })
}

shinyApp(ui, server)