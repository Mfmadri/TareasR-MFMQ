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
  
  output$analisis_final_text <- renderText({
    "A lo largo de las últimas décadas, Costa Rica ha experimentado notables transformaciones en indicadores clave de salud y demografía. Desde 1960 hasta 2022, hemos sido testigos de un impresionante aumento del 34% en la esperanza de vida, pasando de 60 a 81 años. Este avance refleja mejoras significativas en la atención médica, condiciones de vida y otros factores que han contribuido al bienestar de la población.\n\nEn paralelo, la tasa de mortalidad ha experimentado una reducción del 53%, descendiendo de 10.232 a 4.8 por cada 1000 personas. Esta disminución evidencia los progresos en la atención médica, la prevención de enfermedades y la promoción de estilos de vida saludables.\n\nEl número de hijos por mujer ha disminuido notablemente en un 81%, pasando de 7 a 1.3. Esta transición demográfica refleja cambios en las actitudes hacia la planificación familiar y el acceso a métodos anticonceptivos, influyendo en la dinámica de las familias costarricenses.\n\nEn términos de población total, Costa Rica ha experimentado un impresionante aumento del 285%, superando los 5 millones de habitantes desde el millón que registraba en 1960.\n\nNo obstante, el panorama actual presenta desafíos importantes. En 2023, el 10% de la población total es mayor de 65 años, y aunque solo un 10% de este grupo reside en hogares de larga estancia, solo el 1% asiste a hogares diurnos. Es esencial mejorar las opciones de cuidado para la población adulta mayor.\n\nDatos del Ministerio de Salud revelan que las Enfermedades No Transmisibles (ENT) son la principal causa de morbilidad y mortalidad, representando el 80.73% de las defunciones en 2019. Las enfermedades cardiovasculares y el cáncer lideran estas estadísticas.\n\nEn 2022, más de 110,000 personas mayores de 65 años vivían en hogares unipersonales, siendo más de 13,000 mayores de 85 años. Esto destaca la necesidad de abordar la soledad y proporcionar apoyo adecuado a los ancianos.\n\nEste análisis nos llama a la acción como sociedad. Debemos continuar invirtiendo en sistemas de salud sólidos, programas de prevención y cuidado para la población adulta mayor, así como promover hábitos de vida saludables. También es crucial mejorar las opciones de cuidado y abordar las necesidades específicas de quienes envejecen solos.
    **Con Amor se despieden Fernanda Y Celia**"
  })
}
