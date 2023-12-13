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
                        menuItem("Proyecciones de Población", tabName = "proyecciones", icon = icon("chart-line")),
                        menuItem("Análisis Final", tabName = "analisis_final", icon = icon("info"))
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
                        ),
                        tabItem(tabName = "analisis_final",
                                fluidRow(
                                  box(
                                    title = "Análisis Final",
                                    textOutput("analisis_final_text")
                                  )
                                )
                        )
                      )
                    )
)