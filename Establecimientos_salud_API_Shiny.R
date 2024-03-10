library(shiny)
library(leaflet)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(DT)

# Obtener datos de la API
url <- "https://datos.gob.cl/api/3/action/datastore_search?resource_id=2c44d782-3365-44e3-aefb-2c8b8363a1bc&limit=17000"
response <- GET(url)
content <- content(response, "text")
data <- fromJSON(content)
establecimientos_nacional <- data$result$records

options(digits = 10)
# Reemplazar las comas con puntos y eliminar los espacios en blanco
establecimientos_nacional$LatitudGlosa <- gsub(",", ".", establecimientos_nacional$LatitudGlosa)
establecimientos_nacional$LongitudGlosa <- gsub(",", ".", establecimientos_nacional$LongitudGlosa)
# Convertir las columnas de longitud y latitud a números
establecimientos_nacional$LatitudGlosa <- as.numeric(establecimientos_nacional$LatitudGlosa)
establecimientos_nacional$LongitudGlosa <- as.numeric(establecimientos_nacional$LongitudGlosa)
colnames(establecimientos_nacional)[colnames(establecimientos_nacional) == "Nombre Oficial"] <- "NombreOficial"


# Ordenar los datos por nombre de región, servicio de salud y tipo de establecimiento
establecimientos_nacional <- establecimientos_nacional %>%
  arrange(RegionGlosa, SeremiSaludGlosa_ServicioDeSaludGlosa, TipoEstablecimientoGlosa)

# Definir paleta de colores
color_palette <- c("orange", "blue", "green", "red", "purple", "yellow", "cyan", "magenta", "brown", "gray")

# Definir UI
ui <- fluidPage(
  titlePanel("Mapa de Establecimientos de Salud"),
  fluidRow(
    column(4,
           uiOutput("region_ui")),
    column(4,
           uiOutput("servicio_salud_ui")),
    column(4,
           uiOutput("tipo_establecimiento_ui"))
  ),
  fluidRow(
    column(4,
           plotlyOutput("plot")),
    column(4,
           leafletOutput("mapa")),
    column(4,
           dataTableOutput("table"))
  ),
  tags$style(HTML("
    .dataTables_wrapper .dataTables_filter {
      display: none;
    }
    .dataTables_wrapper .dataTables_length {
      display: none;
    }
  "))
  
  
)

# Definir server
server <- function(input, output, session) {
  
  # Filtro de regiones
  output$region_ui <- renderUI({
    if (input$servicio_salud != "Todas") {
      regiones <- unique(establecimientos_nacional$RegionGlosa[establecimientos_nacional$SeremiSaludGlosa_ServicioDeSaludGlosa == input$servicio_salud])
    } else {
      regiones <- unique(establecimientos_nacional$RegionGlosa)
    }
    pickerInput("region", "Región:",
                choices = c("Todas", sort(regiones)),
                selected = "Todas")
  })
  
  # Filtro de servicio de salud según la región seleccionada
  output$servicio_salud_ui <- renderUI({
    servicios <- unique(establecimientos_nacional$SeremiSaludGlosa_ServicioDeSaludGlosa)
    selected_servicio <- if ("Servicio de Salud Metropolitano Oriente" %in% servicios) {
      "Servicio de Salud Metropolitano Oriente"
    } else {
      "Todas"
    }
    pickerInput("servicio_salud", "Servicio de Salud:",
                choices = c("Todas", sort(servicios)),
                selected = selected_servicio)
  })
  
  # Filtro de tipo de establecimiento según la región y/o servicio de salud seleccionados
  output$tipo_establecimiento_ui <- renderUI({
    tipos <- unique(establecimientos_nacional$TipoEstablecimientoGlosa[
      if (input$region == "Todas" & input$servicio_salud == "Todas") TRUE else 
        (establecimientos_nacional$RegionGlosa == input$region | input$region == "Todas") &
        (establecimientos_nacional$SeremiSaludGlosa_ServicioDeSaludGlosa == input$servicio_salud | input$servicio_salud == "Todas")])
    pickerInput("tipo_establecimiento", "Tipo de Establecimiento:",
                choices = c("Todas", sort(tipos)),
                selected = "Todas")
  })
  
  outputOptions(output, "region_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "servicio_salud_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "tipo_establecimiento_ui", suspendWhenHidden = FALSE)
  
  output$mapa <- renderLeaflet({
    filtered_data <- establecimientos_nacional %>%
      filter((input$region == "Todas" | RegionGlosa == input$region) &
               (input$servicio_salud == "Todas" | SeremiSaludGlosa_ServicioDeSaludGlosa == input$servicio_salud) &
               (input$tipo_establecimiento == "Todas" | TipoEstablecimientoGlosa == input$tipo_establecimiento))
    
    # Asignar un color diferente a cada nivel de atención
    colors <- color_palette[as.numeric(factor(filtered_data$NivelAtencionEstabglosa))]
    
    leaflet(data = filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(~LongitudGlosa, ~LatitudGlosa,
                       color = colors,
                       popup = ~paste("<b>Establecimiento:</b> ", `NombreOficial`, "<br>",
                                      "<b>Dependencia:</b> ", SeremiSaludGlosa_ServicioDeSaludGlosa, "<br>",
                                      "<b>Nivel de atención:</b> ", NivelAtencionEstabglosa, "<br>",
                                      "<b>Comuna:</b> ", ComunaGlosa)
                       
      )
  })
  
  output$table <- DT::renderDataTable({
    filtered_data <- establecimientos_nacional %>%
      filter((input$region == "Todas" | RegionGlosa == input$region) &
               (input$servicio_salud == "Todas" | SeremiSaludGlosa_ServicioDeSaludGlosa == input$servicio_salud) &
               (input$tipo_establecimiento == "Todas" | TipoEstablecimientoGlosa == input$tipo_establecimiento))
    
    summarised_data <- filtered_data %>%
      group_by(TipoEstablecimientoGlosa) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    DT::datatable(summarised_data,
                  options = list(
                    searching = FALSE,
                    lengthMenu = c(8, 20, 30)
                  ),
                  rownames = FALSE)
  })
  
  
  
  
  
  
  
  
  output$plot <- renderPlotly({
    filtered_data <- establecimientos_nacional %>%
      filter((input$region == "Todas" | RegionGlosa == input$region) &
               (input$servicio_salud == "Todas" | SeremiSaludGlosa_ServicioDeSaludGlosa == input$servicio_salud) &
               (input$tipo_establecimiento == "Todas" | TipoEstablecimientoGlosa == input$tipo_establecimiento))
    
    
    filtered_data %>%
      group_by(NivelAtencionEstabglosa) %>%
      summarise(count = n()) %>%
      plot_ly(labels = ~NivelAtencionEstabglosa, values = ~count, type = 'pie', marker = list(colors = color_palette)) %>%
      layout(title = "Distribución de Niveles de Atención")
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)


