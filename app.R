# ----------------------------------------------------------
# app.R: Shiny + Leaflet (No shinydashboard) Multi-Tab App
# ----------------------------------------------------------

# Install packages if needed:
# install.packages(c("shiny", "leaflet", "dplyr", "DT", "leaflet.extras",
#                    "rnaturalearth", "rnaturalearthdata", "sf"))

library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(leaflet.extras)   # for draw toolbar & heatmap
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# ----------------------------------------------------------
# 1) DATA IMPORT
# ----------------------------------------------------------

# Update this path to your actual CSV file path
csv_path <- "C:/Users/jhaad/OneDrive/Documents/myLeafletApp/cleaned_earthquake_data.csv"

eq_data <- read.csv(csv_path, stringsAsFactors = FALSE)

# Filter out invalid or missing lat/lon
eq_data <- eq_data %>%
  filter(
    !is.na(latitude), !is.na(longitude),
    latitude >= -90,  latitude <=  90,
    longitude >= -180,longitude <= 180
  )

# (Optional) If you have year, month, day, combine into a date, if needed
# eq_data$datetime <- as.POSIXct(
#   paste(eq_data$year, eq_data$month, eq_data$day, sep="-"),
#   format = "%Y-%m-%d", tz = "UTC"
# )

# Ensure "severity" exists. If your CSV uses different words, adapt this.
# Also ensure magnitude, year, hemisphere columns exist or remove filters referencing them.

# ----------------------------------------------------------
# 2) FETCH WORLD POLYGONS AS SF (no sp usage)
# ----------------------------------------------------------
world_sf <- ne_countries(scale = "medium", returnclass = "sf")
# If you want bigger polygons, you can use scale="large" (slower to render)

# ----------------------------------------------------------
# 3) DEFINE UI
# ----------------------------------------------------------

ui <- navbarPage(
  title = "Earthquake Dashboard (No shinydashboard)",
  
  # ============== TAB 1: MAP DASHBOARD ==============
  tabPanel(
    "Map Dashboard",
    sidebarLayout(
      sidebarPanel(
        # Magnitude Range
        sliderInput(
          "magRange", "Select Magnitude Range:",
          min = floor(min(eq_data$magnitude, na.rm = TRUE)),
          max = ceiling(max(eq_data$magnitude, na.rm = TRUE)),
          value = c(
            floor(min(eq_data$magnitude, na.rm = TRUE)),
            ceiling(max(eq_data$magnitude, na.rm = TRUE))
          ),
          step = 0.1
        ),
        
        # Hemisphere Filter
        selectInput(
          "hemisphereInput", "Hemisphere:",
          choices = c("All", sort(unique(eq_data$hemisphere))),
          selected = "All"
        ),
        
        # Severity Filter (multi-select)
        selectInput(
          "severityInput", "Severity (multi-select):",
          choices  = sort(unique(eq_data$severity)),
          selected = sort(unique(eq_data$severity)),
          multiple = TRUE
        ),
        
        # Year Range
        sliderInput(
          "yearRange", "Select Year Range:",
          min = min(eq_data$year, na.rm = TRUE),
          max = max(eq_data$year, na.rm = TRUE),
          value = c(
            min(eq_data$year, na.rm = TRUE),
            max(eq_data$year, na.rm = TRUE)
          ),
          step = 1
        )
      ),
      mainPanel(
        leafletOutput("eqMap", height = 600)
      )
    )
  ),
  
  # ============== TAB 2: SUMMARY & TABLE ==============
  tabPanel(
    "Summary & Table",
    fluidRow(
      column(
        width = 6,
        h4("Events by Severity"),
        plotOutput("severityPlot", height = 300)
      ),
      column(
        width = 6,
        h4("Histogram of Magnitude"),
        plotOutput("magHistPlot", height = 300)
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 12,
        h4("Filtered Data Table"),
        DTOutput("eqTable")
      )
    )
  ),
  
  # ============== TAB 3: DRAW & HEATMAP ==============
  tabPanel(
    "Draw & Heatmap",
    leafletOutput("drawMap", height = 600),
    p("Use the drawing toolbar to add shapes. A heatmap of quake magnitudes is also shown.")
  ),
  
  # ============== TAB 4: POLYGON OVERLAY ==============
  tabPanel(
    "Polygon Overlay",
    leafletOutput("polygonMap", height = 600),
    p("Country boundaries from rnaturalearth (sf).")
  )
)

# ----------------------------------------------------------
# 4) DEFINE SERVER
# ----------------------------------------------------------
server <- function(input, output, session) {
  
  # ---- Reactive filter for the main map and summary ----
  filteredData <- reactive({
    data <- eq_data
    
    # Magnitude range
    data <- data %>%
      filter(
        magnitude >= input$magRange[1],
        magnitude <= input$magRange[2]
      )
    
    # Hemisphere
    if (input$hemisphereInput != "All") {
      data <- data %>% filter(hemisphere == input$hemisphereInput)
    }
    
    # Severity
    data <- data %>% filter(severity %in% input$severityInput)
    
    # Year range
    data <- data %>%
      filter(
        year >= input$yearRange[1],
        year <= input$yearRange[2]
      )
    
    data
  })
  
  # ============== MAP DASHBOARD ==============
  output$eqMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Create a colorFactor that covers all unique severities in the data
  severityPalette <- reactive({
    colorFactor(
      palette = c("green", "yellow", "orange", "red", "purple", "blue"),
      domain  = sort(unique(eq_data$severity)),  # covers all possible values
      na.color= "gray"
    )
  })
  
  observe({
    data <- filteredData()
    
    leafletProxy("eqMap", data = data) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~severityPalette()(severity),
        radius = ~ifelse(
          magnitude < 5, 4,
          ifelse(magnitude < 6, 6,
                 ifelse(magnitude < 7, 8, 10))
        ),
        fillOpacity = 0.7, stroke = FALSE,
        popup = ~paste0(
          "<b>Magnitude:</b> ", magnitude, "<br/>",
          "<b>Severity:</b> ", severity, "<br/>",
          "<b>Date (Y/M/D):</b> ", year, "/", month, "/", day, "<br/>",
          "<b>Depth (km):</b> ", round(depth, 2), "<br/>",
          "<b>Significance:</b> ", sig
        ),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # ============== SUMMARY & TABLE ==============
  output$severityPlot <- renderPlot({
    data <- filteredData()
    # Count by severity
    df <- data %>%
      group_by(severity) %>%
      summarize(count = n()) %>%
      arrange(severity)
    
    barplot(
      height    = df$count,
      names.arg = df$severity,
      col       = c("green","yellow","orange","red","purple","blue")[seq_along(df$severity)],
      main      = "Number of Events by Severity",
      xlab      = "Severity", ylab = "Count"
    )
  })
  
  output$magHistPlot <- renderPlot({
    data <- filteredData()
    hist(
      data$magnitude, breaks = 15,
      main  = "Histogram of Magnitudes",
      xlab  = "Magnitude",
      col   = "steelblue",
      border= "white"
    )
  })
  
  output$eqTable <- renderDT({
    data <- filteredData()
    datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ============== DRAW & HEATMAP ==============
  output$drawMap <- renderLeaflet({
    leaflet(data = eq_data) %>%
      addProviderTiles("OpenStreetMap") %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      
      addDrawToolbar(
        targetGroup        = "draw",
        polygonOptions     = TRUE,
        rectangleOptions   = TRUE,
        circleOptions      = TRUE,
        markerOptions      = TRUE,
        polylineOptions    = FALSE,
        circleMarkerOptions= FALSE,
        editOptions        = editToolbarOptions()
      ) %>%
      addLayersControl(
        overlayGroups = c("draw", "heatmap"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    leafletProxy("drawMap", data = eq_data) %>%
      clearHeatmap() %>%
      addHeatmap(
        lng       = ~longitude,
        lat       = ~latitude,
        intensity = ~magnitude,
        blur      = 20,
        max       = 0.05,
        radius    = 15,
        group     = "heatmap"
      )
  })
  
  # ============== POLYGON OVERLAY ==============
  output$polygonMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        data       = world_sf,    # sf object from rnaturalearth
        fillColor  = "lightblue",
        color      = "darkblue",
        weight     = 1,
        fillOpacity= 0.3,
        popup      = ~iso_a3
      )
  })
}

# ----------------------------------------------------------
# 5) RUN THE APP
# ----------------------------------------------------------
shinyApp(ui, server)
