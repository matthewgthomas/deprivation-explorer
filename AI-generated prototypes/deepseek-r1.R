# Load required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(IMD)

# IMD for Local Authority Districts (LAD)
imd_england_lad <- IMD::imd_england_lad |>
  left_join(
    lookup_ltla21_region21 |>
      select(lad_code = ltla21_code, region_name = region21_name)
  )

# IMD for Neighbourhoods (LSOA)
imd_england_lsoa <- IMD::imd_england_lsoa |>
  left_join(
    lookup_lsoa11_ltla21 |>
      select(lsoa_code = lsoa11_code, lad_code = ltla21_code)
  )

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Deprivation Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regional Deprivation", tabName = "regional", icon = icon("map")),
      menuItem("Neighbourhood Deprivation", tabName = "neighbourhood", icon = icon("chart-bar")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("line-chart")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "regional",
        h2("Regional Deprivation Analysis"),
        fluidRow(
          box(leafletOutput("regional_map"), width = 12)
        ),
        fluidRow(
          box(plotlyOutput("regional_bar"), width = 12)
        )
      ),

      tabItem(
        tabName = "neighbourhood",
        h2("Neighbourhood Deprivation Analysis"),
        fluidRow(
          box(leafletOutput("neighbourhood_map"), width = 12)
        ),
        fluidRow(
          box(DTOutput("neighbourhood_table"), width = 12)
        )
      ),

      tabItem(
        tabName = "correlation",
        h2("Correlation Analysis"),
        fluidRow(
          box(selectInput("x_var", "X-axis Variable", choices = names(imd_england_lad)[3:19]), width = 6),
          box(selectInput("y_var", "Y-axis Variable", choices = names(imd_england_lad)[3:19]), width = 6)
        ),
        fluidRow(
          box(plotlyOutput("correlation_plot"), width = 12)
        )
      ),

      tabItem(
        tabName = "data",
        h2("Data Table"),
        fluidRow(
          box(DTOutput("data_table"), width = 12)
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$regional_map <- renderLeaflet({
    # Create a choropleth map (replace with actual map data)
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = imd_england_lad, fillColor = ~colorQuantile("YlOrRd", Score)(Score), color = "black", weight = 1)
  })

  output$regional_bar <- renderPlotly({
    # Create a bar chart of regional deprivation scores
    regional_summary <- imd_england_lad %>%
      group_by(region_name) %>%
      summarise(Average_Score = mean(Score, na.rm = TRUE))

    ggplotly(
      ggplot(regional_summary, aes(x = reorder(region_name, -Average_Score), y = Average_Score, fill = region_name)) +
        geom_bar(stat = "identity") +
        labs(x = "Region", y = "Average Deprivation Score", title = "Regional Deprivation Scores") +
        theme_minimal()
    )
  })

  output$neighbourhood_map <- renderLeaflet({
    # Create a choropleth map for neighbourhoods (replace with actual map data)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = imd_england_lsoa, lng = ~longitude, lat = ~latitude, radius = 3, color = ~colorQuantile("YlOrRd", IMD_decile)(IMD_decile))
  })

  output$neighbourhood_table <- renderDT({
    # Display an interactive table of neighbourhood data
    datatable(imd_england_lsoa, options = list(pageLength = 10))
  })

  output$correlation_plot <- renderPlotly({
    # Create a scatterplot based on user-selected variables
    ggplotly(
      ggplot(imd_england_lad, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
        geom_point(aes(color = region_name)) +
        labs(x = input$x_var, y = input$y_var, title = "Correlation Between Deprivation Domains") +
        theme_minimal()
    )
  })

  output$data_table <- renderDT({
    # Display an interactive table of the full dataset
    datatable(imd_england_lad, options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui, server)
