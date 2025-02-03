# app.R

# Load required packages
library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(ggplot2)
library(stringr)
library(plotly)
library(DT)
library(sf)
library(shinythemes)
library(reshape2)
library(geographr)
library(IMD)

# ---------------------
# Load Data
# ---------------------
lad_names <- boundaries_ltla21 |>
  st_drop_geometry() |>
  rename(lad_code = ltla21_code, lad_name = ltla21_name)

# IMD for Local Authority Districts (LAD)
imd_lad <- IMD::imd_england_lad |>
  left_join(
    lookup_ltla21_region21 |>
      select(lad_code = ltla21_code, region_name = region21_name)
  ) |>
  left_join(lad_names)

# IMD for Neighbourhoods (LSOA)
imd_lsoa <- IMD::imd_england_lsoa |>
  left_join(
    lookup_lsoa11_ltla21 |>
      select(lsoa_code = lsoa11_code, lad_code = ltla21_code)
  )

# Load LAD boundaries as an sf object (GeoJSON or shapefile)
# (Replace "lad_boundaries.geojson" with your actual file)
lad_boundaries <- boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  rename(lad_code = ltla21_code)

# Merge the LAD-level IMD data with spatial boundaries based on a common code.
# (Make sure the field names match; here we assume both have 'lad_code'.)
lad_boundaries <- left_join(lad_boundaries, imd_lad, by = "lad_code")


# ---------------------
# Define UI
# ---------------------
ui <- page_fillable(
  includeCSS("styles.css"),

  # The title includes an inline drop-down to choose a region.
  tags$h3(
    tags$div(
      style = "align-items: center;",
      tags$span("Explore deprivation in "),
      tags$div(
        class = "flex-select",
        style = "display: inline-block; border: none;",
        selectInput("region_filter", label = NULL,
                    choices = c("England", sort(unique(imd_lad$region_name))),
                    selected = "England",
                    selectize = FALSE,
                    width = "150px")
      )
    )
  ),

  theme = bs_theme("lumen", version = 5),

  navset_underline(
    # ---- Tab 1: Interactive Map ----
    nav_panel(
      "Deprivation in Local Authorities",

      card(
        tags$div(
          style = "align-items: center;",
          tags$strong(
            tags$span("Choose a measure of deprivation: ")
          ),
          tags$div(
            class = "flex-select",
            style = "display: inline-block; border: none;",

            selectizeInput(
              "map_var",
              "",
              choices = c(
                "Population-weighted average score" = "Score",
                "% of highly deprived neighbourhoods" = "Proportion",
                "% of people living in the most deprived neighbourhoods" = "Extent",
                "Income Score" = "Income_Score",
                "Employment Score" = "Employment_Score",
                "Education Score" = "Education_Score",
                "Health Score" = "Health_Score",
                "Crime Score" = "Crime_Score",
                "Housing & Access Score" = "Housing_and_Access_Score",
                "Environment Score" = "Environment_Score"
              ),
              options = list(dropdownParent = 'body')
            )
          )
        )
      ),

      card(
        leafletOutput("map", height = 600)
      )
    ),

    nav_panel(
      "Compare Local Authorities",

      card(
        tags$div(
          style = "align-items: center;",
          tags$strong(
            tags$span("Choose a measure of deprivation: ")
          ),
          tags$div(
            class = "flex-select",
            style = "display: inline-block; border: none;",

            selectizeInput(
              "imd_var",
              "",
              choices = c("Population-weighted average score" = "Score",
                          "% of highly deprived neighbourhoods" = "Proportion",
                          "% of people living in the most deprived neighbourhoods" = "Extent",
                          "Income Score" = "Income_Score",
                          "Employment Score" = "Employment_Score",
                          "Education Score" = "Education_Score",
                          "Health Score" = "Health_Score",
                          "Crime Score" = "Crime_Score",
                          "Housing & Access Score" = "Housing_and_Access_Score",
                          "Environment Score" = "Environment_Score"),
              options = list(dropdownParent = 'body')
            )
          )
        )
      ),

      card(
        plotlyOutput("lad_comparison")
      )
    ),

    # ---- Tab 2: Scatter Plot Analysis ----
    nav_panel(
      "Deprivation in neighbourhoods",
    ),

    # ---- Tab 4: Correlation Heatmap ----
    nav_panel("Domains of deprivation",
              sidebarLayout(
                sidebarPanel(
                  helpText("Select the LAD-level domain scores to include in the correlation analysis:"),
                  checkboxGroupInput("corr_vars", "Variables",
                                     choices = c("Overall Score" = "Score",
                                                 "Income Score" = "Income_Score",
                                                 "Employment Score" = "Employment_Score",
                                                 "Education Score" = "Education_Score",
                                                 "Health Score" = "Health_Score",
                                                 "Crime Score" = "Crime_Score",
                                                 "Housing & Access Score" = "Housing_and_Access_Score",
                                                 "Environment Score" = "Environment_Score"),
                                     selected = c("Score", "Income_Score", "Employment_Score", "Education_Score", "Health_Score"))
                ),
                mainPanel(
                  plotOutput("corrHeatmap")
                )
              )
    ),

    # ---- Tab 5: Data Table ----
    nav_panel("Data Table",
              fluidPage(
                DTOutput("dataTable")
              )
    )
  )
)

# ---------------------
# Define Server Logic
# ---------------------
server <- function(input, output, session) {

  # --- Reactive subset for LAD-level data based on Region filter ---
  filtered_lad <- reactive({
    if (input$region_filter == "England") {
      imd_lad
    } else {
      imd_lad %>% filter(region_name == input$region_filter)
    }
  })

  # --- Map Output ---
  output$map <- renderLeaflet({
    # Ensure spatial boundaries exist
    req(nrow(lad_boundaries) > 0)

    # Filter spatial data if a region is selected
    if (input$region_filter != "England") {
      boundaries <- lad_boundaries %>% filter(region_name == input$region_filter)
    } else {
      boundaries <- lad_boundaries
    }

    # Create a color palette based on the selected variable
    pal <- colorNumeric("YlOrRd", domain = boundaries[[input$map_var]])

    leaflet(boundaries) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(get(input$map_var)),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0(lad_code, ": ", round(get(input$map_var), 2))
      ) %>%
      addLegend(pal = pal, values = ~get(input$map_var),
                opacity = 0.7, title = input$map_var,
                position = "bottomright")
  })

  # --- LA comparison ---
  output$lad_comparison <- renderPlotly({
    data <- filtered_lad()

    plt <- data |>
      ggplot(aes(x = reorder(lad_name, .data[[input$imd_var]]), y = .data[[input$imd_var]])) +
      geom_col(aes(fill = region_name))

    ggplotly(plt)
  })

  # --- Scatter Plot Output ---
  output$scatterPlot <- renderPlot({
    data <- filtered_lad()
    ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(color = "blue", size = 3, alpha = 0.6) +
      labs(x = input$x_var,
           y = input$y_var,
           title = paste("Scatter Plot:", input$x_var, "vs", input$y_var)) +
      theme_minimal()
  })

  # --- LSOA Distribution Boxplot ---
  output$lsoaBoxplot <- renderPlot({
    data <- imd_lsoa %>% filter(lad_code == input$selected_lad)
    ggplot(data, aes_string(x = "factor(1)", y = input$lsoa_domain)) +
      geom_boxplot(fill = "skyblue") +
      labs(x = "Local Authority",
           y = input$lsoa_domain,
           title = paste("Distribution of", input$lsoa_domain, "in", input$selected_lad)) +
      theme_minimal() +
      theme(axis.text.x = element_blank())
  })

  # --- Correlation Heatmap ---
  output$corrHeatmap <- renderPlot({
    req(input$corr_vars)
    data <- filtered_lad()[, input$corr_vars, drop = FALSE]
    corr_matrix <- cor(data, use = "complete.obs")
    # Reshape for ggplot2
    corr_df <- melt(corr_matrix)
    ggplot(corr_df, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = "Correlation") +
      theme_minimal() +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
  })

  # --- Data Table Output ---
  output$dataTable <- renderDT({
    datatable(imd_lad, options = list(pageLength = 10))
  })

}

# ---------------------
# Run the App
# ---------------------
shinyApp(ui, server)
