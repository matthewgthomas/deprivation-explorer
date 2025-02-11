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
# ---- Local Authority data ----
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

# Load LAD boundaries as an sf object (GeoJSON or shapefile)
# (Replace "lad_boundaries.geojson" with your actual file)
lad_boundaries <- boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  rename(lad_code = ltla21_code)

# Merge the LAD-level IMD data with spatial boundaries based on a common code.
# (Make sure the field names match; here we assume both have 'lad_code'.)
lad_boundaries <- left_join(lad_boundaries, imd_lad, by = "lad_code")

# ---- Neighbourhood-level data ----
# Calculate quintiles
imd_lsoa <- IMD::imd_england_lsoa |>
  left_join(
    lookup_lsoa11_ltla21 |>
      select(lsoa_code = lsoa11_code, lad_code = ltla21_code)
  ) |>
  mutate(
    IMD_quintile = ceiling(IMD_decile / 2),
    Income_quintile = ceiling(Income_decile / 2),
    Employment_quintile = ceiling(Employment_decile / 2),
    Education_quintile = ceiling(Education_decile / 2),
    Health_quintile = ceiling(Health_decile / 2),
    Crime_quintile = ceiling(Crime_decile / 2),
    Housing_and_Access_quintile = ceiling(Housing_and_Access_decile / 2),
    Environment_quintile = ceiling(Environment_decile / 2)
  ) |>
  select(lsoa_code, lad_code, ends_with("quintile"))

# Show only 20% most deprived areas on the map
lsoa_boundaries <-
  boundaries_lsoa11 |>
  left_join(imd_lsoa, by = join_by(lsoa11_code == lsoa_code))

# ---- Map legend ----
imd_lad_variables <-
  c(
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
  )

# Function to get name of imd_lad_variables entry from value
imd_lad_variables_name <- function(value) {
  names(imd_lad_variables)[match(value, imd_lad_variables)]
}

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
        full_screen = TRUE,
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
        card_header(textOutput("lad_comparison_title")),
        plotlyOutput("lad_comparison")
      )
    ),

    # ---- Neighourhood IMD map ----
    nav_panel(
      "Deprivation in neighbourhoods",

      layout_column_wrap(
        width = "200px",

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
                "neighbourhood_map_var",
                "",
                choices = c(
                  "Overall deprivation" = "IMD_quintile",
                  "Income" = "Income_quintile",
                  "Employment" = "Employment_quintile",
                  "Education" = "Education_quintile",
                  "Health" = "Health_quintile",
                  "Crime" = "Crime_quintile",
                  "Housing & Access" = "Housing_and_Access_quintile",
                  "Environment" = "Environment_quintile"
                ),
                options = list(dropdownParent = 'body')
              )
            )
          )
        ),

        card(
          card_header("Choose one or more Local Authorities"),
          tags$div(
            style = "align-items: center;",
            tags$strong(
              tags$span("")
            ),
            tags$div(
              class = "flex-select",
              style = "display: inline-block; border: none;",

              selectizeInput(
                "neighbourhood_lad",
                "",
                choices = sort(lad_names$lad_name),
                multiple = TRUE,
                options = list(dropdownParent = 'body')
              )
            )
          )
        )
      ),

      card(
        full_screen = TRUE,
        leafletOutput("neighbourhood_map", height = 600)
      )
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

  # --- Local Authority map ---
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

    # Make a labelFormat function that formats the labels as percentages if the variable is a proportion, otherwise as numeric
    formatNumberOrPercentage <- function(type = "numeric", x) {
      if (input$map_var %in% c("Proportion", "Extent")) {
        scales::percent(x, accuracy = 0.1)
      } else {
        scales::number(x, accuracy = 0.1)
      }
    }

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
                opacity = 0.7, title = imd_lad_variables_name(input$map_var),
                labFormat = formatNumberOrPercentage,
                position = "bottomright")
  })

  # ---- LA comparison ----
  output$lad_comparison_title <- renderText({
    str_glue("{imd_lad_variables_name(input$imd_var)} for Local Authorities in {input$region_filter}")
  })

  output$lad_comparison <- renderPlotly({
    data <- filtered_lad()

    # Format the labels as percentages if the variable is a proportion, otherwise as numeric
    formatNumberOrPercentage <- function(x) {
      if (input$imd_var %in% c("Proportion", "Extent")) {
        scales::percent(x, accuracy = 0.1)
      } else {
        scales::number(x, accuracy = 0.1)
      }
    }

    plt <- data |>
      na.omit() |>
      ggplot(aes(x = reorder(lad_name, .data[[input$imd_var]]), y = .data[[input$imd_var]])) +
      geom_col(aes(
        fill = region_name,
        text = str_glue("{imd_lad_variables_name(input$imd_var)} in {lad_name}: {formatNumberOrPercentage(.data[[input$imd_var]])}")
      )) +
      coord_flip() +
      scale_y_continuous(labels = formatNumberOrPercentage) +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      theme(
        legend.position = "top"
      ) +
      labs(
        x = NULL,
        y = imd_lad_variables_name(input$imd_var)
      )

    ggplotly(plt, height = nrow(data) * 15, tooltip = "text") |>
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          "zoom",
          "pan",
          "select",
          "zoomIn",
          "zoomOut",
          "autoScale",
          "resetScale",
          "lasso2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        ),
        # Download button
        toImageButtonOptions = list(
          height = NULL,
          width = NULL,
          scale = 6
        )
      ) |>
      layout(
        showlegend = if_else(input$region_filter == "England", TRUE, FALSE),
        legend = list(
          orientation = "h",
          x = 0,
          xanchor = "center",
          y = 1,
          yanchor = "bottom",
          title = NA
        ),
        margin = list(t = 50)  # Reduce top margin to bring plot closer to legend
      )
  })

  # ---- Neighourhood map ----
  output$neighbourhood_map <- renderLeaflet({
    # Ensure spatial boundaries exist
    req(nrow(lsoa_boundaries) > 0)

    # Filter spatial data if a Local Authority is selected
    if (length(input$neighbourhood_lad) > 0) {
      lad_codes <- lad_names$lad_code[lad_names$lad_name %in% input$neighbourhood_lad]
      boundaries <- lsoa_boundaries %>% filter(lad_code %in% lad_codes)
      selected_lad_boundaries <- lad_boundaries %>% filter(ltla21_name %in% input$neighbourhood_lad)
    } else {
      boundaries <- lsoa_boundaries
      selected_lad_boundaries <- lad_boundaries
    }

    # Create a color palette based on the selected variable
    # pal <- colorNumeric("YlOrRd", domain = boundaries[[input$neighbourhood_map_var]])

    # Select the variable in boundaries based on what the user selected in neighbourhood_map_var and filter values that are <= 2
    filtered_boundaries <-
      boundaries %>%
      filter(get(input$neighbourhood_map_var) <= 2)

    filtered_boundaries |>
      leaflet() %>%
      addTiles() %>%

      # Add LAD boundaries
      addPolygons(
        data = selected_lad_boundaries,
        fillColor = "transparent",
        weight = 2,
        opacity = 1,
        color = "black",
        fillOpacity = 0.5
      ) %>%

      # Add 20% most deprived LSOAs
      addPolygons(
        fillColor = "red",
        weight = 1,
        opacity = 0.5,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0(lad_code, ": ", round(get(input$neighbourhood_map_var), 2))
      )
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
