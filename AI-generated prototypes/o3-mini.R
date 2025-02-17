# app.R

# Load required packages
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(sf)
library(shinythemes)
library(reshape2)
library(geographr)
library(IMD)

# ---------------------
# Load Data
# ---------------------
# IMD for Local Authority Districts (LAD)
imd_lad <- IMD::imd_england_lad |>
  left_join(
    lookup_ltla21_region21 |>
      select(lad_code = ltla21_code, region_name = region21_name)
  )

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
ui <- navbarPage("IMD England Explorer",
                 theme = shinytheme("flatly"),

                 # ---- Tab 1: Interactive Map ----
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("map_var", "Select Variable to Display",
                                          choices = c("Overall Score" = "Score",
                                                      "Income Score" = "Income_Score",
                                                      "Employment Score" = "Employment_Score",
                                                      "Education Score" = "Education_Score",
                                                      "Health Score" = "Health_Score",
                                                      "Crime Score" = "Crime_Score",
                                                      "Housing & Access Score" = "Housing_and_Access_Score",
                                                      "Environment Score" = "Environment_Score")),
                              selectInput("region_filter", "Select Region (Optional)",
                                          choices = c("All", sort(unique(imd_lad$region_name))),
                                          selected = "All")
                            ),
                            mainPanel(
                              leafletOutput("map", height = 600)
                            )
                          )
                 ),

                 # ---- Tab 2: Scatter Plot Analysis ----
                 tabPanel("Scatter Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("x_var", "X-axis Variable",
                                          choices = c("Overall Score" = "Score",
                                                      "Income Score" = "Income_Score",
                                                      "Employment Score" = "Employment_Score",
                                                      "Education Score" = "Education_Score",
                                                      "Health Score" = "Health_Score",
                                                      "Crime Score" = "Crime_Score",
                                                      "Housing & Access Score" = "Housing_and_Access_Score",
                                                      "Environment Score" = "Environment_Score"),
                                          selected = "Score"),
                              selectInput("y_var", "Y-axis Variable",
                                          choices = c("Overall Score" = "Score",
                                                      "Income Score" = "Income_Score",
                                                      "Employment Score" = "Employment_Score",
                                                      "Education Score" = "Education_Score",
                                                      "Health Score" = "Health_Score",
                                                      "Crime Score" = "Crime_Score",
                                                      "Housing & Access Score" = "Housing_and_Access_Score",
                                                      "Environment Score" = "Environment_Score"),
                                          selected = "Income_Score")
                            ),
                            mainPanel(
                              plotOutput("scatterPlot")
                            )
                          )
                 ),

                 # ---- Tab 3: LSOA Distribution (Boxplot) ----
                 tabPanel("LSOA Distribution",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_lad", "Select Local Authority",
                                          choices = sort(unique(imd_lsoa$lad_code))),
                              selectInput("lsoa_domain", "Select Domain",
                                          choices = c("Overall IMD Decile" = "IMD_decile",
                                                      "Income Decile" = "Income_decile",
                                                      "Employment Decile" = "Employment_decile",
                                                      "Education Decile" = "Education_decile",
                                                      "Children & Young People Decile" = "Children_and_young_people_decile",
                                                      "Adult Skills Decile" = "Adult_skills_decile",
                                                      "Health Decile" = "Health_decile",
                                                      "Crime Decile" = "Crime_decile",
                                                      "Housing & Access Decile" = "Housing_and_Access_decile",
                                                      "Geographical Barriers Decile" = "Geographical_barriers_decile",
                                                      "Wider Barriers Decile" = "Wider_barriers_decile",
                                                      "Environment Decile" = "Environment_decile",
                                                      "Indoors Decile" = "Indoors_decile",
                                                      "Outdoors Decile" = "Outdoors_decile"))
                            ),
                            mainPanel(
                              plotOutput("lsoaBoxplot")
                            )
                          )
                 ),

                 # ---- Tab 4: Correlation Heatmap ----
                 tabPanel("Correlation Heatmap",
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
                 tabPanel("Data Table",
                          fluidPage(
                            DTOutput("dataTable")
                          )
                 )
)

# ---------------------
# Define Server Logic
# ---------------------
server <- function(input, output, session) {

  # --- Reactive subset for LAD-level data based on Region filter ---
  filtered_lad <- reactive({
    if (input$region_filter == "All") {
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
    if (input$region_filter != "All") {
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
