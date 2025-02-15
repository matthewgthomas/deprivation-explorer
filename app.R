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
  rename(lad_code = ltla21_code, lad_name = ltla21_name) |>
  left_join(
    lookup_ltla21_region21 |>
      select(lad_code = ltla21_code, region_name = region21_name)
  )

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

# ---- Dropdown options ----
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

imd_lsoa_variables <-
  c(
    "Overall deprivation" = "IMD_quintile",
    "Income" = "Income_quintile",
    "Employment" = "Employment_quintile",
    "Education" = "Education_quintile",
    "Health" = "Health_quintile",
    "Crime" = "Crime_quintile",
    "Housing & Access" = "Housing_and_Access_quintile",
    "Environment" = "Environment_quintile"
  )

# Function to get name of the chosen variable from from dropdown value
variables_name <- function(value, variables) {
  names(imd_lad_variables)[match(value, variables)]
}

# ---------------------
# Define UI
# ---------------------
ui <- page_sidebar(
  includeCSS("styles.css"),

  # The title includes an inline drop-down to choose a region.
  tags$h3(
    tags$div(
      style = "align-items: center;",
      tags$span("Explore deprivation in "),

      # LA/neighbourhood selector
      tags$div(
        class = "flex-select",
        style = "width: 227px",
        selectInput(
          "lad_or_lsoa",
          label = NULL,
          choices = c("Local Authorities", "neighbourhoods"),
          selected = "Local Authorities",
          selectize = FALSE
        )
      ),

      tags$span(" in "),

      # Region selector
      tags$div(
        class = "flex-select",
        style = "display: inline-block; border: none;",
        selectInput(
          "region_filter",
          label = NULL,
          choices = c("England", sort(unique(imd_lad$region_name))),
          selected = "England",
          selectize = FALSE,
          width = "150px"
        )
      )
    )
  ),

  theme = bs_theme("lumen", version = 5),

  sidebar = sidebar(
    # - Select measure of deprivation -
    card(
      card_header("Choose a measure of deprivation"),

      tags$div(
        style = "align-items: center;",
        tags$div(
          class = "flex-select",
          style = "display: inline-block; border: none;",

          selectizeInput(
            "imd_var",
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

    # - Select Local Authorities -
    card(
      card_header("Filter Local Authorities"),
      tags$div(
        style = "align-items: center;",
        tags$strong(
          tags$span("")
        ),
        tags$div(
          class = "flex-select",
          style = "display: inline-block; border: none;",

          selectizeInput(
            "select_lad",
            "",
            #TODO: Limit LAD choices to the current nation/region
            choices = sort(lad_names$lad_name),
            multiple = TRUE,
            options = list(dropdownParent = 'body')
          )
        )
      )
    )
  ),

  navset_underline(
    # ---- Tab 1: Interactive Map ----
    nav_panel(
      "Map",

      card(
        full_screen = TRUE,
        leafletOutput("map", height = 600)
      )
    ),

    nav_panel(
      "Compare areas",

      card(
        card_header(textOutput("comparison_title")),
        plotlyOutput("area_comparison")
      )
    )
  )
)

# ---------------------
# Define Server Logic
# ---------------------
server <- function(input, output, session) {

  #TODO: Synchronise the LAD choices and deprivation variable choices
  observeEvent(input$lad_or_lsoa, {
    if (input$lad_or_lsoa == "Local Authorities") {
      updateSelectInput(
        session,
        "imd_var",
        choices = imd_lad_variables,
        selected = imd_lad_variables[1]
      )
    } else {
      updateSelectInput(
        session,
        "imd_var",
        choices = imd_lsoa_variables,
        selected = imd_lsoa_variables[1]
      )
    }
  })

  observeEvent(input$region_filter, {
    if(input$region_filter == "England") {
      new_lads <- lad_names$lad_name[str_detect(lad_names$lad_code, "^E")]
    } else {
      new_lads <- lad_names$lad_name[lad_names$region_name == input$region_filter]
    }
    print(new_lads)

    updateSelectInput(
      session,
      "select_lad",
      choices = sort(new_lads)
    )
  })

  # ---- Reactive subset for LAD-level data based on Region filter ----
  filtered_lads_in_region <- reactive({
    if (input$region_filter == "England") {
      imd_lad
    } else {
      imd_lad %>% filter(region_name == input$region_filter)
    }
  })

  # ---- Local Authority map ----
  draw_lad_map <- reactive({
    # Ensure spatial boundaries exist
    req(nrow(lad_boundaries) > 0)

    # Filter spatial data if a region is selected
    if (input$region_filter != "England") {
      boundaries <- lad_boundaries %>% filter(region_name == input$region_filter)
    } else {
      boundaries <- lad_boundaries
    }

    # Create a color palette based on the selected variable
    pal <- colorNumeric("YlOrRd", domain = boundaries[[input$imd_var]])

    # Make a labelFormat function that formats the labels as percentages if the variable is a proportion, otherwise as numeric
    formatNumberOrPercentage <- function(type = "numeric", x) {
      if (input$imd_var %in% c("Proportion", "Extent")) {
        scales::percent(x, accuracy = 0.1)
      } else {
        scales::number(x, accuracy = 0.1)
      }
    }

    leaflet(boundaries) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(get(input$imd_var)),
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
        label = ~paste0(lad_code, ": ", round(get(input$imd_var), 2))
      ) %>%
      addLegend(pal = pal, values = ~get(input$imd_var),
                opacity = 0.7, title = variables_name(input$imd_var, imd_lad_variables),
                labFormat = formatNumberOrPercentage,
                position = "bottomright")
  })

  # ---- Neighbourhood map ----
  draw_neighbourhood_map <- reactive({
    # Ensure spatial boundaries exist
    req(nrow(lsoa_boundaries) > 0)

    # Filter spatial data if a Local Authority is selected
    if (length(input$select_lad) > 0) {
      region_boundaries <- lad_boundaries %>% filter(region_name == input$region_filter)

      lad_codes <- lad_names$lad_code[lad_names$lad_name %in% input$select_lad]
      boundaries <- lsoa_boundaries %>% filter(lad_code %in% lad_codes)

      selected_lad_boundaries <- lad_boundaries %>% filter(ltla21_name %in% input$select_lad)
    } else {
      region_boundaries <- lad_boundaries
      boundaries <- lsoa_boundaries
      selected_lad_boundaries <- lad_boundaries
    }

    # Create a color palette based on the selected variable
    # pal <- colorNumeric("YlOrRd", domain = boundaries[[input$neighbourhood_imd_var]])

    # Select the variable in boundaries based on what the user selected in neighbourhood_imd_var and filter values that are <= 2
    filtered_boundaries <-
      boundaries %>%
      filter(get(input$imd_var) <= 2)

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
        label = ~paste0(lsoa11_name, ": ", round(get(input$imd_var), 2))
      )
  })

  # ---- Render maps ----
  output$map <- renderLeaflet({
    if (input$lad_or_lsoa == "Local Authorities") {
      draw_lad_map()
    } else {
      draw_neighbourhood_map()
    }
  })

  # ---- Comparison tab title ----
  output$comparison_title <- renderText({
    if (input$lad_or_lsoa == "Local Authorities") {
      vars <- variables_name(input$imd_var, imd_lad_variables)
    } else {
      vars <- variables_name(input$imd_var, imd_lsoa_variables)
    }

    str_glue("{vars} for {input$lad_or_lsoa} in {input$region_filter}")
  })

  # ---- Local Authority comparison ----
  render_lad_comparison <- reactive({
    # Fetch the Local Authorities in the selected region
    data <- filtered_lads_in_region()

    # Highlight user-selected Local Authorities
    data$highlight <- ifelse(data$lad_name %in% input$select_lad, "Selected", "Not Selected")

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
        colour = highlight,
        text = str_glue("{variables_name(input$imd_var, imd_lad_variables)} in {lad_name}: {formatNumberOrPercentage(.data[[input$imd_var]])}")
      ), show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(labels = formatNumberOrPercentage) +
      scale_color_manual(values = c("Selected" = "black", "Not Selected" = "transparent")) +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(
        x = NULL,
        y = variables_name(input$imd_var, imd_lad_variables)
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
        showlegend = FALSE  #if_else(input$region_filter == "England", TRUE, FALSE),
        # legend = list(
        #   orientation = "h",
        #   x = 0,
        #   xanchor = "center",
        #   y = 1,
        #   yanchor = "bottom",
        #   title = NA
        # ),
        # margin = list(t = 50)  # Reduce top margin to bring plot closer to legend
      )
  })

  # ---- Neighbourhood comparison ----
  render_neighbourhood_comparison <- reactive({
    # Ask the user to pick a region or Local Authority first
    # otherwise the chart will be too big
    if (length(input$select_lad) == 0) {
      return(NULL)
    }
  })

  # ---- Comparison tab plot ----
  output$area_comparison <- renderPlotly({
    if (input$lad_or_lsoa == "Local Authorities") {
      render_lad_comparison()
    } else {
      render_neighbourhood_comparison()
    }
  })
}

# ---------------------
# Run the App
# ---------------------
shinyApp(ui, server)
