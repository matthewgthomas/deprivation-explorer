# Deprivation Explorer

# Load required packages
library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(plotly)
# library(DT)
library(sf)
# library(shinythemes)
# library(reshape2)
library(geographr)
# library(IMD)

#
# Load data ----
#
imd_lad <- read_csv("data/imd_lad.csv")
imd_lsoa <- read_csv("data/imd_lsoa.csv")

lad_boundaries <- read_sf("data/lad_boundaries.geojson")
lsoa_boundaries <- read_sf("data/lsoa_boundaries.geojson")

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
    "Overall deprivation" = "IMD_decile",
    "Income deprivation" = "Income_decile",
    "Employment deprivation" = "Employment_decile",
    "Education deprivation" = "Education_decile",
    "Health deprivation" = "Health_decile",
    "Crime deprivation" = "Crime_decile",
    "Housing & Access deprivation" = "Housing_and_Access_decile",
    "Environment deprivation" = "Environment_decile"
  )

# Function to get name of the chosen variable from from dropdown value
variables_name <- function(value, variables) {
  names(variables)[match(value, variables)]
}

# Define UI ----
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
            choices = imd_lad_variables,
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

# Server ----
server <- function(input, output, session) {

  # ---- Track user selections ----
  user_selections <- reactiveValues(
    selected_lads = NULL,
    imd_var = imd_lad_variables[1]
  )

  # ---- Update user selections ----
  observeEvent(input$imd_var, {
    user_selections$imd_var <- input$imd_var
  })

  observeEvent(input$select_lad, {
    user_selections$selected_lads <- input$select_lad
  })

  # ---- Update available IMD variables based on whether user is viewing LADs or neighbourhoods ----
  observeEvent(input$lad_or_lsoa, {
    if (input$lad_or_lsoa == "Local Authorities") {
      updateSelectInput(
        session,
        "imd_var",
        choices = imd_lad_variables,
        selected = imd_lad_variables[1]
      )

      user_selections$imd_var <- imd_lad_variables[1]

    } else {
      updateSelectInput(
        session,
        "imd_var",
        choices = imd_lsoa_variables,
        selected = imd_lsoa_variables[1]
      )

      user_selections$imd_var <- imd_lsoa_variables[1]
    }
  })

  # ---- Update LAD dropdown based on region selection ----
  observeEvent(input$region_filter, {
    if(input$region_filter == "England") {
      new_lads <- lad_names$lad_name[str_detect(lad_names$lad_code, "^E")]
    } else {
      new_lads <- lad_names$lad_name[lad_names$region_name == input$region_filter]
    }

    updateSelectInput(
      session,
      "select_lad",
      choices = sort(new_lads)
    )

    # Reset user-selected LADs
    user_selections$selected_lads <- NULL
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

    imd_var <- user_selections$imd_var

    # Filter spatial data if a region is selected
    if (input$region_filter != "England") {
      boundaries <- lad_boundaries %>% filter(region_name == input$region_filter)
    } else {
      boundaries <- lad_boundaries
    }

    # Highlight user-selected Local Authorities
    boundaries$highlight <- ifelse(boundaries$lad_name %in% input$select_lad, "Highlighted", "Not highlighted")

    # Create a color palette based on the selected variable
    pal <- colorNumeric("YlOrRd", domain = boundaries[[imd_var]])

    # Create a color palette for highlighted Local Authorities
    pal_highlight <- colorFactor(c("black", "white"), domain = boundaries$highlight)

    # Make a labelFormat function that formats the labels as percentages if the variable is a proportion, otherwise as numeric
    formatNumberOrPercentage <- function(type = "numeric", x) {
      if (imd_var %in% c("Proportion", "Extent")) {
        scales::percent(x, accuracy = 0.1)
      } else {
        scales::number(x, accuracy = 0.1)
      }
    }

    leaflet(boundaries) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(get(imd_var)),
        weight = ~ifelse(highlight == "Highlighted", 3, 1),  # Change weight based on highlight
        opacity = 1,
        color = ~pal_highlight(highlight),
        dashArray = "",
        fillOpacity = ~ifelse(highlight == "Highlighted", 1, 0.7),  # Change fill opacity based on highlight
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~str_glue("{variables_name(imd_var, imd_lad_variables)} in {lad_name}: {formatNumberOrPercentage(x = get(imd_var))}")
        # label = ~paste0(lad_name, ": ", round(get(imd_var), 2))
      ) %>%
      addLegend(pal = pal, values = ~get(imd_var),
                opacity = 0.7, title = variables_name(imd_var, imd_lad_variables),
                labFormat = formatNumberOrPercentage,
                position = "bottomright")
  })

  # ---- Neighbourhood map ----
  draw_neighbourhood_map <- reactive({
    imd_var <- user_selections$imd_var

    # Ensure spatial boundaries exist
    req(nrow(lsoa_boundaries) > 0)

    # Filter spatial data if a region is selected
    if (input$region_filter != "England") {
      lsoas_in_region <- lsoa_boundaries %>% filter(region_name == input$region_filter)
      lads_in_region <- lad_boundaries %>% filter(region_name == input$region_filter)

    } else {
      lsoas_in_region <- lsoa_boundaries
      lads_in_region <- lad_boundaries

    }

    # Filter spatial data if a Local Authority is selected
    if (length(input$select_lad) > 0) {
      lad_codes <- lad_names$lad_code[lad_names$lad_name %in% input$select_lad]
      lsoas_in_region <- lsoas_in_region %>% filter(lad_code %in% lad_codes)
      lads_in_region <- lad_boundaries %>% filter(lad_name %in% input$select_lad)
    }

    # Select the variable in boundaries based on what the user selected in neighbourhood_imd_var and filter values that are <= 2
    filtered_boundaries <-
      lsoas_in_region %>%
      filter(get(imd_var) <= 2)

    filtered_boundaries |>
      leaflet() %>%
      addTiles() %>%

      # Add LAD boundaries
      addPolygons(
        data = lads_in_region,
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
        label = ~str_glue("{lsoa11_name} is in {tolower(variables_name(imd_var, imd_lsoa_variables))} decile {get(imd_var)}")
        # label = ~paste0(lsoa11_name, ": ", round(get(imd_var), 2))
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
    imd_var <- user_selections$imd_var

    # Fetch the Local Authorities in the selected region
    data <- filtered_lads_in_region()

    # Highlight user-selected Local Authorities
    data$highlight <- ifelse(data$lad_name %in% input$select_lad, "Selected", "Not Selected")

    # Format the labels as percentages if the variable is a proportion, otherwise as numeric
    formatNumberOrPercentage <- function(x) {
      if (imd_var %in% c("Proportion", "Extent")) {
        scales::percent(x, accuracy = 0.1)
      } else {
        scales::number(x, accuracy = 0.1)
      }
    }

    plt <- data |>
      na.omit() |>
      ggplot(aes(x = reorder(lad_name, .data[[imd_var]]), y = .data[[imd_var]])) +
      geom_col(aes(
        fill = region_name,
        colour = highlight,
        text = str_glue("{variables_name(imd_var, imd_lad_variables)} in {lad_name}: {formatNumberOrPercentage(.data[[imd_var]])}")
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
        y = variables_name(imd_var, imd_lad_variables)
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
    imd_var <- user_selections$imd_var
    imd_lsoa_filtered <- imd_lsoa

    # Fetch the neighbourhoods in the selected Local Authorities
    if (length(input$select_lad) > 0) {
      imd_lsoa_filtered <- imd_lsoa_filtered %>% filter(lad_name %in% input$select_lad)
    }

    if (input$region_filter != "England") {
      imd_lsoa_filtered <- imd_lsoa_filtered %>% filter(region_name == input$region_filter)
    }

    # Calculate the proportion of neighbourhoods in each IMD decile,
    # split by rural-urban classification, for the whole of England
    imd_national_summary <-
      imd_lsoa |>
      group_by(.data[[imd_var]], classification) |>
      summarise(n = n()) |>
      ungroup() |>
      group_by(classification) |>
      mutate(prop = n / sum(n)) |>
      ungroup()

    # Calculate the proportion of neighbourhoods in each IMD decile,
    # split by rural-urban classification, for the selected region and LADs
    imd_lsoa_filtered_summary <-
      imd_lsoa_filtered |>
      group_by(region_name, .data[[imd_var]], classification) |>
      summarise(n = n()) |>
      ungroup() |>
      group_by(classification) |>
      mutate(prop = n / sum(n)) |>
      ungroup()

    # Proportion of LSOAs in each IMD decile, split by rural-urban classification
    plt <-
      imd_lsoa_filtered_summary |>
      ggplot(aes(x = .data[[imd_var]], y = prop)) +
      geom_col(
        aes(
          fill = region_name,
          text = str_glue("{scales::comma(n)} ({scales::percent(prop, accuracy = 0.1)}) neighbourhoods in {tolower(classification)}s in {region_name} \nare in {tolower(variables_name(imd_var, imd_lsoa_variables))} decile {.data[[imd_var]]}")
        ),
        position = "stack",
        show.legend = FALSE
      ) +
      geom_point(
        data = imd_national_summary,
        aes(
          text = str_glue("{scales::comma(n)} ({scales::percent(prop, accuracy = 0.1)}) neighbourhoods in {tolower(classification)}s in England \nare in {tolower(variables_name(imd_var, imd_lsoa_variables))} decile {.data[[imd_var]]}")
        )
      ) +
      coord_flip() +
      facet_wrap(~classification) +
      scale_x_continuous(breaks = 1:10, labels = c("Most deprived", 2:9, "Least deprived")) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(
        x = variables_name(imd_var, imd_lsoa_variables),
        y = "Proportion of neighbourhoods"
      )

    ggplotly(plt, height = 500, tooltip = "text") |>
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
        showlegend = FALSE
      )
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

# Run the app ----
shinyApp(ui, server)
