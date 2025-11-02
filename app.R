# Deprivation Explorer

# Load required packages
library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tidyr)
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
imd_metadata <- read_csv("data/metadata_england.csv")

imd_lad <- read_csv("data/imd_lad.csv")
imd_lsoa <- read_csv("data/imd_lsoa.csv")

lad_boundaries <- read_rds("data/lad_boundaries.rds")
lsoa_boundaries <- read_rds("data/lsoa_boundaries.rds")

lad_names <- lad_boundaries |>
  st_drop_geometry() |>
  select(lad_code, lad_name, region_name)

imd_props <- read_csv("data/imd_props.csv")
imd_income_employment <- read_csv("data/imd_income_employment.csv")

# ---- Dropdown options ----
imd_lad_variables <-
  c(
    "Population-weighted average deprivation" = "imd_average_score",
    "% of highly deprived neighbourhoods" = "imd_proportion_of_lso_as_in_most_deprived_10_percent_nationally",
    "% of people living in the most deprived neighbourhoods" = "imd25_extent",
    "Income deprivation" = "income_average_score",
    "Employment deprivation" = "employment_average_score",
    "Education, Skills and Training deprivation" = "education_skills_and_training_average_score",
    "Health Deprivation and Disability deprivation" = "health_deprivation_and_disability_average_score",
    "Crime deprivation" = "crime_average_score",
    "Barriers to Housing and Services deprivation" = "barriers_to_housing_and_services_average_score",
    "Living Environment deprivation" = "living_environment_average_score"
  )

imd_lsoa_variables <-
  c(
    "Overall deprivation" = "IMD_decile",
    "Income deprivation" = "Income_decile",
    "Employment deprivation" = "Employment_decile",
    "Education, Skills and Training deprivation" = "Education_decile",
    "Health Deprivation and Disability deprivation" = "Health_decile",
    "Crime deprivation" = "Crime_decile",
    "Barriers to Housing and Services deprivation" = "Housing_and_Access_decile",
    "Living Environment deprivation" = "Environment_decile"
  )

# Function to get name of the chosen variable from from dropdown value
variables_name <- function(value, variables) {
  names(variables)[match(value, variables)]
}

# Define UI ----
ui <- page_sidebar(
  includeCSS("styles.css"),

  tags$title("Explore deprivation in England"),

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

  # ---- Sidebar ----
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

  # ---- Main panel ----
  navset_underline(
    nav_panel(
      "Map",

      card(
        full_screen = TRUE,
        card_body(
          class = "p-0",
          leafletOutput("map", height = 600)
        )
      )
    ),

    nav_panel(
      "Compare areas",

      card(
        full_screen = TRUE,
        card_header(textOutput("comparison_title")),
        plotlyOutput("area_comparison")
      )
    ),

    nav_panel(
      "Population experiencing deprivation",

      card(
        full_screen = TRUE,
        card_body(
          h3("Most people experiencing income deprivation or employment deprivation do not live in the most deprived neighbourhoods."),
          textOutput("deprived_population"),
          #plotOutput("deprived_population_plot"),
          p("There are regional inequalities in the number of people experiencing income deprivation and employment deprivation who live in otherwise less-deprived neighbourhoods. In the East Midlands, East of England, London, South East and South West, most people experiencing income or employment deprivation do *not* live in the 20% most deprived neighbourhoods, meaning deprived is more likely to be hidden in these places."),
          plotOutput("deprived_population_region_plot")
        )
      )
    ),

    nav_panel(
      "About the data",
      card(
        card_header(
          textOutput("metadata_title")
        ),
        plotlyOutput("metadata", fill = FALSE)
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
        label = ~str_glue("{lsoa_name} is in {tolower(variables_name(imd_var, imd_lsoa_variables))} decile {get(imd_var)}")
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

  # ---- Deprived population tab text ----
  output$deprived_population <- renderText({
    imd_lsoa_filtered <- imd_income_employment

    # Fetch the neighbourhoods in the selected Local Authorities
    if (length(input$select_lad) > 0) {
      imd_lsoa_filtered <- imd_lsoa_filtered %>% filter(lad_name %in% input$select_lad)
    }

    if (input$region_filter != "England") {
      imd_lsoa_filtered <- imd_lsoa_filtered %>% filter(region_name == input$region_filter)
    }

    imd_props <-
      imd_lsoa_filtered |>
      mutate(Core20 = if_else(IMD_decile <= 2, "20% most deprived", "Other")) |>
      group_by(Core20, name) |>
      summarise(n = sum(n, na.rm = TRUE)) |>
      ungroup() |>

      pivot_wider(names_from = name, values_from = n) |>
      rename(people_income_deprived = `Number of income-deprived people`, people_employment_deprived = `Number of employment-deprived people`) |>

      mutate(
        prop_income_deprived = people_income_deprived / sum(people_income_deprived),
        prop_employment_deprived = people_employment_deprived / sum(people_employment_deprived)
      )

    people_income_deprived <- sum(imd_props$people_income_deprived)
    prop_income_deprived_non_core20 <- imd_props[imd_props$Core20 == "Other",]$prop_income_deprived

    people_employment_deprived <- sum(imd_props$people_employment_deprived)
    prop_employment_deprived_non_core20 <- imd_props[imd_props$Core20 == "Other",]$prop_employment_deprived

    # Where are we?
    if (length(input$select_lad) > 0) {
      selected_place_names <- str_flatten_comma(input$select_lad, last = " and ")
    } else {
      selected_place_names <- input$region_filter
    }

    str_glue("In {selected_place_names}, {scales::comma(people_income_deprived)} people are experiencing income deprivation; {scales::percent(prop_income_deprived_non_core20)} do *not* live in the 20% most deprived areas. {scales::comma(people_employment_deprived)} people are experiencing employment deprivation; {scales::percent(prop_employment_deprived_non_core20)} do not live in the 20% most deprived areas.")
  })

  # ---- Deprived population tab plot ----


  # ---- Deprived population by region ----
  output$deprived_population_region_plot <- renderPlot({
    imd_income_employment |>
      mutate(Core20 = if_else(IMD_decile <= 2, "20% most deprived", "Less-deprived areas")) |>
      group_by(region_name, Core20, name) |>
      summarise(n = sum(n, na.rm = TRUE)) |>

      ggplot(aes(x = name, y = n, fill = Core20)) +
      geom_col(position = position_dodge()) +
      coord_flip() +
      facet_wrap(~region_name) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(legend.position = "top") +
      labs(
        x = NULL,
        y = "Number of people",
        fill = NULL
      )
  })

  # ---- Metadata tab title ----
  output$metadata_title <- renderText({
    # Get the current year
    current_year <- as.integer(str_sub(Sys.Date(), 1, 4))

    oldest_indicator <- min(imd_metadata$`Earliest year`)
    newest_indicator <- max(imd_metadata$`Latest year`)

    str_glue("Data in the English Index of Multiple Deprivation are between {current_year - newest_indicator} and {current_year - oldest_indicator} years old")
  })
  # ---- Metadata tab plot ----
  output$metadata <- renderPlotly({
    # Shorten indicator names for better display in the tooltip
    imd_metadata$Indicator <- str_wrap(imd_metadata$Indicator, width = 50)

    # Get the current year
    current_year <- as.integer(str_sub(Sys.Date(), 1, 4))

    oldest_indicator <- min(imd_metadata$`Earliest year`)
    newest_indicator <- max(imd_metadata$`Earliest year`)

    plt <-
      imd_metadata |>
      mutate(year_label = if_else(`Earliest year` == `Latest year`, as.character(`Earliest year`), str_glue("{`Earliest year`} - {`Latest year`}"))) |>
      ggplot(aes(x = reorder(`Indicator (short)`, -`Earliest year`), y = `Earliest year`)) +
      geom_segment(aes(xend = `Indicator (short)`, yend = `Latest year`), colour = "grey") +
      geom_point(
        aes(
          colour = Domain,
          text = str_glue("'{Indicator}' in the {Domain} domain is from {year_label}")
        ),
        size = 3
      ) +
      geom_point(
        aes(
          y = `Latest year`,
          colour = Domain,
          text = str_glue("'{Indicator}' in the {Domain} domain is from {year_label}")
        ),
        size = 3
      ) +
      coord_flip() +
      scale_colour_brewer(palette = "Set2") +
      scale_y_continuous(limits = c(oldest_indicator, current_year), breaks = seq(oldest_indicator, current_year, by = 2)) +
      theme_minimal() +
      # Remove gridlines
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      labs(
        y = "Year the data is from",
        x = NULL
      )

      ggplotly(plt, tooltip = "text", height = 1000) |>
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
          xaxis = list(side = "top"),
          legend = list(
            orientation = "h",
            #x = 0.5,
            xanchor = "center",
            title = "Domain"
          )
          # margin = list(t = 50)  # Reduce top margin to bring plot closer to legend
        )
  })
}

# Run the app ----
shinyApp(ui, server)
