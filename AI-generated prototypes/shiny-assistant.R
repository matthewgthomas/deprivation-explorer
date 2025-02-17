library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
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

# Load data (commented out as data would be loaded differently in practice)
# imd_england_lad <- read.csv("imd_england_lad.csv")
# imd_england_lsoa <- read.csv("imd_england_lsoa.csv")

# Define domain columns for easier reference
lad_domains <- c(
  "Overall" = "Score",
  "Income" = "Income_Score",
  "Employment" = "Employment_Score",
  "Education" = "Education_Score",
  "Health" = "Health_Score",
  "Crime" = "Crime_Score",
  "Housing and Access" = "Housing_and_Access_Score",
  "Environment" = "Environment_Score"
)

ui <- page_sidebar(
  title = "England Deprivation Explorer",
  sidebar = sidebar(
    title = "Controls",
    selectInput("region", "Region:", choices = NULL),
    selectInput("x_var", "X-axis measure:", choices = names(lad_domains), selected = "Income"),
    selectInput("y_var", "Y-axis measure:", choices = names(lad_domains), selected = "Education"),
    checkboxInput("show_correlation", "Show correlation line", value = TRUE)
  ),

  layout_columns(
    col_widths = c(8, 4),
    card(
      full_screen = TRUE,
      card_header("Deprivation Measures Relationship"),
      plotOutput("scatter_plot")
    ),
    card(
      full_screen = TRUE,
      card_header("Local Authorities Summary"),
      DTOutput("lad_table")
    )
  ),

  card(
    full_screen = TRUE,
    card_header("Neighborhood-Level Data"),
    DTOutput("lsoa_table")
  )
)

server <- function(input, output, session) {
  # Update region choices when data is loaded
  observe({
    updateSelectInput(session, "region",
                      choices = c("All", sort(unique(imd_england_lad$region_name))))
  })

  # Filter LAD data based on selected region
  filtered_lad_data <- reactive({
    if (is.null(input$region) || input$region == "All") {
      imd_england_lad
    } else {
      imd_england_lad %>% filter(region_name == input$region)
    }
  })

  # Create scatter plot
  output$scatter_plot <- renderPlot({
    data <- filtered_lad_data()
    x_col <- lad_domains[input$x_var]
    y_col <- lad_domains[input$y_var]

    p <- ggplot(data, aes_string(x = x_col, y = y_col)) +
      geom_point(alpha = 0.6) +
      labs(
        x = paste(input$x_var, "Score"),
        y = paste(input$y_var, "Score"),
        title = paste("Relationship between", input$x_var, "and", input$y_var, "Deprivation")
      ) +
      theme_minimal()

    if (input$show_correlation) {
      p <- p + geom_smooth(method = "lm", se = TRUE)
    }

    p
  })

  # Create LAD summary table
  output$lad_table <- renderDT({
    filtered_lad_data() %>%
      select(region_name, Score, Income_Score, Employment_Score, Education_Score) %>%
      datatable(
        options = list(pageLength = 5),
        caption = "Local Authority Districts Summary"
      )
  })

  # Create LSOA detail table
  output$lsoa_table <- renderDT({
    req(input$region)
    if (input$region == "All") {
      return(NULL)
    }

    # Get LAD codes for selected region
    lad_codes <- filtered_lad_data()$lad_code

    imd_england_lsoa %>%
      filter(lad_code %in% lad_codes) %>%
      select(
        lsoa_code,
        IMD_decile,
        Income_decile,
        Employment_decile,
        Education_decile,
        Health_decile
      ) %>%
      datatable(
        options = list(pageLength = 10),
        caption = "Neighborhood-Level Deprivation Deciles"
      )
  })
}

shinyApp(ui, server)
