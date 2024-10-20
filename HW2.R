# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Link to the publicly hosted CSV file
data_url <- "https://raw.githubusercontent.com/Cynthiaxu7/STAT436_HW2/main/HW2.csv"

# Load data from the public link
data <- read.csv(data_url)

# Data cleaning
data_cleaned <- data %>%
  filter(!is.na(ESTIMATE)) %>%   # Remove rows with missing estimates
  filter(!AGE %in% c("All ages", "15-24 years","25-44 years", "45-64 years", "65 years and over"))  # Remove repetitive age groups

# UI for the application
ui <- fluidPage(
  titlePanel("Suicide Rates in the United States"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting age group (choices from the cleaned data)
      selectInput("age_group", "Select Age Group:", choices = unique(data_cleaned$AGE)),
      
      # Slider for selecting a year range
      sliderInput("year_range", "Select Year Range:",
                  min = min(data_cleaned$YEAR), max = max(data_cleaned$YEAR), value = c(2000, 2018))
    ),
    
    mainPanel(
      # Output plot
      plotOutput("suicidePlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Reactive expression to filter the data based on user inputs (age group and year range)
  filtered_data <- reactive({
    data_cleaned %>%
      filter(AGE == input$age_group, 
             YEAR >= input$year_range[1], YEAR <= input$year_range[2])
  })
  
  # Render the plot based on the filtered data
  output$suicidePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = YEAR, y = ESTIMATE, color = STUB_LABEL)) +
      geom_line() +                              # Line plot for suicide rates over time
      labs(title = paste("Suicide Rates for", input$age_group), 
           x = "Year", y = "Death Rate per 100,000") +
      theme_minimal()                            # Use a clean minimal theme for the plot
  })
}

# Run the application
shinyApp(ui = ui, server = server)