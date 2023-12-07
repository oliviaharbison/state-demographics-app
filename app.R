library(shiny)
library(tidyverse)
library(sf)

#data
county_data <- sf::read_sf("data/county_data.shp") %>%
  mutate(state = str_to_title(state))

state_list <- unique(county_data$state)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("County Demographic Map by State"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      p("Create demographic maps with information from the 2010 US Census."),
      
      #Input: State ----
      selectInput(
        inputId = "state_choice",
        label = "Choose a state to display",
        choices = state_list,
        selected = "Illinois"
      ),
      
      # Input: Variable ----
      selectInput(
        inputId = "var",
        label = "Choose a variable to display",
        choices = list(
          "Percent White",
          "Percent Black",
          "Percent Hispanic",
          "Percent Asian"
        )
      ),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(# Output: Map ----
              plotOutput(outputId = "mapPlot"))
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$mapPlot <- renderPlot({
    var_state <- county_data %>% filter(state == input$state_choice)
    
    var <- switch(
      input$var,
      "Percent White" = var_state$white,
      "Percent Black" = var_state$black,
      "Percent Hispanic" = var_state$hispanic,
      "Percent Asian" = var_state$asian
    )
    
    color <- switch(
      input$var,
      "Percent White" = "darkgreen",
      "Percent Black" = "blue",
      "Percent Hispanic" = "darkorange",
      "Percent Asian" = "darkviolet"
    )
    
    legend <- switch(
      input$var,
      "Percent White" = "% White",
      "Percent Black" = "% Black",
      "Percent Hispanic" = "% Hispanic",
      "Percent Asian" = "% Asian"
    )
  
    
    ggplot(var_state) +
      geom_sf(aes(fill = var)) +
      theme_void() +
      scale_fill_gradient(
        name = legend,
        low = "white",
        high = color,
        limits = c(0, 100)
      ) +
      labs(title = input$state_choice) +
      theme(plot.title = element_text(size = 24, hjust = 0.5))
    
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)