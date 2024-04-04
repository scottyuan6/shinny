library(shiny)
library(ggplot2)
library(DT)
library(dplyr)


# Define UI
ui <- fluidPage(
  titlePanel("Auschwitz Victims by Nationality/Category"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group", "Choose a category:", 
                  choices = c("Birthplace", "Religion"), selected = "Birthplace")
    ),
    mainPanel(
      plotOutput("plot"),
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    group_var <- input$group
    
    # Prepare the data based on the selected group_var
    auschwitz_data <- read.csv("/cloud/project/data/Auschwitz_Death.csv")
    data <- auschwitz_data %>%
      group_by(!!sym(group_var)) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      top_n(10, n)
    
    # Adjusted plotting command to use aes() and !!sym()
    ggplot(data, aes(x = reorder(!!sym(group_var), n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(x = group_var, y = "Number of Victims") +
      coord_flip()
  })
  
  output$table <- renderDT({
    group_var <- input$group
    
    # Prepare data for the table
    auschwitz_data <- read.csv("/cloud/project/data/Auschwitz_Death.csv")
    data_table <- auschwitz_data %>%
      group_by(!!sym(group_var)) %>%
      summarise(n = n()) %>%
      arrange(desc(n))
    
    datatable(data_table, options = list(pageLength = 10))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
