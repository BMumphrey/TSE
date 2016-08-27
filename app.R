library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)

##Test data set until uploading is implemented
boxes <- read_tse_boxes("data/berthoud_female_cohort1_RYGB_40215 low temp.csv", 24)
tse_data <- read_tse_data("data/berthoud_female_cohort1_RYGB_40215 low temp.csv", 24)

ui <- fluidPage(
  tableOutput("boxes"),
  selectInput(inputId = "parameter",
              label = "Parameter",
              choices = as.character(unique(tse_data$Parameter))),
  plotOutput("plot")
)

server <- function(input, output) {
  output$boxes <- renderTable(boxes)
  output$plot <- renderPlot(plot_tse_line(tse_data, input$parameter))
}

shinyApp(ui = ui, server = server)
