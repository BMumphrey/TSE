library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)

##Test data set until uploading is implemented
boxes <- read_tse_boxes("data/berthoud_female_cohort1_RYGB_40215 low temp.csv", 24)
tse_data <- read_tse_data("data/berthoud_female_cohort1_RYGB_40215 low temp.csv", 24)

ui <- fluidPage(
  ##Dropdown menu to select the parameter to plot
  selectInput(inputId = "parameter",
              label = "Parameter",
              choices = as.character(unique(tse_data$Parameter))),

  ##Checkbox to choose whether or not to group the data
  checkboxInput(inputId = "grouped", label = "Grouped", value = TRUE),

  ##Dropdown menu to choose which text field to group/color the graph by
  selectInput(inputId = "groups",
                     label = "Group by:",
                     choices = c("Text1",
                                 "Text2",
                                 "Text3")),

  ##Checkbox group to choose which boxes to plot
  checkboxGroupInput(inputId = "boxes",
                     label = "Boxes",
                     choices = unique(tse_data$Box),
                     selected = unique(tse_data$Box)),
  plotOutput("plot"),
  tableOutput("boxes")
)

server <- function(input, output) {
  output$plot <- renderPlot(plot_tse_line(tse_data,
                                          input$parameter,
                                          grouped = input$grouped,
                                          group = input$groups,
                                          boxes = input$boxes))
  output$boxes <- renderTable(boxes)
}

shinyApp(ui = ui, server = server)
