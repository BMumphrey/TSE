library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
source("R/view_tse.R")
source("R/read_tse.R")
source("R/analyze_tse.R")

##Test data set until uploading is implemented
boxes <- read_tse_boxes("data/berthoud_female_cohort1_RYGB_40215 low temp.csv", 24)
tse_data <- read_tse_data("data/berthoud_female_cohort1_RYGB_40215 low temp.csv", 24)

ui <- fluidPage(
  ## custom CSS for 4 column layout (used below for Boxes options)
  tags$head(
    tags$style(HTML("

                    .multicol {

                    -webkit-column-count: 4; /* Chrome, Safari, Opera */

                    -moz-column-count: 4; /* Firefox */

                    column-count: 4;

                    }

                    "))),
  fluidRow(
    column(4,
           ##Dropdown menu to select the parameter to plot
           selectInput(inputId = "parameter",
                       label = "Parameter",
                       choices = as.character(unique(tse_data$Parameter))),
           ##Checkbox group to choose which boxes to plot
           tags$div(
             class = "multicol",
             checkboxGroupInput(inputId = "boxes",
                                label = "Boxes",
                                choices = unique(tse_data$Box),
                                selected = unique(tse_data$Box)))
    ),
    column(4,
           ##Dropdown menu to choose which text field to group/color the graph by
           selectInput(inputId = "groups",
                       label = "Group by:",
                       choices = c("Text1",
                                   "Text2")),
           ##Checkbox to choose whether or not to group the data
           checkboxInput(inputId = "grouped", label = "Grouped", value = TRUE)
    ),
    column(4,
           ##Text field to input start time
           textInput(inputId = "start",
                     label = "Start time (YYYY-MM-DD HH:MM:SS)",
                     value = min(tse_data$Time)),

           ##Text field to input end time
           textInput(inputId = "end",
                     label = "End time (YYYY-MM-DD HH:MM:SS)",
                     value = max(tse_data$Time)),

           ##Update button to apply changes to time range
           actionButton(inputId = "update_time",
                        label = "Update time range")
    )
  ),
  fluidRow(
    column(9, plotOutput("plot_line")),
    column(3, plotOutput("plot_bar"))
    ),
  fluidRow(tableOutput("boxes"))
)

server <- function(input, output) {
  start <- eventReactive(input$update_time, {
    ymd_hms(input$start)
  })

  end <- eventReactive(input$update_time, {
    ymd_hms(input$end)
  })

  output$plot_line <- renderPlot(plot_tse_line(tse_data,
                                               input$parameter,
                                               grouped = input$grouped,
                                               group = input$groups,
                                               boxes = input$boxes,
                                               time_range = start():end()))

  output$plot_bar <- renderPlot(plot_tse_bar(tse_data,
                                             input$parameter,
                                             period = c("Light", "Dark"),
                                             grouped = input$grouped,
                                             group = input$groups,
                                             boxes = input$boxes,
                                             time_range = start():end()))


  output$boxes <- renderTable(boxes)
}

shinyApp(ui = ui, server = server)
