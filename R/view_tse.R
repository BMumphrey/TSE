##Takes vectorized inputs for each field of a
##TSE data frame and plots it using ggplot2
plot_tse_line <- function(tse_data, parameter,
                          grouped = TRUE,
                          group = "Text1",
                          day_range = "all") {

  if (day_range[[1]] == "all") {
    day_range = min(tse_data$Day):max(tse_data$Day)
  }


  #Subset data based on user inputs
  sub_data <- tse_data[tse_data$Parameter == parameter
                       & tse_data$Day %in% day_range,]

  ##Average data at each time point within each group
  ##unless otherwise specified
  if (grouped == TRUE) {
    sub_data <- aggregate(sub_data$Value,
                          list(group = sub_data[[group]], Time = sub_data$Time),
                          FUN = mean)
    names(sub_data) <- c(group, "Time", "Value")
  }

  group_color <- sub_data[[group]]

  g <- ggplot(data = sub_data, aes(x = Time,
                                   y = Value,
                                   color = group_color))
  g + geom_line()
}
