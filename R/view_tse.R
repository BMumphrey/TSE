##Takes vectorized inputs for each field of a
##TSE data frame and plots it using ggplot2
plot_tse_data <- function(tse_data, parameter,
                          grouped = TRUE,
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
                          list(Text1 = sub_data$Text1, Time = sub_data$Time),
                          FUN = mean)
    names(sub_data) <- c("Text1", "Time", "Value")
  }

  g <- ggplot(data = sub_data, aes(x = Time, y = Value, color = Text1))
  g + geom_line()
}
