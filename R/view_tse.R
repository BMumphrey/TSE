##Takes vectorized inputs for each field of a
##TSE data frame and plots a line graph using ggplot2
plot_tse_line <- function(tse_data, parameter,
                          grouped = TRUE,
                          group = "Text1",
                          day_range = unique(tse_data$Day),
                          boxes = unique(tse_data$Box),
                          time_range = min(tse_data$Time):max(tse_data$Time)) {
  #Subset data based on user inputs
  sub_data <- tse_data[tse_data$Parameter == parameter
                       & tse_data$Day %in% day_range
                       & tse_data$Box %in% boxes
                       & tse_data$Time %in% time_range,]

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

##Takes all data for a given parameter, calculates period
##data for each period, and plots the data as a histogram
plot_tse_bar <- function(tse_data, parameter, period,
                         grouped = TRUE,
                         group = "Text1",
                         day_range = unique(tse_data$Day),
                         boxes = unique(tse_data$Box),
                         time_range = min(tse_data$Time):max(tse_data$Time)) {
  ##Subset data based on user inputs
  sub_data <- tse_data[tse_data$Parameter == parameter
                       & tse_data$Day %in% day_range
                       & tse_data$Box %in% boxes
                       & tse_data$Time %in% time_range, ]

  ##Get period data
  period_data <- get_period_data(tse_data, parameter, period, factors = TRUE)

  ##Aggregate on requested factor
  aggregated_data <- aggregate(period_data$x, by = list(period_data$Text2),
                               FUN = function (x) {c(Mean = mean(x), SD = sd(x), N = length(x))})
  ##Flatten data frame
  aggregated_data <- do.call(data.frame, aggregated_data)

  ##Calculate standard error
  aggregated_data$x.SE <- aggregated_data$x.SD / sqrt(aggregated_data$x.N)

  limits <- aes(ymax = aggregated_data$x.Mean + aggregated_data$x.SE,
                ymin = aggregated_data$x.Mean - aggregated_data$x.SE)

  g <- ggplot(data = aggregated_data, aes(x = Group.1, y = x.Mean, fill = Group.1))
  g + geom_bar(stat = "identity") +
    geom_errorbar(limits, width = 0.4)

  }
