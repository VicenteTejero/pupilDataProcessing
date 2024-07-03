create_plot <- function(df, period) {
  filtered_data <- df[df$Period == period,]
  
  if (nrow(filtered_data) > 0) {
    plot <- ggplot(filtered_data, aes(x = normSP, y = return*100, label = Ticker)) +
      geom_point(aes(size = desvSP, colour = desvReturn)) +
      scale_colour_gradient(low = "darkred", high = "red", name = "Desviación\nRetorno") +
      geom_text(vjust = -1.5, hjust = 1.5) +
      scale_size(range = c(3, 10), name = "Desviación\nASG S&P") +
      scale_x_continuous("Métrica ASG S&P", limits = c(0, 1)) +
      scale_y_continuous("Retorno (%)", limits = c(-25, 40)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0)
      )
    return(plot)
  } else {
    return(NULL)
  }
}

create_plot_combined <- function(df, period) {
  filtered_data <- df[df$Period == period,]
  
  if (nrow(filtered_data) > 0) {
    plot <- ggplot(filtered_data, aes(x = ESGPond, y = return*100, label = Ticker)) +
      geom_point(aes(size = desvPond, colour = desvReturn)) +
      scale_colour_gradient(low = "darkred", high = "red", name = "Desviación\nRetorno") +
      geom_text(vjust = -1.5, hjust = 1.5) +
      scale_size(range = c(3, 10), name = "Desviación\nASG") +
      scale_x_continuous("Promedio Métricas Bloomberg y S&P", limits = c(0, 1)) +
      scale_y_continuous("Retorno (%)", limits = c(-25, 40)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0)
      )
    return(plot)
  } else {
    return(NULL)
  }
}

periods <- unique(data$Period)
plots <- lapply(periods, function(period) create_plot(data, period))
names(plots) <- periods
plotsCombined <- lapply(periods, function(period) create_plot_combined(data, period))
names(plotsCombined) <- periods





