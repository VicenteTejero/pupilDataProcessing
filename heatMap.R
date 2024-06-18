library(ggplot2)
library(dplyr)

# Create heatmaps for each individual and graphic
unique_individuos <- unique(df1$ID)
unique_graficas <- unique(df1$Trial)

for (individuo in unique_individuos) {
  for (grafica in unique_graficas) {
    # Filter the data for each individual and graphic
    subset <- df1 %>% filter(ID == individuo, Trial == grafica)
    
    # Ensure data is within the 0-1 range and remove any NA values
    subset <- subset %>% filter(!is.na(BPOGX), !is.na(BPOGY), BPOGX >= 0, BPOGX <= 1, BPOGY >= 0, BPOGY <= 1)
    
    # Check if the subset is not empty
    if (nrow(subset) > 0) {
      # Check if there are at least two unique points
      if (length(unique(subset$BPOGX)) > 1 && length(unique(subset$BPOGY)) > 1) {
        # Create the heatmap
        p <- ggplot(subset, aes(x = BPOGX, y = BPOGY)) +
          stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5) +
          scale_fill_gradient(low = "blue", high = "red") +
          ggtitle(paste('Mapa de Calor - Individuo', individuo, '- Gráfica', grafica)) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) +
          coord_fixed() +
          scale_y_reverse() +  # Invert y axis to match screen coordinates
          xlim(-0.5, 1.5) +  # Set x range from 0 to 1
          ylim(-0.5, 1)    # Set y range from 0 to 1
        
        # Save the heatmap
        ggsave(filename = paste0('heatMaps/mapa_calor_', individuo, '_', grafica, '.png'), plot = p, width = 8, height = 6)
      } else {
        message(paste('Not enough unique points for Individuo', individuo, 'and Gráfica', grafica))
      }
    } else {
      message(paste('No valid data for Individuo', individuo, 'and Gráfica', grafica))
    }
  }
}



