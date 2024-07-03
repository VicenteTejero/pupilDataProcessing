source('pre-run.R')
library(tidyverse)
library(gazer)
library(zoo)
library(knitr)
library(dplyr)
library(tidyr)
library(devtools)
library(ggplot2)
library(saccades)
library(gridExtra)
library(grid)

datos <- df
pupil_data <- data.frame(
  subject = as.numeric(datos$ID),
  time = as.numeric(datos$Time),
  trial = as.character(datos$Trial),
  pupil = as.numeric(datos$MeanPupil),
  x = as.numeric(datos$BPOGX),
  y = as.numeric(datos$BPOGY),
  age = as.numeric(datos$edad),
  gender = as.character(datos$genero),
  crt = as.integer(datos$total_crt),
  esg_investment = as.character(datos$interes_empresas_asg),
  analisys = as.character(datos$estilo_decisiones)
)

behave_data <- behave_pupil(pupil_data, omiterrors = FALSE, 
                            behave_colnames = c('subject', 'trial', 'x', 'y'))

pup_extend <- pupil_data %>%
  group_by(subject, trial) %>%
  mutate(extendpupil = extend_blinks(pupil, fillback = 30, fillforward = 30, hz = 60))

smooth_interp <- smooth_interpolate_pupil(pup_extend, pupil = 'pupil', 
                                          extendpupil = 'extendpupil',
                                          extendblinks = TRUE, step.first = 'interp',
                                          filter = 'moving', maxgap = Inf, type = 'linear',
                                          hz = 60, n = 5)

baseline_pupil <- baseline_correction_pupil(smooth_interp, pupil_colname = 'pup_interp',
                                            baseline_window = c(0,200),
                                            baseline_method = "sub")

timebinsz <- baseline_pupil %>%
  group_by(subject, trial) %>%
  mutate(pupilz = scale(pup_interp))

pup_missing <- count_missing_pupil(baseline_pupil, pupil = 'pupil', missingthresh = 0.5)

puphist <- ggplot(baseline_pupil, aes(x = baselinecorrectedp)) +
  geom_histogram(aes(y = ..count..), colour = "#468189", binwidth = 0.001) +
  geom_vline(xintercept = 3, linetype = 'dotted') +
  geom_vline(xintercept = 4.56, linetype = 'dotted') +
  xlab('Pupil Size') +
  ylab('Count') +
  theme_bw()
print(puphist)

baseline_pupil <- pup_missing %>%
dplyr::filter(baselinecorrectedp >= -5, baselinecorrectedp <= 5) 



create_plot_pupil <- function(data, trial_name, main_title) {
    trial_data <- filter(data, trial == trial_name) %>%
                  arrange(time)

    
    #smoothingSpline <- smooth.spline(trial_data, spar = 0.4)
    #plot(trial_data, cex = 0.2, type = 'l', col = '#468189', xlab = 'Time', ylab = 'Pupil Size', main = main_title)
    #lines(smoothingSpline, col = 'red')
    
    smoothingSpline1 <- smooth.spline(x = trial_data$time, y = trial_data$baselinecorrectedp, spar = 0.4)
    plot(1, type = 'n', xlim = c(1, max(max(trial_data$time), max(trial_data$time))), ylim = c(min(baseline_pupil$baselinecorrectedp), max(baseline_pupil$baselinecorrectedp)),
         xlab = 'Time', ylab = 'Pupil Size', main = main_title)
    lines(smoothingSpline1, col = 'red')
  }

create_plot_pupil2 <- function(data, main_title) {
  ggplot(data, aes(x = 1:nrow(data), y = pup_interp, color = trial)) +
    geom_line(size = 0.2) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, color = 'red') +
    labs(title = main_title, x = 'Time', y = 'Pupil Size') +
    theme_minimal()
}
create_plot_pupilCRT <- function(data, trial_name, main_title) {
  trial_data <- filter(data, trial == trial_name)

  subset_1 <- filter(trial_data, crt == 2 | crt == 3) %>%
    arrange(time)
  subset_2 <- filter(trial_data, crt == 0 | crt == 1) %>%
    arrange(time)
  
  plot(1, type = "n", xlim = c(0, max(max(subset_1$time), max(subset_2$time))), ylim = c(min(baseline_pupil$baselinecorrectedp), max(baseline_pupil$baselinecorrectedp)),
       xlab = 'Time', ylab = 'Pupil Size', main = main_title)

  smoothingSpline1 <- smooth.spline(x = subset_1$time, y = subset_1$baselinecorrectedp, spar = 0.4)
  lines(smoothingSpline1, col = 'blue')
  
  smoothingSpline2 <- smooth.spline(x = subset_2$time, y = subset_2$baselinecorrectedp, spar = 0.4)
  lines(smoothingSpline2, col = 'red')
}

create_plot_pupil_interest <- function(data, trial_name, main_title) {
  trial_data <- filter(data, trial == trial_name)
  
  subset_1 <- filter(trial_data, esg_investment == 'Nada interesado/a' | esg_investment == 'Poco interesado/a') %>%
    arrange(time)
  subset_2 <- filter(trial_data, esg_investment == 'Algo interesado/a' | esg_investment == 'Muy interesado/a') %>%
    arrange(time)
  
  plot(1, type = "n", xlim = c(0, max(max(subset_1$time), max(subset_2$time))), ylim = c(min(baseline_pupil$baselinecorrectedp), max(baseline_pupil$baselinecorrectedp)),
       xlab = 'Time', ylab = 'Pupil Size', main = main_title)
  
  smoothingSpline1 <- smooth.spline(x = subset_1$time, y = subset_1$baselinecorrectedp, spar = 0.4)
  lines(smoothingSpline1, col = 'red')
  
  smoothingSpline2 <- smooth.spline(x = subset_2$time, y = subset_2$baselinecorrectedp, spar = 0.4)
  lines(smoothingSpline2, col = 'blue')
}

# Set up the layout for 3 columns and 5 rows
par(mfrow = c(5, 3), mar = c(4, 4, 2, 1))

# Create and plot the graphs
create_plot_pupil(baseline_pupil, "1 año (S&P)", "Pupil Size for 1 año (S&P)")
create_plot_pupil(baseline_pupil, "2 años (S&P)", "Pupil Size for 2 años (S&P)")
create_plot_pupil(baseline_pupil, "3 años (S&P)", "Pupil Size for 3 años (S&P)")
create_plot_pupil(baseline_pupil, "4 años (S&P)", "Pupil Size for 4 años (S&P)")
create_plot_pupil(baseline_pupil, "5 años (S&P)", "Pupil Size for 5 años (S&P)")
create_plot_pupil(baseline_pupil, "6 años (S&P)", "Pupil Size for 6 años (S&P)")
create_plot_pupil(baseline_pupil, "7 años (S&P)", "Pupil Size for 7 años (S&P)")
create_plot_pupil(baseline_pupil, "1 año (S&P y Bloomberg)", "Pupil Size for 1 año (S&P y Bloomberg)")
create_plot_pupil(baseline_pupil, "2 años (S&P y Bloomberg)", "Pupil Size for 2 años (S&P y Bloomberg)")
create_plot_pupil(baseline_pupil, "3 años (S&P y Bloomberg)", "Pupil Size for 3 años (S&P y Bloomberg)")
create_plot_pupil(baseline_pupil, "4 años (S&P y Bloomberg)", "Pupil Size for 4 años (S&P y Bloomberg)")
create_plot_pupil(baseline_pupil, "5 años (S&P y Bloomberg)", "Pupil Size for 5 años (S&P y Bloomberg)")
create_plot_pupil(baseline_pupil, "6 años (S&P y Bloomberg)", "Pupil Size for 6 años (S&P y Bloomberg)")
create_plot_pupil(baseline_pupil, "7 años (S&P y Bloomberg)", "Pupil Size for 7 años (S&P y Bloomberg)")

# Reset the layout
par(mfrow = c(1, 1))

# Save baseline data
save(baseline_pupil, file = "baseline_pupil.RData")

create_plot_pupil2(baseline_pupil %>% filter(baseline_pupil$crt == 2 | baseline_pupil$crt == 3), "Pupil Size for 1 año (S&P) Good CRT")
create_plot_pupil2(baseline_pupil %>% filter(baseline_pupil$crt == 1 | baseline_pupil$crt == 0), "Pupil Size for 1 año (S&P) Bad CRT")

# Set up the layout for 3 columns and 5 rows
par(mfrow = c(5, 3), mar = c(4, 4, 2, 1))

# Create and plot the graphs
create_plot_pupilCRT(baseline_pupil, "1 año (S&P)", "Pupil Size for 1 año (S&P)")
create_plot_pupilCRT(baseline_pupil, "2 años (S&P)", "Pupil Size for 2 años (S&P)")
create_plot_pupilCRT(baseline_pupil, "3 años (S&P)", "Pupil Size for 3 años (S&P)")
create_plot_pupilCRT(baseline_pupil, "4 años (S&P)", "Pupil Size for 4 años (S&P)")
create_plot_pupilCRT(baseline_pupil, "5 años (S&P)", "Pupil Size for 5 años (S&P)")
create_plot_pupilCRT(baseline_pupil, "6 años (S&P)", "Pupil Size for 6 años (S&P)")
create_plot_pupilCRT(baseline_pupil, "7 años (S&P)", "Pupil Size for 7 años (S&P)")
create_plot_pupilCRT(baseline_pupil, "1 año (S&P y Bloomberg)", "Pupil Size for 1 año (S&P y Bloomberg)")
create_plot_pupilCRT(baseline_pupil, "2 años (S&P y Bloomberg)", "Pupil Size for 2 años (S&P y Bloomberg)")
create_plot_pupilCRT(baseline_pupil, "3 años (S&P y Bloomberg)", "Pupil Size for 3 años (S&P y Bloomberg)")
create_plot_pupilCRT(baseline_pupil, "4 años (S&P y Bloomberg)", "Pupil Size for 4 años (S&P y Bloomberg)")
create_plot_pupilCRT(baseline_pupil, "5 años (S&P y Bloomberg)", "Pupil Size for 5 años (S&P y Bloomberg)")
create_plot_pupilCRT(baseline_pupil, "6 años (S&P y Bloomberg)", "Pupil Size for 6 años (S&P y Bloomberg)")
create_plot_pupilCRT(baseline_pupil, "7 años (S&P y Bloomberg)", "Pupil Size for 7 años (S&P y Bloomberg)")


par(mfrow = c(1, 1))


summary_table <- baseline_pupil %>%
  group_by(trial) %>%
  summarise(
    min = min(baselinecorrectedp, na.rm = TRUE),
    max = max(baselinecorrectedp, na.rm = TRUE),
    mean = mean(baselinecorrectedp, na.rm = TRUE),
    median = median(baselinecorrectedp, na.rm = TRUE),
    sd = sd(baselinecorrectedp, na.rm = TRUE)
  )


kable(summary_table, caption = "Resumen Estadístico por Trial")
