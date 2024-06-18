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
  y = as.numeric(datos$BPOGY)
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
                                            baseline_window = c(500,1000),
                                            baseline_method = "sub")

timebinsz <- baseline_pupil %>%
  group_by(subject, trial) %>%
  mutate(pupilz = scale(pup_interp))

pup_missing <- count_missing_pupil(baseline_pupil, pupil = 'pupil', missingthresh = .5)

puphist <- ggplot(baseline_pupil, aes(x = pup_interp)) +
  geom_histogram(aes(y = ..count..), colour = "#468189", binwidth = 0.001) +
  geom_vline(xintercept = 3, linetype = 'dotted') +
  geom_vline(xintercept = 4.56, linetype = 'dotted') +
  xlab('Pupil Size') +
  ylab('Count') +
  theme_bw()

create_plot <- function(data, trial_name, main_title) {
  trial_data <- filter(data, trial == trial_name)$pup_interp
  smoothingSpline <- smooth.spline(trial_data, spar = 0.4)
  plot(trial_data, cex = 0.2, type = 'l', col = '#468189', xlab = 'Time', ylab = 'Pupil Size', main = main_title)
  lines(smoothingSpline, col = 'red')
}

# Set up the layout for 3 columns and 5 rows
par(mfrow = c(5, 3), mar = c(4, 4, 2, 1))

# Create and plot the graphs
create_plot(baseline_pupil, "1 año (S&P)", "Pupil Size for 1 año (S&P)")
create_plot(baseline_pupil, "2 años (S&P)", "Pupil Size for 2 años (S&P)")
create_plot(baseline_pupil, "3 años (S&P)", "Pupil Size for 3 años (S&P)")
create_plot(baseline_pupil, "4 años (S&P)", "Pupil Size for 4 años (S&P)")
create_plot(baseline_pupil, "5 años (S&P)", "Pupil Size for 5 años (S&P)")
create_plot(baseline_pupil, "6 años (S&P)", "Pupil Size for 6 años (S&P)")
create_plot(baseline_pupil, "7 años (S&P)", "Pupil Size for 7 años (S&P)")
create_plot(baseline_pupil, "1 año (S&P y Bloomberg)", "Pupil Size for 1 año (S&P y Bloomberg)")
create_plot(baseline_pupil, "2 años (S&P y Bloomberg)", "Pupil Size for 2 años (S&P y Bloomberg)")
create_plot(baseline_pupil, "3 años (S&P y Bloomberg)", "Pupil Size for 3 años (S&P y Bloomberg)")
create_plot(baseline_pupil, "4 años (S&P y Bloomberg)", "Pupil Size for 4 años (S&P y Bloomberg)")
create_plot(baseline_pupil, "5 años (S&P y Bloomberg)", "Pupil Size for 5 años (S&P y Bloomberg)")
create_plot(baseline_pupil, "6 años (S&P y Bloomberg)", "Pupil Size for 6 años (S&P y Bloomberg)")
create_plot(baseline_pupil, "7 años (S&P y Bloomberg)", "Pupil Size for 7 años (S&P y Bloomberg)")

# Reset the layout
par(mfrow = c(1, 1))
