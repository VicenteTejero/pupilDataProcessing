source('pre-run.R')
library(tidyverse)
library(gazer)
library(zoo)
library(knitr)
library(dplyr)
library(tidyr)
library(devtools)
library(ggplot2)



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


#baseline_pupil <- baseline_correction_pupil_msg(smooth_interp, pupil_colname = "pup_interp",
#                                                baseline_dur = 100,
#                                                event = "Trial Start",
#                                                baseline_method = "div")    


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

print(puphist)

pup_outliers <- pup_missing %>%
  filter(pup_interp >= 3, pup_interp <= 4.56)    #inspección visual
