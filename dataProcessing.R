library(PupillometryR)
library(ggplot2)
library(readr)
library(zoo)
library(dplyr)

s17 <- read_tsv('../ESG_exp/subject-17.tsv')
s17 <- s17[-c(1:4), ]

# Pre-procesamiento db
s17$USER <- na.locf(s17$USER, na.rm = FALSE)
s17 <- s17 %>% rename(Trial = USER)
s17$ID <- 17
s17$ID <- as.character(s17$ID)
s17 <- s17[s17$FPOGV != 0, ]
s17 <- s17 %>% filter(Trial != 'STOP_TRIAL')
s17 <- s17 %>%
  mutate(Type = ifelse(grepl("COMBINED", Trial), "S&P y Bloomberg", "S&P"))
s17 <- s17 %>% select(ID, Trial,TIME,  LPD, RPD, Type)
s17 <- s17 %>% rename(
  Time = TIME,
  LPupil = LPD,
  RPupil = RPD
)

#Theme graficos
theme_set(theme_classic(base_size = 12))

Sdata17 <- make_pupillometryr_data(data = s17,
                                 subject = ID,
                                 trial = Trial,
                                 time = Time,
                                 condition = Type)

new_data <-  Sdata17

plot(new_data, pupil = LPupil, group = 'condition')
plot(new_data, pupil = LPupil, group = 'subject') 

regressed_data <- regress_data(data = new_data,
                               pupil1 = RPupil,
                               pupil2 = LPupil)

mean_data <- calculate_mean_pupil_size(data = regressed_data, 
                                       pupil1 = RPupil, 
                                       pupil2 = LPupil)

plot(mean_data, pupil = mean_pupil, group = 'subject')

mean_data <- downsample_time_data(data = mean_data,
                                  pupil = mean_pupil,
                                  timebin_size = 10,
                                  option = 'median')

missing <- calculate_missing_data(mean_data, 
                                  mean_pupil)

mean_data2 <- clean_missing_data(mean_data,
                                 pupil = mean_pupil,
                                 trial_threshold = .75,
                                 subject_trial_threshold = .75)

filtered_data <- filter_data(data = mean_data2,
                             pupil = mean_pupil,
                             filter = 'median',
                             degree = 11)

plot(filtered_data, pupil = mean_pupil, group = 'subject')






