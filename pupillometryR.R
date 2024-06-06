library(PupillometryR)
library(ggplot2)

data("pupil_data")
#Check that IDs are not numeric
pupil_data$ID <- as.character(pupil_data$ID)
#remove participant number 8, who had problematic data
pupil_data <- subset(pupil_data, ID != 8)
#blinks were registered as -1, so replace with NAs
pupil_data$LPupil[pupil_data$LPupil == -1] <- NA
pupil_data$RPupil[pupil_data$RPupil == -1] <- NA

theme_set(theme_classic(base_size = 12))
Sdata <- make_pupillometryr_data(data = pupil_data,
                                 subject = ID,
                                 trial = Trial,
                                 time = Time,
                                 condition = Type)

new_data <- replace_missing_data(data = Sdata)

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
                                  timebin_size = 50,
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



