library(PupillometryR)
library(ggplot2)
library(readr)
library(zoo)
library(dplyr)

list_of_dataframes <- list()

for (i in 2:17) {

  if (i == 12 || i ==9) next

  file_name <- paste0('../ESG_exp/subject-', i, '.tsv')
  df <- read_tsv(file_name)
  df <- df[-c(1:4), ]
  
  # Preprocesamiento
  df$USER <- na.locf(df$USER, na.rm = FALSE)
  df <- df %>% rename(Trial = USER)
  df$Trial <- as.character(df$Trial)  # Asegurar que Trial sea de tipo character
  df$ID <- as.character(i)
  df <- df[df$FPOGV != 0, ]
  df <- df %>% filter(Trial != 'STOP_TRIAL')
  
  # Añadir el dataframe a la lista
  list_of_dataframes[[i]] <- df
}

# Combinar todos los dataframes en uno solo
df <- bind_rows(list_of_dataframes)

df <- df %>%
  mutate(Type = ifelse(grepl("COMBINED", Trial), "S&P y Bloomberg", "S&P"))
df <- df %>% select(ID, Trial,TIME,  LPD, RPD, Type)
df <- df %>% rename(
  Time = TIME,
  LPupil = LPD,
  RPupil = RPD
)

#Theme graficos
theme_set(theme_classic(base_size = 12))

df <- make_pupillometryr_data(data = df,
                                 subject = ID,
                                 trial = Trial,
                                 time = Time,
                                 condition = Type)

plot(df, pupil = LPupil, group = 'condition')
plot(df, pupil = LPupil, group = 'subject') 

regressed_data <- regress_data(data = df,
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

filtered_data <- filter_data(data = mean_data,
                             pupil = mean_pupil,
                             filter = 'median',
                             degree = 11)

plot(filtered_data, pupil = mean_pupil, group = 'subject')






