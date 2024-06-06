# Cargar los paquetes necesarios
library(PupillometryR)
library(ggplot2)
library(readr)
library(zoo)
library(dplyr)

# Inicializar una lista para almacenar los dataframes
list_of_dataframes <- list()

# Bucle para leer y preprocesar cada archivo
for (i in 2:17) {
  if (i == 12 || i == 9) next
  
  file_name <- paste0('../ESG_exp/subject-', i, '.tsv')
  df <- read_tsv(file_name)
  df <- df[-c(1:4), ]
  
  # Preprocesamiento
  df <- df %>% rename(Trial = USER)
  
  df <- df %>%
    mutate(Time_Trial = {
      start_time <- NA
      time_trial <- numeric(length(df$TIME))
      
      for (j in seq_along(df$TIME)) {
        if (grepl("START_TRIAL", df$Trial[j])) {
          start_time <- df$TIME[j]
          time_trial[j] <- 0
        } else if (!is.na(start_time)) {
          time_trial[j] <- df$TIME[j] - start_time
        }
      }
      time_trial
    })
  
  df$Trial <- na.locf(df$Trial, na.rm = FALSE)
  df$Trial <- as.character(df$Trial)  # Asegurar que Trial sea de tipo character
  df$ID <- as.character(i)
  df <- df[df$FPOGV != 0, ]
  df <- df %>% filter(Trial != 'STOP_TRIAL')
  

  
  # Añadir el dataframe a la lista
  list_of_dataframes[[i]] <- df
}

# Combinar todos los dataframes en uno solo
df <- bind_rows(list_of_dataframes)

# Crear la columna Type basada en la columna Trial
df <- df %>%
  mutate(Type = ifelse(grepl("COMBINED", Trial), "S&P y Bloomberg", "S&P"))

# Seleccionar y renombrar columnas
df <- df %>% select(ID, Trial, Time_Trial, LPD, RPD, Type) %>%
  rename(
    Time = Time_Trial,
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

plot(mean_data, pupil = mean_pupil, group = 'condition')
plot(mean_data, pupil = mean_pupil, group = 'subject')

mean_data_downsample <- downsample_time_data(data = mean_data,
                                  pupil = mean_pupil,
                                  timebin_size = 0.1,
                                  option = 'median')

filtered_data <- filter_data(data = mean_data_downsample,
                             pupil = mean_pupil,
                             filter = 'median',
                             degree = 11)

plot(filtered_data, pupil = mean_pupil, group = 'condition')
plot(filtered_data, pupil = mean_pupil, group = 'subject')
plot(filtered_data, pupil = mean_pupil, group = 'Type')

ggplot(filtered_data, aes(x = Time, y = Trial, color = Type)) +
  geom_line() +
  labs(title = "Pupil Dilation Over Time by Trial",
       x = "Time Trial",
       y = "Mean Pupil Dilation",
       color = "Trial") +
  theme_minimal() +
  theme(legend.position = "right")





