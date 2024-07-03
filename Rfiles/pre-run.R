library(readr)
library(tidyverse)
library(zoo)
library(readxl)

respuestasCuestionario <- read_excel("Data/respuestasCuestionario.xlsx")
data <- read_excel("Data/filteredData.xlsx", sheet = "Analysis (2)")
dataSQM <- read_excel("Data/filteredData.xlsx", sheet = 'SQM',
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))
# Inicializar una lista para almacenar los dataframes
list_of_dataframes <- list()

# Bucle para leer y preprocesar cada archivo
for (i in 2:17) {
  if (i == 12 || i == 9) next
  
  file_name <- paste0('Data/ESG_exp/subject-', i, '.tsv')
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
  df$ID <- as.integer(i)
  df <- df %>% filter(Trial != 'STOP_TRIAL')
  
  # Renombrar Trial basado en patrones
  df <- df %>%
    mutate(Trial = case_when(
      grepl("PLOT_1YTD", Trial) ~ "1 año (S&P)",
      grepl("PLOT_COMBINED_1YTD", Trial) ~ "1 año (S&P y Bloomberg)",
      grepl("PLOT_2YTD", Trial) ~ "2 años (S&P)",
      grepl("PLOT_COMBINED_2YTD", Trial) ~ "2 años (S&P y Bloomberg)",
      grepl("PLOT_3YTD", Trial) ~ "3 años (S&P)",
      grepl("PLOT_COMBINED_3YTD", Trial) ~ "3 años (S&P y Bloomberg)",
      grepl("PLOT_4YTD", Trial) ~ "4 años (S&P)",
      grepl("PLOT_COMBINED_4YTD", Trial) ~ "4 años (S&P y Bloomberg)",
      grepl("PLOT_5YTD", Trial) ~ "5 años (S&P)",
      grepl("PLOT_COMBINED_5YTD", Trial) ~ "5 años (S&P y Bloomberg)",
      grepl("PLOT_6YTD", Trial) ~ "6 años (S&P)",
      grepl("PLOT_COMBINED_6YTD", Trial) ~ "6 años (S&P y Bloomberg)",
      grepl("PLOT_7YTD", Trial) ~ "7 años (S&P)",
      grepl("PLOT_COMBINED_7YTD", Trial) ~ "7 años (S&P y Bloomberg)",
      TRUE ~ Trial  # Mantener los valores originales si no coinciden
    ))
  
  # Añadir el dataframe a la lista
  list_of_dataframes[[i]] <- df
}

# Combinar todos los dataframes en uno solo
df <- bind_rows(list_of_dataframes)

df <- df %>%
  mutate(
    RPD = ifelse(FPOGV == 0, NA, RPD),
    LPD = ifelse(FPOGV == 0, NA, LPD)
  )

# Crear la columna Type basada en la columna Trial
df <- df %>%
  mutate(Type = ifelse(grepl("Bloomberg", Trial), "S&P y Bloomberg", "S&P"))

# Seleccionar y renombrar columnas
df <- df %>%
  select(ID, Trial, Time_Trial, LPD, RPD, BPOGX, BPOGY, Type) %>%
  rename(
    Time = Time_Trial,
    LPupil = LPD,
    RPupil = RPD
  ) %>%
  mutate(MeanPupil = (LPupil + RPupil) / 2)

# Rename the subject column to ID for consistency
respuestasCuestionario <- respuestasCuestionario %>%
  rename(ID = subject)

# Select specific columns from respuestasCuestionario
selected_columns <- respuestasCuestionario %>%
  select(ID, total_crt, edad, genero, interes_empresas_asg, estilo_decisiones)  # Replace column1, column2, column3 with actual column names you need

# Perform the left join to add the selected columns to df
df <- df %>%
  left_join(selected_columns, by = "ID")




