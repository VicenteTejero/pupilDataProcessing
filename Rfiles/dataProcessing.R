# Cargar los paquetes necesarios
library(PupillometryR)
library(ggplot2)
library(zoo)
library(dplyr)
library(kableExtra)


#Theme graficos
theme_set(theme_classic(base_size = 12))

df <- make_pupillometryr_data(data = df,
                                 subject = ID,
                                 trial = Trial,
                                 time = Time,
                                 condition = Type)

regressed_data <- regress_data(data = df,
                               pupil1 = RPupil,
                               pupil2 = LPupil)

mean_data <- calculate_mean_pupil_size(data = regressed_data, 
                                       pupil1 = RPupil, 
                                       pupil2 = LPupil)

mean_data2 <- clean_missing_data(mean_data,
                                 pupil = mean_pupil,
                                 trial_threshold = .75,
                                 subject_trial_threshold = .75)

test <- mean_data %>% filter(ID == 13)

mean_data_downsample <- downsample_time_data(data = mean_data2,
                                  pupil = mean_pupil,
                                  timebin_size = 0.05,
                                  option = 'median')


filtered_data <- filter_data(data = mean_data_downsample,
                             pupil = mean_pupil,
                             filter = 'median',
                             degree = 11)

int_data <- interpolate_data(data = mean_data_downsample,
                             pupil = mean_pupil,
                             type = 'linear')

base_data <- baseline_data(data = int_data,
                           pupil = mean_pupil,
                           start = 0,
                           stop = 100)

base_data <- base_data %>%
  filter(mean_pupil < 10)

base_data_correctAnswer <- base_data %>%
  filter(Trial %in% c('1 año (S&P)', '2 años (S&P)', '3 años (S&P)'))


base_data_valid <- base_data %>%
  filter(Trial != '1 año (S&P)' & Trial != '2 años (S&P)' & Trial != '3 años (S&P)')

#MEANS
print(mean(base_data_valid$mean_pupil))
print(mean(base_data_correctAnswer$mean_pupil))

mean_data_byID <- int_data %>%
  group_by(ID) %>%
  summarise(mean_pupil_mean = mean(mean_pupil, na.rm = TRUE)) %>%
  arrange(ID)

# Output the table
kable(mean_data_byID, format = "simple", booktabs = TRUE, col.names = c("ID", "Promedio del Tamaño de Pupila"))

deviation_summary <- int_data %>%
  group_by(ID) %>%
  summarise(
    mean_pupil_mean = mean(mean_pupil, na.rm = TRUE),
    std_deviation = sd(mean_pupil, na.rm = TRUE)
  ) %>%
  arrange(ID)

# Output the table
kable(deviation_summary, format = "simple", booktabs = TRUE, col.names = c("ID", "Promedio del Tamaño de Pupila", "Desviación Estándar"))

##PLOTS

plot(mean_data_downsample, pupil = mean_pupil, group = 'condition')
plot(mean_data_downsample, pupil = mean_pupil, group = 'subject')
plot(mean_data_downsample, pupil = mean_pupil, group = 'Type')
plot(int_data, pupil = mean_pupil, group = 'subject')
plot(int_data, pupil = mean_pupil, group = 'subject')
plot(base_data, pupil = mean_pupil, group = 'subject')


# Grafico de la dilatacion pupilar promedio por tiempo y por ensayo
ggplot(base_data, aes(x = Time, y = mean_pupil, color = Trial)) +
  geom_line() +
  labs(title = "Pupil Dilation Over Time by Trial",
       x = "Time Trial",
       y = "Mean Pupil Size",
       color = "Trial") +
  theme_minimal() +
  theme(legend.position = "right")


# Crear el gráfico faceteado por 'Type'
ggplot(base_data, aes(x = Time, y = mean_pupil, color = Trial)) +
  geom_line() +
  facet_wrap(~ Type) +
  labs(title = "Pupil Dilation Over Time by Trial",
       x = "Time Trial",
       y = "Mean Pupil Size",
       color = "Trial") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(base_data_correctAnswer, aes(x = Time, y = mean_pupil, color = Trial)) +
  geom_line() +
  facet_wrap(~ Type) +
  labs(title = "Pupil Dilation Over Time by Trial in correct answer graphs",
       x = "Time Trial",
       y = "Mean Pupil Size",
       color = "Trial") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(base_data_valid, aes(x = Time, y = mean_pupil, color = Trial)) +
  geom_line() +
  labs(title = "Pupil Dilation Over Time by Tria in decision graphs",
       x = "Time Trial",
       y = "Mean Pupil Size",
       color = "Trial") +
  theme_minimal() +
  theme(legend.position = "right")






