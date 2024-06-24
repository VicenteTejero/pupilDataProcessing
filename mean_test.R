library(knitr)
# Filtrar para obtener el tiempo máximo por trial
filtered_data <- baseline_pupil %>%
  group_by(subject, trial) %>%
  filter(time == max(time)) %>%
  ungroup()

# Crear los dos grupos basados en crt
group1 <- filtered_data %>%
  filter(crt == 0 | crt == 1) %>%
  summarise(mean_max_time = mean(time))

group2 <- filtered_data %>%
  filter(crt == 2 | crt == 3) %>%
  summarise(mean_max_time = mean(time))

# Extraer los tiempos para cada grupo
times_group1 <- filtered_data %>%
  filter(crt == 0 | crt == 1) %>%
  pull(time)

times_group2 <- filtered_data %>%
  filter(crt == 2 | crt == 3) %>%
  pull(time)

# Realizar el t-test
t_test_result <- t.test(times_group1, times_group2)

# Mostrar los resultados
t_test_summary <- data.frame(
  statistic = t_test_result$statistic,
  p.value = t_test_result$p.value,
  conf.int.lower = t_test_result$conf.int[1],
  conf.int.upper = t_test_result$conf.int[2],
  mean_group1 = mean(times_group1),
  mean_group2 = mean(times_group2)
)

# Imprimir con kable
kable(t_test_summary, caption = "T-Test Results")
