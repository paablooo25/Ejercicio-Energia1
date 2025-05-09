# 4. Privacidad Diferencial
set.seed(202)

# Selección de columnas
df_consumoE <- df %>% dplyr::select(c(1, 2, 3, 4, 7:11))

# Sumamos las columnas para obtener el consumo en una columna CET
df_consumoE$CET <- apply(df_consumoE[, c(5, 6, 7, 8, 9)], 1, sum)

# Elimina las columnas originales que contribuyen al CET
df_consumoE <- df_consumoE[, -c(5, 6, 7, 8, 9)]

# Calculamos el dato real de consumo medio por region
gasto_region <- df_consumoE %>%
  group_by(Region) %>%
  summarise(consumo_medio = mean(CET))

# Calculamos el dato real del consumo medio por division
gasto_division <- df_consumoE %>%
  group_by(Division) %>%
  summarise(consumo_medio = mean(CET))

# Implementación de ruido diferencial para los datos por región
eps <- 0.1

# Sensibilidad para las regiones
max_reg <- max(gasto_region$consumo_medio)
min_reg <- min(gasto_region$consumo_medio)
n_reg <- nrow(gasto_region)
sensibilidad <- (max_reg - min_reg) / n_reg

# Aplicación de ruido Laplaciano a los datos de consumo medio por región
consumo_medio_ruido <- rdoublex(1, gasto_region$consumo_medio, sensibilidad / eps)

# Combina los datos originales con el consumo medio con ruido
df_para_rusia1 <- cbind(gasto_region[, 1], consumo_medio_ruido)

# Sensibilidad para las divisiones
max_div <- max(gasto_division$consumo_medio)
min_div <- min(gasto_division$consumo_medio)
n_div <- nrow(gasto_division)
sensibilidad <- (max_div - min_div) / n_div

# Aplicación de ruido Laplaciano a los datos de consumo medio por división
consumo_medio_ruido <- rdoublex(1, gasto_division$consumo_medio, sensibilidad / eps)

# Combina los datos originales con el consumo medio con ruido
df_para_rusia2 <- cbind(gasto_division[, 1], consumo_medio_ruido)
