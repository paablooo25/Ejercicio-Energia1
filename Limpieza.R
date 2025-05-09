# 2. Limpieza de datos
variable <- miss_var_summary(df)
variable[variable$n_miss < 5,]

miss_case_summary(df)

vis_miss(df, cluster = TRUE)

# Reemplazo de valores faltantes por ceros
df[is.na(df)] <- 0