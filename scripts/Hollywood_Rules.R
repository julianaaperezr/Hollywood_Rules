# CASO 2 - HOLLYWOOD RULES
# Integrantes: Juliana Perez, Maria Camila Salamanca y Veronica Sandoval

# Cargar librerías
library(readxl)
library(dplyr)
library(gt)

# Importar base de datos
Hollywood <- read_excel(
  "data/Hollywood.xls",
  sheet = "Exhibit 1"
)


# Validacion inicial de la base de datos 
# Numero de observaciones
nrow(Hollywood)

# Nombres de las variables
colnames(Hollywood)

# Resumen de las variables
summary(Hollywood)


# PUNTO 1 - Descripcion inicial de los datos
# Opening Gross
min(Hollywood$`Opening Gross`)
mean(Hollywood$`Opening Gross`)
max(Hollywood$`Opening Gross`)

# Total U.S. Gross
min(Hollywood$`Total U.S. Gross`)
mean(Hollywood$`Total U.S. Gross`)
max(Hollywood$`Total U.S. Gross`)

# Total Non-U.S. Gross
min(Hollywood$`Total Non-U.S. Gross`)
mean(Hollywood$`Total Non-U.S. Gross`)
max(Hollywood$`Total Non-U.S. Gross`)

# Opening Theatres
min(Hollywood$`Opening Theatres`)
mean(Hollywood$`Opening Theatres`)
max(Hollywood$`Opening Theatres`)

# Número de películas por género
Hollywood %>%
  count(Genre)

# Número de películas según clasificación R
Hollywood %>%
  count(MPAA_D)

# Tabla para el informe
tabla_punto1 <- data.frame(
  Variable = c(
    "Opening Gross (USD)",
    "Total U.S. Gross (USD)",
    "Total Non-U.S. Gross (USD)",
    "Opening Theatres"
  ),
  Minimo = c(
    min(Hollywood$`Opening Gross`),
    min(Hollywood$`Total U.S. Gross`),
    min(Hollywood$`Total Non-U.S. Gross`),
    min(Hollywood$`Opening Theatres`)
  ),
  Promedio = c(
    mean(Hollywood$`Opening Gross`),
    mean(Hollywood$`Total U.S. Gross`),
    mean(Hollywood$`Total Non-U.S. Gross`),
    mean(Hollywood$`Opening Theatres`)
  ),
  Maximo = c(
    max(Hollywood$`Opening Gross`),
    max(Hollywood$`Total U.S. Gross`),
    max(Hollywood$`Total Non-U.S. Gross`),
    max(Hollywood$`Opening Theatres`)
  )
)

tabla_punto1

# Formato de la tabla para el informe
tabla_punto1_gt <- tabla_punto1 %>%
  gt() %>%
  tab_header(
    title = "Estadísticas descriptivas de las películas"
  ) %>%
  cols_label(
    Variable = "Variable",
    Minimo = "Mínimo",
    Promedio = "Promedio",
    Maximo = "Máximo"
  ) %>%
  fmt_number(
    columns = c(Minimo, Promedio, Maximo),
    decimals = 2,
    use_seps = TRUE
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboración propia con base en Hollywood Rules."
  )

tabla_punto1_gt


# PUNTO 2 - Retorno sobre la inversion (ROI)
# A.
# Calcular el ROI de cada pelicula
Hollywood <- Hollywood %>%
  mutate(
    ROI_US = (`Total U.S. Gross` - Budget) / Budget
  )

# Ver el ROI promedio
mean(Hollywood$ROI_US)

# B.
# Intervalo de confianza del 95% para el ROI promedio
resultado_ROI <- t.test(Hollywood$ROI_US)

resultado_ROI$conf.int

# C. 
# Prueba de hipotesis: ROI promedio mayor al 12%
prueba_ROI <- t.test(
  Hollywood$ROI_US,
  mu = 0.12,
  alternative = "greater"
)

prueba_ROI

# Tabla para el informe
# Crear tabla con los resultados obtenidos directamente del análisis
tabla_punto2 <- data.frame(
  Resultado = c(
    "ROI promedio",
    "Límite inferior IC 95%",
    "Límite superior IC 95%",
    "ROI de referencia",
    "p-value"
  ),
  Valor = c(
    mean(Hollywood$ROI_US),
    resultado_ROI$conf.int[1],
    resultado_ROI$conf.int[2],
    0.12,
    prueba_ROI$p.value
  )
)

tabla_punto2

# Dar formato a la tabla para el informe
tabla_punto2_gt <- tabla_punto2 %>%
  gt() %>%
  tab_header(
    title = "Resultados del análisis del ROI"
  ) %>%
  cols_label(
    Resultado = "Resultado",
    Valor = "Valor"
  ) %>%
  fmt_number(
    columns = Valor,
    decimals = 4
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboración propia con base en Hollywood Rules."
  )

# Mostrar tabla final
tabla_punto2_gt


# PUNTO 3 - Comedias v.s otros generos 
# A. 
# Comparación del Total U.S. Gross
comedias <- Hollywood %>%
  filter(Genre == "Comedy")

no_comedias <- Hollywood %>%
  filter(Genre != "Comedy")

# Promedio del Total U.S. Gross por grupo
mean(comedias$`Total U.S. Gross`)
mean(no_comedias$`Total U.S. Gross`)

# Prueba de diferencia de medias del Total U.S. Gross
prueba_gross_comedia <- t.test(
  comedias$`Total U.S. Gross`,
  no_comedias$`Total U.S. Gross`
)

prueba_gross_comedia

# B. 
# Comparacion del ROI
# ROI promedio por grupo
mean(comedias$ROI_US)
mean(no_comedias$ROI_US)

# Prueba de diferencia de medias del ROI
prueba_ROI_comedia <- t.test(
  comedias$ROI_US,
  no_comedias$ROI_US
)

prueba_ROI_comedia

# Tabla para el informe
# Crear tabla comparativa entre comedias y otros generos
tabla_punto3 <- data.frame(
  Indicador = c(
    "Total U.S. Gross (USD)",
    "ROI"
  ),
  Comedias = c(
    mean(comedias$`Total U.S. Gross`),
    mean(comedias$ROI_US)
  ),
  Otros_generos = c(
    mean(no_comedias$`Total U.S. Gross`),
    mean(no_comedias$ROI_US)
  ),
  p_value = c(
    prueba_gross_comedia$p.value,
    prueba_ROI_comedia$p.value
  )
)

tabla_punto3

# Dar formato a la tabla para el informe
tabla_punto3_gt <- tabla_punto3 %>%
  gt() %>%
  tab_header(
    title = "Comparación entre comedias y otros géneros"
  ) %>%
  cols_label(
    Indicador = "Indicador",
    Comedias = "Comedias",
    Otros_generos = "Otros géneros",
    p_value = "p-value"
  ) %>%
  fmt_number(
    columns = c(Comedias, Otros_generos, p_value),
    decimals = 4,
    use_seps = TRUE
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboración propia con base en Hollywood Rules."
  )

# Mostrar tabla final
tabla_punto3_gt
