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

# PUNTO 4
# A
# Seleccionar las peliculas clasificadas R
peliculas_R <- Hollywood %>%
  filter(MPAA_D == 1)

# Seleccionar las peliculas con otras clasificaciones
peliculas_otras <- Hollywood %>%
  filter(MPAA_D == 0)

# Calcular el numero de peliculas clasificadas R
nrow(peliculas_R)

# Calcular el numero de peliculas con otras clasificaciones
nrow(peliculas_otras)

# Calcular el promedio de ingresos de las peliculas R
mean(peliculas_R$`Total U.S. Gross`)

# Calcular el promedio de ingresos de las otras peliculas
mean(peliculas_otras$`Total U.S. Gross`)

# Realizar la prueba de diferencia de medias
prueba_R <- t.test(
  peliculas_R$`Total U.S. Gross`,
  peliculas_otras$`Total U.S. Gross`
)

# Mostrar los resultados de la prueba
prueba_R

# Crear una tabla con los resultados
tabla_punto4 <- data.frame(
  Grupo = c("Clasificacion R", "Otras clasificaciones"),
  Peliculas = c(
    nrow(peliculas_R),
    nrow(peliculas_otras)
  ),
  Promedio_USD = c(
    mean(peliculas_R$`Total U.S. Gross`),
    mean(peliculas_otras$`Total U.S. Gross`)
  ),
  p_value = c(
    prueba_R$p.value,
    prueba_R$p.value
  )
)

# Mostrar la tabla de resultados
tabla_punto4

# Dar formato a la tabla para el informe
tabla_punto4_gt <- tabla_punto4 %>%
  gt() %>%
  tab_header(
    title = "Comparacion de ingresos segun clasificacion"
  ) %>%
  cols_label(
    Grupo = "Clasificacion",
    Peliculas = "Numero de peliculas",
    Promedio_USD = "Promedio de ingresos (USD)",
    p_value = "p-value"
  ) %>%
  fmt_number(
    columns = c(Promedio_USD, p_value),
    decimals = 4,
    use_seps = TRUE
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
  )

# Mostrar la tabla final
tabla_punto4_gt

# Comprobar si existe una diferencia significativa al 5%
prueba_R$p.value < 0.05


# PUNTO 5  
# A
# Revisar los nombres de las variables
colnames(Hollywood)

# Crear una variable para identificar las comedias
Hollywood <- Hollywood %>%
  mutate(
    Comedia = ifelse(Genre == "Comedy", 1, 0)
  )

# Verificar la cantidad de peliculas de cada grupo
Hollywood %>%
  count(Comedia)

# Crear una regresion con los factores antes de producir
modelo5 <- lm(
  `Total U.S. Gross` ~ Budget + Comedia + MPAA_D +
    Sequel + `Known Story`,
  data = Hollywood
)

# Mostrar los resultados de la regresion
summary(modelo5)

# Crear una tabla con los resultados del modelo
tabla_punto5a <- as.data.frame(
  summary(modelo5)$coefficients
)

# Agregar los nombres de las variables
tabla_punto5a$Variable <- rownames(tabla_punto5a)

# Organizar las columnas de la tabla
tabla_punto5a <- tabla_punto5a %>%
  select(Variable, everything())

# Mostrar la tabla de resultados
tabla_punto5a

# Dar formato a la tabla para el informe
tabla_punto5a_gt <- tabla_punto5a %>%
  gt() %>%
  tab_header(
    title = "Punto 5 - Regresion inicial"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 4
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
  )

# Mostrar la tabla final
tabla_punto5a_gt


# PUNTO 5.B

# Crear una copia del modelo inicial
modelo5_final <- modelo5

# Repetir el proceso para eliminar variables no significativas
repeat {
  
  # Obtener los valores p sin incluir el intercepto
  valores_p <- summary(modelo5_final)$coefficients[-1, 4]
  
  # Detener si no quedan variables explicativas
  if (length(valores_p) == 0) break
  
  # Identificar el valor p mas alto
  mayor_p <- max(valores_p)
  
  # Detener cuando todas las variables sean significativas al 10%
  if (mayor_p <= 0.10) break
  
  # Identificar la variable con mayor valor p
  variable_eliminar <- names(which.max(valores_p))
  
  # Eliminar la variable del modelo
  modelo5_final <- update(
    modelo5_final,
    paste(". ~ . -", variable_eliminar)
  )
  
}

# Mostrar los resultados del modelo final
summary(modelo5_final)

# Crear una tabla con los resultados finales
tabla_punto5b <- as.data.frame(
  summary(modelo5_final)$coefficients
)

# Agregar los nombres de las variables
tabla_punto5b$Variable <- rownames(tabla_punto5b)

# Organizar las columnas
tabla_punto5b <- tabla_punto5b %>%
  select(Variable, everything())

# Dar formato a la tabla para el informe
tabla_punto5b_gt <- tabla_punto5b %>%
  gt() %>%
  tab_header(
    title = "Punto 5 - Regresion final"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 4
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
  )

# Mostrar la tabla final
tabla_punto5b_gt

# Mostrar el R cuadrado del modelo final
summary(modelo5_final)$r.squared

#punto 5c

# Calcular el ingreso promedio de las secuelas
mean(Hollywood$`Total U.S. Gross`[Hollywood$Sequel == 1])

# Calcular el ingreso promedio de las peliculas que no son secuelas
mean(Hollywood$`Total U.S. Gross`[Hollywood$Sequel == 0])

# Mostrar los coeficientes del modelo final
coef(modelo5_final)

# Verificar si la variable Sequel permanece en el modelo
"Sequel" %in% names(coef(modelo5_final))

# Mostrar el efecto de las secuelas si permanece en el modelo
if ("Sequel" %in% names(coef(modelo5_final))) {
  
  # Mostrar el coeficiente de Sequel
  coef(modelo5_final)["Sequel"]
  
  # Mostrar su valor p
  summary(modelo5_final)$coefficients[
    "Sequel", "Pr(>|t|)"
  ]
  
}

# PUNTO 6 
# A

# Revisar los nombres de las variables
colnames(Hollywood)

# Crear una regresion con los factores antes del estreno
modelo6 <- lm(
  `Opening Gross` ~ Budget + Comedia + MPAA_D +
    Sequel + `Known Story` + Summer + Holiday +
    Christmas + `Opening Theatres`,
  data = Hollywood
)

# Mostrar los resultados del modelo inicial
summary(modelo6)

# Crear una tabla con los coeficientes
tabla_punto6a <- as.data.frame(
  summary(modelo6)$coefficients
)

# Agregar los nombres de las variables
tabla_punto6a$Variable <- rownames(tabla_punto6a)

# Organizar las columnas
tabla_punto6a <- tabla_punto6a %>%
  select(Variable, everything())

# Dar formato a la tabla inicial
tabla_punto6a_gt <- tabla_punto6a %>%
  gt() %>%
  tab_header(
    title = "Punto 6 - Regresion inicial"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 4
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
  )

# Mostrar la tabla inicial
tabla_punto6a_gt

# PUNTO 6.B 
# Crear una copia del modelo inicial
modelo6_final <- modelo6

# Repetir el proceso para eliminar variables no significativas
repeat {
  
  # Obtener los valores p sin incluir el intercepto
  valores_p <- summary(modelo6_final)$coefficients[-1, 4]
  
  # Detener si no quedan variables explicativas
  if (length(valores_p) == 0) break
  
  # Identificar el valor p mas alto
  mayor_p <- max(valores_p)
  
  # Detener cuando todas las variables sean significativas al 10%
  if (mayor_p <= 0.10) break
  
  # Identificar la variable con mayor valor p
  variable_eliminar <- names(which.max(valores_p))
  
  # Eliminar la variable del modelo
  modelo6_final <- update(
    modelo6_final,
    paste(". ~ . -", variable_eliminar)
  )
  
}

# Mostrar el modelo final
summary(modelo6_final)

# Crear una tabla con los coeficientes finales
tabla_punto6b <- as.data.frame(
  summary(modelo6_final)$coefficients
)

# Agregar el nombre de cada variable
tabla_punto6b$Variable <- rownames(tabla_punto6b)

# Organizar las columnas
tabla_punto6b <- tabla_punto6b %>%
  select(Variable, everything())

# Dar formato a la tabla final
tabla_punto6b_gt <- tabla_punto6b %>%
  gt() %>%
  tab_header(
    title = "Punto 6 - Regresion final"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 4
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
  )

# Mostrar la tabla final
tabla_punto6b_gt

# Mostrar el R cuadrado ajustado
summary(modelo6_final)$adj.r.squared

# PUNTO 6.C
# Mostrar los coeficientes del modelo final
coef(modelo6_final)

# Crear una tabla con los coeficientes
tabla_punto6c <- data.frame(
  Variable = names(coef(modelo6_final)),
  Coeficiente = as.numeric(coef(modelo6_final))
)

# Dar formato a la tabla
tabla_punto6c_gt <- tabla_punto6c %>%
  gt() %>%
  tab_header(
    title = "Punto 6 - Coeficientes del modelo final"
  ) %>%
  fmt_number(
    columns = Coeficiente,
    decimals = 2,
    use_seps = TRUE
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
  )

# Mostrar la tabla
tabla_punto6c_gt

# PUNTO 6.D 

# Verificar si Teatros permanece en el modelo final
"Opening Theatres" %in% names(coef(modelo6_final))

# Calcular el efecto si la variable permanece
if ("`Opening Theatres`" %in% names(coef(modelo6_final)) ||
    "Opening Theatres" %in% names(coef(modelo6_final))) {
  
  # Obtener el coeficiente de los teatros
  efecto_teatros <- coef(modelo6_final)[
    grep("Opening Theatres", names(coef(modelo6_final)))
  ]
  
  # Calcular el efecto de aumentar 100 teatros
  aumento100 <- efecto_teatros * 100
  
  # Calcular el intervalo de confianza del 95%
  intervalo100 <- confint(
    modelo6_final,
    parm = grep("Opening Theatres",
                names(coef(modelo6_final))),
    level = 0.95
  ) * 100
  
  # Crear una tabla con los resultados
  tabla_punto6d <- data.frame(
    Resultado = c(
      "Aumento estimado",
      "Limite inferior IC 95%",
      "Limite superior IC 95%"
    ),
    Valor_USD = c(
      aumento100,
      intervalo100[1],
      intervalo100[2]
    )
  )
  
  # Dar formato a la tabla
  tabla_punto6d_gt <- tabla_punto6d %>%
    gt() %>%
    tab_header(
      title = "Punto 6 - Efecto de aumentar 100 teatros"
    ) %>%
    fmt_number(
      columns = Valor_USD,
      decimals = 2,
      use_seps = TRUE
    ) %>%
    tab_source_note(
      source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
    )
  
  # Mostrar la tabla
  tabla_punto6d_gt
  
}

# PUNTO 7 - A

# Crear una regresion lineal simple
modelo7 <- lm(
  `Total U.S. Gross` ~ `Opening Gross`,
  data = Hollywood
)

# Mostrar los resultados
summary(modelo7)

# Crear una tabla con los coeficientes
tabla_punto7a <- as.data.frame(
  summary(modelo7)$coefficients
)

# Agregar los nombres de las variables
tabla_punto7a$Variable <- rownames(tabla_punto7a)

# Organizar las columnas
tabla_punto7a <- tabla_punto7a %>%
  select(Variable, everything())

# Dar formato a la tabla
tabla_punto7a_gt <- tabla_punto7a %>%
  gt() %>%
  tab_header(
    title = "Punto 7 - Regresion lineal simple"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 4
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
  )

# Mostrar la tabla final
tabla_punto7a_gt

# Definir la participacion teorica del estreno
participacion <- 0.25

# Calcular la pendiente teorica
pendiente_teorica <- 1 / participacion

# Mostrar la pendiente teorica
pendiente_teorica

# Mostrar la pendiente estimada por el modelo
coef(modelo7)[2]

# Obtener el R cuadrado del modelo
R2_punto7 <- summary(modelo7)$r.squared

# Convertir el R cuadrado a porcentaje
porcentaje_explicado <- R2_punto7 * 100

# Mostrar el porcentaje explicado
porcentaje_explicado

# Crear una tabla con los resultados
tabla_punto7 <- data.frame(
  Indicador = c(
    "Pendiente estimada",
    "Pendiente teorica",
    "R cuadrado",
    "Variacion explicada (%)"
  ),
  Valor = c(
    coef(modelo7)[2],
    pendiente_teorica,
    R2_punto7,
    porcentaje_explicado
  )
)

# Dar formato a la tabla
tabla_punto7_gt <- tabla_punto7 %>%
  gt() %>%
  tab_header(
    title = "Punto 7 - Resultados de la regresion"
  ) %>%
  fmt_number(
    columns = Valor,
    decimals = 4
  ) %>%
  tab_source_note(
    source_note = "Fuente: elaboracion propia con base en Hollywood Rules."
  )

# Mostrar la tabla final
tabla_punto7_gt

# Crear la grafica de dispersion
plot(
  Hollywood$`Opening Gross` / 1000000,
  Hollywood$`Total U.S. Gross` / 1000000,
  main = "Ingresos de estreno e ingresos totales",
  xlab = "Ingresos de estreno (millones de USD)",
  ylab = "Ingresos totales (millones de USD)",
  pch = 19,
  col = "steelblue"
)

# Crear la regresion con los valores en millones
modelo_grafica7 <- lm(
  I(`Total U.S. Gross` / 1000000) ~
    I(`Opening Gross` / 1000000),
  data = Hollywood
)

# Agregar la linea de regresion
abline(
  modelo_grafica7,
  col = "red",
  lwd = 2
)

