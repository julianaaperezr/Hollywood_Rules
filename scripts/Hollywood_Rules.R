# CASO 2 - HOLLYWOOD RULES
# Integrantes: Juliana Perez, Maria Camila Salamanca y Veronica Sandoval

# Cargar librerías
library(readxl)
library(dplyr)

# Importar base de datos
Hollywood <- read_excel(
  "data/Hollywood.xls",
  sheet = "Exhibit 1"
)

# Para ver los nombres de las columnas
colnames(Hollywood)

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
