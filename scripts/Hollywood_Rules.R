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
