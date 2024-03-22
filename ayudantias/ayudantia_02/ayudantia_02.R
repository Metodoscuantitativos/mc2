
# Ayudantía 2

# Carga de paquetes tidyverse con pacman
# install.packages("pacman")
pacman::p_load(tidyverse)

# Carga del conjunto de datos
datos_anime <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

# Ejecutar esta limpieza para evitar casos repetidos
datos_anime <- datos_anime %>%
  distinct(animeID, .keep_all = TRUE)

# 0. Observar las variables con names() 
names (datos_anime)

# Select ------------------------------------------------------------------

# 1. Seleccionar las columnas name, genre y score. Guarda esta selección como base: columnas_seleccionadas
columnas_seleccionadas <- datos_anime %>%
  select(name, genre, score)

# 2. Seleccionar las columnas name hasta studio. Guarda esta selección como base: seleccion_rango
seleccion_rango <- datos_anime %>%
  select(name:studio)

# 3. Excluir las columnas related y background. Guarda esta selección como base: excluir_columnas
excluir_columnas <- datos_anime %>%
  select(-related, -background)

# Filter ------------------------------------------------------------------

# 1. Filtrar para quedarse solo con el Studio Ghibli, primero observar cuáles son los estudios en la variable studio con unique(). Guarda este filtro como base: ghibli
ghibli <- datos_anime %>%
  filter(studio == "Studio Ghibli")

# 2. Para el Studio Ghibli, quedarse solo con los géneros "Action", "Adventure", "Fantasy". Guarda este filtro como base: ghibli_generos
ghibli_generos <- datos_anime %>%
  filter(studio == "Studio Ghibli" & 
           (genre %in% c("Action", "Adventure", "Fantasy")))

# 3. Filtrar animes que aún se están emitiendo. Guarda este filtro como base: emitiendose
# variable si se están o no emitiendo: status
#en estatus las que se están emitiendo son: Currently Airing

emitiendose <- datos_anime %>%
  filter(status == "Currently Airing")

# Mutate ------------------------------------------------------------------

# Añadir una columna de clasificación según la puntuación: 
# guardando base nueva como: etiqueta_puntuacion
# generando una variable nueva llamada: categoria_puntuacion
# recodificando con case_when, score cuando:
# es mayor o igual a 8 es "Alto"
# menor a 8 y mayor o igual a 6 es "Medio"
# el resto "Bajo"

unique(datos_anime$score)
etiqueta_puntuacion <- datos_anime %>%
  mutate(categoria_puntuacion = case_when(
    score >= 8 ~ "Alto",
    score >= 6 & score < 8 ~ "Medio",
    TRUE ~ "Bajo"
  ))

table(etiqueta_puntuacion$categoria_puntuacion)

# Group By y Summarise -----------------------------------------------------

# 1. Contar películas por estudio y encontrar el estudio con más películas. Guarda este conteo como base: conteo_peliculas_estudio
conteo_peliculas_estudio <- datos_anime %>%
  filter(type == "Movie") %>%
  group_by(studio) %>%
  summarise(cantidad_peliculas = n()) %>%
  arrange(desc(cantidad_peliculas))


