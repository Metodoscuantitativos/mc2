#Clase 2. Ejercicios. Limpiar los datos (tidydata con dplyr vía dplyr)

# Instala y carga la biblioteca tidyverse
library(tidyverse)

# Importar el archivo CSV y guardarlo como importacion_poblacion_estados_eeuu
# Asegúrate de tener la ruta correcta al archivo

importacion_poblacion_estados_eeuu <-read_csv("Cursos/Clase 2/ejemplo_introductorio_estados.csv")

# Filtrar solo los estados de California y Nueva York
poblacion_california_nueva_york <- importacion_poblacion_estados_eeuu %>%
  filter(estado %in% c("California", "Nueva York"))

# Visualizar las poblaciones de California y Nueva York
ggplot(data=poblacion_california_nueva_york, aes(x=año, y=poblacion, color=estado)) +
  geom_line() +
  geom_point()

# Filtrar solo los estados de Mississippi y Virginia
poblacion_mississipi_y_virginia <- importacion_poblacion_estados_eeuu %>%
  filter(estado %in% c("Mississippi", "Virginia"))

# Visualizar las poblaciones de Mississippi y Virginia
ggplot(data=poblacion_mississipi_y_virginia, aes(x=año, y=poblacion, color=estado)) +
  geom_line() +
  geom_point()



# Línea de operaciones ----------------------------------------------------

#ejercicio con vieja escuela: ir anidando
mean(sum(sqrt(importacion_poblacion_estados_eeuu$poblacion)))


#ir creando vectores
# Calcular la raíz cuadrada de la población de cada estado
vector_raiz_cuadrada_poblacion_estados <- sqrt(importacion_poblacion_estados_eeuu$poblacion)

# Calcular la suma de las raíces cuadradas de la variable temporal
suma_del_vector_raices_cuadradas_poblacion_estados <- sum(vector_raiz_cuadrada_poblacion_estados)

# Calcular la media de la variable temporal
media_suma_del_vector_raices_cuadradas_poblacion_estados <- mean(suma_del_vector_raices_cuadradas_poblacion_estados)

# Mostrar la mediana
media_suma_del_vector_raices_cuadradas_poblacion_estados


#mediante el %>% 
importacion_poblacion_estados_eeuu$poblacion%>%sqrt%>%sum%>%mean

# Asegúrate de poner el operador al final de la línea
importacion_poblacion_estados_eeuu$poblacion%>%
  sqrt%>%
  sum%>%
  mean

vector_raiz_y_suma_permanente_poblacion_estados <- importacion_poblacion_estados_eeuu$poblacion%>%sqrt%>%sum%>%mean
vector_raiz_y_suma_permanente_poblacion_estados


# Nuevo conjunto de datos -------------------------------------------------
# Instalar el paquete historydata
install.packages("historydata")

# Cargar el paquete historydata
library(historydata)


#leer y asignar early_colleges
data(early_colleges)
early_colleges


#limpiar un dato
early_colleges[1,6] <- "Congregational"
early_colleges



# ¿Qué es Dplyr? ----------------------------------------------------------
#manipulación y transformación de datos


#select (seleccionar)
# Deshazte de la columna de nombres originales ("original_name") usando select()
# Nota que no tienes que añadir el símbolo $ (dólar) al nombre de la columna al final de early_colleges porque `dplyr` asume que "," (una coma) representa Y (AND en inglés) automáticamente

select(early_colleges, college, city, state, established, sponsorship)


early_colleges%>%
  select(college, city, state, established, sponsorship)


early_colleges%>%
  select(-original_name)


#filer (filtrar)
#filtrar filas según un requisito

early_colleges%>%
  filter(established < 1800)

#mutate
#añadir una columna siguiendo una operación

early_colleges%>%
  mutate(location=paste(city,state,sep=","))

primeras_universidades_con_localizacion <- early_colleges%>%
  mutate(location=paste(city, state, sep=","))

#arrange (ordenar)
early_colleges %>%
  arrange(desc(established))

#summarise (resumir)
early_colleges%>%summarise(mean(established))


# Poniéndolo todo junto ---------------------------------------------------

universidades_seculares_antes_1812 <- early_colleges%>%
  filter(established < 1812)%>%
  mutate(es_laica=ifelse(sponsorship!="Secular", "no", "si"))

#graficar
ggplot(universidades_seculares_antes_1812) +
  geom_bar(aes(x=es_laica, fill=es_laica))+
  labs(title="Tipo de universidad antes de 1812", x="¿Es laica la universidad?", y="Recuento")


#después de 1812
universidades_seculares_despues_1812<-early_colleges%>%
  filter(established > 1812)%>%
  mutate(es_laica=ifelse(sponsorship!="Secular", "no", "si"))

ggplot(universidades_seculares_despues_1812) +
  geom_bar(aes(x=es_laica, fill=es_laica))+
  labs(x="¿Es laica la universidad?", y="Recuento")
