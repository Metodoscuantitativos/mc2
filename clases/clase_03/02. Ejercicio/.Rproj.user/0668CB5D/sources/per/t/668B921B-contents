# R base
# Práctica independiente


# Parte 1: R base ---------------------------------------------------------
#0.1. Crear un OBJETO llamado COSA cuyo contenido sea el resultado de la multiplicación: 5*6
COSA <- 5*6 
#0.2 Crear un VECTOR llamado EDAD que contenga las edades de los/as miembros/as de su grupo (si no sabe invente)
EDAD <- c(20, 21, 22, 23, 20)
#0.3 Modificar el segundo elemento de EDAD, asignandole el valor 25
EDAD [2] <- 25  
#0.4 Crear 3 vectores (llamarlos NOMBRE, COMUNA, TENDENCIA). con la misma cantidad de elementos que EDAD, pero con variables de tipo character (texto).
NOMBRE <- c("Juanito", "Pepito", "Fulanita", "Menganita", "Gertudis")
COMUNA <- c("Ñuñoa", "Peñalolen", "Vitacura", "Maipu", "Cerronavia")
TENDENCIA <- c("Izquieda", "Derecha", "Centro", "Derecha", "Izquierda")
 
#0.5 Crear un dataframe (llamarla: BASE_DATOS) que contenga los vectores creados previamente
BASE_DATOS <- data.frame(NOMBRE, EDAD, COMUNA, TENDENCIA)

#0.6 Crear un objeto llamado OTRA_COSA que guarde el valor contenido de la segunda fila del dataframe BASE_DATOS
OTRA_COSA <- BASE_DATOS[2,]

#0.7 Crear un objeto llamado OTRA_COSA1 que guarde el valor contenido de la tercera columna del dataframe BASE_DATOS
OTRA_COSA1 <- BASE_DATOS[,3]

#0.8 Crear un objeto llamado OTRA_COSA2 que guarde el valor contenido de la segunda fila y tercera columna del dataframe BASE_DATOS
OTRA_COSA1 <- BASE_DATOS[2,3]

# Parte 2: Abrir, guardar e indexar bases de datos ---------------------------------------
#01. Cree una carpeta en el escritorio (eg. Ejercicio clase 3), cree un proyecto e importen la base de datos sobre los casos de covid 
#detallando la ruta desde el punto de partida del proyecto y asignandola a base_covid
#recuerden que dicha base es una muestra del 2% de los casos diarios
#pista: use readRDS

base_covid <- readRDS(file = "base_covid_sample.RDS")


#02. Supongan que sólo les interesa trabajar con las variables sexo, edad y fallecido. 
# para saber los nombres utilice función names ()
names (base_covid)

# Utilice el método de acceso de los corchetes [] para crear un nuevo objeto que contenga 
# todas las filas de la base, 
# pero solo esas columnas
#02.1 realicelo con el nombre de las variables (base_covid_r1)
base_covid_r1 <- base_covid [,c("sexo", "edad", "fallecido")]

#02.2 realicelo con la posición de las variables (base_covid_r2)
base_covid_r2 <- base_covid [,c(2, 3, 15)]

#02.3 observe si son iguales 
#puede comprobarlo con dim()
dim(base_covid_r1)
dim(base_covid_r2)

#03. Tome esa misma base (base_covid_r1) y con el método de acceso de corchetes [] conserve  los primeros 500 casos/filas
#guardela en base_covid_r3
base_covid_r3 <- base_covid_r1 [1:500, ]



#04. Cree un vector con todos los números enteros entre 18 y 60.
# Nómbrelo como vector_filtro
vector_filtro <- 18:60

# Ejecute la siguiente sentencia 
# para comprobar qué valores del vector en cuestión están dentro del rango contemplado por vector_filtro
c(1,5,20,55,60,72) %in%  vector_filtro

#05. Ejecute una sentencia que compruebe para toda la columna edad de la base con 500 filas
# qué valores están dentro del rango contemplado por vector_filtro
base_covid_r3$edad %in%  vector_filtro

#06. Borre todos los objetos del Environment
rm(list=ls())

# Parte 3: tidyverse: select, filter, mutate ---------------------------------------------------

#01. Importar la base de datos de casos COVID (base_covid_sample.RDS) y cargar tidyverse
#asignela con el nombre base_covid 

library(tidyverse)
base_covid <- readRDS(file = "base_covid_sample.RDS")

#02. Crear un objeto nuevo (base_covid2) y seleccionar entre 3 y 6 columnas de interés combinando la función select() con:
# El nombre de las variables
# La posición de las variables
# Un patrón común en el nombre de las variables (starts_with() - ends_with() - contains())
# para observar el nombre de las variables puede hacerlo con names ()

names(base_covid)
base_covid2 <-base_covid %>% 
  select("edad", 2, 
         starts_with("fecha"), 
         ends_with("nombre"), 
         contains("id"))

#03. Crear un objeto nuevo (base_covid3) que contenga unicamente a la población residente de 3 provincias a elección 
#y sólo una categoría de la variable sexo [a elección]
#realicelo con filter, puede ver las categorias de provincias y sexo con unique()
names(base_covid)
unique(base_covid$residencia_provincia_nombre)
unique(base_covid$sexo)

base_covid3 <- base_covid %>% 
  filter (residencia_provincia_nombre %in% c("Buenos Aires", "Córdoba", "CABA"), 
          sexo == "M")

#04. mediante la función table () observe si su filtro está bien realizado para residencia_provincia_nombre
table(base_covid3$residencia_provincia_nombre)


#0.5 mediante la función table () observe si su filtro está bien realizado para sexo
table(base_covid3$sexo)


#06. Crear una variable recodificando (base_covid4) la variable edad en 3 rangos etáreos.
#recuerde que para recodificar se usa: mutate(variable_nueva = case_when(variable_vieja %in% c(desde:hasta) ~ "desde a hasta"
# para poner el "~": presione AltGR y asterisco
names (base_covid)
base_covid$edad


base_covid4 <- base_covid %>% 
  mutate(edadr = case_when(edad %in% c (0:18) ~ "O a 18", 
                           edad %in% c (19:64) ~ "19 a 64", 
                           edad >= 65 ~ "65 o más"))


#07. compruebe lo realizado con una table() a edadr
table(base_covid4$edadr)



