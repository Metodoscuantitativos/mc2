#clase 2: paquetes y tidyverse

# 0. Repaso de acuerdos y de lo que son los data.frame 
# I. Instalación de Paquetes y tidyverse -------------------------------------

#1. Instalar
install.packages ("tidyverse")

#2. Llamar paquetes
library(tidyverse)

#3. Uso de pacman
#install.packages("pacman")
pacman::p_load(tidyverse,Lock5Data, openintro) #tidyverse y bases de datos de cine





# II. tidyverse -----------------------------------------------------------
#Lenguaje de programación de la "nueva escuela"
#Versus r base

#00.1 Manipulación datos con tidyverse %>% 
# %>% el pipe: encadenar funciones, realizar procedimientos por capas
#va de lo macro a lo micro, de izquierda a derecha
#aplica tal función o realiza tal procedimiento
#un embudo

data %>% 
  select (Movie, LeadStudio, Genre)


#00.2 Abrimos base y asignamos base de datos
data <-  Lock5Data::HollywoodMovies
oscars <- openintro::oscars

#observar variables
names(data)
names(oscars)

#observar categorías (unicas de variables)
unique(data$Genre)


#Otra posibilidad, cuando ya está cargada la librería
data <-  HollywoodMovies
oscars <- oscars



#01. Select####

names(data)

#forma 1
data %>% 
  dplyr::select(Movie, LeadStudio, Genre)

#forma 2
data %>% select(Movie, LeadStudio, Genre)

#¿Por qué se usan los dos puntos?
#Cada paquete puede incluir subpaquetes
#los doble dos puntos (::) llaman a esos subpaquetes


#asingar
data_select <- data %>% select(Movie, LeadStudio, Genre)



#otras selecciones
#a. todas las variables que no sean -c
#ni Budget, ni WorldGross

data %>% select(-c(Budget, WorldGross))

#b. ordenar
data %>% select(Year, everything()) %>% #pone primero Year, luego el resto
  view()

#c. las que comiencen con start_with
data %>% select(starts_with(match="M")) %>% 
  view()

#d. las que terminen con ends_with
data %>% select(ends_with(match="E")) %>% 
view()

#e. las que son numéricas
data %>% select_if(is.numeric)

#f. las que son categóricas
data %>% select_if(is.character)


#f. transformar a mayúsculas
data %>% select_all(toupper)

#Práctica 1#### 
#00. En la base de datos oscars
#01. Seleccionar año del premio, nombre de actor y película
#02. Seleccionar las columnas 2,4,6 de la base de datos
#03. Seleccionar todas la variables a excepción de columna 1 y 6
#04. Eliminar todas las columnas que comiencen con "birth"
#05. Realice una selección con las variables, con las columnas, con todas menos 2 variables



#realización: 
#01. 
oscars %>% names () #observo nombre de variables
oscars %>% head () #primera mirada de datos
oscars %>% select(oscar_yr, name, movie)


#02. 
oscars %>% select(c(2,4,6))

#03. 
oscars %>% select(-c(1,6))

#04. 
o1 <- oscars %>% select(-c(starts_with(match="birth"))) %>% 
  view ()


o2 <-  oscars %>% select(!starts_with("birth"))%>% 
  view ()

#¿Les sale algo distinto?



# 02. Filter --------------------------------------------------------------
#en base data

names(data) #r base
data %>% names() #forma tidyverse

data %>% filter (Year ==2007) # doble igual
data %>% dplyr::filter (Year==2007)  # con::

#mayor o igual a 2009
data %>% filter (Year >=2009) 

#presupuesto mayor que 210
data %>% filter (Budget >=210) 

#filtrar por variables categóricas
data %>% filter (Genre== "Drama") # ojo que las categóricas van con comillas


#filtrar por dos tipos de variables
data %>% filter (Genre=="Action" & Year==2013)

#filtrar por una y otra (| al lado del 1 en pc)
data %>% filter (Genre== "Comedy"| Genre == "Drama")
data %>% filter (Genre %in% c("Comedy", "Drama")) #cuando se selecciona más de una variable



#Práctica 2####

#01. Quienes ganaron oscar desde el año 2015?
#02. Actrices mayores de 50 años que hayan ganados oscar?: award
#03. Cuántos oscar ganó Meryl Streep?
#04. Películas que ganó Di Carpio o Al Pacino?
 

#Realización
names(oscars)
unique(oscars$oscar_yr)

#01. 
oscars %>% filter(oscar_yr >= 2015)

#02. 
unique(oscars$award)
oscars %>% filter(award == "Best actress" &  age >=50)

#03. 
oscars %>% filter(name == "Meryl Streep")

#04.
unique(oscars$name)
oscars %>% filter(name == "Leonardo Di Caprio" | name == "Al Pacino")
oscars %>% filter(name %in% c("Leonardo Di Caprio", "Al Pacino"))


#05. Otras selecciones:
#seleccione: 
#los que ganaron el oscar el 2017, 2018, 2019
#los mejores actores menores de 30
#seleccione tres actores/actrices que le guste y vea cuando ganaron
unique(oscars$name)


# 03. Mutate ------------------------------------------------------------------
#crear, rescribir, actualizar una nueva variable

#01. ROI: retorno de inversión, pedidas o ganancias
#en data

data %>% mutate(ROI = (WorldGross - Budget)/Budget) %>% view() 
# WorldGross: cuando se recaudo
# Budget: cuánto costo (presupuesto)

# nombre de nueva variable
# lo que se va a hacer

data %>% 
  mutate(ROI = (WorldGross - Budget)/Budget) %>% 
  arrange(desc(ROI)) %>% 
  view() 


#02. Diferencia de ratting: películas controversiales
data %>% mutate(Dif_rating = (RottenTomatoes - AudienceScore)) %>% view() 


#03. Con oscars: case_when para recodificar!

oscars %>% mutate (Mayores60 = case_when (age < 59 ~ "Menor 60", 
                                        age >= 60 ~ "Mayor 60"))



#Práctica 3####
#01. Generar una variable de edad recodificada: 18 a 35, 36 a 50, 51 a 59, 60 y más. 




# 04. Group by ----------------------------------------------------------------
# Agrupar de tipo categóricas
#requiere alguna operación posterior
#cuáles son los estudios con mayor cantidad de películas ganadoras

data %>% 
  group_by(LeadStudio) %>% 
  count() %>% #cuenta la cantidad
  arrange(desc(n)) #ordena de mayor a menor, descendente, sin desc desde menor



#Práctica 4####
# Agrupar por nombre y contabilizar números de oscars
# Filtrar cuando tengan más de dos oscars

oscars %>% group_by(name) %>% 
  count() %>% 
  filter (n>=2) %>% 
  arrange(desc(n))


# Agrupar por pelicula y contabilizar las que tengan más de dos oscar
# Ojo, base de datos sólo de ganadorxs mejor actor/actriz 




# 05. Summarise -----------------------------------------------------------
#suele usarse con group_by
# quiero saber el total de ganadoras por cada estudio y el promedio de audiencia de películas

data %>% 
  group_by(LeadStudio) %>% 
  summarise(Freq = n(), 
            Prom = mean(AudienceScore, na.rm = TRUE)) %>% 
  rename(Productora= LeadStudio) %>% view()



#seleccionando las siguientes productoras (LeadStudio):  
# "Warner Bros. " , "Universal Pictures ", "Lionsgate ", "Twentieth Century Fox "
# ponga antención como están escritas. 

# a) Cálcule el presupuesto promedio (mean)
# b) el desvío (sd)
# c) el total de presupuesto por productora (sum)


unique(data$LeadStudio)

data %>% 
  group_by(LeadStudio) %>% 
  count() %>% 
  arrange(desc(n))


data %>% 
  filter (LeadStudio %in% c("Warner Bros. " , "Universal Pictures ", 
                            "Lionsgate ", "Twentieth Century Fox ")) %>% 
  group_by(LeadStudio) %>% 
  summarise(Prom = mean(Budget, na.rm = TRUE),
            SD = sd (Budget, na.rm = TRUE), 
            Total = sum (Budget, na.rm = TRUE)) 











