                                         ###         PRUEBA 2        ###

#                                       ***********Instrucciones************


# a. Ir a File y crear un Rproject denominelo: apellido_prueba2.Rproj, por ejemplo: "Ocampo_prueba2.Rproj" 

# b. Guardar este script (prueba_2.R) y base de datos de la encuesta del curso (base_antropologia_limpia.xlsx) en la carpeta en que se encuentra su proyecto apellido_prueba2.Rproj

# c. Comenzar y desarrollar la prueba en este script.

# d. Al finalizar debe guardar este script en la carpeta donde se encuentra el proyecto denominado apellido_prueba2.Rproj,
# el script se debe denominar igual que el archivo del proyecto, es decir, apellido_prueba2.R (sólo cambia su extensión de .Rproject a .R) 
# comprimir la carpeta del proyecto (esta carpeta debe contener al menos 3 archivos: apellido_prueba2.Rproj, apellido_prueba2.R y base_antropologia_limpia.xlsx) 
# y enviarla a ginowrs@gmail.com y semunoz@uahurtado.cl Con el asunto: apellido prueba 2

# La prueba consta de 2 partes, la primera de desarrollo.
# Y la segunda de código en R. 

# Mucha Suerte, que les vaya bien!!

#                                                 Primera parte

# Seleccione 3 de las siguientes 8 preguntas y responda debajo de la pregunta correspondiente,
# su respuesta debe ser clara y responder específicamente lo que se consulta, (mínimo un párrafo por respuesta, máximo 2). (6 ptos en total) cada pregunta tiene 2 ptos.

# 1. Describa la diferencia entre la hipótesis nula y la hipótesis alternativa en un contraste de hipótesis.

# la hipótesis nula  es el "resultado de la investigación" se asume verdadera hasta que se demuestre lo contrario, 
# generalmente representando una posición de no efecto o no diferencia. La hipótesis alternativa es la que se considera
# cuando se rechaza la hipótesis nula, representando una nueva teoría o un efecto específico que el investigador quiere 
# comprobar. Las dos hipótesis son excluyentes entre sí, es decir, si una es verdadera, la otra debe ser falsa.

### En general se da luces de que una hipótesis es contraria a la otra, y que son excluyentes, sin embargo falta precisar. 
### Las hipótesis evaluan si existe relación o no entre las variables. 
### Por ejemplo, en el test de hipótesis Chi Cuadrado, la hipótesis nula indica que las variables no se relacionan, es decir, son independientes, 
### mientras que la hipótesis alternativa indica que sí hay relación entre las variables, es decir, son dependientes entre sí. 
### Y en el test de ANOVA, la hipótesis nula indica que las medias (promedios) de los grupos evaluados son iguales, 
### mientras que la hipótesis alternativa indica que no lo son.
  
# 3. Describa qué son los estadísticos de tendencia central, de 2 ejemplos de este tipo de estadísticos y cuáles son sus diferencias con respecto a las medidas de dispersión

# las medidas de tendencia central son el conjuto de valores entre los datos que determinan el valor central
# o el promedio, se diferencian con las medidas de dispersion describen la variabilidad o el 
# esparcimiento de los datos en torno a la media. EJEMPLOS:
# MEDIA: promedio de un conjunto de datos, por ejemplo: notas 6 + 5 + 5,5 = la media de
# notas es = 5,5 
# MODA: son los datos estadisticos que se repiten o aparecen con mayor frecuencia
# ejemplo: en un una sala de clases hay 28 alumnos y 17 de estos tienen 13 años por lo tanto la moda es 13

### Bien :)

# 4. Cuál es la diferencia entre la estadística descriptiva y la estadística inferencial?

# La estadística descriptiva se enfoca en resumir y describir las características de un conjunto de datos, utiliza diversos métodos para 
# organizar, presentar y analizar datos de manera comprensible. Incluye medidas de tendencia central (media, mediana, moda), medidas de 
# dispersión (rango, varianza, desviación estándar), tablas, gráficos y diagramas.

# La estadística inferencial se enfoca en hacer inferencias y predicciones sobre una población basada en una muestra de datos, esta
# utiliza técnicas para generalizar los resultados de una muestra a la población más grande de la que se extrajo la muestra.
# Incluye intervalos de confianza, pruebas de hipótesis, regresión, análisis de varianza (ANOVA) y otros métodos de estimación y predicción.

### Bien 

#



#                                                 Segunda parte

# Código en R 
  
# 00. Carga de Paquetes --------------------------------------------------------
pacman::p_load(tidyverse,
               openxlsx,
               summarytools,
               kableExtra,#Tablas elegantes
               webshot2,#exportar tablas
               chromote,
               viridis, #temas de gráficos
               hrbrthemes)#temas de gráficos

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer, 
               datos,  knitr, gt, summarytools, ggthemes, hrbrthemes, foreign, DescTools, ineq)


libro_codigos<- read.xlsx("base_antropologia_limpia.xlsx") 

# 01. Cargar datos-------------------------------------------------------- 
base <- read.xlsx("base_antropologia_limpia.xlsx")
libro_codigos<- read.xlsx("base_antropologia_limpia.xlsx") 
  
  
# 01.1 ¿cuántas filas y columnas tienen los datos? (0.5 ptos)
#(debe escribir código que entregue la información, no se aceptan respuestas sin código)

glimpse(base)
names(base)
view(base)
base<- janitor::clean_names(base)

#02. Renombrar variable
names(base)

#02.02: renombre 4 variables pertenecientes al grupo del cual usted participa(2 ptos)
# Para designar nuevos nombres a sus variables se puede apoyar en las siguientes etiquetas de las preguntas (use sólo las de su grupo):


# "RE_01 ¿Cómo califica su creencia de un ser supremo o deidades?"
# "RE_02 ¿Cuál es su afiliación religiosa o creencia espiritual? (Selecciona una opción)"
# "RE_03 ¿Con qué frecuencia acude a su religión o a instancias en dónde conectes con tu espiritualidad? (rezo, oración, meditación u otro)"
# "RE_04 ¿Asiste con regularidad a algún lugar destinado al culto religioso?"
# "RE_05 ¿Consideras que hay una influencia de la religión o la espiritualidad en SUS decisiones éticas y morales que toma en su vida cotidiana?"


base<- base %>% dplyr::rename(tipo_creencia = re_01 , afiliacion = re_02 , cuantas_veces = re_03 , asiste_si_no = re_04)
base<- base %>% dplyr::rename(influencia_oral = re_05)

# 03. Suponga que está trabajando para una investigación interesada en los habitos de escucha y consumo de música.
# Su jefe le solicita un gráfico que indique la frecuencia de los lugares donde los estudiantes de antropología escuchan música.
# Para lo cual debe realizar las siguientes acciones

# 03.1 Cambie el  nombre de la variable cm_05 por lugar_musica (0.5 ptos) en la misma base de datos

base <-base %>% dplyr::rename(lugar_muscia = cm_05)

# 03.2 observe las categorías de la variable lugar_musica, puede utilizar funciones como freq, table, unique, etc. (0.5 ptos)

unique(base$lugar_muscia)
# 03.3 Dado que la respuesta viene en una variable que a su vez contiene respuestas múltiples usted se da cuenta de que debe separar las respuestas.
# Para ello separe las respuestas de la variable lugar_musica por coma (,). 
# Utilice la función strsplit() y guardelo en un objeto llamado "respuestas" (1.5 ptos)

respuestas <- strsplit(base$lugar_muscia, ",")
table(base$lugar_muscia)

# 03.4 Transforme el objeto respuestas a un vector con la función unlist (0.5 pto)

respuestas <- unlist(respuestas) 

# 03.5 observe las respuestas con freq (1 pto)

freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()
# Hay respuestas que se contabilizan 2 veces como "En el transporte público" y "En otros lugares", 
# 03.6 homologue ambas categorías de respuesta eliminando el espacio que antecede utilizando la función gsub


 respuestas <-gsub("En el transporte público", "En el transporte público", respuestas)
 
 #(1 pto)

 respuestas <-gsub(" En otros lugares", "En otros lugares",respuestas)

# 03.7 vuelva a observar sus respuestas con freq (1 pto)

 freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
   tb()
 
# 03.8 Ahora, utilizando la misma función de la última pregunta (03.7) guarde el resultado en un objeto
# el nombre del objeto debe ser su "nombre_apellido", (0.5 pto)
 
amanda_baez <- freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# 04. Ahora recodifique la variable re_02 (¿Cuál es su afiliación religiosa o creencia espiritual?)
# 04.1 observe las categorías de respuesta de esta variable (0.5 pto)

unique(base$tipo_creencia)
table(base$tipo_creencia)
base$tipo_creencia <-gsub (pattern = " ", replacement = "", x = base$tipo_creencia)

# 04.2 recodifique las categorías de esta variable en una nueva variable denominada "religion_rec" de la siguiente forma (3 ptos)
# Hint:  debe utilizar la función case_when()
base <- base %>% mutate(tipo_creencia=case_when(tipo_creencia=="religion_rec")
                        
# Las nuevas 3 categorías

# 1. Espiritualidad sin afiliación religiosa:
  
- "No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías)."
- "seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre"
- "deísmo"

# 2. Religión específica:

- "Catolico"
- "Cristianismo Protestante (Evangélico, anglicano, etcétera)"
- "Grecorromana "
- "Yoruba "
- "Ortodoxo"

# 3. Sin religión:

- "Pagana"    
- "Ateo"
- "Agnóstico"
- "No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria"
- "Ninguno "

# Utilice la siguiente estructura de código

base <- base %>% mutate(tipo_creencia=case_when(afiliacion=case_when(afiliacion%in% c("No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías).","seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre", "deísmo") ~"Espiritualidad sin afiliación religiosa",
                                               (afiliacion%in%c("Catolico","Cristianismo Protestante (Evangélico, anglicano, etcétera)","Grecorromana ","Yoruba ","Ortodoxo") ~"Religion especifica",
                                                (afiliacion%in%c("Pagana","Ateo","Agnóstico","No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria","Ninguno ") ~"Sin religion", 
                                                 TRUE ~ afiliacion))
                                           


# 04.3 observe la nueva variable "religion_rec" con la función table (0.5 ptos)

table(base$afiliacion)

# 05.1 Ahora recodifique la variable to_01  (tiempo libre) en 2 categorías en la misma variable (2 ptos) 
# "Tiempo disponible": Bastante tiempo.
#                      Suficiente tiempo.
# "Tiempo limitado": No tengo tiempo.
#                    Poco tiempo.


base <- base %>% mutate(to_01=case_when(to_01 %in% c("Bastante
tiempo.", "Suficiente tiempo.")~"Tiempo disponible",
                                        to_01 %in% c("No tengo
tiempo.", "Poco tiempo.")~ "Tiempo limitado",
                                        TRUE~to_01))

# 06. Obtenga los siguientes estadísticos descriptivos de la variable "ea_04_notas_ultimo_semestre"

# Media (0.5 ptos)

mean_notas <- mean(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)
#media notas 5.691

# Mediana (0.5 ptos)
mediana <-median(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)
#5.8

# desviación estándar (0.5 ptos)

desviacion_e <-sd(base$ea_04_notas_ultimo_semestre, na.rm = TRUE) 
#desviacion estandar 0.472

# varianza (0.5 ptos)

varianza <- var(base$ea_04_notas_ultimo_semestre, na.rm = TRUE) 

# 07. Grafique los cruces de la variable  "ea_04_notas_ultimo_semestre" con las siguientes variables:
# Hint: La variable "ea_04_notas_ultimo_semestre" es la variable dependiente.

# 07.1 ea_04_notas_ultimo_semestre vs religion_rec (3 ptos)

ggplot(base, aes(x=afiliacion, y=ea_04_notas_ultimo_semestre, fill=afiliacion)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Promedio de notas según tipo de religión", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "ea_04_notas_ultimo_semestre",
       fill = "afiliacion")+
  theme(legend.position="none")

# 07.2 ea_04_notas_ultimo_semestre vs to_01 (3 ptos)
ggplot(base, aes(x=to_01, y=ea_04_notas_ultimo_semestre, fill=ea_04_notas_ultimo_semestre)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Notas último semestre según tiempo", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "ea_04_notas_ultimo_semestre",
       fill = "ea_04_notas_ultimo_semestre")+
  theme(legend.position="none")

# 07.3 ahora grafique la distribución de los resultados guardados en el punto 03.8 con el siguiente código (3 ptos)
# recuerde que el nombre era su "nombre_apellido"


ggplot(amanda_baez, aes(x = pct, y = fct_reorder(respuestas, pct), fill=respuestas)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Lugares donde escuchan música", 
       subtitle = "Estudiantes de antropología", 
       caption = "Fuente: Encuesta Estudiantes Antropología 2024")+
  geom_text(aes(label = paste0(round(pct, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")
