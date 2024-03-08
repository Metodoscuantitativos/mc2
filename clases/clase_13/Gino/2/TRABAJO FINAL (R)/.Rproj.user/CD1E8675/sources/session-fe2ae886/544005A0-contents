
# Paquetes ----------------------------------------------------------------
#install.packages("pacman")

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer,
               knitr, gt, summarytools, ggthemes, hrbrthemes, stringr, tidyselect, kableExtra)

# Base --------------------------------------------------------------------
base <- read.csv(file = "base/Encuesta antropología 2023.csv")

#libro de códigos
libro <- read.csv(file = "base/Encuesta antropología 2023.csv")

# Conteo Entrevistadores ----------------------------------------------------
fechas <- base$Marca.temporal
solo_fechas <-substr(fechas, 1, 10)
base$Marca.temporal <- solo_fechas 

base <- base %>%
  rename(nombre_encuestador = ID1..Escriba.el.nombre.y.apellido.del.a.encuestador.a)

base <- base[-96,]
base$nombre_encuestador[10] <- "Elisa Matte"
base$nombre_encuestador[59] <- "Joaquín Meléndez"
base$nombre_encuestador[69] <- "Marithe Gorteau"
base$nombre_encuestador[13] <- "Javiera Bustos"
base$nombre_encuestador[87] <- "Javiera Bustos"

table(base$nombre_encuestador)

base$nombre_encuestador <- tolower(base$nombre_encuestador) #todas a minusculas
base$nombre_encuestador  <- gsub(pattern = " ", replacement = "", x = base$nombre_encuestador)
base$nombre_encuestador <- iconv(base$nombre_encuestador, to = "ASCII//TRANSLIT")

table(base$nombre_encuestador)

base <- base %>%
  mutate(nombre_encuestador = case_when(
    tolower(nombre_encuestador) == "carolinacofre" ~ "Carolina Cofre",
    tolower(nombre_encuestador) == "cristobalaldana" ~ "Cristobal Aldana",
    tolower(nombre_encuestador) == "cristobalsilva" ~ "Cristobal Silva",
    tolower(nombre_encuestador) %in% c("elisa", "elisamatte") ~ "Elisa Matte",
    tolower(nombre_encuestador) %in% c("estefanogonzalez", "estafanogonzalez")  ~ "Estefano Gonzalez",
    tolower(nombre_encuestador) %in% c("franciscadelgado", "franciscaantoniadelgadovielma") ~ "Francisca Delgado",
    tolower(nombre_encuestador) == "isidorabesa" ~ "Isidora Besa",
    tolower(nombre_encuestador) == "javierabustos" ~ "Javiera Bustos",
    tolower(nombre_encuestador) == "joaquinmelendez" ~ "Joaquin Melendez",
    tolower(nombre_encuestador) %in% c("marithegorteau", "marithegorteaupaillafil", "marithegortieau") ~ "Marithe Gorteau",
    tolower(nombre_encuestador) %in% c("matiasalvarez.", "matiasalvarez") ~ "Matias Alvarez",
    tolower(nombre_encuestador) == "matildehernandez" ~ "Matilde Hernandez",
    tolower(nombre_encuestador) == "nayarethastudillo" ~ "Nayareth Astudillo",
    tolower(nombre_encuestador) == "octaviopena" ~ "Octavio Peña",
    tolower(nombre_encuestador) %in% c("pablocornejorojas", "pablocornejo") ~ "Pablo Cornejo",
    tolower(nombre_encuestador) == "renatafuentes" ~ "Renata Fuentes",
    tolower(nombre_encuestador) %in%  c("samantaescafi", "samanthaescafi", "samescafi") ~ "Samantha Escafi",
    tolower(nombre_encuestador) %in%  c("sheilafogarty", "sheilamaureenfogartychrystal") ~ "Sheila Fogarty",
    tolower(nombre_encuestador) == "valentinaecheverria" ~ "Valentina Echeverria",
    tolower(nombre_encuestador) == "vanessasmith" ~ "Vanessa Smith",
    tolower(nombre_encuestador) %in% c("victoriasvalenzuela", "victoriavalenzuela") ~ "Victoria Valenzuela",
    TRUE ~ nombre_encuestador
  ))

#Crear la tabla con fechas
tabla_con_fechas <- data.frame(Nombre_Encuestador = base$nombre_encuestador,
                    Marca_Temporal = base$Marca.temporal)

tabla_con_fechas <- tabla_con_fechas %>%
  kable(col.names = c("Nombre Encuestador", "Marca Temporal"),
        caption = "Tabla de datos",
        format = "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

print(tabla_con_fechas)# Imprimir la tabla formateada

# Crear Tabla de frecuencias -
contenido_de_frecuencias <- table(base$nombre_encuestador)

tabla_de_frecuencias <- data.frame(Nombre_Encuestador = names(contenido_de_frecuencias), # Crear un data frame con el contenido
                    Frecuencia = as.vector(contenido_de_frecuencias))

tabla_de_frecuencias <- tabla_de_frecuencias %>% # Formatear la tabla con kable y kableExtra
  kable(col.names = c("Nombre Encuestador", "Frecuencia"),
        caption = "Tabla de frecuencias",
        format = "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

print(tabla_de_frecuencias) # Imprimir la tabla formateada



# Procesamiento SD -------------------------------------------------------------

#Quitar columna irrelevante
base <- base %>% 
  select(-1)

#Limpiar variables
base <- janitor::clean_names(base)

#faltaba: le07g
substr(names(base), 1, 4)
nuevos_nombres <-  c("id1","id2",
                     "sd01","sd02","sd03","sd04","sd05","sd06","sd07","sd08","sd09","sd10",
                     "le01","le02","le03","le04","le05","le06","le07a","le07b","le07c","le07d","le07e","le07f","le07g",
                     "le08a","le08b","le08c","le08d","le08e",
                     "le09","le10","le11","le12",
                     "hs01","hs02","hs03","hs04","hs05","hs06","hs07","hs08","hs09","hs10","hs11","hs12","hs13","hs14","hs15",
                     "al01","al02","al03","al04","al05","al06","al07","al08","al09","al10",
                     "eta1","eta2","eta3","eta4","etb1","et01","etb2a","etb2b","etb2c","etb3d","etb4","etb5","etb6","etb7","etb8")





base <- base %>%
  rename_with(~ nuevos_nombres, everything())
  
#Renombrar variables SD
base <- base %>% dplyr::rename(entrevistadornombre = id1,
                               nombre = id2,
                               edad = sd01, 
                               genero = sd02, 
                               ciudad_actual = sd03, 
                               comuna_actual = sd04, 
                               anio_de_ingreso= sd05, 
                               avr_notas = sd06, 
                               educacion = sd07, 
                               clase_social = sd08, 
                               educacion_madre = sd09, 
                               educacion_padre = sd10)


# DataExplorer::create_report(base)

###Limpieza de categorías de variables

  #edad
base <- base %>%
  mutate(edad = str_extract(edad, "\\d+")) %>% # solo números de la variable edad
  mutate(edad = as.numeric(edad)) %>%  #la transformo en variable numérica numérico
  mutate(edadr= case_when (edad %in% c(18:20) ~ "18 a 20", 
                           edad %in% c(21:23) ~ "21 a 23", 
                           edad %in% c(24:29) ~ "24 a 29", 
                           edad >= 30 ~ "30 o más")) #genero otra variable con categorías de edad

  #ciudad_actual
base <- base %>%
  mutate(ciudad_actual = case_when(
    tolower(ciudad_actual) %in% c("santiago", "santiago de chile", "santiago.", " Santiago ") ~ "Santiago",
    tolower(ciudad_actual) == "general libertador bernardo o'higgins" ~ "General Libertador Bernardo O'Higgins",
    tolower(ciudad_actual) == "localidad del parron" ~ "El Parrón",
    tolower(ciudad_actual) == "los andes" ~ "Los Andes",
    tolower(ciudad_actual) == "melipilla" ~ "Melipilla",
    tolower(ciudad_actual) == "paine" ~ "Paine",
    tolower(ciudad_actual) == "metropolitana" ~ "Región Metropolitana",
    tolower(ciudad_actual) == "valparaíso" ~ "Valparaíso",
    TRUE ~ ciudad_actual
  ))

  #comuna_actual
base$comuna_actual <- tolower(base$comuna_actual)
base$comuna_actual  <- gsub(pattern = " ", replacement = "", x = base$comuna_actual)

base <- base %>%
  mutate(comuna_actual = case_when(
    tolower(comuna_actual) == "buin" ~ "Buin",
    tolower(comuna_actual) %in% c("santiago", "santiagocentro") ~ "Santiago",
    tolower(comuna_actual) == "laflorida" ~ "La Florida",
    tolower(comuna_actual) %in% c("puentealto", "puentealto") ~ "Puente Alto",
    tolower(comuna_actual) == "lobarnechea" ~ "Lo Barnechea",
    tolower(comuna_actual) == "ñuñoa" ~ "Ñuñoa",
    tolower(comuna_actual) == "sanbernardo" ~ "San Bernardo",
    tolower(comuna_actual) == "quilicura" ~ "Quilicura",
    tolower(comuna_actual) == "recoleta" ~ "Recoleta",
    tolower(comuna_actual) == "estacióncentral" ~ "Estación Central",
    tolower(comuna_actual) == "maipú" ~ "Maipú",
    tolower(comuna_actual) == "sanjoaquín" ~ "San Joaquín",
    tolower(comuna_actual) == "sanmiguel" ~ "San Miguel",
    tolower(comuna_actual) == "providencia" ~ "Providencia",
    tolower(comuna_actual) == "huechuraba" ~ "Huechuraba",
    tolower(comuna_actual) == "valparaíso" ~ "Valparaíso",
    tolower(comuna_actual) == "lareina" ~ "La Reina",
    tolower(comuna_actual) == "colina" ~ "Colina",
    tolower(comuna_actual) == "curacaví" ~ "Curacaví",
    tolower(comuna_actual) == "lagranja" ~ "La Granja",
    tolower(comuna_actual) == "lampa" ~ "Lampa",
    tolower(comuna_actual) == "peñaflor" ~ "Peñaflor",
    tolower(comuna_actual) == "peñalolén" ~ "Peñalolén",
    tolower(comuna_actual) == "mariapinto" ~ "María Pinto",
    tolower(comuna_actual) == "tiltil" ~ "Tiltil",
    TRUE ~ comuna_actual
  ))


#sumé esto (!)

base <- base %>%
  mutate(comuna_actual = stringi::stri_trans_general(comuna_actual, "Latin-ASCII"),
         comuna_actual = tolower (comuna_actual),
         comuna_actual = gsub(" ", "_", comuna_actual)) 


freq(base$comuna_actual, prop = TRUE, order = "freq", report.nas =  FALSE)

base <- base %>% 
  mutate (comuna_actual = case_when(comuna_actual=="lascondes" ~ "las_condes",
                                    comuna_actual=="lafloridaypuentealto" ~ "la_florida",
                                    comuna_actual=="loprado" ~ "lo_prado",
                                    comuna_actual=="losandes" ~ "los_andes", 
                                    comuna_actual=="oaine" ~ "paine", 
                                    comuna_actual=="pedroaguirrecerda" ~ "pedro_aguirre_cerda", 
                                    comuna_actual=="quintanormal." ~ "quintanormal", 
                                    comuna_actual=="sanfelipedeaconcagua" ~ "san_felipe", 
                                    TRUE ~ comuna_actual))
   
freq(base$comuna_actual, prop = TRUE, order = "freq", report.nas =  FALSE)


  #anio_de_ingreso
#Quizá acá se puede hacer un mutate de "antes de pandemia, dps de pandemia"? no sé de qué sería útil

  #avr_notas
#lo mismo acá, no sé qué analisis se podría hacer

  #educacion
#lo mismo acá, no sé qué analisis se podría hacer

  #clase_social
#lo mismo acá, no sé qué analisis se podría hacer

  #educacion_madre y educación_padre
base <-  base %>% 
  mutate(generacion = ifelse(educacion_madre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ) | 
                               educacion_padre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ), 
                             "No es primera generación", 
                             "Si es primera generación")) 

base[base == ''] <- NA


#OMITIR DE MOMENTO XD hasta q sirva


# Homogeneizar las letras a minúsculas en SD03 y SD04
# base$SD03...En.qué.ciudad.vives.actualmente. <- gsub("[áÁ]", "a", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD03...En.qué.ciudad.vives.actualmente.  <- gsub("[éÉ]", "e", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD03...En.qué.ciudad.vives.actualmente.  <- gsub("[íÍ]", "i", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD03...En.qué.ciudad.vives.actualmente.  <- gsub("[óÓ]", "o", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD03...En.qué.ciudad.vives.actualmente.  <- gsub("[úÚ]", "u", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD03...En.qué.ciudad.vives.actualmente.  <- gsub("ñ", "n", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# table(base$SD03...En.qué.ciudad.vives.actualmente.)
# 
# base$SD04...En.qué.comuna.vives.actualmente. <- gsub("[áÁ]", "a", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD04...En.qué.comuna.vives.actualmente.  <- gsub("[éÉ]", "e", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD04...En.qué.comuna.vives.actualmente.  <- gsub("[íÍ]", "i", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD04...En.qué.comuna.vives.actualmente.  <- gsub("[óÓ]", "o", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD04...En.qué.comuna.vives.actualmente.  <- gsub("[úÚ]", "u", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# base$SD04...En.qué.comuna.vives.actualmente.  <- gsub("ñ", "n", x = base$SD03...En.qué.ciudad.vives.actualmente.)
# table(base$SD04...En.qué.comuna.vives.actualmente.)



write.xlsx(x = base_en_environment,file = "ruta_donde_la_guardo") # en general se usa output


write.xlsx(x = base,file = "base/EncuestaAntropologia_2023.xlsx")




# Trabajo en clase --------------------------------------------------------
base <- read.xlsx(xlsxFile = "base/EncuestaAntropologia_2023.xlsx")


#01. Renombrar variables
names(base) #ver variables
names(libro) # ver libro de códigos

#renombro un conjunto de variables
base <- base %>% dplyr::rename(tiempo_libre = le01,  
                                 frutas = hs01,
                                 limite_alcohol = al01, 
                                 rapport= eta1)


#renombre usted 4 variables que utilizará su grupo: Práctica

# base <- base %>% dplyr::rename(_______ = _______,  
#                                _______ = _______,
#                                _______ = _______, 
#                                _______ = _______)



# limpio: le10: donde lee libros
names(libro)
freq(base$le10, prop = TRUE, order = "freq", report.nas =  FALSE)
unique(base$le10)

base <- base %>%
  mutate(le10 = ifelse(le10=="Donde pueda encontrar espacio en mi casa", yes= "Otros lugares de la casa", 
                          no=ifelse(le10 %in% c("donde sea", "Todos los anteriores ", "Todos", "Todas las anteriores "), yes="Indefinido", 
                                    no=le10)))

# compruebo con una tabla de frecuencias
freq(base$le10, prop = TRUE, order = "freq", report.nas =  FALSE)


# limpio: le11: porqué dispositivo: múltiples respuestas (multiple response)
unique(base$le11)

#separo las respuestas
respuestas <- strsplit(base$le11, ";") # separo las respuestas que tienen punto y come (;)
respuestas <- unlist(respuestas) #las unlisto, las saco de una lista

#observo las respuestas
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#cambios quienes contestaron otros
respuestas <- gsub("libgen", "A través de sitios web no oficiales (por ejemplo: Lectulandia, Thepiratebay, Sci-hub, etc)", respuestas)
respuestas <- gsub("wattpad", "Wattpad", respuestas)

#Observo
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#Guardo para graficar
le11f <- freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()



#Práctica: haremos lo mismo para hs07: actividades físicas que realizó####
unique(base$hs07)

respuestas <- strsplit(base$hs07, ";")
respuestas <- unlist(respuestas)

#Observo
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() 

freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()%>% 
  print(n = 26)


#cambios quienes contestaron distintos tipos de danza, a danza. 
#Danza contemporánea·
#zumba
respuestas <- gsub("Danza contemporánea·", "Danza", respuestas)
respuestas <- gsub("zumba", "Danza", respuestas)

#cambie quienes contestaron: salir a caminar·, Caminatas Rutinarias a Caminar



# preguntarle a chatgpt: qué recomiendas para codificar preguntas abiertas en una encuesta?
unique(base$etb2a)
unique(base$etb2b)
unique(base$etb2c)

#¿Qué tipo de palabras son?
#adjetivos: características: positivos/negativos?
#sustantivos comunes/ propios: en qué área/ámbito?
#tendrán que codificar y observar tendencias


#recodificaciones: edad
base <- base %>%
  mutate(edad = str_extract(edad, "\\d+")) %>% # solo números de la variable edad
  mutate(edad = as.numeric(edad)) %>%  #la transformo en variable numérica numérico
  mutate(edadr= case_when (edad %in% c(18:20) ~ "18 a 20", 
                           edad %in% c(21:23) ~ "21 a 23", 
                           edad %in% c(24:29) ~ "24 a 29", 
                           edad >= 30 ~ "30 o más"))

freq(base$edadr, prop=TRUE,  report.nas = FALSE) %>% 
  tb() 

#práctica recodifique: anio_de_ingreso en: pre_pandemicos, pandemicos, pos_pandemia
unique(base$anio_de_ingreso)
freq(base$anio_de_ingreso, prop=TRUE,  report.nas = FALSE) %>% 
  tb() 


# Distribución de frecuencias --------------------------------------------
names(base)

#genero
options(OutDec= ",") # que el separador de decimales sea coma (,)


base %>% 
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()


base %>% 
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") 

#cómo haría para eliminar una categoría: "prefiero no responder"
class(base$genero)
unique(base$genero)

base %>% 
  mutate(genero = if_else(genero =="Prefiero no responder", as.character(NA), genero)) %>%
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") 

#para guardarla
if(!dir.exists("tablas")) dir.create("tablas")

base %>% 
  mutate(genero = if_else(genero =="Prefiero no responder", as.character(NA), genero)) %>%
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/dfgenero1.png", zoom = 3)


#Práctica: realice dos distribuciones de frecuencias
names(base)
names(libro)



# Gráficos categóricos univariados ----------------------------------------

dfgenero <- base %>% 
  mutate(genero = if_else(genero =="Prefiero no responder", as.character(NA), genero)) %>%
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() 

names(dfgenero)

#opción 1
ggplot(dfgenero, aes(x = pct, y = fct_reorder(genero, pct), fill=genero)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Género Antropología UAH", 
       subtitle = "Encuesta Estudiantes 2023",
       caption = "fuente: Encuesta Estudiantes Antropología 2023")+
  geom_text(aes(label = paste0(round(pct, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

#opción 2
ggplot(dfgenero, aes(x =fct_reorder(genero, pct), y = pct, fill=genero)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Género Antropología UAH", 
       caption = "fuente: Encuesta Estudiantes Antropología 2023")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")


#Práctica: realice dos gráficos de barras, con dos variables distintas


# gráfico de tortas
ggplot(dfgenero, aes(x = "", y = pct, fill = genero)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + # esto permite hacer gráfico de torta
  labs(title = "Género Estudiantes UAH", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "Religión") +
  theme_ipsum() +
  scale_fill_viridis_d( guide = "legend", limits = dfgenero$genero)

#por default: geom_bar cuenta las frecuencias, esto sería geom_bar(stat = "count")
#stat = "identity": cada barra representa el porcentaje de la categoría.

#Práctica: realice dos gráficos de torta.


# Análisis bivariados  --------------------------------------------------

names(libro)
names(base)

#¿La preferencia de ficción o no ficción dependerá del género?

names(base)
names(libro)
unique(base$le06)


#acorto los nombres para que aparezcan bien en la tabla
base <- base %>% 
  mutate(le06 = fct_recode(le06,
                           "Ficción" = "Ficción (novelas de romance, acción, suspenso, fanfics, ciencia ficción, policiales, poesía, etc)",
                           "No ficción" = "No ficción (documentos científicos, académicos, revistas, biografías, etc)"))


#Realizo tabla de contingencia
ct_lecgenero <- base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le06, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 


#graficar

#elijo ciertos colores
colors <- c("#440154", "#b2df8a", "#365A8C", "#277E8E", "#1FA088", "#44A96C", "#7FBC41", 
                     "#B3CC2A", "#FDE725", "#46337E", "#b07aa1", "#ff9da7", "#9c755f", 
                     "#bab0ac", "#5c5c5c") 


#realizo un gráfico de barras 
ggplot(data = subset(base, genero != "Prefiero no responder"), aes(x = genero, fill = le06)) +
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)


#¿Por qué los hombres leerán más ficción que las mujeres?


# Realice lo mismo para actividad física: hs08
unique(base$hs08)
table(base$hs08)

base %>%
  filter(genero != "Prefiero no responder") %>%
  select(hs08, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

#recodifico para tener menos categorías 
base <- base %>% 
  mutate(hs08 = fct_recode(hs08,
                           "Entre 1 y 4 horas" = "Entre 1 y 4 horas",
                           "Más de 5" = "Entre 5 y 8 horas", 
                           "Más de 5" = "Más de 8 horas", 
                           "No realizo actividad física o deportiva" = "No realizo ninguna actividad física o deportiva durante la semana"))


#lo guardo
ct_acfisgenero <- base %>%
  filter(genero != "Prefiero no responder") %>%
  select(hs08, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 



# para la realización de actividad física 

#graficar
ggplot(data = subset(base, genero != "Prefiero no responder"), aes(x = genero, fill = hs08)) +
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)


base <- base %>% 
  mutate(hs08 = case_when(
    hs07 == "No realicé ninguna actividad física o deportiva" ~ "No realizo actividad física o deportiva",
    TRUE ~ hs08
  ))

ggplot(data = subset(base, genero != "Prefiero no responder"), aes(x = genero, fill = hs08)) +
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)


ggplot(data = subset(base, genero != "Prefiero no responder" & complete.cases(hs08)), aes(x = genero, fill = hs08)) + #solo utilizo filas completas
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)




