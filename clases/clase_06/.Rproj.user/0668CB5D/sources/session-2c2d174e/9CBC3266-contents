# Analizando datos categóricos: distribución de frecuencias y tablas de contingencia

# install.packages("rlang")
# library(rlang)

# install.packages("rlang")
#library(xaringan)

install.packages ("xaringanthemer")
library(xaringanthemer)
                 

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer, 
               datos,  knitr, gt, 
               summarytools, ggthemes)

#knitr y gt me permitirán dar formato a las tablas
# webshot::install_phantomjs() # para exportar tablas de knitr

#abro la base a trabajar
datos <- datos::encuesta


# I. Distribución de frecuencias ------------------------------------------
#00. Observo la base
glimpse(datos)

#¿Quiero saber cuántas personas son de cada religión?
table(datos$religion)


#01. Forma 1: Con freq de summarytools####
freq(datos$religion) #está medio desordenada: frecuencias válidas y acumuladas por default
freq(datos$religion, prop = TRUE, order = "freq", report.nas =  FALSE) #eliminé valores NA y ordené por frecuencia

#argumentos (son separados por coma):
#prop= TRUE: pon proporciones (no sólo frecuencias absolutas)
#order= "freq": ordena por frecuencas
#report.nas: FALSE: no reportes datos perdidos


#problema: aparece "Sin respuestas" y  "No aplica" en la tabla ¿Cómo eliminarlas?
datos %>% 
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), NA, religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)

#ifelse: para religión. 
# Si incluye "Sin respuesta", "No aplica"
# Transforma en NA
# Si no los incluye dejalo en religión


class(datos$religion) #ifelse parece tener problemas para trabajar con variables factor: convierto religión y NA en character

datos %>% 
  mutate(religion = as.character(religion)) %>% #transformo a character
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)

#Importancia de tipo de dato (class()) para hacer ciertos procedimientos. 


#ejercicio: pruebe hacer una tabla de frecuencia con freq() de la variable raza


#02.Forma 2: mediante tidyverse####


datos %>%
  count(religion) %>% # contar las frecuencias de cada religión
  mutate(Porcentaje = n / sum(n) * 100) %>% # genero un porcentaje
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #redondeo en un decimal
  arrange(desc(Porcentaje)) %>%  #ordeno de mayor a menor
  rename(Frecuencia = n, Religion= religion) 

#observemos que sin respuesta aparece en el cálculo, preferimos eliminarla. 
datos %>%
  filter (!(religion =="Sin respuesta")) %>% #elimino "Sin respuesta"
  count(religion) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  mutate(Porcentaje = round (Porcentaje, 1)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, Religion= religion) 


#Acá le agrego el total con bind_rows
datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  mutate(Porcentaje = round (Porcentaje, 1)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, Religion= religion) %>% 
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 

#Ojo:  busqué en StackOverflow,  https://chat.openai.com/, https://rtutor.ai/


#ejercicio: pruebe hacer una tabla de frecuencia mediante tidyverse con la variable raza





# II. Tablas de contingencia ----------------------------------------------
#01.mediante ctable()####

summarytools::ctable( x = datos$religion, y = datos$raza)

# cruce de dos variables categóricas:
# en la X suele ir variable dependiente: religión
# en la y la independiente: raza
# la pregunta es: ¿cómo la dependiente modifica a la independiente?


#01.1. Orden general de tabla ####

#ordeno re factorizando según orden deseado, ex. cantidad de casos por categoría. 

datos$raza <- datos$raza %>% fct_relevel(c("Blanca", "Negra", "Otra")) %>% 
  fct_drop("No aplica") #ordeno los resultados según raza y elimino la categoría no aplica


#para religión me fijo en el orden de importancia en la distribución de frecuencias y ordeno la tabla de esa manera.

# Protestante   10846    50.49    50.49
# Católica    5124    23.85    74.34
# Ninguna    3523    16.40    90.74
# Cristiana     689     3.21    93.94
# Judía     388     1.81    95.75
# Otra     224     1.04    96.79
# Budismo     147     0.68    97.48
# Inter o no confesional     109     0.51    97.98
# Musulmana/Islam     104     0.48    98.47
# Cristiana ortodoxa      95     0.44    98.91
# Sin respuesta      93     0.43    99.34
# Hinduismo      71     0.33    99.67
# Otra religión oriental      32     0.15    99.82
# Nativa americana      23     0.11    99.93
# No sabe      15     0.07   100.00
# No aplica       0     0.00   100.00

datos$religion <- datos$religion %>% fct_relevel(c("Protestante", 
                                                   "Católica", 
                                                   "Ninguna", 
                                                   "Cristiana",
                                                   "Judía", 
                                                   "Budismo", 
                                                   "Inter o no confesional", 
                                                   "Musulmana/Islam", 
                                                   "Cristiana ortodoxa", 
                                                   "Hinduismo", 
                                                   "Otra religión oriental", 
                                                   "Nativa americana", 
                                                   "Otra", 
                                                   "No sabe", 
                                                   "No aplica", 
                                                   "Sin respuesta"
)) %>% 
  fct_drop("No aplica")

#observar los niveles de una variable facto
levels(datos$religion)


#01.2. Uso de proporciones ####

# sin proporciones
ctable( x = datos$religion, y = datos$raza, prop = "n", justify = "l")

#prop = "n" : es proporciones sin proporciones
#justify: "l": es ajustar la tabla a la izquierda

# proporciones total
ctable( x = datos$religion, y = datos$raza, prop = "t", justify = "l")

#¿Qué se puede interpretar?

#proporciones según fila
ctable( x = datos$religion, y = datos$raza, prop = "r", justify = "l") 
#prop = "r" : es proporciones en filas

#¿Qué se puede interpretar?

#proporciones según columna
ctable( x = datos$religion, y = datos$raza, prop = "c", justify = "l")

#prop = "c" : es proporciones en columnas

#¿Qué se puede interpretar?
#más allá de un 5% del total se suele considerar una diferencia importante. 

#01.3. Aproximandonos a la prueba de hipótesis chi cuadrado ####
ctable( x = datos$religion, y = datos$raza, prop = "c", justify = "l", 
        chisq = T)

#práctica#### 
#realice una tabla entre partido y raza

#primero pase la siguiente sintaxis: 
#¿quién la puede interpretar?
unique(datos$partido)
datos %>%
  mutate(partido = as.character(partido)) %>%
  mutate(partido = if_else(partido %in% c("Sin respuesta", "No aplica"), as.character(NA), partido)) %>%
  freq(partido, prop = TRUE, order = "freq", report.nas =  FALSE)

datos <- datos %>%
  mutate(partido_r = case_when(partido ==  "No fuertemente demócrata" ~ "Demócrata",
                               partido ==  "Fuertemente demócrata" ~ "Demócrata",
                               partido ==  "Ind, pro dem" ~ "Demócrata",
                               partido ==  "Fuertemente republicano" ~ "Republicano",
                               partido ==  "No fuertemente republicano" ~ "Republicano",
                               partido ==  "Ind, pro rep" ~ "Republicano",
                               partido ==  "Otro partido" ~ "Otro partido",
                               partido ==  "Independiente" ~ "Independiente", 
                               partido ==  "No sabe" ~ "No sabe"))

class(datos$partido_r)
datos$partido_r <- as.factor(datos$partido_r) #por qué se transforma a factor?
class(datos$partido_r)

datos$partido_r <- datos$partido_r %>% fct_relevel(c("Demócrata", "Republicano", 
                                                     "Independiente", "Otro partido", "No sabe"))

#realice la tabla que estime conveniente e interprete: 
#¿Cuál es la variable dependiente y cuál es la independiente?

ctable( x = datos$partido_r, y = datos$raza, prop = "c", justify = "l", useNA = "no")


#02.mediante prop.table()#### ¿Cómo sacar los sin respuesta?
#01.1. Uso de proporciones#### 

#atención con table!

#hacerlo sin tidyverse
table(datos$religion, datos$raza)

#hacerlo con tidyverse
datos %>% 
  table(religion, raza) #no reconoce

datos %>% 
  select(religion, raza) %>% 
  table() #así funciona, primeramente seleccionamos y luego hacemos la tabla



#ahora realizo la tabla y le saco proporciones
datos %>% 
  select(religion, raza) %>% 
  table(.) %>% 
  prop.table(.)*100

#me fijo que hay una categoría que no me interesa "Sin respuesta"


#elimino categoría sin respuesta
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>% #elimina las categorías que no se utilizan en la columna
  table(.) %>% 
  prop.table(.)*100


#redondeo e interpreto
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>% #elimina las categorías que no se utilizan en la columna
  table(.) %>% 
  prop.table(.) %>% #el punto remite a que se hace para los datos anteriores
  round(4)*100 # con esto primero redondeo en sólo cuatro decimales y multiplico por 100
  

#proporciones por filas
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>% 
  table(.) %>% 
  prop.table(.,1) %>% # el segundo argumento de prop.table es si se hace por filas (1) o columnas (2)
  round(4)*100 


#proporciones por columnas
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>% 
  table(.) %>% 
  prop.table(.,2) %>% # el segundo argumento de prop.table es si se hace por filas (1) o columnas (2)
  round(4)*100 



#01.2. Agrego addmargins #### 
#en tablas anteriores no están los totales, addmargins permite agregarlos

datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,1) #agrega total de filas 

datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) #agrega total de columnas


#01.3. Combino addmargins y prop.table #### 

#proporciones por filas
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  prop.table(.,1) %>% #primero prop.table
  addmargins(.,2) %>% #segundo addmargins
  round(4)*100

#es similar a:
ctable( x = datos$religion, y = datos$raza, prop = "r", justify = "l")



#proporciones por columnas
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

#es similar a:
ctable( x = datos$religion, y = datos$raza, prop = "c", justify = "l")


#práctica####
#realice una tabla de contingencia entre partido_r y raza
datos %>%
  filter(partido_r != "Sin respuesta") %>%
  select(partido_r, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 


#03.mediante group_by
# datos %>% 
#   group_by(religion, raza) %>% 
#   count() %>% 
#   pivot_wider(names_from = "raza", 
#               values_from = n) 

#deseo eliminar los "Sin respuesta" y "No aplica"
#cambiar a 0 ese NA que aparece en Negra y nativa americana

# datos %>% mutate(religion = as.character(religion)) %>%
#   mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
#   group_by(religion, raza) %>% 
#   count() %>% 
#   pivot_wider(names_from = "raza", 
#               values_from = n) %>% 
#   filter(!is.na(religion)) %>% 
#   replace_na(list(Negra = 0)) %>% 
#   arrange(desc(Blanca)) 
 

# III. Formatear y Guardar tablas -----------------------------------------

#01.Distribución de frecuencias####
#01.1 exportarla a excel####

#A. con freq()
f_religion1 <- datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() #con esto lo convierto al formato data.frame, el que puede ser exportado a excel

class(f_religion1)

#guardo la tabla en un excel (puedo pasarla a un ppt)
if(!dir.exists("tablas")) dir.create("tablas") 
#si dentro de mi carpeta de proyecto no existe el directorio tablas, crealo.

write.xlsx(f_religion1, "tablas/f_religion1.xlsx") #guardalo en un excel

#B. con tidyverse
f_religion2 <- datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  mutate(Porcentaje = round (Porcentaje, 2)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, Religion= religion) %>% 
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 

write.xlsx(f_religion2, "tablas/f_religion2.xlsx") #guardalo en un excel

#01.2 exportarla formateada con kable####
#A. freq()
#kable
datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Relgión")

#kablExtra: mayores posibilidades para formatear
#install.packages("kableExtra")
library(kableExtra)

datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") 

#observo que los decimales están en punto (.) y los prefiero en coma (,)
options(OutDec= ",")

datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") 


#si la queremos guardar en una imagen usar save_kable
datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/f_religion1.png", zoom = 3)

# para ver las posibilidades de formato:
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

#B.tidyverse
datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>% # contar las frecuencias de cada religión
  mutate(Porcentaje = n / sum(n) * 100) %>% # genero un porcentaje
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #redondeo en dos decimales
  arrange(desc(Porcentaje)) %>%  #ordeno de mayor a menor
  rename(Frecuencia = n, Religion= religion) %>% #renombro dos categorías
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) %>% 
  kable(col.names = c("Religion", "Frecuencia", "Porcentaje"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15)  #le doy formate con kable


#si la queremos guardar en una imagen sumamos save_kable ()
datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>% # contar las frecuencias de cada religión
  mutate(Porcentaje = n / sum(n) * 100) %>% # genero un porcentaje
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #redondeo en dos decimales
  arrange(desc(Porcentaje)) %>%  #ordeno de mayor a menor
  rename(Frecuencia = n, Religion= religion) %>% #renombro dos categorías
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) %>% 
  kable(col.names = c("Religion", "Frecuencia", "Porcentaje"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formato con kable
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
  save_kable(file = "tablas/f_religion2.png", zoom = 3) #file = donde la guardamos, zoom = cuan grande se guarda


#02. Tablas de contingencia####
#02.1 vía prop.table()####

#proporciones por columnas 
c_religionxraza1 <- 
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

write.xlsx(c_religionxraza, "tablas/c_religionxraza1.xlsx") #guardalo en un excel
 


#para ver una opción 
c_religionxraza1 %>% 
  kable(., caption="Tabla de contingencia para religión y raza") %>% 
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) 


#para guardarlo

c_religionxraza1 %>% 
  kable(., caption="Tabla de contingencia para religión y raza") %>% 
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
  save_kable(file = "tablas/c_religionxraza1", zoom = 2)




#pruebas para agregar columnas: 
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(2) %>% 
  prop.table(2) %>% 
  round(4)*100 %>% 
  as.data.frame(.) %>% 
  addmargins(1) 


t1 <- datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(2) %>% 
  prop.table(2) %>% 
  round(4)*100


t1 <- as.data.frame(t1)

t1  

class(t1)
  
  mutate(Total = rowSums(select(., -1))) %>% 
  bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(., na.rm = TRUE) else "Total")), .)


datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(2) %>%
  prop.table(2) %>% 
  round(4)*100 %>% 
  as.data.frame() %>% 
  add_row(Total = colSums(.[,-1]), .before = 1)

datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>%
  prop.table(.,2) %>% 
  round(4)*100 %>% 
  addmargins(.,2, name = "Total2")


datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(., 1, margin = "Total2") %>%
  prop.table(.,2) %>% 
  round(4)*100 



install.packages("Rcmdr")
library(Rcmdr)


 
 #otra opción es gt (tables): genera una tabla en distintos formatos.
 # tabla_religion <- datos %>%
 #   filter (!(religion =="Sin respuesta")) %>% 
 #   count(religion) %>%
 #   mutate(Porcentaje = n / sum(n) * 100) %>%
 #   mutate(Porcentaje = round (Porcentaje, 1)) %>% 
 #   arrange(desc(Porcentaje)) %>% 
 #   rename(Frecuencia = n, Religion= religion) %>% 
 #   bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 
 # 
 #  tabla_religion %>% 
 #   gt(rowname_col = "religion") %>%
 #   tab_header(
 #     title = "Tabla de distribución de frecuencias",
 #     subtitle = "Redondeado a dos dígitos decimales") 
 
 
 # install.packages("gmodels")
 # library(gmodels)
 # gmodels::CrossTable(x = datos$religion, y = datos$raza)
 # ??CrossTable
 # gmodels::CrossTable(x = datos$religion, y = datos$raza, prop.c = FALSE, prop.t = FALSE, 
 #                     prop.chisq = FALSE, prop.r = TRUE, digits = 1, format="SPSS")
 # gmodels::CrossTable(x = datos$religion, y = datos$raza, prop.c = TRUE, prop.t = FALSE, 
 #                     prop.chisq = FALSE, prop.r = FALSE, digits = 1, format="SPSS")
 
 
 # # create a contingency table using dplyr::tally and tidyr::spread
 # a_table <-
 #   fk_data %>% 
 #   group_by(category_1, category_2) %>% 
 #   tally() %>% 
 #   spread(key = category_2, value = n)
 # print.data.frame(a_table)
 # 