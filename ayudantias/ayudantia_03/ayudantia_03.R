# 03. Procesamiento de base de datos --------------------------------------

# install.packages("pacman")
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#leer archivos xlsx
               readxl,# leer archivos xl      #dos formatos de excel xlsx y xl
               janitor,#limpieza de datos
               writexl,#Guardar tablas formato excel
               DataExplorer) #Exploración rápida


#Importar el archivo y asignarlo en el environment------------------------------
base_am <- read.xlsx(xlsxFile = "/Users/fran/Desktop/R ayudantías/mc2/ayudantias/ayudantia_03/ALTOMAIPO.xlsx", colNames = TRUE, detectDates = TRUE)

#Explorar
glimpse(base_am) #Una primera mirada de lo que hay en mis datos, la primera fila es extraña, dice "respuesta" o repite el nombre de la variable.

base_am <- base_am[-1,]

names(base_am) #observo que hay puntos, mayúsculas y minúsculas, etcétera. Está sucia


#limpieza inicial----
base_am <- janitor::clean_names(base_am) #con esto transformo todo a minúscula, quito tildes, saco signos, borro espacios

names(base_am)



#observación de base
nrow(base_am) #473 cantidad de casos
ncol(base_am) #26 cantidad de variables
sapply(base_am, FUN = class) # sapply: realiza un a función a varias variables 
str(base_am) #estructura del objeto base de datos

# Renombrar variables -----------------------------------------------------

#extraigo el nombre de todas las variables
names (base_am)

# [1] "comuna"                                                                                                                                                                                    
# [2] "localidad"                                                                                                                                                                                 
# [3] "sexo"                                                                                                                                                                                      
# [4] "otra_comuna"                                                                                                                                                                               
# [5] "hace_cuantos_anos_vive_en_esta_comuna_enc_anotar_cantidad_de_anos_si_es_menos_de_un_ano_anotar_0"                                                                                          
# [6] "la_vivienda_que_usted_ocupa_es_enc_leer_alternativas"                                                                                                                                      
# [7] "cual_es_el_valor_que_paga_mensualmente"                                                                                                                                                    
# [8] "parentezco_con_jedfe_de_hogar"                                                                                                                                                             
# [9] "edad"                                                                                                                                                                                      
# [10] "genero"                                                                                                                                                                                    
# [11] "situacion_ocupacional"                                                                                                                                                                     
# [12] "cual_es_la_ocupacion_principal_del_jefe_de_hogar_enc_respuesta_espontanea_clasificar_en_una_de_las_siguientes_categorias"                                                                  
# [13] "principales_traslados"                                                                                                                                                                     
# [14] "lugar_traslado"                                                                                                                                                                            
# [15] "temporalidad_traslado"                                                                                                                                                                     
# [16] "medio_transporte"                                                                                                                                                                          
# [17] "tiempo_traslado"                                                                                                                                                                           
# [18] "a_que_sistema_de_salud_pertenece_su_familia_enc_leer_alternativas_y_responder_todas_las_que_correspondan"                                                                                  
# [19] "cuando_usted_o_algun_familiar_necesita_atencion_de_salud_adonde_acude_principalmente_enc_leer_alternativas_y_responder_una"                                                                
# [20] "ha_cambiado_la_calidad_de_la_atencion_en_los_ultimos_seis_meses"                                                                                                                           
# [21] "si_nota_un_cambio_como_ha_cambiado_en_enc_leer_cada_tipo_de_cambio_y_anotar_1_si_2_no_todos_los_tipos_de_cambios_deben_quedar_con_una_respuesta_anotada"                                   
# [22] "en_los_ultimos_seis_meses_han_ocurrido_incidentes_que_alteren_el_orden_publico_en_la_localidad_donde_vive_enc_leer_alternativas_y_responder_una"                                           
# [23] "la_presencia_de_aes_gener_en_la_comuna_le_ha_traido_beneficios_a_usted_o_a_su_familia_enc_leer_cada_tipo_de_beneficio_todos_los_tipos_de_beneficios_deben_quedar_con_una_respuesta_anotada"
# [24] "usted_conoce_el_mecanismo_por_el_cual_opera_la_produccion_de_electricidad_en_la_central_hidroelectrica_alto_maipo"                                                                         
# [25] "solo_si_conoce_el_mecanismo_me_podria_indicar_cual_es_el_mecanismo_que_se_utiliza"                                                                                                         
# [26] "desde_el_inicio_de_las_actividades_de_alto_maipo_ha_notado_diferencias_en_el_nivel_del_caudal_del_rio_enc_leer_cada_uno_de_los_ambitos_elegir_una_alternativa"  


# genero un vector con todas las columnas que quiero renombrar
cols_a_renombrar <- c(                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
  "comuna",                                                                                                                                                                                    
  "localidad",                                                                                                                                                                                 
  "sexo",                                                                                                                                                                                      
  "otra_comuna",                                                                                                                                                                               
  "hace_cuantos_anos_vive_en_esta_comuna_enc_anotar_cantidad_de_anos_si_es_menos_de_un_ano_anotar_0",                                                                                          
  "la_vivienda_que_usted_ocupa_es_enc_leer_alternativas",                                                                                                                                      
  "cual_es_el_valor_que_paga_mensualmente",                                                                                                                                                    
  "parentezco_con_jedfe_de_hogar",                                                                                                                                                             
  "edad",                                                                                                                                                                                    
  "genero",                                                                                                                                                                                    
  "situacion_ocupacional",                                                                                                                                                                     
  "cual_es_la_ocupacion_principal_del_jefe_de_hogar_enc_respuesta_espontanea_clasificar_en_una_de_las_siguientes_categorias",                                                                  
  "principales_traslados",                                                                                                                                                                     
  "lugar_traslado",                                                                                                                                                                            
  "temporalidad_traslado",                                                                                                                                                                     
  "medio_transporte",                                                                                                                                                                          
  "tiempo_traslado",                                                                                                                                                                           
  "a_que_sistema_de_salud_pertenece_su_familia_enc_leer_alternativas_y_responder_todas_las_que_correspondan",                                                                                  
  "cuando_usted_o_algun_familiar_necesita_atencion_de_salud_adonde_acude_principalmente_enc_leer_alternativas_y_responder_una",                                                                
  "ha_cambiado_la_calidad_de_la_atencion_en_los_ultimos_seis_meses",                                                                                                                           
  "si_nota_un_cambio_como_ha_cambiado_en_enc_leer_cada_tipo_de_cambio_y_anotar_1_si_2_no_todos_los_tipos_de_cambios_deben_quedar_con_una_respuesta_anotada",                                   
  "en_los_ultimos_seis_meses_han_ocurrido_incidentes_que_alteren_el_orden_publico_en_la_localidad_donde_vive_enc_leer_alternativas_y_responder_una",                                           
  "la_presencia_de_aes_gener_en_la_comuna_le_ha_traido_beneficios_a_usted_o_a_su_familia_enc_leer_cada_tipo_de_beneficio_todos_los_tipos_de_beneficios_deben_quedar_con_una_respuesta_anotada",
  "usted_conoce_el_mecanismo_por_el_cual_opera_la_produccion_de_electricidad_en_la_central_hidroelectrica_alto_maipo",                                                                         
  "solo_si_conoce_el_mecanismo_me_podria_indicar_cual_es_el_mecanismo_que_se_utiliza",                                                                                                         
  "desde_el_inicio_de_las_actividades_de_alto_maipo_ha_notado_diferencias_en_el_nivel_del_caudal_del_rio_enc_leer_cada_uno_de_los_ambitos_elegir_una_alternativa" ) 

#genero un vector sólo con las 3 primeras letras: p01
nuevos_nombres <- str_sub(string = cols_a_renombrar, start = 1, end = 4 ) #muestro los argumentos

nuevos_nombres <- str_sub(cols_a_renombrar, 1, 4)  #no muestro los argumentos
nuevos_nombres

#primer argumento - string = de donde saco los nombres: el vector creado
#segundo argumento - start = desde que posición extraigo (p)
#tercer argumento - end= hasta donde (1)


#renombro considerando todas las columnas elegidas asignando nuevos nombres
base_am <- base_am %>%
  rename_at(vars(cols_a_renombrar), ~ nuevos_nombres) #recodificación múltiples con un vector

#Problema! "cual" at locations 7 and 12.!

#renombro algunas variables en específico


#veo categorías de todas las variables
sapply(base_am, FUN = unique)

#posibilidad de renombrar uno por uno las variables de interés. # primero nuevo nombre y luego nombre antiguo

base_am <- base_am %>% dplyr::rename( anios_comuna = hace_cuantos_anos_vive_en_esta_comuna_enc_anotar_cantidad_de_anos_si_es_menos_de_un_ano_anotar_0,
                                propiedad_vivienda = la_vivienda_que_usted_ocupa_es_enc_leer_alternativas,
                                sistema_salud = a_que_sistema_de_salud_pertenece_su_familia_enc_leer_alternativas_y_responder_todas_las_que_correspondan,
                                pago_vivienda = cual_es_el_valor_que_paga_mensualmente,  
                                ocupacion_jefe_hogar = cual_es_la_ocupacion_principal_del_jefe_de_hogar_enc_respuesta_espontanea_clasificar_en_una_de_las_siguientes_categorias, 
                                servicio_salud = cuando_usted_o_algun_familiar_necesita_atencion_de_salud_adonde_acude_principalmente_enc_leer_alternativas_y_responder_una, 
                                calidad_atencion = ha_cambiado_la_calidad_de_la_atencion_en_los_ultimos_seis_meses, 
                                cambio_atencion = si_nota_un_cambio_como_ha_cambiado_en_enc_leer_cada_tipo_de_cambio_y_anotar_1_si_2_no_todos_los_tipos_de_cambios_deben_quedar_con_una_respuesta_anotada, 
                                ocurrencia_incidentes= en_los_ultimos_seis_meses_han_ocurrido_incidentes_que_alteren_el_orden_publico_en_la_localidad_donde_vive_enc_leer_alternativas_y_responder_una, 
                                beneficios_aes_gener = la_presencia_de_aes_gener_en_la_comuna_le_ha_traido_beneficios_a_usted_o_a_su_familia_enc_leer_cada_tipo_de_beneficio_todos_los_tipos_de_beneficios_deben_quedar_con_una_respuesta_anotada, 
                                mecanismo_produccion = usted_conoce_el_mecanismo_por_el_cual_opera_la_produccion_de_electricidad_en_la_central_hidroelectrica_alto_maipo, 
                                nombre_mecanismo = solo_si_conoce_el_mecanismo_me_podria_indicar_cual_es_el_mecanismo_que_se_utiliza, 
                                diferencia_caudal = desde_el_inicio_de_las_actividades_de_alto_maipo_ha_notado_diferencias_en_el_nivel_del_caudal_del_rio_enc_leer_cada_uno_de_los_ambitos_elegir_una_alternativa)



names(base_am)


# selección y transformación de variables ---------------------------------
DataExplorer::create_report(base_am) 

# sino funciona el DataExplorer
# install.packages("htmltools", version = "0.5.4")
# library(htmltools)
# library(DataExplorer)


#función para realizar un reporte general introductorio
#me permite ver posibles transformaciones o limpiezas



# Transformaciones/limpieza en variables categóricas

names (base_am)

#Situación Ocupacional
unique(base_am$situacion_ocupacional)

#realizaremos un conjunto de transformaciones/limpiezas de nuestra BBDD. 

# para situación ocupacional (respuesta abierta)
base_am$situacion_ocupacional <- tolower(base_am$situacion_ocupacional) #todas a minusculas
base_am$situacion_ocupacional  <- gsub(pattern = " ", replacement = "", x = base_am$situacion_ocupacional) #elimino los espacios

# base_am$situacion_ocupacional  <- gsub(" ", "", base_am$situacion_ocupacional) (forma por default)

table(base_am$situacion_ocupacional)

#Recodificar a 3 categorías 
#1. Trabajo remunerado; 2. Trabajo no remunerado; 3. No trabaja.

unique(base_am$situacion_ocupacional)

#Recodificamos con mutate y case_when

base_am <- base_am %>% mutate(situacion_ocupacional=case_when(situacion_ocupacional=="Trabajo remunerado"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajando"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajo independiente"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajando remunerado"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajador remunerado"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajaba"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Si"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Aseo"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Activo"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Garzona"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Ayudante de cocina"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajando municipla"~"Trabajo remunerado",
                                                       situacion_ocupacional=="trabaja"~"Trabajo remunerado",
                                                       situacion_ocupacional=="1"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabaja"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajo rumerado"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajo remerado"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajo renunerado"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajando desde casa"~"Trabajo remunerado",
                                                       situacion_ocupacional=="trabajo remunerado"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Trabajo renminerado"~"Trabajo remunerado",
                                                       situacion_ocupacional=="Dueña de casa"~"Trabajo no remunerado",
                                                       situacion_ocupacional=="Trabajo remunerdo"~"Trabajo no remunerado",
                                                       situacion_ocupacional=="2"~"Trabajo no remunerado",
                                                       situacion_ocupacional=="Trabajo no remunerado"~"Trabajo no remunerado",
                                                       situacion_ocupacional=="2?"~"Trabajo no remunerado",
                                                       situacion_ocupacional=="Cesante"~"No trabaja",
                                                       situacion_ocupacional=="No trabajaba"~"No trabaja",
                                                       situacion_ocupacional=="No"~"No trabaja",
                                                       situacion_ocupacional=="Reservas"~"No trabaja",
                                                       situacion_ocupacional=="Estudiando"~"No trabaja",
                                                       situacion_ocupacional=="No trabaja"~"No trabaja",
                                                       situacion_ocupacional=="no trabaja"~"No trabaja",
                                                       situacion_ocupacional=="Licencia por cirugía"~"No trabaja",
                                                       situacion_ocupacional=="3"~"No trabaja",
                                                       situacion_ocupacional=="Estudiante"~"No trabaja",
                                                       situacion_ocupacional=="Jubilada"~"No trabaja",
                                                       situacion_ocupacional=="No trajaba"~"No trabaja",
                                                       situacion_ocupacional=="3 jubilado"~"No trabaja",
                                                       situacion_ocupacional=="3 jubilada"~"No trabaja",
                                                       TRUE~situacion_ocupacional))