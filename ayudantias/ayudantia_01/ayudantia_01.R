# Script de R - Ejercicios No Resueltos.

# I. Preguntas básicas iniciales ----------------------------------------------
# a) Describe en tus propias palabras la diferencia entre R como lenguaje de programación y RStudio.
# b) Enumera las pestañas principales de RStudio y describe brevemente su función.
# c) Explica qué elementos tiene una función en R y cómo se estructura.

# II. Uso de R como calculadora -------------------------------------------------
# Realiza las siguientes operaciones matemáticas:
# 1. Suma 150 y 350.
# 2. Resta 1000 de 500.
# 3. Multiplica 25 por 43.
# 4. Divide 1000 por 250.

# III. Concatenación de Objetos ------------------------------------------------
# Realiza y guarda los resultados en vectores:
# 1. Concatena los números 10, 20, y 30 en un vector llamado 'vector1'.
# 2. Concatena los números 5, 15, y 25 en un vector llamado 'vector2'.
# 3. Suma 5 a cada elemento de 'vector1' y guarda el resultado en 'resultado1'.

# IV. Operadores Lógicos -----------------------------------------------------
# Evalúa las siguientes expresiones lógicas y predice el resultado:
# 1. ¿Es 50 igual a 50?
# 2. ¿Es 100 diferente de 200?
# 3. ¿Es 30 mayor que 25 y menor que 50 al mismo tiempo?

# V. Lenguaje orientado a objetos --------------------------------------------
# Asigna valores y realiza operaciones, guardando los resultados:
# 1. Asigna el número 100 a un objeto llamado 'numero1' y 200 a 'numero2'.
# 2. Suma 'numero1' y 'numero2', y guarda el resultado en 'suma_total'.
# 3. Multiplica 'numero1' por 2 y guarda el resultado en 'doble_numero1'.

# VI. Funciones básicas [seq, rep] ---------------------------------------------
# 1. Crea una secuencia del 1 al 20 con intervalo de 2 y guárdala en 'secuencia1'.
# 2. Repite el número 5, diez veces y guarda el resultado en 'repeticion1'.
# 3. Crea una secuencia del 10 al 50 con un intervalo de 5 y guárdala en 'secuencia2'.

# VII. Indexación y data.frame -------------------------------------------------
# Con los vectores dados, crea un data.frame llamado 'estudiantes_antropologia' y realiza las tareas solicitadas:
# Edad, Sexo, Tendencia Política, Ingreso Familiar, Comuna de Residencia

# Utiliza los siguientes vectores para crear un data.frame llamado 'estudiantes_antropologia':
edad <- c(22, 18, 25, 20, 21, 19, 23, 24, 26, 22, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36)
sexo <- c("masculino", "femenino", "no-binario", "femenino", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario", "femenino", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario")
tendencia_politica <- c("izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha")
ingreso_familiar <- c(1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000, 8500, 9000, 9500, 10000, 10500)
comuna_residencia <- c("Providencia", "Las Condes", "Ñuñoa", "Santiago", "Vitacura", "La Reina", "Peñalolén", "Macul", "La Florida", "Puente Alto", "Maipú", "Pudahuel", "Cerrillos", "Quilicura", "Recoleta", "Independencia", "Conchalí", "Renca", "Cerro Navia", "Lo Prado")


# 1. Calcula la edad promedio de los estudiantes.
# 2. Calcula la desviación estándar de la edad de los estudiantes.
# 3. Muestra la información del décimo estudiante.
# 4. Muestra todas las comunas de residencia de los estudiantes.
