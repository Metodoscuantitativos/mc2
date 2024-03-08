# Evolución de graduados Ed. Superior -------------------------------------

pregrado <- c(140, 70, 20, 120, 240)
postgrado <- c(24, 50, 12, 40, 80)

pregrado
postgrado

anios <- c("2017", "2018", "2019", "2020", "2021")
anios

names(pregrado) <- anios
pregrado

names(postgrado) <- anios

pregrado
postgrado

# Comparar la evolución de graduados por nivel de formación.

pregrado > postgrado
pregrado == postgrado

# ¿Qué nivel obtuvo menos bajas de graduados?

sum(pregrado)
sum(postgrado)

# ¿Qué año hubo más y menos graduados por nivel?

which.min(pregrado)
which.min(postgrado)

which.max(pregrado)
which.max(postgrado)

postgrado[5]
postgrado[which.max(pregrado)]

# Calcular total de graduados

sum(pregrado, postgrado)
total_matriculas <- sum(pregrado) + sum(postgrado)
total_matriculas

# Comparar deuda gobierno central -----------------------------------------

deuda_interna <- c(70417.1, 58575.6, 55703.2, 56128.7, 43284.4)
deuda_externa <- c(21208.1, 15815.6, 14544.3, 12807.5, 10080.8)

# Crear vector que contenga cifras de deuda interna y externa.

deuda_interna
deuda_externa

# Crear vector numérico llamado año, que considere desde el año 2016 al 2020.

anio <- seq(from = 2016, to = 2020, by = 1)

# Asignar los años a la deuda interna y deuda externa.

names(deuda_interna) <- anio
deuda_interna

names(deuda_externa) <- anio
deuda_externa

# Calcule el total de deuda interna por todos los años.
sum(deuda_interna)

# Identificar años en los que deuda externa es mayor a 16000.

deuda_externa > 16000

# Determinar si la deuda interna es mayor o igual que la deuda externa.

sum(deuda_interna) >= sum(deuda_externa)

# Determinar en qué año se encuentra la mayor deuda interna y externa.
which.max(deuda_interna)
deuda_interna[which.max(deuda_interna)]

# Calcular tasa de crecimiento de la deuda interna para el año 2020.

deuda_interna
# ((V2 - V1) / V1) * 100
# (valor presente - valor pasado) / valor pasado

(deuda_interna[5] - deuda_interna[4]) / deuda_interna[4] * 100

V2 <- deuda_interna[5]
V1 <- deuda_interna[4]

((V2 - V1) / V1) * 100
