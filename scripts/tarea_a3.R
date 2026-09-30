#scripts/tarea_a3.R
# A3 — Limpieza de datos reales del INE
### Módulo II · Semana 6 · **Actividad formativa — sin nota, entrega obligatoria**
#### Fundamentos de Programación para Análisis Económico · UdeC-EAN

#Autor: Tomas Troncoso
#Fecha: 2026-09-29
#Qué hace: Este script fue desarrollado para realizar la tarea A3 del curso de Fundamentos de Programacin

# Datos: `data/raw/ene-2026-06-mjj.csv` — Encuesta Nacional de Empleo (INE),
# trimestre móvil mayo–julio 2026. **97.946 filas y 222 columnas**, separador `;`
# y decimales con coma. Es el archivo con que Chile calcula su tasa de desocupación.
# Manual de variables: `data/raw/codigos-ene-2020.pdf`. **Lo vas a necesitar.**


library(dplyr)

### 1. Declara tu pregunta y documenta la fuente

#Y la ficha de la fuente, como en el laboratorio: institución, encuesta, trimestre,
# **enlace de descarga, fecha de descarga y unidad de observación**.

#Pregunta a respnder: 
# ¿Existe una diferencia en las horas efectivas de trabajo entre hombres y mujeres ocupados?

# TODO: completa la ficha (búscala en el sitio del INE).
#
# FUENTE:            Instituto Nacional de Estadísticas (INE), Chile
# ENCUESTA:          Encuesta Nacional de Empleo (ENE)
# TRIMESTRE MÓVIL:   mayo - junio - julio 2026
# ARCHIVO:           ene-2026-06-mjj.csv
# ENLACE:            https://www.ine.gob.cl/estadisticas-por-tema/mercado-laboral/ocupacion-y-desocupacion
# FECHA DE DESCARGA: 29 de septiembre de 2026
# UNIDAD DE OBSERVACIÓN: Personas


### 2. Abre el archivo y reduce las 222 columnas

# Justifica en un comentario **por qué** `read.csv()` no sirve aquí.

intento <- read.csv("data/raw/ene-2026-06-mjj.csv", nrows = 5)
ncol(intento)

# "read.csv()" asume que las columnas se separan con comas (,) y los decimales usan punto (.). 
# El archivo del INE en este caso, separa las columnas con punto y coma (;) y los decimales con coma (,)
# Por lo que "read.csv()" no sirve y entrega todo como una sola cadena.

data_trim <- read.csv2("data/raw/ene-2026-06-mjj.csv")
dim(trim)

# Por otro lado, "read.csv2()" reconoce el (;) para separar correctamente las 222 columnas y 
# entiende la coma decimal (,) para que variables numéricas como entren a R como números y no como texto.

## Quédate solo con las variables que tu pregunta necesita, usando al menos un
## **selector por patrón** (`starts_with()`, `where()`) además de los nombres sueltos.

# Recuerda: `activ` es la columna 221. Las columnas `b1`, `b2`… son *preguntas del
# cuestionario*, no indicadores. Si dudas de qué es una variable, búscala en el
# manual antes de usarla.

### 3. Recodifica los códigos a algo legible

# Al menos **dos** variables categóricas, con `case_when()`, pasando de números a
# etiquetas con significado. Cierra con `TRUE ~` y verifica con
# `table(x, useNA = "ifany")` que no quedó ningún `NA` inesperado.
# Los códigos están todos en el manual. Copiarlos mal es el error más común.

names(data_trim)

names(select(data_trim, starts_with("efectiva"))) # [1] "efectivas" -> esta es la que se utilizara para las horas efectivas
names(select(data_trim, starts_with("habitual"))) # [1] "habituales" -> si se desea comparar.
names(select(data_trim, starts_with("fact"))) #[1] "fact_cal"

empleo_efectivo <- 
  data_trim |> 
  select( region, # Va desde la 1 hasta la 16 (enumerada por codigo)
          sexo, # 1: Hombre y 2: Mujer
          edad, # va desde 0 a 120
          activ, # 1: Ocupado/a. 2: Desocupado/a y 3: Fuera de la fuerza de trabajo
          efectivas, # de 1 a 168, 888: No sabe y 999: No responde.
          fact_cal) #Factor de expansión trimestral calibrado. 


empleo_efectivo <-
  empleo_efectivo |>
  mutate(
    sexo_txt = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE      ~ "Sin dato"                
    ),
    situacion = case_when(
      activ == 1 ~ "Ocupado",
      activ == 2 ~ "Desocupado",
      activ == 3 ~ "Fuera de la F.Trabajo",
      TRUE       ~ "Fuera de edad de trabajar"
    )
  )

# Verificacion
table(empleo_efectivo$sexo_txt, useNA = "ifany")
table(empleo_efectivo$situacion, useNA = "ifany")

# No se observan NAs inesperados, el conteo de las filas por cada variable es aparentemente correcto.

### 4. Clasifica los faltantes: estructural o real

# Para **cada** variable que conserves, reporta cuántos `NA` tiene y responde en un
# comentario: *¿se perdió el dato, o nunca existió?*
  
# Demuestra que entendiste la diferencia con **una** de estas dos vías:
  
#  - Corre `na.omit()` sobre tu subconjunto y muestra **a quién borró**; explica por
# qué eso sesgaría tu respuesta.
# - O imputa una variable con la mediana y muestra **a quién le inventaste un dato**;
# explica por qué no corresponde.

# > No basta con decir «hay 56.096 NA». Hay que decir de quiénes son y qué implica.


colSums(is.na(empleo_efectivo)) # se realiza un comteo entre las variables que componen a "empleo_efectivo" y se ven si hay NAs

# Solo se detectaron NAs en activ y efectivas.

sin_NAs <- na.omit(empleo_efectivo)

table(sin_NAs$situacion) # un total personas en situacion "Ocupado" == 41850



### 5. Caza los centinelas — en una variable nueva

# En clase limpiamos `habituales` (el `999` visible y el `888` escondido). **Esa ya
# no cuenta.** Elige otra variable de tu subconjunto y audítala:
  
#  ```r
# sort(table(tu_variable), decreasing = TRUE)   # mira TODA la distribución
# ```

# Busca valores imposibles o sospechosamente redondos: `88`, `99`, `888`, `999`,
# `9999`, `-9`. Confirma en el manual qué significan antes de tocarlos, conviértelos
# con `na_if()` y **cuantifica el efecto**: ¿cuánto cambió tu estadístico?
  
#   > Pista honesta: hay más de una variable con centinelas en esta base, y no todas
# > los esconden en el máximo.
      

sort(table(empleo_efectivo$efectivas[empleo_efectivo$efectivas > 80]), decreasing = TRUE) 
sort(table(empleo_efectivo$efectivas[empleo_efectivo$efectivas < 0]), decreasing = TRUE) 

# los valores sumamnete altos como 888 se repiten 103 veces y 999 se repiten 8 veces, 0 valores por debajo de 0.


horas_sucias <- empleo_efectivo$efectivas[empleo_efectivo$situacion == "Ocupado"] # extrae las horas efectivas de trabajo exclusivamente de las personas ocupadas
promedio_antes <- mean(horas_sucias, na.rm = TRUE) # [1] 37.73532 Promedio de horas sin limpiar la base


empleo_efectivo <- 
  empleo_efectivo |>
  mutate(efectivas_limpia = na_if(efectivas, 999), 
         efectivas_limpia = na_if(efectivas_limpia, 888)) # se limpia la base de los valores 999 y 888, omitiendolos, no eliminandolos

horas_limpias <- empleo_efectivo$efectivas_limpia[empleo_efectivo$situacion == "Ocupado"]
promedio_despues <- mean(horas_limpias, na.rm = TRUE) #[1] 35.45286 Promedio de horas limpienado la base 

c(Antes = promedio_antes, Despues = promedio_despues) 

Brecha <- (promedio_antes - promedio_despues) #[1] 2.282455


### 6. Pondera

# Toda cifra que reportes debe usar `fact_cal`. Incluye **una comparación explícita**
# entre tu resultado ponderado y el mismo cálculo sin ponderar, y comenta qué cambió:
#  ¿la tasa, el nivel, o ambos?
  

comparacion <- 
  empleo_efectivo |> 
  filter(situacion == "Ocupado") |>
  group_by(sexo_txt) |>
  summarise(nivel_sin_ponderar = n(),                             
            nivel_ponderado = sum(fact_cal, na.rm = TRUE),
            horas_sin_ponderar = mean(efectivas_limpia, na.rm = TRUE),
            horas_ponderadas = weighted.mean(efectivas_limpia, w = fact_cal, na.rm = TRUE)
  )

# El nivel sin ponderar muestra mostrará unas cuantas miles de filas (la muestra), 
# mientras que el nivel ponderado saltará a millones de personas, representando a la verdadera fuerza de trabajo chilena. 
# Sin fact_cal tu resultado pierde representatividad nacional y no es comparable con las cifras oficiales del INE.


### 7. Responde tu pregunta

#  **5 a 8 líneas**, con los números que obtuviste. Tres exigencias:
  
#  - Cifras **concretas** (puntos porcentuales, horas, personas), no «es más alto».
# - Di «**se asocia**», nunca «causa».
# - Menciona **una limitación** de estos datos para tu pregunta.


# Al analizar el mercado laboral, se observa una clara brecha de género: los hombres 
# ocupados registran un promedio ponderado de [38.58] horas efectivas de trabajo a la 
# semana, mientras que las mujeres alcanzan las [33.56] horas. Esta diferencia de [5.02] 
# horas confirma que ser hombre se asocia a jornadas laborales remuneradas más extensas, 
# en un mercado que representa a más de [9314017] ocupados totales. Sin embargo, una 
# limitación de estos datos es que las horas "efectivas" son autorreportadas, lo que 
# las sujeta a sesgos de memoria o sobreestimación por parte del encuestado, y la 
# variable no mide la carga de trabajo doméstico no remunerado que restringe el tiempo.


### 8. Guarda y deja la bitácora

 ```r
dir.create("data/processed", showWarnings = FALSE)
write.csv(empleo_efectivo, "data/processed/ene_a3.csv", row.names = FALSE)

```

# Cierra con una bitácora de limpieza: lectura, columnas conservadas, recodificaciones,
# faltantes (cuáles y qué hiciste), centinelas encontrados y su efecto, ponderación,
# y filas iniciales contra finales.

  
  
