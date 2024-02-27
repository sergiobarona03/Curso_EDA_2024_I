
############################
############################
## Módulo I: básicos de R ##
############################
############################


#---------------------------------#
# PRIMERA PARTE: INTRODUCCIÓN A R #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------#

#----------------------#
# A. Elementos básicos #
#----------------------#

# Los objetos son nombres para referirnos 
# a datos almacenados
x <- 1:6

# Sensible a mayúsculas
A1 <- 1
a1 <- 2
A1 + 1

# Vectores
text <- c("x", "y", "z") 
text[1] # seleccionar elementos del vector
length(text) # longitud del vector
names(text) <- c("uno", "dos", "tres") # asignar nombres

# Factores
gender <- factor(c("male", "female", "female", "female"))
gender.recod <- factor(gender,
                      levels = c("male", "female"),
                      labels = c("M", "F")) # Recodificación

# Matrices
m1 <- matrix(1:9,
             nrow = 3, ncol = 3, byrow = F) # Crear matriz
m1[2,1] #seleccionar entrada (2,1)
m1[2,] # seleccionar fila 2
m1[,1] # seleccionar columna 1

# Creación de listas
v1 = c(1, 2, 3, 4)
v2 = c("a", "b", "c", "d", "e")
m1 = matrix(v1, 
            nrow = 2, ncol = 2, byrow = T)

lista <- list(vector1 = v1, vector2 = v2, matriz1 = m1)
lista

# Sintaxis para listas
lista$vector2
lista[[2]] # segundo objeto de la lista
lista[[2]][1] # primer elemento del segundo objeto de la lista

# Crear data frame
nombre = c("x1" ,"x2", "x3", "x4")
sexo = factor(c("male", "male", "female", "female"))
edad = c(15, 26, 43, 56)

df = data.frame(nombre, sexo, edad)
df

# Sintaxis del data frame
df[1,2] # seleccionar entrada (1,2)
df[1,c(1,2)] # seleccionar fila 1 y columnas 1 y 2
df[1,] # seleccionar fila 1
df[,1] # seleccionar columna 1
df$edad # seleccionar variable "edad"
df$edad[1] # seleccionar elemento 1 de la variable "edad"
df[-c(1,2),] # eliminar filas 1 y 2
df[,-c(1)] # eliminar columna 1
df[c("sexo", "edad")] # seleccionar variables "sexo" y "edad"

#------------------------------#
# B. Condicionales y funciones #
#------------------------------#

# Definición del condicional
x <- 10
if (x > 5) {
  print("x es mayor que 5")
}

x <- 2
if (x > 5) {
  print("x es mayor que 5")
} else {
  print("x es menor o igual que 5")
}

# Definición de una función 
f_z = function(x){
  z <- (x - mean(x))/sd(x)
  return(z)
}

x <- 1:20
z <- f_z(x)
mean(z)
sd(z)

# Definición de la función a partir de un condicional
par_impar <- function(x){
  if(x %% 2 == 0){
    return("El número es par")
  } else {
    return("El número es impar")
  }
}

par_impar(3)

#--------------#
# Ejercicios   #
#--------------#
# 1. Crear tres vectores (una cadena y dos numéricos),
# crear una matriz y convertir en data frame. Seleccione el elemento
# en la segunda fila y la tercera columna.

# Solución:
x <- c("a", "b", "c", "d", "e")
y <- c(1:5)
z <- c(10:14)

matrix1 <- matrix(c(x,y,z), ncol = 3, nrow = 5)
df1 <- data.frame(matrix1)
df1[2,3]

# 2. Crear una función que identifique si el vector es numérico; y, si
# es así, calcule la media, mediana, percentil 25 y 75 (use una lista).

# Solución:
f <- function(v){
  if (is.numeric(v)) {
    mean <- mean(v)
    median <- median(v)
    q1 <- quantile(v, 0.25)
    q3 <- quantile(v, 0.75)
    y <- list(mean = mean, 
              median = median, 
              Q1 = q1, 
              Q3 = q3)
    return(y)
  } else {
    "El vector no es numérico"
  }
}

#-----------------------------------------#
# SEGUNDA PARTE: INTRODUCCIÓN A TIDYVERSE #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------#

#--------------------#
# Nota: Librerías    #
#--------------------#
help("tidyverse")
?read_excel

#--------------------------#
# C. General: tidyverse    #
#--------------------------#
library(wooldridge)
writexl::write_xlsx(attend,"C:/Users/Portatil/Desktop/Quant_Macro_Model-main/attend.xlsx")
library(tidyverse)
library(readxl)
# Tidyverse es una colección de paquetes disponibles en R
# para la manipulación, importación, exploración y 
# visualización de datos (dplyr, readr, ggplot2, purrr, etc.)

# IMPORTACIÓN DE DATOS
dataset <- readxl::read_excel("C:/Users/Portatil/Desktop/Quant_Macro_Model-main/attend.xlsx")
dataset <- readxl::read_excel(file.choose())

# Crear id en la base de datos
dataset$id <- seq(1, nrow(dataset), by = 1)

# Noten que lo mismo se puede hacer a partir de ventanas

# PIPELINE

# Pipeline (%>%) o tuberías de comandos es una herramienta de "dplyr"
# para el encadenamiento de funciones

# Forma estándar:
dataset_2 <- filter(dataset, attend > 15) 

# Forma encadenada:
dataset_2 <- dataset %>% filter(attend > 15)

# En el caso anterior no es muy útil. Véase el siguiente ejemplo:
# Forma estándar:
dataset_3 <- select(dataset, attend, termGPA, priGPA) # seleccionar variables
dataset_3 <- filter(dataset_3, attend > 15 & attend < 28) # filtrar para (15, 28)
dataset_3 <- arrange(dataset_3, desc(termGPA))

# Forma encadenada
dataset_4 <- dataset %>% select(attend, termGPA, priGPA) %>%
  filter(attend > 15 & attend < 28) %>% arrange(desc(termGPA))

#---------------------#
# D. Funciones: dplyr #
#---------------------#

# Arrange(): reordenar los datos con base en columnas
# Arrange(dataframe, variables)
new_dataset <- dataset %>% arrange(attend, desc(ACT)) #notar filas 4 y 5
new_dataset <- dataset %>% arrange(attend, ACT) #notar filas 4 y 5

# Mutate(): modifica o agrega nuevas variables
new_dataset <- dataset %>% mutate(Int_attend = cut(attend,
                                                   breaks = seq(0,50, by = 10),
                                                   right = F))
new_dataset <- dataset %>% mutate(Int_attend = cut(attend,
                                                   breaks = seq(0,50, by = 10),
                                                   right = F,
                                                   labels = c("Group 1",
                                                              "Group 2",
                                                              "Group 3",
                                                              "Group 4",
                                                              "Group 5"))) # Asignar etiquetas
View(new_dataset)

# Count(): contar categorías en una tabla
# Count(dataframe, variable, sort, name)
new_table <- new_dataset %>% count(Int_attend, sort =T, name = "n")

# Filter(): subconjunto de datos definido por una condición lógica
newest_dataset <- new_dataset %>% filter(Int_attend == "Group 3")

# Group_by(): subconjunto de datos según categorías. (VER FIGURA)
# (Generalmente se emplea con summarize() para descriptivas diferenciadas)
newest_dataset <- new_dataset %>% group_by(Int_attend) %>% summarize(MeanAttend = 
                                                                       mean(attend),
                                                                     SdAttend = sd(attend),
                                                                     Q1Attend = quantile(attend,0.25),
                                                                     Q2Attend = quantile(attend, 0.5),
                                                                     Q3Attend = quantile(attend, 0.75))

# Rename(): renombrar columnas (también rename_with())
newest_dataset <- new_dataset %>% rename(n.attend = attend,
                                         TGPA = termGPA,
                                         PGPA= priGPA) #renombrar
newest_dataset <- new_dataset %>% rename_with(tolower) # Todo a minúsculas
newest_dataset <- new_dataset %>% rename_with(toupper) # Todo a mayúsculas

# Select(): seleccionar de un subconjunto de variables (VER FIGURA)
newest_dataset <- new_dataset %>% select(id, attend, termGPA)

# Summarize(): resumen descriptivo general y diferenciado (VER FIGURA)
table_1 <- new_dataset %>% filter(Int_attend == "Group 4") %>% 
  summarize(MeanAttend = mean(attend),
            SdAttend = sd(attend)) # Resumen descriptivo con filtro

table_2 <- new_dataset %>% group_by(Int_attend) %>% 
  summarize(MeanAttend = mean(attend),
            SdAttend = sd(attend)) # Resumen descriptivo diferenciado


#----------------------#
# E. Sintaxis: ggplot2 #
#----------------------#

# Ggplot2 es una librería para la visualización de datos
# Sintaxis general: ggplot(dataset, estética) + geometría + 
# opciones + faceta (ver diapositiva)

ggplot2::ggplot(new_dataset, aes(x = termGPA,
                             y = priGPA, 
                             color = Int_attend))+
  geom_point() + labs(
    title = "GPA for term and cumulative GPA prior to term by number of classes attended out of 32",
    caption = "Data collected by professors Fisher and Liedholm at Michigan State University",
    x = "GPA for term",
    y = "Cumulative GPA prior to term"
    
  )+ scale_color_discrete(name = "Attend intervals")

# Gráfico en ggplot2 con facetas
ggplot2::ggplot(new_dataset, aes(x = termGPA,
                                y = priGPA,
                                color = Int_attend))+
  geom_point() + facet_wrap(~Int_attend, scale = "free_y")+ labs(
    title = "GPA for term and cumulative GPA prior to term by number of classes attended out of 32",
    caption = "Data collected by professors Fisher and Liedholm at Michigan State University",
    x = "GPA for term",
    y = "Cumulative GPA prior to term"
    
  ) + scale_color_discrete(name = "Attend intervals")


#---------------------------------#
# Ejercicios: funciones Tidyverse #
#---------------------------------#

# 1. Filtrar la base de datos de ocupados para los siguientes grupos:
# Hombres de entre 14 - 25 años en la ciudad de Bogotá.
# Mostrar el gráfico de barras según rango de ingresos




# 2. Filtrar la base de datos de ocupados para los siguientes grupos:
# Mujeres con ingresos de entre 2.000.000 y 5.000.000 de pesos
# Gráfico de barras sobre el nivel de educación asociado





#-----------------------------------------------------------------------#
# Anexo*: funciones útiles para el tratamiento de variables categóricas #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------#

#--------------------------------------------------------------#
# F. Funciones: forcats (tratamiento de variables categóricas) #
#--------------------------------------------------------------#

# Declarar variable como factor
new_dataset$Int_attend = factor(new_dataset$Int_attend,
                                levels = c("Group 1",
                                           "Group 2",
                                           "Group 3",
                                           "Group 4",
                                           "Group 5"),
                                labels = c("Group A",
                                           "Group B",
                                           "Group C",
                                           "Group D",
                                           "Group E"))

# fct_relevel()
# Reordenar manualmente los niveles de un factor
fct_relevel(new_dataset$Int_attend, c("Group A", "Group B"),
            after = 3)


# fct_infreq()
# Reordenar los valores de acuerdo con su frecuencia
fct_infreq(new_dataset$Int_attend)

# fct_shift()
# Reordenar moviendo los niveles de izquierda a derecha
fct_shift(new_dataset$Int_attend)

# fct_recode()
# Recodificar manualmente los niveles de las variables categóricas
fct_recode(new_dataset$Int_attend,
           `Group alpha` = "Group A")

# fct_collapse()
# Agrupar distintos niveles en un grupo específico
fct_collapse(new_dataset$Int_attend,
            `Group beta` = c("Group B", "Group C"))

# fct_lump_prop()
# Agrupar niveles en categoría "Otros" de acuerdo con una proporción
fct_lump_prop(new_dataset$Int_attend, 
              prop = 0.10, other_level = "Other") 

fct_lump_prop(new_dataset$Int_attend, 
              prop = 0.11, other_level = "Other") 

# fct_reorder()
# Reordenar factor variable ordenando según otra variable
ggplot2::ggplot(new_dataset, aes(x = Int_attend,
                                 y = attend, fill = Int_attend)) + geom_boxplot() # Gráfico no ordenado

ggplot2::ggplot(new_dataset, aes(x = fct_reorder(Int_attend, attend),
                y = attend, fill = Int_attend)) + geom_boxplot() # Gráfico ordenado

