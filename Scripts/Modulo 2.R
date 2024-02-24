
###############################################
###############################################
## Módulo II: Análisis exploratorio de datos ##
###############################################
###############################################
library(readxl)
library(tidyverse)

#----------------------#
# Cargar base de datos #-------------------------------------------------------------------------------------------------------------------------
#----------------------#
setwd("C:/Users/PC/Desktop/Nuevo_Curso_2024I")
dataset <- readxl::read_excel("Input/attend.xlsx")

#--------------------------------#
# Análisis exploratorio de datos #-------------------------------------------------------------------------------------------------------------------------
#--------------------------------#

# El EDA es un proceso cuyo objetivo es desarrollar un entendimiento más amplia sobre las estructuras de datos
# En general, el EDA es un proceso de transformación, visualización de datos y modelación
# Las dos preguntas generales que orientan el proceso del EDA son las siguientes:
# 1. ¿Cuál es el patrón de valores en una variable? (variación)
# 2. ¿Cómo cambian conjuntamente los valores de dos o más variables? (covariación)

#-----------#
# Variación #-------------------------------------------------------------------------------------------------------------------------
#-----------#

# Por variación, entendemos el interés por estudiar el comportamiento de los valores de una variable
# (Análisis univariado)

#-----------------------#
# Variables categóricas #
#-----------------------#
# Para examinar la distribución de una variable categórica, nos interesa su frecuencia
new_dataset <- dataset %>% mutate(Int_attend = cut(attend,
                                                   breaks = seq(0,50, by = 10),
                                                   right = F,
                                                   labels = c("Group 1",
                                                              "Group 2",
                                                              "Group 3",
                                                              "Group 4",
                                                              "Group 5"))) # Asignar etiquetas

ggplot(data = new_dataset) +
  geom_bar(mapping = aes(x = Int_attend), 
           color = "white", fill = "lightskyblue") + theme_bw()

# La misma información se puede obtener con la función dplyr::count()
new_dataset %>% dplyr::count(Int_attend)

#-----------------------#
# Variables continuas   #
#-----------------------#

# Para examinar una variable continua, usamos histogramas
ggplot(data = new_dataset) +
  geom_histogram(mapping = aes(x = attend),
                 binwidth = 0.8)

# Manualmente, los datos del histograma se calculan con cut_width
new_dataset %>% count(cut_width(attend, 0.8))

# Si el interés es graficar múltiples histogramas de acuerdo con 
# una variable categórica, deberíamos usar geom_freqpoly()
new_dataset$soph = as.factor(new_dataset$soph)
ggplot(data = new_dataset, aes(x = attend, colour = soph)) +
  geom_freqpoly(binwidth = 1) + theme_bw()

# Además de adoptar un tema, podemos modificar el tema de la gráfica
new_dataset$soph = as.factor(new_dataset$soph)
ggplot(data = new_dataset, aes(x = attend, colour = soph)) +
  geom_freqpoly(binwidth = 1) + theme_bw() +
  theme(legend.position = c(0.24, 0.8), # cambiar posición
        legend.text = element_text(colour = "black",
                                   size = 10), # cambiar fuente y tamaño del texto
        legend.title  = element_text(colour = "black",
                                   size = 10,
                                   face = "bold"), # cambiar fuente y tamaño del título
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black" # Cambiar fondo
                                         )) +
          scale_colour_manual(name = "Sophomore", labels = c("Non-sophomore", "Sophomore"),
                             values = c("gray50", "blue") # Cambiar leyenda
                             ) +
  labs(title = "Classes attended out of 32 (sophomore vs. non-sophomore)",
       x = "Classes attended out of 32",  
       y = "Count" # Cambiar etiquetas
       )


# Detección de valores atípicos (outliers)
# Los valores atípicos pueden, o bien ser errores en el ingreso de la información, o bien
# puede proporcionar información útil

# Un histograma puede dar algunas luces
ggplot(data = new_dataset) +
  geom_histogram(mapping = aes(attend),
                 binwidth = 0.8)

# El eje x sugiere la existencia de valores atípicos, pero
# podemos fijarnos en las coordenadas específicas
ggplot(data = new_dataset) +
  geom_histogram(mapping = aes(attend),
                 binwidth = 0.8) + coord_cartesian(ylim = c(0,5))

# El diagrama de caja sirve para identificar outliers
ggplot(data = new_dataset, aes(y = attend)) +
  geom_boxplot(fill = "cyan") + theme_bw()

# Criterio IQR para la detección de outliers
# Cualquier observación fuera de [q0.25 - 1.5IQR, q0.75 + 1.5IQR]
# La siguiente función permite la detección de outliers
boxplot.stats(new_dataset$attend)$out

# Lo mismo puede ser obtenido manualmente
Q1 <- quantile(new_dataset$attend, .25)
Q3 <- quantile(new_dataset$attend, .75)
IQR <- IQR(new_dataset$attend)

outliers <- new_dataset %>% filter(attend<(Q1 - 1.5*IQR) | attend>(Q3 + 1.5*IQR))

# Comparación
outliers$attend
boxplot.stats(new_dataset$attend)$out



##########################################
##########################################
## Nota pendiente: pruebas estadísticas ##
##########################################
##########################################






