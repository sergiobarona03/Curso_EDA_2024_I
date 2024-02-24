
################################################
################################################
## Módulo III: Análisis exploratorio de datos ##
################################################
################################################
library(readxl)
library(tidyverse)

#----------------------#
# Cargar base de datos #-------------------------------------------------------------------------------------------------------------------------
#----------------------#
setwd("C:/Users/PC/Desktop/Nuevo_Curso_2024I")
dataset <- readxl::read_excel("Input/attend.xlsx")

#----------------#
# Covariación    #-------------------------------------------------------------------------------------------------------------------------
#----------------#

# La variación describe el comportamiento de los valores de una variable.
# La covariación describe el comportamiento de los valores de dos o más variables.

#-------------------------------------#
# Variables continuas y categóricas   #-------------------------------------------------------------------------------------------------------------------------
#-------------------------------------#

# Obtener la distribución de la variable continua diferenciada según la variable categórica
dataset$soph = as.factor(dataset$soph)
ggplot(data = dataset, aes(x = attend, colour = soph)) +
  geom_freqpoly(binwidth = 1) + theme_bw() +
  theme(legend.position = c(0.24, 0.8), 
        legend.text = element_text(colour = "black",size = 10), 
        legend.title  = element_text(colour = "black", size = 10, face = "bold"), 
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black" )) +
  scale_colour_manual(name = "Sophomore", labels = c("Non-sophomore", "Sophomore"),
                      values = c("gray50", "blue")) + labs(
                        title = "Classes attended out of 32 (sophomore vs. non-sophomore)",
       x = "Classes attended out of 32",  
       y = "Count")

# Cuando las clases no están balanceadas, una mejor aproximación es obtenida
# después de normalizar tal que el AUC es igual a 1
ggplot(data = dataset, aes(x = attend, y = ..density..)) +
  geom_freqpoly(aes(colour = soph), binwidth = 1)

# Una aproximación también útil es derivada del diagrama de caja
ggplot(data = dataset) +
  geom_boxplot(mapping = aes(x = reorder(soph,
                                         attend, FUN = median),
                             y = attend)) + coord_flip()
# El gráfico anterior implica la rotación del eje y la ordenación 
# según la mediana

#-----------------------------#
# Dos variables categóricas   #-------------------------------------------------------------------------------------------------------------------------
#-----------------------------#
# Para visualizar la covariación entre dos variables categóricas:
dataset$frosh = as.factor(dataset$frosh)
dataset$soph = as.factor(dataset$soph)
ggplot(data = dataset) +
  geom_count(mapping = aes(x = frosh,
                           y = soph))

# Otra opción es la siguiente:
dataset %>% count(frosh, soph) %>%
  ggplot(mapping = aes(x = frosh,
                       y= soph)) +
  geom_tile(mapping = aes(fill = n))

#-----------------------------#
# Dos variables continuas     #-------------------------------------------------------------------------------------------------------------------------
#-----------------------------#
# La aproximación básica es un scatter plot:
ggplot(data = dataset) +
  geom_point(mapping = aes(x = termGPA,
                           y = priGPA))

# El parámetro de transparencia (alpha) puede ser usado para 
# el caso de grandes bases de datos 
ggplot(data = dataset) + 
  geom_point(mapping = aes(x = termGPA,
                           y = priGPA),
             alpha = 0.2)

# Una forma de agrupar los datos es la siguiente:
# (PENSAR SI TERMINA SIENDO AGREGADO)
ggplot(data = dataset) + 
  geom_bin2d(mapping = aes(x = termGPA,
                           y = priGPA))

# El mismo resultado se puede alcanzar con el paquete Hexbin
install.packages("hexbin")
library(hexbin)
ggplot(data = dataset,
       mapping = aes(x = termGPA,
                     y = priGPA)) +
  geom_boxplot(aes(group = cut_width(termGPA, 
                                     0.9)))



##########################################
##########################################
## Nota pendiente: pruebas estadísticas ##
##########################################
##########################################




