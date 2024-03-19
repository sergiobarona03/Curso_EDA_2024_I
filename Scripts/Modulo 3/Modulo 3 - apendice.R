
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
setwd("C:/Users/PC/Desktop/Curso_EDA_2024_I")
dataset <- readxl::read_excel("Datos/Formatos/geih_dataset.xlsx")

#----------------#
# Covariación    #-------------------------------------------------------------------------------------------------------------------------
#----------------#

# La variación describe el comportamiento de los valores de una variable.
# La covariación describe el comportamiento de los valores de dos o más variables.

#--------------------------#
# Variables  categóricas   #-------------------------------------------------------------------------------------------------------------------------
#--------------------------#

# Obtener la distribución de la variable continua diferenciada según la variable categórica
ggplot(dataset,
       aes(x = factor(edu),
           fill = factor(edu))) + 

  geom_bar(
    aes(y = after_stat(count / ave(count, PANEL, FUN = sum)*100)),
    position = "dodge"
  ) + 

  labs(x = "Nivel de educación máxima", y = "Proporción (%)") + 
  
  ggtitle("Nivel de educación según área metropolitana") + 

  theme_bw() + 
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~area, ncol = 5) 

# Lo mismo puede ser aplicado para las variables con pocas categorías

count_cat <- dataset %>% count(sexo, cambiar, estable, -
                                 
                                 l, caja, cotiza_fondo)
melt_count_cat <- reshape2::melt(count_cat)

ggplot(tabulated, aes(x=1, y=N, fill=color)) +
  geom_bar(position = 'fill', stat = 'identity')  +
  facet_grid(clarity ~ cut) + 
  xlim(0.5, 2.5) +
  coord_polar(theta = 'y') + 
  labs(x=NULL, y=NULL)
dat2 <- reshape2::melt(dat)
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




