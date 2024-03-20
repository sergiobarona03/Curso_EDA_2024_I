
################################################
################################################
## Módulo III: Análisis exploratorio de datos ##
################################################
################################################

library(tidyverse)

#----------------------#
# Cargar base de datos #-------------------------------------------------------------------------------------------------------------------------
#----------------------#
setwd("C:/Users/Portatil/Desktop/Curso_EDA_2024_I")
dataset <- readxl::read_excel("Datos/Formatos/geih_dataset.xlsx")

#----------------#
# Covariación    #-------------------------------------------------------------------------------------------------------------------------
#----------------#

# La variación describe el comportamiento de los valores de una variable.
# La covariación describe el comportamiento de los valores de dos o más variables.

#--------------------------#
# Variables  categóricas   #-------------------------------------------------------------------------------------------------------------------------
#--------------------------#

edu_area <- dataset %>% count(edu, area) %>%
  group_by(area) %>% mutate(perc = (n/sum(n))*100)

# Obtener la distribución de la variable continua diferenciada según la variable categórica

ggplot(edu_area,
       aes(x = fct_reorder(factor(edu), -perc),
           y= perc, fill = factor(edu))) + 
  geom_bar(stat = "identity") + facet_wrap(~area) + 
  
  labs(x = "Nivel de educación máxima", y = "Proporción (%)") + 
  
  ggtitle("Nivel de educación según área metropolitana") + 
  
  theme_bw() + 
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~area, ncol = 5) 

# Examinar la tabla de contigencia
con1<-table(dataset$area,dataset$edu)
addmargins(con1)

mosaicplot(con1, las = 2, shade = T)

chisq.test(dataset$area, dataset$edu)

# En lo sucesivo, el mismo ejercicio es realizado para 
# examinar las diferencias según el sexo

count_prop = dataset %>% select(c("sexo","parent",
                                 "edu", "posic", "estable", "medio", "sintrab",
                                 "cotiza_fondo", "arl", "caja",
                                 "actividad")) %>%
  mutate(across(.fns = as.factor)) %>%
  tidyr::pivot_longer(cols = c("parent",
                               "edu", "posic", "estable", "medio", "sintrab",
                               "cotiza_fondo", "arl", "caja",
                               "actividad")) %>%
  dplyr::count(sexo, name, value, name = 'N') %>%
  group_by(name) %>%
  mutate(N = N,
         Share = prop.table(N) * 100)

count_prop <- tibble(readRDS("C:/Users/Portatil/Desktop/Curso_EDA_2024_I/Módulos/Módulo 2/Figuras/count_prop.rds"))

count_prop <- count_prop %>% dplyr::filter(!is.na(value))
count_prop <- count_prop %>% dplyr::filter(name %in% c("actividad",
                                                       "edu",
                                                       "cotiza_fondo"))

count_prop$name <-  revalue(factor(count_prop$name), 
                            c("actividad"="Actividad económica", "edu" = "Educación",
                              "cotiza_fondo" = "Fondo de pensiones"))  
#count_prop$value = substr(count_prop$value, 1, 35)
count_prop$Share = round(count_prop$Share, 2)



# Ahora bien, según la educación
# Agregamos categorías menos frecuente

# Creamos una función que permita replicar la prueba para
# múltiple variables categóricas

f_cat_var <- function(x, cat_var, df){
  output <- data.frame(var_1 = rep(x, length(cat_var)),
                       var_2 = NA, p_value = NA)
  
  for (i in 1:length(cat_var)) {
    input <- df %>% select(x, cat_var[i]) 
    colnames(input) = c("x", "y")
    
    if(ncol(input) == 2){
      out <- chisq.test(as.factor(input$x),
                        as.factor(input$y), 
                        simulate.p.value = F)
      
      output$var_1[i] = x
      output$var_2[i] = cat_var[i]
      output$p_value[i] = round(out$p.value, 3)
    } else {
      output$var_1[i] = x
      output$var_2[i] = cat_var[i]
      output$p_value[i] = NA
    }
    
    
  }
  
  return(output)
  
}

# Para obtener el resultado, necesitamos (1) la variable categórica principal
# y (2) el vector completo de variables categóricas

vec_cat <- c("sexo", "parent", "edu", "posic",
             "estable", "medio", "sintrab", "arl",
             "caja", "actividad")

f_cat_var("caja", vec_cat, dataset)

# Lo mismo puede ser aplicado para las variables con pocas categorías

dic_area <- dataset %>% count(area, sexo,
                              arl, caja,
                              estable, mas_h) %>%
  group_by(area) %>% mutate(perc = (n/sum(n))*100)

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




