
###############################################
###############################################
## Módulo II: Análisis exploratorio de datos ##
###############################################
###############################################
library(readxl)
library(tidyverse)
library(ggpubr)
library(gt)
library(treemapify)

#----------------------#
# Cargar base de datos #-------------------------------------------------------------------------------------------------------------------------
#----------------------#
setwd("C:/Users/PC/Desktop/Nuevo_Curso_2024I")
dataset <- readxl::read_excel("Input/attend.xlsx")

#--------------------------------#
# Análisis exploratorio de datos #-------------------------------------------------------------------------------------------------------------------------
#--------------------------------#

#-----------------------#
# Variables categóricas #-------------------------------------------------------------------------------------------------------------------------
#-----------------------#

#-------------------------------------------#
# Variables categóricas: diagrama de barras #
#-------------------------------------------#
# Para examinar la distribución de una variable categórica, nos interesa su frecuencia
new_dataset <- dataset %>% mutate(Int_attend = cut(attend,
                                                   breaks = seq(0,50, by = 10),
                                                   right = F,
                                                   labels = c("Group 1",
                                                              "Group 2",
                                                              "Group 3",
                                                              "Group 4",
                                                              "Group 5"))) # Asignar etiquetas

# Definir variables categóricas
new_dataset$Int_attend = as.factor(new_dataset$Int_attend)
new_dataset$soph = as.factor(new_dataset$soph)
new_dataset$frosh = as.factor(new_dataset$frosh)

# Graficar proporción y conteo
count.plot = ggplot(data = new_dataset) +
  geom_bar(mapping = aes(x = Int_attend), 
           color = "white", fill = "lightskyblue") + 
  labs(title = "Intervalos para los puntajes de la prueba",
       x = "Puntajes", y = "Conteo (n)") + theme_bw()

share.plot = ggplot(data = new_dataset) +
  geom_bar(mapping = aes(x = Int_attend,
                         y =after_stat(count / sum(count))), 
           color = "white", fill = "coral") + 
  labs(title = "Intervalos para los puntajes de la prueba",
       x = "Puntajes", y = "Proporción (%)") +  scale_y_continuous(labels = scales::percent) +
  theme_bw()

ggarrange(count.plot, share.plot, ncol = 2, nrow = 1)

# La misma información se puede obtener con la función dplyr::count()
count_attend = new_dataset %>% count(Int_attend) %>%
  mutate(perc = prop.table(n)*100)

# Un resumen de las variables categóricas se puede obtener así
count_prop = new_dataset %>% select(c("Int_attend", 
                                      "soph", "frosh")) %>%
  mutate(across(.fns = as.character)) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  count(name, value, name = 'N') %>%
  group_by(name) %>%
  mutate(N = N,
         Share = prop.table(N) * 100)


# Mejorar la presentación de los resultados
descriptive_summary = gt(as.data.frame(count_prop[c("value", "N", "Share")]))

gt_tbl <- 
  descriptive_summary |> 
  tab_row_group(
    label = "Int_attend",
    rows = 1:4
  ) |>
  tab_row_group(
    label = "Frosh",
    rows = 5:6
  ) |>
  tab_row_group(
    label = "Soph",
    rows = 7:8
  )


#-----------------------------------------------#
# Variables categóricas: creamos otros gráficos #
#-----------------------------------------------#

# Pie chart
ggplot(count_attend, aes(x="", y=n, fill=Int_attend)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void() +
  geom_label(aes(label = n),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) 
  

# Donut chart
h_size = 5
ggplot(count_attend, aes(x = h_size, y = n, fill = Int_attend)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(0.2, h_size + 0.5)) + theme_void()+
  geom_label(aes(label = n),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) 

# Treemap
ggplot(count_attend, aes(area = n,
                         fill = n, label = Int_attend)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) + scale_fill_viridis_c()

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






