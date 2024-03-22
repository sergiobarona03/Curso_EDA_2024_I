
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
## INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- INICIO: MÓDULO 3 ----- #######
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################


library(tidyverse)

#----------------------#
# Cargar base de datos #-------------------------------------------------------------------------------------------------------------------------
#----------------------#
setwd("C:/Users/Portatil/Desktop/Curso_EDA_2024_I")
dataset <- readxl::read_excel("Datos/Formatos/geih_dataset.xlsx")

#--------------------------------------------------------#
# PRIMERA PARTE: sintaxis de ggplot2 (un recordatorio)   #-------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------#

# Ggplot2 es una librería para la visualización de datos
# Sintaxis general: ggplot(dataset, estética) + geometría + 
# opciones + faceta (ver diapositiva)

# Nos centramos en las personas que cotizan
dataset_2 <- dataset %>% filter(ingreso > 2000000 &
                                  ingreso < 10000000 &
                                  cotiza_fondo != "No cotiza")

ggplot2::ggplot(dataset_2, aes(x = edad,
                               y = ingreso, 
                               color = cotiza_fondo))+
  geom_point() 

# Agregar etiquetas y definir límites

ggplot2::ggplot(dataset_2, aes(x = edad,
                               y = ingreso/1000, 
                               color = cotiza_fondo))+
  geom_point() + labs(
    title = "Ingreso laboral y edad diferenciado según plan de pensiones",
    caption = "Fuente: Gran Encuesta Integrada de Hogares - diciembre 2023",
    x = "Edad (años cumplidos)",
    y = "Ingreso laboral (miles $)",
    col = "Plan de pensiones"
    
  )

# Ajustar el color
library(RColorBrewer)
ggplot2::ggplot(dataset_2, aes(x = edad,
                               y = ingreso/1000, 
                               color = cotiza_fondo))+
  geom_point() + labs(
    title = "Ingreso laboral y edad diferenciado según plan de pensiones",
    caption = "Fuente: Gran Encuesta Integrada de Hogares - diciembre 2023",
    x = "Edad (años cumplidos)",
    y = "Ingreso laboral (miles $)",
    col = "Plan de pensiones") + 
  scale_color_brewer(palette = "PuOr")


# Gráfico en ggplot2 con facetas
# Incorporamos el plan de pensiones como una faceta
ggplot2::ggplot(dataset_2, aes(x = edad,
                               y = ingreso/1000, 
                               color = cotiza_fondo))+
  geom_point() + facet_wrap(~cotiza_fondo, scale = "free_y") + labs(
    title = "Ingreso laboral y edad diferenciado según plan de pensiones",
    caption = "Fuente: Gran Encuesta Integrada de Hogares - diciembre 2023",
    x = "Edad (años cumplidos)",
    y = "Ingreso laboral (miles $)",
    col = "Plan de pensiones") + 
  scale_color_brewer(palette = "PuOr")

#-----------------------------------------------------------------#
# ANEXO1 : Uso de FORCATS (tratamiento de variables categóricas)  #-------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------#

# Declarar variable como factor
dataset_2$edu = factor(dataset_2$edu)

# fct_relevel()
# Reordenar manualmente los niveles de un factor
# Organiza los niveles después del 4to
fct_relevel(dataset_2$edu, c("Primaria", "Secundaria"),
            after = 4)


# fct_infreq()
# Reordenar los valores de acuerdo con su frecuencia
fct_infreq(dataset_2$edu)

# fct_shift()
# Reordenar moviendo los niveles de izquierda a derecha
fct_shift(dataset_2$edu)

# fct_recode()
# Recodificar manualmente los niveles de las variables categóricas
fct_recode(dataset_2$edu,
           `Maestria` = "Maestría")

# fct_collapse()
# Agrupar distintos niveles en un grupo específico
fct_collapse(dataset_2$edu,
             `Posgrado` = c("Maestría", "Doctorado"))

# fct_lump_prop()
# Agrupar niveles en categoría "Otros" de acuerdo con una proporción
fct_lump_prop(dataset_2$edu, 
              prop = 0.10, other_level = "Other") 

fct_lump_prop(dataset_2$edu, 
              prop = 0.04, other_level = "Other") 

# fct_lump_n()
# Agrupa niveles según la posición

fct_lump_n(dataset_2$edu, 
           n = 2, other_level = "Other") 

# fct_reorder()
# Reordenar factor variable ordenando según otra variable
ggplot2::ggplot(dataset_2, aes(x = cotiza_fondo,
                               y = edad, fill = cotiza_fondo)) + 
  geom_boxplot() + scale_fill_brewer(palette = "PuOr") # Gráfico no ordenado

ggplot2::ggplot(dataset_2, aes(x = fct_reorder(cotiza_fondo, edad),
                               y = edad, 
                               fill = cotiza_fondo)) + 
  geom_boxplot() + scale_fill_brewer(palette = "PuOr") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Gráfico ordenado


#--------------------------------------------------------------------#
# SEGUNDA PARTE: ANÁLISIS EXPLORATORIO DE DATOS (EDA) UNIVARIANTE    #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------#

#------------------------------------------------#
# Análisis univariado para variables categóricas #
#------------------------------------------------#

#### Para empezar el análisis de variables categóricas
## se propone la construcción de un diagrama de barras para
## la variable actividad económica

# Conservar los n=11 categorías más frecuentes
dataset$actividad = fct_lump_n(dataset$actividad, 
                               n = 11, other_level = "Other") 

actividad <- dataset %>% dplyr::count(actividad) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "actividad")
colnames(actividad) = c("categories", "n", "perc", "variable")

actividad <- actividad %>% filter(categories != "Other")

figure_1 <- ggplot(actividad, aes(reorder(categories, -n), perc,
                                  fill = categories, col = categories)) +
  geom_bar(stat = "identity")

# Agregar parámetros
labs(title = "Actividad económica del trabajador",
     x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 60)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) + guides(fill="none",
                                                                col = "none") +
  coord_flip()

figure_1

##### Práctica: un ejercicio análogo 
##### con la variable del nivel educativo

edu <- dataset %>% dplyr::count(edu) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "edu")
colnames(edu) = c("categories", "n", "perc", "variable")

figure_2 <- ggplot(edu, aes(reorder(categories, -n), perc,
                            fill = categories, col = categories)) +
  geom_bar(stat = "identity")

# Añadir los siguientes parámetros
labs(title = "Nivel de educación máximo",
     x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) + guides(fill="none",
                                                                col = "none")
figure_2

##### Creamos un panel 
panel <- rbind(actividad, edu)

figure_3 <- ggplot(panel, aes(reorder(categories, -n), perc,
                              fill = variable, col = variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, scales="free", ncol = 1,
             labeller = labeller(variable = c(`actividad` = "Actividad económica",
                                              `edu` = "Nivel educativo"))) +
  labs(title = "Resumen de variables categóricas",
       x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 60)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) + guides(fill="none",
                                                                col = "none") +
  coord_flip()

figure_3

# Añadir una tercera variable: cotización en fondo de pensiones
cotiza_fondo = dataset %>% dplyr::count(cotiza_fondo) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "cotiza_fondo")
colnames(cotiza_fondo) = c("categories", "n", "perc", "variable")

panel <- rbind(panel, cotiza_fondo)

figure_4 <- ggplot(panel, aes(reorder(categories, n), perc,
                              fill = variable, col = variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, scales="free", ncol = 1,
             labeller = labeller(variable = c(`actividad` = "Actividad económica",
                                              `edu` = "Nivel educativo",
                                              `cotiza_fondo` = "Cotiza a fondo de pensiones"))) +
  labs(title = "Resumen de variables categóricas",
       x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 60)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) + guides(fill="none",
                                                                col = "none") +
  coord_flip()

figure_4

########
#### Un resumen de las variables categóricas se puede obtener así
########

# Resumen descriptivo (n y porcentaje [%])
# Preparar el resumen descriptivo para el nivel educativo máximo
count_prop <- dataset %>%
  dplyr::count(edu, name = 'N', sort = TRUE) %>%
  mutate(N = N,
         Share = prop.table(N) * 100)

# Preparar un resumen descriptivo general
general_count_prop <- dataset %>% select(c("parent",
                                           "edu", "posic", "actividad", "medio", "sintrab",
                                           "cotiza_fondo", "lugar")) %>%
  mutate(across(.fns = as.factor)) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  dplyr::count(name, value, name = 'N') %>%
  group_by(name) %>%
  mutate(N = N,
         Share = prop.table(N) * 100)



########
### Para variables con pocas categorías
########

# Consideremos la cotización, la afliación a ARL y la caja de compensación
# Primero, examinamos la cotización a fondo de pensiones
cotiza <- dataset %>% dplyr::count(cotiza_fondo) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "cotiza_fondo")
colnames(cotiza) = c("categories", "n", "perc", "variable")

figure_5 <- ggplot(cotiza, aes(x = " ", n,
                               fill = categories)) +
  geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) +
  labs(title = " ",
       x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  geom_label(aes(label = paste0(categories, ": ",round(perc,2), "%")),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) + guides(fill="none",
                                           col = "none") + theme_void()

figure_5

# Hacer lo mismo para la cotización de ARL

arl <- dataset %>% dplyr::count(arl) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "arl")
colnames(arl) = c("categories", "n", "perc", "variable")

figure_6 <- ggplot(arl, aes(x = " ", n,
                            fill = categories)) +
  geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) +
  labs(title = " ",
       x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  geom_label(aes(label = paste0(categories, ": ",round(perc,2), "%")),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) + guides(fill="none",
                                           col = "none") + theme_void()

figure_6

# También es posible construir un panel idéntico
# Para complementar, añadimos la afiliación a caja de compensación
caja <- dataset %>% dplyr::count(caja) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "caja")
colnames(caja) = c("categories", "n", "perc", "variable")

plot_dic <- rbind(cotiza_fondo, arl, caja)

figure_7 <- ggplot(plot_dic, aes(x = " ", n,
                                 fill = categories)) +
  geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) +
  facet_wrap(~variable, scales="free", ncol = 3,
             labeller = labeller(variable = c(`cotiza_fondo` = "Cotización fondo",
                                              `arl` = "Afiliación a ARL",
                                              `caja` = "Afiliación a caja de compensación"))) +
  labs(title = " ",
       x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  geom_label(aes(label = paste0(categories, ": ",round(perc,2), "%")),
             position = position_stack(vjust = 0.42),
             show.legend = FALSE) + guides(fill="none",
                                           col = "none") + theme_void()

figure_7

#----------------------------------------------------------#
# Otros gráficos para variables categóricas: visualización #
#----------------------------------------------------------#

# Pie chart
ggplot(cotiza_fondo, aes(x="", y=n, fill=categories)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void() +
  geom_label(aes(label = round(perc,2)),
             position = position_stack(vjust = 1),
             show.legend = FALSE) 

# Donut chart
h_size = 5
ggplot(cotiza_fondo, aes(x = h_size, y = n, fill = categories)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(2, h_size + 0.5)) + theme_void()+
  geom_label(aes(label = paste0(round(perc,1), "%")),
             position = position_stack(vjust = 0.8),
             show.legend = FALSE,
             angle = 45) 

# Treemap
library(treemapify)
ggplot(edu, aes(area = n,
                fill = n, label = categories)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) + scale_fill_viridis_c()

#--------------------------------------------------------------#
# Análisis univariado para variables continuas: visualización  #
#--------------------------------------------------------------#

########## Creamos un histograma
ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = ingreso/1000, col = ingreso),
                 fill = "lightskyblue", col = "black",
                 binwidth = 400) + xlim(c(0, 15000))  + theme_bw()

# Los datos del histograma pueden ser representados
# mediante la siguiente tabla
count = dataset %>% dplyr::count(cut_width(ingreso/1000, 1000, boundary = 0, dig.lab = 6),
                                 name = "n") 


########## La función de densidad empírica es obtenida usando
########## la función density()
ggplot(data = dataset, aes(x = ingreso/1000)) + 
  geom_density(color = "black",
               alpha = 0.2, fill = "gray45") +
  theme_bw() + labs(x = "Ingreso laboral", y = "Density",
                    title = "Ingreso laboral (miles $)") +
  xlim(0, 8000)

# La función de distribución acumulada empírica puede ser
# obtenida mediante la función ECDF
ggplot(data = dataset,
       aes(x = ingreso/1000)) + stat_ecdf(geom = "step",
                                          color = "gray45") +
  theme_bw() + labs(x = "Ingreso laboral", y = "ECDF",
                    title = "Ingreso laboral (miles $)") +
  xlim(0,8000)


##########
########## Práctica: construir ambas gráficas para la variable edad
##########


########## Construcción del diagrama de caja
# Crear boxplot
ggplot(dataset, aes(x = "", y = edad)) +
  geom_boxplot()

# Ajustar parámetros
ggplot(dataset, aes(x = "", y = edad)) + 
  stat_boxplot(geom ='errorbar', col = "black")+
  geom_boxplot(outliers = T, col = "black",
               fill = "white") + theme_classic() + ylim(c(0,35)) + coord_flip()

##########
########## Práctica: analizar la distribución en el ingreso
##########

#-----------------------------------------------------#
# Resumen general: histograma, boxplot y scatter plot #
#-----------------------------------------------------#

# Para el caso de las variables continuas un resumen útil
# puede ser alcanzado usando library(StatDA)

library(StatDA)
library(moments)

me = mean(dataset$edad)
sd = sd(dataset$edad)

StatDA::edaplot(dataset$edad, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(0,100, by = 4),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(1, 91), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram,
                Density Plot, Scatterplot,
	and Boxplot of Age",
                P.xlab="Edad (años)", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(dataset$edad), lwd=2, col='blue')
curve(dnorm(x, mean=me, sd=sd), from=0, to=100, add=T,
      col='red', lwd=3)
leg.txt <- c(paste0("Min. = ", round(min(dataset$edad),4)),
             paste0("Max. = ", round(max(dataset$edad),4)),
             paste0("Mean = ", round(mean(dataset$edad),4)),
             paste0("Median = ", round(median(dataset$edad),4)),
             paste0("Std. dev. = ", round(sd(dataset$edad),4)),
             paste0("Kurtosis = ", round(kurtosis(dataset$edad),4)),
             paste0("Skewness = ", round(skewness(dataset$edad),4)))
legend (x=-3, y=0.028, bty="n", leg.txt)


#########
######### Práctica: hacer la misma representación
######### para la variable de ingreso laboral (miles $)
#########


#--------------------------------------------------------------------#
# TERCERA PARTE: ANÁLISIS EXPLORATORIO DE DATOS (EDA) MULTIVARIANTE  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------#

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

# Examinamos idénticas diferencias según la variable educación

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

f_cat_var("sexo", vec_cat, dataset)

# Lo mismo puede ser aplicado para la variable educación

count_prop = dataset %>% select(c("edu","parent",
                                  "sexo", "posic", "estable", "medio", "sintrab",
                                  "cotiza_fondo", "arl", "caja",
                                  "actividad")) %>%
  mutate(across(.fns = as.factor)) %>%
  tidyr::pivot_longer(cols = c("parent",
                               "sexo", "posic", "estable", "medio", "sintrab",
                               "cotiza_fondo", "arl", "caja",
                               "actividad")) %>%
  dplyr::count(edu, name, value, name = 'N') %>%
  group_by(name) %>%
  mutate(N = N,
         Share = prop.table(N) * 100)

f_cat_var("edu", vec_cat, dataset)

# Se verifica, por ejemplo, que el nivel educativo no es independiente
# del fondo de pensiones en el cual está cotizando el trabajador (o si lo hace)

# Para la presentación: se muestra la matriz completa

dataset$medio = fct_lump_n(factor(dataset$medio), 5)

edu_fondo <- dataset %>% dplyr::count(sexo, medio) %>%
  group_by(sexo) %>% mutate(perc = (n/sum(n))*100)

p.value <- chisq.test(dataset$sexo, dataset$medio)$p.value

ggplot(edu_fondo, aes(x = sexo, y = perc,
                  fill = fct_reorder(medio, perc)) ) +
  geom_bar(stat = "identity", position = "dodge") + 
  annotate("text", x=1, y=13, label=paste0("Chi-2 test, p-value: ", signif(p.value,4))) +
  theme_bw() + scale_fill_brewer(palette = "PuOr") +
  labs(x = "Sexo", y = "Participación (%)",
       fill = "Medio")

p.value <- chisq.test(dataset$sexo, dataset$posic)$p.value

sp <- dataset %>% dplyr::count(sexo, posic) %>%
  group_by(sexo) %>% mutate(perc = (n/sum(n))*100)

ggplot(sp, aes(x = sexo, y = perc,
                      fill = fct_reorder(posic, perc)) ) +
  geom_bar(stat = "identity", position = "dodge") + 
  annotate("text", x=1, y=28, label=paste0("Chi-2 test, p-value: ", signif(p.value,4))) +
  theme_bw() + scale_fill_brewer(palette = "PuOr") +
  labs(x = "Sexo", y = "Participación (%)",
       fill = "Puesto laboral")




#-----------------------------------------------#
# Variables continuas y variables categóricas   #-------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------#

library(ggpubr)

dataset$ingreso2 = dataset$ingreso/1000

# Ingreso laboral según el nivel educativo
ggboxplot(dataset, x = "edu", y = "ingreso2",
          color = "edu", palette = "jco",
          outlier.shape = NA)+
  stat_compare_means() + ylim(c(0, 6000)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Ingreso laboral (miles $) según el nivel educativo",
       y = "Ingreso (miles $)", x = "",
       color = "Nivel")

# Añadir la comparación por pares
comparisons <- list( c("Maestría", "Doctorado"),
                     c("Secundaria", "Maestría"),
                     c("Primaria", "Secundaria") )

ggboxplot(dataset, x = "edu", y = "ingreso2",
          color = "edu", palette = "jco",
          outlier.shape = NA) + 
  stat_compare_means(comparisons = comparisons, 
                     label.y = c(10000, 11000, 12000),
                     bracket.size = 0.2,
                     label =  "p.signif") + 
  stat_compare_means(label.y = 11000)  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Ingreso laboral (miles $) según el nivel educativo",
       y = "Ingreso (miles $)", x = "",
       color = "Nivel") + ylim(c(0,18000))

# Ingreso laboral según si el nivel educativo y el sexo

comparisons <- list( c("Maestría", "Doctorado"),
                     c("Secundaria", "Maestría"),
                     c("Primaria", "Secundaria") )

ggboxplot(dataset, x = "edu", y = "ingreso2",
          color = "edu", palette = "jco",
          facet.by = "sexo",
          outlier.shape = NA) + 
  stat_compare_means(comparisons = comparisons, 
                     label.y = c(10000, 11000, 12000),
                     bracket.size = 0.2,
                     label =  "p.signif") + 
  stat_compare_means(label.y = 11000,
                     label.x = 2)  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Ingreso laboral (miles $) según el nivel educativo",
       y = "Ingreso (miles $)", x = "",
       color = "Nivel") + ylim(c(0,18000))

# Un ejercicio similar es realizado para otras variables
# Presento las diferencias según variables continuas y variables categóricas

lista_kruskal = list(area = data.frame(),
                     sexo = data.frame(),
                     parent = data.frame(),
                     edu = data.frame(),
                     lugar = data.frame(),
                     medio = data.frame(),
                     cotiza_fondo = data.frame(),
                     actividad = data.frame())

categoricas = c("area", "sexo", "parent", "edu", "lugar",
                "medio", "cotiza_fondo", "actividad")

continuas = c("ingreso", "edad","horas_semana", "t_actual",
              "t_viaje")

data_kruskal = dataset[c("id", categoricas, continuas)]

hipotesis = continuas

for (i in categoricas) {
  item_cat = which(names(lista_kruskal) ==i)
  lista_kruskal[[item_cat]] = data.frame(Variables = hipotesis,
                                         Chi_2 = rep(NA, length(hipotesis)),
                                         p = rep(NA, length(hipotesis)))
  
  for (j in hipotesis) {
    item_hyp = which(lista_kruskal[[item_cat]]$Variables == j )  
    
    kruskal_aux = data_kruskal[c(j,i)] 
    colnames(kruskal_aux) = c("continua", "discreta")
    kruskal_aux$continua = as.numeric(kruskal_aux$continua)
    kruskal_aux$discreta = as.factor(kruskal_aux$discreta)
    kruskal_test_aux = kruskal.test(kruskal_aux$continua~kruskal_aux$discreta)
    lista_kruskal[[item_cat]]$Chi_2[item_hyp] = paste0(round(kruskal_test_aux$statistic, digits = 1),
                                                       " (",
                                                       kruskal_test_aux$parameter
                                                       , ")")
    
    lista_kruskal[[item_cat]]$p[item_hyp]  = round(kruskal_test_aux$p.value, digits = 2)
  }
  
  colnames(lista_kruskal[[item_cat]]) = c("Variables", 
                                          paste0(i,"_Chi_2"),
                                          paste0(i,"_p"))
}

# Resultados generales
kruskal = merge(lista_kruskal[[1]], lista_kruskal[[2]], by = "Variables")
kruskal = merge(kruskal, lista_kruskal[[3]], by = "Variables")
kruskal = merge(kruskal, lista_kruskal[[4]], by = "Variables")
kruskal = merge(kruskal, lista_kruskal[[5]], by = "Variables")
kruskal = merge(kruskal, lista_kruskal[[6]], by = "Variables")
kruskal = merge(kruskal, lista_kruskal[[7]], by = "Variables")
kruskal = merge(kruskal, lista_kruskal[[8]], by = "Variables")

#-----------------------------#
# Dos variables continuas     #-------------------------------------------------------------------------------------------------------------------------
#-----------------------------#

pairs(mtcars[, c(1, 3:6)],
      main = "Scatter Plot Matrix for mtcars Dataset")

# La aproximación básica es un scatter plot:
ggplot(data = dataset) +
  geom_point(mapping = aes(x = edad,
                           y = ingreso/1000,
                           col = sexo)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4,"Blues")[3:4])

# El parámetro de transparencia (alpha) puede ser usado para 
# el caso de grandes bases de datos 
ggplot(data = dataset) + 
  geom_point(mapping = aes(x = edad,
                           y = t_actual),
             alpha = 0.2)

# Una forma de agrupar los datos es la siguiente:
# (PENSAR SI TERMINA SIENDO AGREGADO)
ggplot(data = dataset) + 
  geom_bin2d(mapping = aes(x = edad,
                           y = t_actual))

# El mismo resultado se puede alcanzar con el paquete Hexbin
install.packages("hexbin")
library(hexbin)
ggplot(data = dataset,
       mapping = aes(x = edad,
                     y = t_actual)) +
  geom_boxplot(aes(group = cut_width(edad, 
                                     10)))

# Es posible determinar el ajuste mediante la función geom_smooth
ggplot(data = dataset, aes(x = edad, y = t_actual)) +
  geom_point() +
  geom_smooth(method = "lm", 
                se = TRUE) +
  labs(title = "Edad (años cumplidos) y tiempo en el trabajo actual",
       x = "Edad (en años)",
       y = "Tiempo en el trabajo actual")


ggplot(data = dataset, aes(x = edad, y = t_actual)) +
  geom_point(aes(color = factor(sexo))) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              aes(color = factor(sexo)))  +
  labs(title = "Edad (años cumplidos) y tiempo en el trabajo actual",
       x = "Edad (en años)",
       y = "Tiempo en el trabajo actual")

# Lo mismo se puede hacer para  cotiza_fondo y la edad
ggplot(dataset, aes(edad, log(ingreso))) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              se = TRUE) +
  facet_wrap(~edu)

# Para las demás variables es  útil examinar un scatter plot conjunto
# Para eso es útil la siguiente función base
cont_ds <- dataset %>% select(ingreso, edad, horas_semana, t_actual)
pairs(cont_ds, 
      main = "Scatter Plot Matrix for Dataset",
      col = RColorBrewer::brewer.pal(4,"Blues"))

# Matriz de correlación para múltiples variables
library(corr)

  
cor_ds <- cor(cont_ds)
head(round(cor_ds,2))

# visualizing correlogram
# as circle
corrplot(cor_ds, method="circle")

# as pie
corrplot(M, method="pie")

# as colour
corrplot(M, method="color")

# as number
corrplot(M, method="number")

# Correlación sobre las variables transformadas


corr1 = cor(cont_ds[,-1], 
            cont_ds$ingreso) %>% as.data.frame() %>% rownames_to_column(var = "variable")

corr2 = cor(cont_ds[,-1], 
            log(cont_ds$ingreso))%>% as.data.frame() %>% rownames_to_column(var = "variable")

corr3 = cor(log(cont_ds[,-1]), 
            log(cont_ds$ingreso)) %>% as.data.frame() %>% rownames_to_column(var = "variable")

plot.corr1 = ggplot(corr1, aes(x = variable,
                               y = V1, fill = V1)) +
  geom_bar(stat = "identity") + theme_classic() +
  labs(x = "Variable", y = "Corr", title = "lin-lin",
       fill = "") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.1, hjust=1))

plot.corr2 = ggplot(corr2, aes(x = variable,
                               y = V1, fill = V1)) +
  geom_bar(stat = "identity") + theme_classic() +
  labs(x = "Variable", y = "Corr", title = "log-lin",
       fill = "") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.1, hjust=1)) 

plot.corr3 = ggplot(corr3, aes(x = variable,
                               y = V1, fill = V1)) +
  geom_bar(stat = "identity") + theme_classic() +
  labs(x = "Variable", y = "Corr", title = "log-log",
       fill = "") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.1, hjust=1))


ggarrange(plot.corr1,
          plot.corr2,
          plot.corr3, ncol = 3,nrow = 1)

# El paquete psych
library(psych)

pairs.panels(cont_ds, main = "Scatter Plot Matrix for mtcars Dataset")


#-----------------------------------------------------------#
# TERCERA PARTE: DETECCIÓN UNIVARIANTE DE VALORES ATÍPICOS  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------#


#------------------------------------------------------#
# Valores atípicos: histograma, boxplot y scatter plot #
#------------------------------------------------------#

# La perspectiva univariante selecciona como valores atípicos u outliers
# aquellas observaciones que corresponden a
# Errores en los datos
# Errores voluntarios
# Errores de muestreo
# Outliers legítimos
# Veamos dos aproximaciones básicas para la identificación de outliers.

# Consideremos la variable de ingresos laborales
# Un histograma nos puede ofrecer algunas luces
ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = ingreso/1000),
                 fill = "lightskyblue", col = "black",
                 binwidth = 400) + theme_bw() + xlim(0, 15000)

# Para identificar los outliers es útil un diagrama de caja
boxplot(dataset$ingreso/1000, horizontal = T)

# Primer criterio: criterio IQR para la detección de outliers
# Cualquier observación fuera del intervalo [q0.25 - 1.5IQR, q0.75 + 1.5IQR]
# es identificado como un outlier
# A partir de la función boxplot.stats() identificamos outliers:
boxplot.stats(dataset$ingreso)$out

# El mismo resultado se puede obtener manualmente
Q1 <- quantile(dataset$ingreso, .25, na.rm = T)    # Cálculo de q0.25
Q3 <- quantile(dataset$ingreso, .75, na.rm = T)    # Cálculo de q0.75
IQR <- IQR(dataset$ingreso, na.rm = T)             # Cálculo del IQR (Q3-Q1)

outliers1 <- dataset %>% filter(ingreso<(Q1 - 1.5*IQR) | ingreso>(Q3 + 1.5*IQR))

# Comparación
length(outliers1$ingreso)
length(boxplot.stats(dataset$ingreso)$out)

# Identificación de los outliers en la gráfica
plot(dataset$ingreso, type='p',
     col=ifelse(dataset$ingreso %in% outliers1$ingreso, "red", "black"),
     pch = ifelse(dataset$ingreso %in% outliers1$ingreso, 17, 1),
     ylim = c(-5000000, 10000000),
     ylab  = "Ingreso laboral")



# En general, el problema de la detección univariante de valores atípicos
# es un problema de definición de umbrales
# Usando los valores estandarizados de la variable, Hair et al. (1999)
# propone el siguiente criterio:

# Primer paso: estandarizar la variable

# Definición de función para estandarizar
f_z <- function(x){
  z <- (x-mean(x, na.rm = T))/sd(x, na.rm = T)
  return(z)
}

# Definición del data frame con la variable estandarizada
z <- data.frame(id = seq(1, nrow(dataset), by = 1),
                x = dataset$ingreso,
                z = f_z(dataset$ingreso))

# Seguimos el criterio según el cual z > 4, en muestras grandes,
# es considerado un valor atípico
outliers1 <- z  %>% filter(abs(z) > 2.5)
outliers2 <- z  %>% filter(abs(z) > 4)

par(mfrow = c(1,2))
plot(dataset$ingreso, type='p',
     col=ifelse(dataset$ingreso %in% outliers1$x, "red", "black"),
     pch = ifelse(dataset$ingreso %in% outliers1$x, 17, 1))
abline(h = min(outliers1$x), col="blue", lwd=3, lty=2)
plot(dataset$ingreso, type='p',
     col=ifelse(dataset$ingreso %in% outliers2$x, "red", "black"),
     pch = ifelse(dataset$ingreso %in% outliers2$x, 17, 1))
abline(h = min(outliers2$x), col="blue", lwd=3, lty=2)


#------------------------------------------------------#
# Comparación: distribuciones con y sin outliers       #
#------------------------------------------------------#

# Regresemos al caso de los ingresos
# Considérese el criterio de Hair et al. (2019)

# Definición del data frame con la variable estandarizada
z <- data.frame(x = dataset$ingreso,
                z = f_z(dataset$ingreso))

# Seguimos el criterio según el cual z > 4, en muestras grandes,
# es considerado un valor atípico
outliers2 <- z  %>% filter(abs(z) > 4)

# Eliminar valores NA
# El tratamiento de los valores NA y su estudio
# corresponden al tercer módulo del curso
dataset <- dataset %>% filter(!is.na(ingreso))

# Creación de base de datos sin outliers
dataset_no <- dataset %>% filter(!ingreso %in% outliers2$x)

# Comparación entre ambas distribuciones


par(mfrow = c(1,2))
StatDA::edaplot(dataset$ingreso/1000, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(0,90000, by = 400),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(1, 20000), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram,
                Density Plot, Scatterplot,
	and Boxplot of Rate (outliers)",
                P.xlab="Ingreso laboral (miles $)", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(dataset$ingreso/1000), lwd=2, col='blue')
curve(dnorm(x, mean=mean(dataset$ingreso/1000, na.rm = T),
            sd=sd(dataset$ingreso/1000, na.rm = T)), from=0, to=90000, add=T,
      col='red', lwd=3)
leg.txt <- c(paste0("Min. = ", round(min(dataset$ingreso/1000),4)),
             paste0("Max. = ", round(max(dataset$ingreso/1000),4)),
             paste0("Mean = ", round(mean(dataset$ingreso/1000),4)),
             paste0("Median = ", round(median(dataset$ingreso/1000),4)),
             paste0("Std. dev. = ", round(sd(dataset$ingreso/1000),4)),
             paste0("Kurtosis = ", round(kurtosis(dataset$ingreso/1000),4)),
             paste0("Skewness = ", round(skewness(dataset$ingreso/1000),4)))
legend (x= 8000, y=0.0008, bty="n", leg.txt)

StatDA::edaplot(dataset_no$ingreso/1000, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(0,90000, by = 400),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(1, 20000), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram,
                Density Plot, Scatterplot,
	and Boxplot of Rate (no-outliers)",
                P.xlab="Ingreso laboral (miles $)", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(dataset_no$ingreso/1000), lwd=2, col='blue')
curve(dnorm(x, mean=mean(dataset_no$ingreso/1000, na.rm = T),
            sd=sd(dataset_no$ingreso/1000, na.rm = T)), from=0, to=90000, add=T,
      col='red', lwd=3)
leg.txt <- c(paste0("Min. = ", round(min(dataset_no$ingreso/1000),4)),
             paste0("Max. = ", round(max(dataset_no$ingreso/1000),4)),
             paste0("Mean = ", round(mean(dataset_no$ingreso/1000),4)),
             paste0("Median = ", round(median(dataset_no$ingreso/1000),4)),
             paste0("Std. dev. = ", round(sd(dataset_no$ingreso/1000),4)),
             paste0("Kurtosis = ", round(kurtosis(dataset_no$ingreso/1000),4)),
             paste0("Skewness = ", round(skewness(dataset_no$ingreso/1000),4)))
legend (x=8000, y=0.0008, bty="n", leg.txt)


# Una práctica común sugiere que, cuando la interpretación de la variable
# no está enteramente sujeta a la escala, una transformación puede
# funcionar para suavizar la influencia de outliers

# Outliers para log(x)
z <- data.frame(x = log(dataset$ingreso),
                z = f_z(log(dataset$ingreso)))

# Seguimos el criterio según el cual z > 4, en muestras grandes,
# es considerado un valor atípico
outliers3 <- z  %>% filter(abs(z) > 4)

plot(log(dataset$ingreso), type='p',
     col=ifelse(log(dataset$ingreso) %in% outliers3$x, "red", "black"),
     pch = ifelse(log(dataset$ingreso) %in% outliers3$x, 17, 1))
abline(h = min(outliers3$x), col="blue", lwd=3, lty=2)

# Considérese la transformación logarítmica
dataset$log_ingreso <- log(dataset$ingreso)

dataset_no <- dataset %>% filter(!log_ingreso %in% outliers3$x)

# Comparación entre ambas distribuciones
par(mfrow = c(1,2))
StatDA::edaplot(dataset$log_ingreso, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(12,20, by = 0.1),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(12, 20), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram,
                Density Plot, Scatterplot,
	and Boxplot of Rate (outliers)",
                P.xlab="Log Ingreso laboral", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(dataset$log_ingreso), lwd=2, col='blue')
curve(dnorm(x, mean=mean(dataset$log_ingreso, na.rm = T),
            sd=sd(dataset$log_ingreso, na.rm = T)), from=12, to=20, add=T,
      col='red', lwd=3)
leg.txt <- c(paste0("Min. = ", round(min(dataset$log_ingreso),4)),
             paste0("Max. = ", round(max(dataset$log_ingreso),4)),
             paste0("Mean = ", round(mean(dataset$log_ingreso),4)),
             paste0("Median = ", round(median(dataset$log_ingreso),4)),
             paste0("Std. dev. = ", round(sd(dataset$log_ingreso),4)),
             paste0("Kurtosis = ", round(kurtosis(dataset$log_ingreso),4)),
             paste0("Skewness = ", round(skewness(dataset$log_ingreso),4)))
legend (x= 15, y=2, bty="n", leg.txt)

StatDA::edaplot(dataset_no$log_ingreso, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(12,18, by = 0.1),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(12, 18), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram,
                Density Plot, Scatterplot,
	and Boxplot of Rate (no-outliers)",
                P.xlab="Log Ingreso laboral", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(dataset_no$log_ingreso), lwd=2, col='blue')
curve(dnorm(x, mean=mean(dataset_no$log_ingreso, na.rm = T),
            sd=sd(dataset_no$log_ingreso, na.rm = T)), from=12, to=18, add=T,
      col='red', lwd=3)
leg.txt <- c(paste0("Min. = ", round(min(dataset_no$log_ingreso),4)),
             paste0("Max. = ", round(max(dataset_no$log_ingreso),4)),
             paste0("Mean = ", round(mean(dataset_no$log_ingreso),4)),
             paste0("Median = ", round(median(dataset_no$log_ingreso),4)),
             paste0("Std. dev. = ", round(sd(dataset_no$log_ingreso),4)),
             paste0("Kurtosis = ", round(kurtosis(dataset_no$log_ingreso),4)),
             paste0("Skewness = ", round(skewness(dataset_no$log_ingreso),4)))
legend (x= 15, y=2, bty="n", leg.txt)



########
######## Práctica: verifique que en la variable edad (años cumplidos)
######## no hay valores atípicos
########

########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
## FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3 ----- FIN DEL MÓDULO 3---- ##
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

