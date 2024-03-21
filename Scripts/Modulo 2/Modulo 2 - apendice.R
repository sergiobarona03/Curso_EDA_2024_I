
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
library(moments)
library(reshape2)

#----------------------#
# Cargar base de datos #-------------------------------------------------------------------------------------------------------------------------
#----------------------#
setwd("C:/Users/Portatil/Desktop/Curso_EDA_2024_I")
dataset <- readxl::read_excel("Datos/Formatos/geih_dataset.xlsx")

dataset <- dataset %>% filter(ingreso > 0)

#--------------------------------#
# Análisis exploratorio de datos #-------------------------------------------------------------------------------------------------------------------------
#--------------------------------#

#-----------------------#
# Variables categóricas #-------------------------------------------------------------------------------------------------------------------------
#-----------------------#

#-------------------------------------------#
# Variables categóricas: diagrama de barras #
#-------------------------------------------#
cat_var <- c("id","area", "dpto", "sexo", "parent",
             "edu", "posic", "actividad", "cotiza_fondo", "medio",
             "sintrab", "lugar")

# Para examinar la distribución de una variable categórica, nos interesa su frecuencia
ds_cat <- dataset[cat_var]
ds_cat <- ds_cat %>% mutate_at(c("area", "parent",
                              "edu", "posic", "actividad", "medio", "sintrab",
                              "cotiza_fondo", "lugar"),
                              fct_lump_n, 10)

# La misma información se puede obtener con la función dplyr::count()

parent = ds_cat %>% dplyr::count(parent) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "parent")
colnames(parent) = c("categories", "n", "perc", "variable")

edu = ds_cat %>% dplyr::count(edu) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "edu")
colnames(edu) = c("categories", "n", "perc", "variable")

posic = ds_cat %>% dplyr::count(posic) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "posic")
colnames(posic) = c("categories", "n", "perc", "variable")

actividad = ds_cat %>% dplyr::count(actividad) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "actividad")
colnames(actividad) = c("categories", "n", "perc", "variable")

medio = ds_cat %>% dplyr::count(medio) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "medio")
colnames(medio) = c("categories", "n", "perc", "variable")

sintrab = ds_cat %>% dplyr::count(sintrab) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "sintrab")
colnames(sintrab) = c("categories", "n", "perc", "variable")

cotiza_fondo = ds_cat %>% dplyr::count(cotiza_fondo) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "cotiza_fondo")
colnames(cotiza_fondo) = c("categories", "n", "perc", "variable")

lugar = ds_cat %>% dplyr::count(lugar) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "lugar")
colnames(lugar) = c("categories", "n", "perc", "variable")

plot_cat <- rbind(parent, edu, posic, actividad, medio, sintrab,
                  cotiza_fondo, lugar)

# Graficar proporción

# Remover la categoría de otros
plot_cat <- plot_cat %>% filter(categories != "Other")

figure_1 <- ggplot(plot_cat, aes(reorder(categories, -n), perc,
                     fill = variable, col = variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, scales="free", ncol = 4,
             labeller = labeller(variable = c(`actividad` = "Actividad económica",
                                                `cotiza_fondo` = "Fondo de pensiones",
                                                `edu` = "Nivel de educación",
                                                `lugar` = "Lugar de trabajo",
                                                `medio` = "Medio de transporte",
                                                `parent` = "Parentesco con jefe de hogar",
                                                `posic` = "Posición laboral",
                                                `sintrab` = "Alternativa de recursos"))) +
  labs(title = "Resumen de variables categóricas",
       x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) + guides(fill="none",
                                                                col = "none")
saveRDS(figure_1, "C:/Users/Portatil/Desktop/FIGURAS_M2/plot_cat.rds")


# Un resumen de las variables categóricas se puede obtener así

count_prop = ds_cat %>% select(c("parent",
                                 "edu", "posic", "actividad", "medio", "sintrab",
                                 "cotiza_fondo", "lugar")) %>%
  mutate(across(.fns = as.factor)) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  dplyr::count(name, value, name = 'N') %>%
  group_by(name) %>%
  mutate(N = N,
         Share = prop.table(N) * 100)

saveRDS(count_prop, "Módulos/Módulo 2/Figuras/count_prop.rds")


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

gt_tbl

#------------------------------------------------------------#
# Variables categóricas: gráficos para variables dicotómicas #
#------------------------------------------------------------#

dic_var <- c("id", "sexo","estable", "arl", "caja", "mas_h")

# Para examinar la distribución de una variable categórica, nos interesa su frecuencia
ds_dic <- dataset[dic_var]

# La misma información se puede obtener con la función dplyr::count()
sexo = ds_dic %>% dplyr::count(sexo) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "sexo")
colnames(sexo) = c("categories", "n", "perc", "variable")

estable = ds_dic %>% dplyr::count(estable) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "estable")
colnames(estable) = c("categories", "n", "perc", "variable")

arl = ds_dic %>% dplyr::count(arl) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "arl")
colnames(arl) = c("categories", "n", "perc", "variable")

caja = ds_dic %>% dplyr::count(caja) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "caja")
colnames(caja) = c("categories", "n", "perc", "variable")

mas_h = ds_dic %>% dplyr::count(mas_h) %>%
  mutate(perc = prop.table(n)*100) %>% mutate(variable = "mas_h")
colnames(mas_h) = c("categories", "n", "perc", "variable")

plot_dic <- rbind(estable, arl, caja, mas_h)

# Graficar proporción

figure_2 <- ggplot(plot_dic, aes(x = " ", n,
                                 fill = categories)) +
  geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) +
  facet_wrap(~variable, scales="free", ncol = 2,
             labeller = labeller(variable = c(`arl` = "Afiliación a ARL",
                                              `caja` = "Afiliación a caja de compensación",
                                              `estable` = "Considera su empleo estable",
                                              `mas_h` = "Quiere trabajar más horas"))) +
  labs(title = " ",
       x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  geom_label(aes(label = paste0(categories, ": ",round(perc,2), "%")),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) + guides(fill="none",
                                           col = "none") + theme_void()

sex_figure <- ggplot(sexo, aes(x = " ", n,
                                fill = categories)) +
  geom_bar(stat = "identity", width = 1) + coord_polar("y", start=0) +
  facet_wrap(~variable, scales="free", ncol = 3,
             labeller = labeller(variable = c(`sexo` = "Sexo al nacer"))) +
  labs(title = " ",
       x = "Categorías", y = " ") + 
  scale_x_discrete(label = function(x) stringr::str_trunc(x, 15)) +
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  geom_label(aes(label = paste0(categories, ": ",round(perc,2), "%")),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) + guides(fill="none",
                                           col = "none") + theme_void()

plot_dic2 <- ggarrange(sex_figure, figure_2, ncol = 2, nrow = 1)

saveRDS(plot_dic2, "C:/Users/Portatil/Desktop/Curso_EDA_2024_I/Módulos/Módulo 2/Figuras/plot_dic.rds")

#-----------------#
# Otros gráficos  #
#-----------------#

# Pie chart
ggplot(edu, aes(x="", y=n, fill=categories)) +
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
  xlim(c(0.2, h_size + 0.5)) + theme_void()+
  geom_label(aes(label = round(perc,2)),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) 

# Treemap
ggplot(edu, aes(area = n,
                         fill = n, label = categories)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) + scale_fill_viridis_c()

#--------------------------------------------------#
# Variables continuas: histogramas y frecuencias   #
#--------------------------------------------------#
# Para examinar una variable continua, usamos histogramas
ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = ingreso/1000, col = ingreso),
                 fill = "lightskyblue", col = "black",
                 binwidth = 400) + xlim(c(0, 15000)) + theme_bw()

# Manualmente, los datos del histograma se calculan con cut_width
count = dataset %>% count(cut_width(ingreso/1000, 1000, boundary = 0),
                          name = "n") 
colnames(count) = c("Interval", "n")
count = gt(count)
count

# Lo mismo puede ser aplicado para las demás variables cuantitativas
hist_ds <- dataset[c("id", "edad",
                     "horas_semana", "t_actual",
                     "t_viaje")]

hist_melt <- melt(hist_ds)

figure_3 <- ggplot(data = hist_melt, aes(x = value,
                                         fill = variable)) + geom_histogram(bins = 15,
                                                                            col = "black") + 
  facet_wrap(~variable, scales = "free",
             labeller = labeller(variable = c(`edad` = "Edad",
                                              `horas_semana` = "Horas trabajadas (semana)",
                                              `t_actual` = "Tiempo (empleo actual)",
                                              `t_viaje` = "Tiempo de viaje"))) + guides(fill = "none") + theme_bw() +
  labs(x = " ")

#---------------------------------------------------------------#
# Funciones de densidad y funciones de distribución acumulada   #
#---------------------------------------------------------------#

# La función de densidad representa la distribución de la variable numérica
# Es una versión suavizada del histograma.
ggplot(data = dataset,
       aes(x = ingreso)) + geom_density() 

# Ajustar parámetros
ggplot(data = dataset, aes(x = edad)) + 
  geom_density(color = "black",
               alpha = 0.2, fill = "gray45") +
  theme_bw() + labs(x = "Edad", y = "Density",
                    title = "Edad (años cumplidos)")

# La función empírica de distribución acumulativa (ECDF) muestra
# una forma alternativa de representar la distribución
ggplot(data = dataset,
       aes(x = edad)) + stat_ecdf(geom = "step")

# Ajustar parámetros
ggplot(data = dataset,
       aes(x = edad)) + stat_ecdf(geom = "step",
                                    color = "gray45") +
  theme_bw() + labs(x = "Edad", y = "ECDF",
                    title = "Edad (años cumplidos)")


#---------------------#
# Diagramas de caja   #
#---------------------#

############ esto es sólo para construir la figura #############################
# Vertical box plot by group
x1 = ggplot(dataset, aes(x = "", y = ingreso)) + 
  geom_boxplot(outliers = F, col = "white",
               fill = "white") +
  geom_jitter(alpha = 0.3, size = 1,
              col = "black") + ylim(c(0,35)) + theme_classic() 

# Density function (q1, q2, q3)
library(ggridges)
x2 = ggplot(dataset, aes(x = ingreso, y = 1)) +
  geom_density_ridges(jittered_points = TRUE, col = "black", alpha = 0.3,
                      fill = "white") +
  coord_flip() + theme_classic() +
  geom_vline(xintercept = quantile(dataset$ingreso, 0.25, na.rm = T),
             col = "black") +
  geom_vline(xintercept = quantile(dataset$ingreso, 0.5, na.rm = T),
             col = "black") +
  geom_vline(xintercept = quantile(dataset$ingreso, 0.75, na.rm = T),
             col = "black") + xlim(c(0,35))

# Box plot
x3 = ggplot(dataset, aes(x = "", y = ingreso)) + 
  stat_boxplot(geom ='errorbar', col = "black")+
  geom_boxplot(outliers = T, col = "black",
               fill = "white") + theme_classic() 

ggarrange(x1,x2,x3, ncol = 3, nrow = 1)
############ Fin del paréntesis Fin del paréntesis #############################

# Crear boxplot
ggplot(dataset, aes(x = "", y = ingreso)) +
  geom_boxplot()

# Ajustar parámetros
ggplot(dataset, aes(x = "", y = ingreso)) + 
  stat_boxplot(geom ='errorbar', col = "black")+
  geom_boxplot(outliers = T, col = "black",
               fill = "white") + theme_classic() + ylim(c(0,35)) + coord_flip()


# Se pueden mostrar diferenciados según grupos
ggplot(dataset, aes(x = sexo, y = ingreso, fill = soph)) + 
  stat_boxplot(geom ='errorbar', col = "black")+
  geom_boxplot(outliers = T, col = "black") + theme_classic() + 
  ylim(c(0,35))


#-----------------------------------------------------#
# Resumen general: histograma, boxplot y scatter plot #
#-----------------------------------------------------#
library(StatDA)


me = mean(dataset$edad)
sd = sd(dataset$edad)

StatDA::edaplot(dataset$edad, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(0,100, by = 4),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(1, 91), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram, Density Plot, Scatterplot,
	and Boxplot of Rate",
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


#-------------------------#
# Normalidad univariada   #
#-------------------------#

# Analizar la normalidad para una variable
library(car)
par(mfrow = c(2,3))
qqPlot(dataset$ingreso/1000, main = "Ingreso laboral (miles $)", ylab  = " ")
qqPlot(dataset$edad, main = "Edad (años)", ylab = " ")
qqPlot(dataset$horas_semana, main = "Horas trabajadas (semana)", ylab = " ")
qqPlot(dataset$t_actual, main = "Tiempo en el trabajo actual", ylab = " ")
qqPlot(dataset$t_viaje, main = "Tiempo de desplazamiento", ylab = " ")

# Analizar la normalidad de las variables continuas
library(nortest) # Anderson-Darling, Kolmogorov-Smirnov (Lilliefors)



############################## Paréntesis: esto es únicamente para la presentación ######################################
nortest_function <- function(x, y){
  if (is.function(y)) {
    w <- x[!is.na(x)]
    return(y(w)$p.value)
  } else {return("Y no es una función")}
}


ad <- dataset %>% dplyr::select(ingreso,
                                    edad, horas_semana,
                                    t_actual, t_viaje) %>% 
  summarise(across(c("ingreso",
                     "edad", "horas_semana",
                     "t_actual", "t_viaje"),
                   ~ nortest_function(.x, ad.test))) %>% t() 

li <- dataset %>% dplyr::select(ingreso,
                                    edad, horas_semana,
                                    t_actual, t_viaje) %>% 
  summarise(across(c("ingreso",
                     "edad", "horas_semana",
                     "t_actual", "t_viaje"),
                   ~ nortest_function(.x, lillie.test))) %>% t()  

pearson <- dataset %>% dplyr::select(ingreso,
                                         edad, horas_semana,
                                         t_actual, t_viaje) %>% 
  summarise(across(c("ingreso",
                     "edad", "horas_semana",
                     "t_actual", "t_viaje"),
                   ~ nortest_function(.x, pearson.test))) %>% t() 



df_summary <- data.frame(variable = rownames(ad),
                         ad_test = round(ad, 6),
                         li_test = round(li,6),
                         pearson_test = round(pearson, 6))

gt(df_summary)

######################################### Fin del paréntesis #########################################


ad.test(dataset$ingreso)      
lillie.test(dataset$ingreso)   
pearson.test(dataset$ingreso)  
sf.test(dataset$ingreso)       
shapiro.test(dataset$ingreso) 


# Examinar el efecto de las transformaciones
# Consideremos tres transformaciones: log, sqr y cuberoot
windows()
par(mfrow = c(2,2))
qqPlot(new_dataset$priGPA, ylab = "pri GPA", main  = "Base")
qqPlot(log(new_dataset$priGPA), ylab = "pri GPA", main = "Log transformation ")
qqPlot((new_dataset$priGPA)^(1/2), ylab = "pri GPA", main = "Square root transformation")
qqPlot((new_dataset$priGPA)^(1/3), ylab = "pri GPA", main = "Cubic root transformation")

# Examinemos el cambio en las distribuciones
windows()
par(mfrow = c(4,1))
boxplot(new_dataset$priGPA, horizontal=TRUE)
boxplot(log(new_dataset$priGPA), horizontal=TRUE)
boxplot(new_dataset$priGPA^(1/2), horizontal=TRUE)
boxplot(new_dataset$priGPA^(1/3), horizontal=TRUE)

#--------------------------------#
# Resumen: variables continuas   #
#--------------------------------#

# Resumen descriptivo total
library(janitor)

dataset$ingreso2 = dataset$ingreso/1000

# Definición de función Q2(Q1 - Q3)
quantile_f <- function(x){
  q1 <- quantile(x, na.rm = T, 0.25)
  q2 <- quantile(x, na.rm = T, 0.5)
  q3 <- quantile(x, na.rm = T, 0.75)
  y <- paste0(round(as.numeric(q2), 1), " (",
              round(as.numeric(q1), 2), " - ",
              round(as.numeric(q3), 2), ")")
  return(y)
}

total <- dataset %>% dplyr::select(ingreso2,
                                    edad, horas_semana,
                                    t_actual, t_viaje) %>% 
  summarise(across(everything(),
                   ~ quantile_f(.x)))

# Resumen descriptivo por grupos

q_group <- dataset %>%
  group_by(area) %>%
  summarise(across(c("ingreso2",
                     "edad", "horas_semana",
                     "t_actual", "t_viaje"),
                   list(sum = quantile_f))) %>% as.data.frame() 


# Ejercicio: hacer lo mismo para la media y la desviación estándar



#--------------------#
# Valores atípicos   #------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------#
# Detección de valores atípicos (outliers)
# Los valores atípicos pueden, o bien ser errores en el ingreso de la información, o bien
# puede proporcionar información útil

# Se propone el mismo ejercicio con otra variable: edad
ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = horas_semana, col = edad),
                 fill = "lightskyblue", col = "black",
                 binwidth = 2) + theme_bw() 

# El diagrama de caja sirve para identificar outliers
boxplot(dataset$horas_semana, horizontal = T)

# Criterio IQR para la detección de outliers
# Cualquier observación fuera de [q0.25 - 1.5IQR, q0.75 + 1.5IQR]
# La siguiente función permite la detección de outliers
boxplot.stats(dataset$horas_semana)$out

# Lo mismo puede ser obtenido manualmente
Q1 <- quantile(dataset$horas_semana, .25, na.rm = T)
Q3 <- quantile(dataset$horas_semana, .75, na.rm = T)
IQR <- IQR(dataset$horas_semana, na.rm = T)

outliers <- dataset %>% filter(horas_semana<(Q1 - 1.5*IQR) | horas_semana>(Q3 + 1.5*IQR))

# Comparación
length(outliers$horas_semana)
length(boxplot.stats(dataset$horas_semana)$out)

# Identificación de los outliers en la gráfica
plot(dataset$horas_semana, type='p',
     col=ifelse(dataset$horas_semana %in% outliers$horas_semana, "red", "black"),
     pch = ifelse(dataset$horas_semana %in% outliers$horas_semana, 17, 1),
     ylim = c(0, 100))


# El problema de la identificación de valores atípicos es la definición de umbrales
# Véase el criterio de Hair et al. (1999), por ejemplo:

# Estandarizar la variable
z <- data.frame(id = seq(1, nrow(dataset), by = 1),
                x = dataset$horas_semana,
                z = scale(dataset$horas_semana))

outliers1 <- z  %>% filter(abs(z) > 2.5)
outliers2 <- z  %>% filter(abs(z) > 3)

par(mfrow = c(1,2))
plot(dataset$horas_semana, type='p',
     col=ifelse(dataset$horas_semana %in% outliers1$x, "red", "black"),
     pch = ifelse(dataset$horas_semana %in% outliers1$x, 17, 16))
plot(dataset$horas_semana, type='p',
     col=ifelse(dataset$horas_semana %in% outliers2$x, "red", "black"),
     pch = ifelse(dataset$horas_semana %in% outliers2$x, 17, 16))


#-------------------------------------------#
# Ejercicio: Valores atípicos en ingresos   #------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------#

# Concentrémonos en los ingresos de Cali

cali <- dataset %>% filter(area == "Cali")

# Un histograma puede dar algunas luces
ggplot(data = cali) +
  geom_histogram(mapping = aes(x = ingreso2, col = ingreso2),
                 fill = "lightskyblue", col = "black",
                 binwidth = 600) + theme_bw() + xlim(c(0, 20000))

# El diagrama de caja sirve para identificar outliers
boxplot(cali$ingreso2, horizontal = T)

# Criterio IQR para la detección de outliers
# Cualquier observación fuera de [q0.25 - 1.5IQR, q0.75 + 1.5IQR]
# La siguiente función permite la detección de outliers
boxplot.stats(cali$ingreso)$out

# Lo mismo puede ser obtenido manualmente
Q1 <- quantile(cali$ingreso2, .25, na.rm = T)
Q3 <- quantile(cali$ingreso2, .75, na.rm = T)
IQR <- IQR(cali$ingreso2, na.rm = T)

outliers <- cali %>% filter(ingreso2<(Q1 - 1.5*IQR) | ingreso2>(Q3 + 1.5*IQR))

# Comparación
length(outliers$ingreso2)
length(boxplot.stats(cali$ingreso2)$out)

# Identificación de los outliers en la gráfica
plot(cali$ingreso2, type='p',
     col=ifelse(cali$ingreso2 %in% outliers$ingreso2, "red", "black"),
     pch = ifelse(cali$ingreso2 %in% outliers$ingreso2, 17, 1),
     ylim = c(0, 20000))


# El problema de la identificación de valores atípicos es la definición de umbrales
# Véase el criterio de Hair et al. (1999), por ejemplo:

# Estandarizar la variable
z <- data.frame(id = seq(1, nrow(cali), by = 1),
                x = cali$ingreso2,
                z = scale(cali$ingreso2))

outliers1 <- z  %>% filter(abs(z) > 2.5)
outliers2 <- z  %>% filter(abs(z) > 3)

par(mfrow = c(1,2))
plot(cali$ingreso2, type='p',
     col=ifelse(cali$ingreso2 %in% outliers1$x, "red", "black"),
     pch = ifelse(cali$ingreso2 %in% outliers1$x, 17, 16))
plot(cali$ingreso2, type='p',
     col=ifelse(cali$ingreso2 %in% outliers2$x, "red", "black"),
     pch = ifelse(cali$ingreso2 %in% outliers2$x, 17, 16))



#---------------------------------------------------------------#
# Comparación de las distribuciones: outliers vs. no-outliers   #------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------#

# Comparación de las distribuciones

# Variables continuas
dataset_continuas = dataset[c("ingreso2",
                                  "edad", "horas_semana",
                                  "t_actual", "t_viaje")]
dataset_continuas$group = "outliers"

limits <- list(ingreso2 = c(0, 5000),
               edad = c(0,100),
               horas_semana = c(20, 80),
               t_actual = c(0,250),
               t_viaje = c(0,100))

# Estandarización de las variables
# Función para estandarización
plot_list = list()
length(plot_list) = 5

for (k in 1:5) {
  df_1 = df_2 = dataset_continuas[,k]
  df_2$z = as.vector(scale(df_1[,1]))
  
  df_2 = df_2 %>% filter(abs(z) <= 4)
  
  df_1$group = "outliers"
  df_2$group = "no-outliers"
  
  df_2 = df_2[,c(1,3)]
  colnames(df_2) = c(colnames(df_1)[1], "group")
  
  df = rbind(df_1, df_2)
  colnames(df) = c("x", "group")

  plot_list[[k]] =  ggplot(df, aes(x=group,
                                     y=x, fill=group)) + 
    geom_boxplot() + coord_flip() +
    guides(fill = "none")
}

ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
       plot_list[[4]], plot_list[[5]], ncol = 1, nrow = 5)

#-------------------------------------------------------------------#
# Normalidad de las variables continuas: outliers vs. no-outliers   #------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------#

outliers_df <- data.frame(variable = rownames(ad),
                         outliers_ad = round(ad, 4),
                         outliers_lillie = round(li,4),
                         outliers_pearson = round(pearson, 4))
no_outliers_df <- data.frame(variable = rownames(ad),
                             no.outliers_ad = round(ad, 4),
                             no.outliers_lillie = round(li,4),
                             no.outliers_pearson = round(pearson, 4))

for (k in 1:5) {
  df_1 = df_2 = dataset_continuas[,k]
  df_2$z = as.vector(scale(df_1[,1]))
  
  df_2 = df_2 %>% filter(abs(z) <= 2.5)
  
  df_1$group = "outliers"
  df_2$group = "no-outliers"
  
  df_2 = df_2[,c(1,3)]
  colnames(df_2) = c(colnames(df_1)[1], "group")
  
  outliers_df$variable[k] = names(df_1)[1]
  outliers_df$outliers_ad[k] = round(ad.test(as.vector(df_1[,1])[[1]])$p.value, 3)
  outliers_df$outliers_lillie[k] = round(lillie.test(as.vector(df_1[,1])[[1]])$p.value, 3)
  outliers_df$outliers_pearson[k] = round(pearson.test(as.vector(df_1[,1])[[1]])$p.value, 3)
  
  no_outliers_df$variable[k] = names(df_1)[1]
  no_outliers_df$no.outliers_ad[k] = round(ad.test(as.vector(df_2[,1])[[1]])$p.value, 3)
  no_outliers_df$no.outliers_lillie[k] = round(lillie.test(as.vector(df_2[,1])[[1]])$p.value, 3)
  no_outliers_df$no.outliers_pearson[k] = round(pearson.test(as.vector(df_2[,1])[[1]])$p.value, 3)
 
}

summary_out <- merge(outliers_df, no_outliers_df)

gt(summary_out, rowname_col = "variable") %>% 
  tab_spanner_delim(
    delim = "_"
  ) 

#----------------------------------------------------------------#
# Resumen de las variables continuas: outliers vs. no-outliers   #------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------#

# Resumen descriptivo total
outliers_summary <- dataset %>% dplyr::select(ingreso2,
                                              edad, horas_semana,
                                              t_actual, t_viaje) %>% 
  summarise(across(everything(),
                   ~ quantile_f(.x))) %>% t()

out_summary <- data.frame(Variable = rownames(outliers_summary),
                          Total = outliers_summary[,1])


no.outliers_summary <- out_summary

for (k in 1:5) {
  df_1 = df_2 = dataset_continuas[,k]
  df_2$z = as.vector(scale(df_1[,1]))
  
  df_2 = df_2 %>% filter(abs(z) <= 2.5)
  
  df_1$group = "outliers"
  df_2$group = "no-outliers"
  
  df_2 = df_2[,c(1,3)]
  colnames(df_2) = c(colnames(df_1)[1], "group")
  
  q1 = quantile(as.vector(df_2[,1])[[1]], na.rm = T, 0.25)
  q2 = quantile(as.vector(df_2[,1])[[1]], na.rm = T, 0.5)
  q3 = quantile(as.vector(df_2[,1])[[1]], na.rm = T, 0.75)
  
  no.outliers_summary$Variable[k] = names(df_1)[1]
  no.outliers_summary$Total[k] = paste0(round(q2, 2), " (",
                                        round(q1, 2), " - ",
                                        round(q3, 2), ")")
}


colnames(out_summary) = c("Variable", "Baseline")
colnames(no.outliers_summary) = c("Variable", "No.Outliers")
whole_summary = merge(out_summary,
                      no.outliers_summary, by = "Variable")

gt(whole_summary, rowname_col = "Variable")

