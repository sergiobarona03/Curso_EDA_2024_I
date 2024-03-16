
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

#----------------------#
# Cargar base de datos #-------------------------------------------------------------------------------------------------------------------------
#----------------------#
setwd("C:/Users/PC/Desktop/Curso_EDA_2024_I")
dataset <- readxl::read_excel("Datos/attend.xlsx")

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

gt_tbl

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

#--------------------------------------------------#
# Variables continuas: histogramas y frecuencias   #
#--------------------------------------------------#
# Para examinar una variable continua, usamos histogramas
ggplot(data = new_dataset) +
  geom_histogram(mapping = aes(x = attend, col = attend),
                 fill = "lightskyblue", col = "black",
                 binwidth = 2) + theme_bw()

# Manualmente, los datos del histograma se calculan con cut_width
count = new_dataset %>% count(cut_width(attend, 0.8),
                              name = "n") %>% arrange(desc(n))
colnames(count) = c("Interval", "n")
count = gt(count)
count

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



#---------------------------------------------------------------#
# Funciones de densidad y funciones de distribución acumulada   #
#---------------------------------------------------------------#

# La función de densidad representa la distribución de la variable numérica
# Es una versión suavizada del histograma.
ggplot(data = new_dataset,
       aes(x = attend)) + geom_density()

# Ajustar parámetros
ggplot(data = new_dataset, aes(x = attend)) + 
  geom_density(color = "black",
               alpha = 0.2, fill = "gray45") +
  theme_bw() + labs(x = "Attend", y = "Density",
                    title = "Distribución de los puntajes del examen")

# La función empírica de distribución acumulativa (ECDF) muestra
# una forma alternativa de representar la distribución
ggplot(data = new_dataset,
       aes(x = attend)) + stat_ecdf(geom = "step")

# Ajustar parámetros
ggplot(data = new_dataset,
       aes(x = attend)) + stat_ecdf(geom = "step",
                                    color = "gray45") +
  theme_bw() + labs(x = "Attend", y = "ECDF",
                    title = "Attend ECDF")


#---------------------#
# Diagramas de caja   #
#---------------------#

############ esto es sólo para construir la figura #############################
# Vertical box plot by group
x1 = ggplot(new_dataset, aes(x = "", y = attend)) + 
  geom_boxplot(outliers = F, col = "white",
               fill = "white") +
  geom_jitter(alpha = 0.3, size = 1,
              col = "black") + ylim(c(0,35)) + theme_classic() 

# Density function (q1, q2, q3)
library(ggridges)
x2 = ggplot(new_dataset, aes(x = attend, y = 1)) +
  geom_density_ridges(jittered_points = TRUE, col = "black", alpha = 0.3,
                      fill = "white") +
  coord_flip() + theme_classic() +
  geom_vline(xintercept = quantile(new_dataset$attend, 0.25),
             col = "black") +
  geom_vline(xintercept = quantile(new_dataset$attend, 0.5),
             col = "black") +
  geom_vline(xintercept = quantile(new_dataset$attend, 0.75),
             col = "black") + xlim(c(0,35))

# Box plot
x3 = ggplot(new_dataset, aes(x = "", y = attend)) + 
  stat_boxplot(geom ='errorbar', col = "black")+
  geom_boxplot(outliers = T, col = "black",
               fill = "white") + theme_classic() + ylim(c(0,35))

ggarrange(x1,x2,x3, ncol = 3, nrow = 1)
############ Fin del paréntesis Fin del paréntesis #############################

# Crear boxplot
ggplot(new_dataset, aes(x = "", y = attend)) +
  geom_boxplot()

# Ajustar parámetros
ggplot(new_dataset, aes(x = "", y = attend)) + 
  stat_boxplot(geom ='errorbar', col = "black")+
  geom_boxplot(outliers = T, col = "black",
               fill = "white") + theme_classic() + ylim(c(0,35)) + coord_flip()


# Se pueden mostrar diferenciados según grupos
ggplot(new_dataset, aes(x = soph, y = attend, fill = soph)) + 
  stat_boxplot(geom ='errorbar', col = "black")+
  geom_boxplot(outliers = T, col = "black") + theme_classic() + 
  ylim(c(0,35))


#-----------------------------------------------------#
# Resumen general: histograma, boxplot y scatter plot #
#-----------------------------------------------------#
library(StatDA)
me = mean(new_dataset$attend)
sd = sd(new_dataset$attend)

par(mfrow = c(1,1))
StatDA::edaplot(new_dataset$attend, scatter=TRUE, H.freq=FALSE, box=TRUE, 
                H.breaks=seq(0,32, by = 1),
                H.col="lightgray", H.border=TRUE, H.labels=FALSE,
                S.pch=1, S.col="blue", S.cex=0.5,
                D.lwd=2, D.lty=1, D.plot=FALSE,
                P.xlim=c(0, 32), P.cex.lab =1.2,
                P.log=FALSE, P.main="Histogram, Density Plot, Scatterplot,
	and Boxplot of Rate",
                P.xlab="In", P.plot=TRUE,
                P.ylab="Density",
                B.pch=1,B.cex=0.5, B.col="red")
lines(density(new_dataset$attend), lwd=2, col='blue')
curve(dnorm(x, mean=me, sd=sd), from=0, to=32, add=T,
      col='red', lwd=3)
leg.txt <- c(paste0("Min. = ", round(min(new_dataset$attend),4)),
             paste0("Max. = ", round(max(new_dataset$attend),4)),
             paste0("Mean = ", round(mean(new_dataset$attend),4)),
             paste0("Median = ", round(median(new_dataset$attend),4)),
             paste0("Std. dev. = ", round(sd(new_dataset$attend),4)),
             paste0("Kurtosis = ", round(kurtosis(new_dataset$attend),4)),
             paste0("Skewness = ", round(skewness(new_dataset$attend),4)))
legend (x=0, y=0.08, bty="n", leg.txt)


#--------------------------------#
# Resumen: variables continuas   #
#--------------------------------#

# Resumen descriptivo total
q1 <- new_dataset %>% dplyr::select(attend,
                                     priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>% 
  summarise(across(everything(),
                   ~ quantile(.x, na.rm = T, 0.25))) %>% t() 
q2 <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>%  
  summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>% t()

q3 <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>%  
  summarise(across(everything(), ~ quantile(.x, na.rm = T, 0.75))) %>% t()

total <- data.frame(Variable = rownames(q1),
                    Total = paste0(round(q2, 2), " (",
                                   round(q1, 2), " - ",
                                   round(q3, 2), ")"))

# Resumen descriptivo por grupos
q1_group <- new_dataset %>%
  group_by(soph) %>%
  summarise(across(c("attend",
                     "priGPA", "termGPA",
                     "ACT", "final",
                     "atndrte",
                     "hwrte", "stndfnl"),
                   ~ quantile(.x, na.rm = T, 0.25))) %>% t() 

q2_group <- new_dataset %>%
  group_by(soph) %>%
  summarise(across(c("attend",
                     "priGPA", "termGPA",
                     "ACT", "final",
                     "atndrte",
                     "hwrte", "stndfnl"),
                   ~ median(.x, na.rm = TRUE))) %>% t() 

q3_group <- new_dataset %>%
  group_by(soph) %>%
  summarise(across(c("attend",
                     "priGPA", "termGPA",
                     "ACT", "final",
                     "atndrte",
                     "hwrte", "stndfnl"),
                   ~ quantile(.x, na.rm = T, 0.75))) %>% t() 

group <- data.frame(soph_0 = paste0(round(as.numeric(q2_group[-1,1]), 1), " (",
                                    round(as.numeric(q1_group[-1,1]), 2), " - ",
                                    round(as.numeric(q3_group[-1,1]), 2), ")"),
                    soph_1 = paste0(round(as.numeric(q2_group[-1,2]), 1), " (",
                                    round(as.numeric(q1_group[-1,2]), 2), " - ",
                                    round(as.numeric(q3_group[-1,2]), 2), ")"))

summary <- cbind(total,group) %>% gt()
summary

# Ejercicio: hacer lo mismo para la media y la desviación estándar
mean <- new_dataset %>% dplyr::select(attend,
                                      priGPA, termGPA) %>% 
  summarise(across(everything(),
                   ~ mean(.x, na.rm = TRUE))) %>% t() 
sd <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA) %>%  
  summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) %>% t()

total <- data.frame(Variable = rownames(mean),
                    Total = paste0(round(mean, 2), " (",
                                   round(sd, 2), ")"))

# Resumen descriptivo por grupos
mean_group <- new_dataset %>%
  group_by(soph) %>%
  summarise(across(c("attend", "priGPA", "termGPA"),
                   ~ mean(.x, na.rm = TRUE))) %>% t() 
sd_group <- new_dataset %>%
  group_by(soph) %>%
  summarise(across(c("attend", "priGPA", "termGPA"),
                   ~ sd(.x, na.rm = TRUE))) %>% t() 

group <- data.frame(soph_0 = paste0(round(as.numeric(mean_group[-1,1]),1), " (",
                                    round(as.numeric(sd_group[-1,1]),1),")"),
                    soph_1 = paste0(round(as.numeric(mean_group[-1,2]),1)," (",
                                    round(as.numeric(sd_group[-1,2]),1), ")"))

summary <- cbind(total,group) %>% gt()
summary

#-------------------------#
# Normalidad univariada   #
#-------------------------#

# Analizar la normalidad para una variable
library(car)
qqPlot(new_dataset$termGPA, ylab = "Term GPA")

# Analizar la normalidad de las variables continuas
library(nortest) # Anderson-Darling, Kolmogorov-Smirnov (Lilliefors)



############################## Paréntesis: esto es únicamente para la presentación ######################################
nortest_function <- function(x, y){
  if (is.function(y)) {
    return(y(x)$p.value)
  } else {return("Y no es una función")}
}


ad <- new_dataset %>% dplyr::select(attend,
                                      priGPA, termGPA,
                                      ACT, final,
                                      atndrte,
                                      hwrte, stndfnl) %>% 
  summarise(across(c("attend",
                     "priGPA", "termGPA",
                     "ACT", "final",
                     "atndrte",
                     "hwrte", "stndfnl"),
                   ~ nortest_function(.x, ad.test))) %>% t() 
li <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>% 
  summarise(across(c("attend",
                     "priGPA", "termGPA",
                     "ACT", "final",
                     "atndrte",
                     "hwrte", "stndfnl"),
                   ~ nortest_function(.x, lillie.test))) %>% t()  

pearson <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>% 
  summarise(across(c("attend",
                     "priGPA", "termGPA",
                     "ACT", "final",
                     "atndrte",
                     "hwrte", "stndfnl"),
                   ~ nortest_function(.x, pearson.test))) %>% t() 

sf <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>% 
  summarise(across(c("attend",
                     "priGPA", "termGPA",
                     "ACT", "final",
                     "atndrte",
                     "hwrte", "stndfnl"),
                   ~ nortest_function(.x, sf.test))) %>% t() 

sw <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>% 
  summarise(across(c("attend",
                     "priGPA", "termGPA",
                     "ACT", "final",
                     "atndrte",
                     "hwrte", "stndfnl"),
                   ~ nortest_function(.x, shapiro.test))) %>% t() 

df_summary <- data.frame(variable = rownames(ad),
                         ad_test = round(ad, 4),
                         li_test = round(li,4),
                         pearson_test = round(pearson, 4),
                         sw_test = round(sw, 4),
                         sf_test = round(sf, 4))

gt(df_summary)

######################################### Fin del paréntesis #########################################


ad.test(new_dataset$attend)      
lillie.test(new_dataset$attend)   
pearson.test(new_dataset$attend)  
sf.test(new_dataset$attend)       
shapiro.test(new_dataset$attend) 


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

#--------------------#
# Valores atípicos   #------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------#
# Detección de valores atípicos (outliers)
# Los valores atípicos pueden, o bien ser errores en el ingreso de la información, o bien
# puede proporcionar información útil

# Un histograma puede dar algunas luces
ggplot(data = new_dataset) +
  geom_histogram(mapping = aes(x = priGPA, col = priGPA),
                 fill = "lightskyblue", col = "black",
                 binwidth = .2) + theme_bw()

# El diagrama de caja sirve para identificar outliers
boxplot(new_dataset$priGPA, horizontal = T)

# Criterio IQR para la detección de outliers
# Cualquier observación fuera de [q0.25 - 1.5IQR, q0.75 + 1.5IQR]
# La siguiente función permite la detección de outliers
boxplot.stats(new_dataset$priGPA)$out

# Lo mismo puede ser obtenido manualmente
Q1 <- quantile(new_dataset$priGPA, .25)
Q3 <- quantile(new_dataset$priGPA, .75)
IQR <- IQR(new_dataset$priGPA)

outliers <- new_dataset %>% filter(priGPA<(Q1 - 1.5*IQR) | priGPA>(Q3 + 1.5*IQR))

# Comparación
outliers$priGPA
boxplot.stats(new_dataset$priGPA)$out

# Identificación de los outliers en la gráfica
plot(new_dataset$priGPA, type='p',
     col=ifelse(new_dataset$priGPA==outliers$priGPA, "red", "black"),
     pch = ifelse(new_dataset$priGPA==outliers$priGPA, 17, 16))


# El problema de la identificación de valores atípicos es la definición de umbrales
# Véase el criterio de Hair et al. (1999), por ejemplo:

# Estandarizar la variable
z <- data.frame(id = seq(1, nrow(new_dataset), by = 1),
                x = new_dataset$priGPA,
                z = scale(new_dataset$priGPA))

outliers1 <- z  %>% filter(abs(z) > 2.5)
outliers2 <- z  %>% filter(abs(z) > 3)

par(mfrow = c(1,2))
plot(new_dataset$priGPA, type='p',
     col=ifelse(new_dataset$priGPA %in% outliers1$x, "red", "black"),
     pch = ifelse(new_dataset$priGPA %in% outliers1$x, 17, 16))
plot(new_dataset$priGPA, type='p',
     col=ifelse(new_dataset$priGPA %in% outliers2$x, "red", "black"),
     pch = ifelse(new_dataset$priGPA %in% outliers2$x, 17, 16))


#---------------------------------------------------------------#
# Comparación de las distribuciones: outliers vs. no-outliers   #------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------#

# Comparación de las distribuciones

# Variables continuas
dataset_continuas = new_dataset[c("attend",
                                  "priGPA", "termGPA",
                                  "ACT", "final",
                                  "atndrte",
                                  "hwrte", "stndfnl")]
dataset_continuas$group = "outliers"
# Estandarización de las variables
# Función para estandarización
plot_list = list()
length(plot_list) = 8

for (k in 1:8) {
  df_1 = df_2 = dataset_continuas[,k]
  df_2$z = as.vector(scale(df_1[,1]))
  
  df_2 = df_2 %>% filter(abs(z) <= 2.5)
  
  df_1$group = "outliers"
  df_2$group = "no-outliers"
  
  df_2 = df_2[,c(1,3)]
  colnames(df_2) = c(colnames(df_1)[1], "group")
  
  df = rbind(df_1, df_2)
  colnames(df) = c("x", "group")
  plot_list[[k]] = ggplot(data=df, aes(x=x, group=group, fill=group)) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_classic() + theme(legend.position = c(0.2, 0.8),
                            legend.title=element_blank()) +
    labs(x = colnames(dataset_continuas[,k]))
}

ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]],
       plot_list[[4]], plot_list[[5]], plot_list[[6]],
       plot_list[[7]], plot_list[[8]], ncol = 4, nrow = 2)

#-------------------------------------------------------------------#
# Normalidad de las variables continuas: outliers vs. no-outliers   #------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------#

outliers_df <- data.frame(variable = rownames(ad),
                         outliers_ad = round(ad, 4),
                         outliers_lillie = round(li,4),
                         outliers_pearson = round(pearson, 4),
                         outliers_sw = round(sw, 4),
                         outliers_sf = round(sf, 4))
no_outliers_df <- data.frame(variable = rownames(ad),
                             no.outliers_ad = round(ad, 4),
                             no.outliers_lillie = round(li,4),
                             no.outliers_pearson = round(pearson, 4),
                             no.outliers_sw = round(sw, 4),
                             no.outliers_sf = round(sf, 4))

for (k in 1:8) {
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
  outliers_df$outliers_sw[k] = round(shapiro.test(as.vector(df_1[,1])[[1]])$p.value, 3)
  outliers_df$outliers_sf[k] = round(sf.test(as.vector(df_1[,1])[[1]])$p.value, 3)
  
  no_outliers_df$variable[k] = names(df_1)[1]
  no_outliers_df$no.outliers_ad[k] = round(ad.test(as.vector(df_2[,1])[[1]])$p.value, 3)
  no_outliers_df$no.outliers_lillie[k] = round(lillie.test(as.vector(df_2[,1])[[1]])$p.value, 3)
  no_outliers_df$no.outliers_pearson[k] = round(pearson.test(as.vector(df_2[,1])[[1]])$p.value, 3)
  no_outliers_df$no.outliers_sw[k] = round(shapiro.test(as.vector(df_2[,1])[[1]])$p.value, 3)
  no_outliers_df$no.outliers_sf[k] = round(sf.test(as.vector(df_2[,1])[[1]])$p.value, 3)
}

summary_out <- merge(outliers_df, no_outliers_df)

gt(summary_out, rowname_col = "variable") %>% 
  tab_spanner_delim(
    delim = "_"
  ) 

#-------------------------------------------------------------------#
# Normalidad de las variables continuas: outliers vs. no-outliers   #------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------#

# Resumen descriptivo total
q1 <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>% 
  summarise(across(everything(),
                   ~ quantile(.x, na.rm = T, 0.25))) %>% t() 
q2 <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>%  
  summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>% t()

q3 <- new_dataset %>% dplyr::select(attend,
                                    priGPA, termGPA,
                                    ACT, final,
                                    atndrte,
                                    hwrte, stndfnl) %>%  
  summarise(across(everything(), ~ quantile(.x, na.rm = T, 0.75))) %>% t()

outliers_summary <- data.frame(Variable = rownames(q1),
                    Total = paste0(round(q2, 2), " (",
                                   round(q1, 2), " - ",
                                   round(q3, 2), ")"))

no.outliers_summary <- outliers_summary

for (k in 1:8) {
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


colnames(outliers_summary) = c("Variable", "Baseline")
colnames(no.outliers_summary) = c("Variable", "No.Outliers")
whole_summary = merge(outliers_summary,
                      no.outliers_summary, by = "Variable")

gt(whole_summary, rowname_col = "Variable")

