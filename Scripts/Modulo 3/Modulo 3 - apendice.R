
################################################
################################################
## Módulo III: Análisis exploratorio de datos ##
################################################
################################################

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

###########################################################
###########################################################
## Pendiente1 : Valores atípicos: detección multivariada ##
###########################################################
###########################################################

##########################################################
##########################################################
## Pendiente2: Valores atípicos: detección multivariada ##
##########################################################
##########################################################



