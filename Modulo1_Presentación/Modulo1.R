

##############################################
##############################################
## Módulo 2: análisis exploratorio de datos ##
##############################################
##############################################

library(readxl)
library(dplyr)

dataset = read.csv("SISBEN_2019.csv")


##########################
## Selección de muestra ##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##########################

# Descriptivas de variables continuas y categóricas (población)
cat = c("SEXO", "ESTCIVIL", "ESTRATO", "ASISTE", "GRADO", "NIVEL", "ACTIVI")
cont =  c("INGRESOS", "PUNTAJE", "BUSCANDO")

# Convertir variables en continuas y categóricas, respectivamente
dataset[cat] = lapply(dataset[cat], as.factor)
dataset[cont] = lapply(dataset[cont], as.numeric)

# Summary according to class
summary.pob.cat = summary(dataset[cat])
summary.pob.cont = summary(dataset[cont])

# Sample function (vector)
set.seed(11)
x = seq(1, 1000, by = 1)
rd_x = sample(x, size = 10)

# Sample function (data frame)
set.seed(11)
id_rd = sample(nrow(dataset), size = 1000)
sample.ds = dataset[id_rd,]

# Descriptivas de variables continuas y categóricas (muestra)
summary.sample.cat = summary(sample.ds[cat])
summary.sample.cont = summary(sample.ds[cont])


#######################
## Summary by groups ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################

# Interesados en: Sexo vs. Ingreso
sample.ds.ingresos = sample.ds %>% filter(PERCIBE == 1)
sexo_ingresos = sample.ds.ingresos %>% group_by(SEXO) %>% summarise(mean = mean(INGRESOS),
                                                           sd = sd(INGRESOS),
                                                            median = median(INGRESOS),
                                                           Q1 = quantile(INGRESOS, 0.25),
                                                           Q3 = quantile(INGRESOS, 0.75))

# Interesados en: Estrato vs. Puntaje
estrato_puntaje = sample.ds %>% group_by(ESTRATO) %>% summarise(mean = mean(PUNTAJE),
                                                                    sd = sd(PUNTAJE),
                                                                    median = median(PUNTAJE),
                                                                    Q1 = quantile(PUNTAJE, 0.25),
                                                                    Q3 = quantile(PUNTAJE, 0.75))

# Interesados en Nivel de estudios vs. Ingresos
estudios_ingresos = sample.ds %>% group_by(NIVEL) %>% summarise(mean = mean(INGRESOS),
                                                             sd = sd(INGRESOS),
                                                             median = median(INGRESOS),
                                                             Q1 = quantile(INGRESOS, 0.25),
                                                             Q3 = quantile(INGRESOS, 0.75))


#################################
## Creación de tablas cruzadas ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#################################
# Tabla 1: categórica - categórica (Estrato vs. Nivel de Estudios)
prop.table(table(Estrato = sample.ds$ESTRATO, Estudios = sample.ds$NIVEL))*100


##################
## Histograma I ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##################
# Histograma: base de datos de Ingresos (Distribución multimodal)
sample.ds.ingresos = sample.ds %>% filter(INGRESOS > 0)
hist(sample.ds.ingresos$INGRESOS, breaks = 10, probability = T)

###################
## Histograma II ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###################

# Histograma: base de datos de puntaje
hist(sample.ds$PUNTAJE, breaks = 50)
hist(sample.ds$PUNTAJE, breaks = 50,
     main="Puntaje individual (SISBEN 2019)",
     xlab="Puntaje",
     xlim=c(0,100),
     col="lightskyblue",
     border="black")

# Añadir media y mediana (densidad de probabilidad)
hist(sample.ds$PUNTAJE, breaks = 50,
     main="Puntaje individual (SISBEN 2019)",
     xlab="Puntaje",
     xlim=c(0,100),
     col="lightskyblue",
     border="black",
     probability = TRUE)
abline(v = mean(sample.ds$PUNTAJE), col='red', lwd = 3)
abline(v = median(sample.ds$PUNTAJE), col='green', lwd = 3)
lines(density(sample.ds$PUNTAJE), col = 'black', lwd = 3)


# Comparar con el histograma poblacional
par(mfrow = c(1, 2))

hist(sample.ds$PUNTAJE, breaks = 50,
     main="Puntaje individual (SISBEN 2019 - Muestra)",
     xlab="Puntaje",
     xlim=c(0,100),
     col="lightskyblue",
     border="black",
     probability = TRUE)
abline(v = mean(sample.ds$PUNTAJE), col='red', lwd = 3)
abline(v = median(sample.ds$PUNTAJE), col='green', lwd = 3)
lines(density(sample.ds$PUNTAJE), col = 'black', lwd = 3)

hist(dataset$PUNTAJE, breaks = 50,
     main="Puntaje individual (SISBEN 2019 - Población)",
     xlab="Puntaje",
     xlim=c(0,100),
     col="maroon",
     border="black",
     probability = TRUE)
abline(v = mean(dataset$PUNTAJE), col='red', lwd = 3)
abline(v = median(dataset$PUNTAJE), col='green', lwd = 3)
lines(density(dataset$PUNTAJE), col = 'black', lwd = 3)

###################################################
## 	Empirical cumulative distribution function   ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###################################################
plot(ecdf(sample.ds$PUNTAJE), xlab="Data", ylab="Cumulative Probability",  
     main="Empirical Cumulative Distribution Function (Sample)")

plot(ecdf(dataset$PUNTAJE), xlab="Data", ylab="Cumulative Probability",  
     main="Empirical Cumulative Distribution Function (Population)")

#################################
## QQPlot: normal distribution ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#################################
mean(sample.ds$PUNTAJE < 70)
pnorm(70, mean = mean(sample.ds$PUNTAJE), sd = sd(sample.ds$PUNTAJE))

ps = seq(0.01, 0.99, by = 0.01)
qs = quantile(sample.ds$PUNTAJE, ps)

normalqs = qnorm(ps, mean = mean(sample.ds$PUNTAJE), 
                 sd = sd(sample.ds$PUNTAJE))

par(mfrow = c(1,1))
plot(normalqs, qs, xlab = "Normal percentiles",
      ylab = "Puntaje Sisben 2019")
abline(0,1)

# Procedimiento directo
qqnorm(sample.ds$PUNTAJE)
qqline(sample.ds$PUNTAJE)

# Prueba
mean(sample.ds$PUNTAJE < 5)
pnorm(5, mean = mean(sample.ds$PUNTAJE), sd = sd(sample.ds$PUNTAJE))

#############
## Boxplot ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#############

boxplot(sample.ds$PUNTAJE, horizontal = T,
        xlab = "Puntaje",
        main = "Distribución: puntaje SISBEN (2019)",outline = TRUE,
        col = rgb(0, 0.5, 1, alpha = 0.45),
        boxwex = 0.5, # Ancho de la caja
        boxlty = 1,   # Tipo de línea de la caja
        boxlwd = 2,   # Ancho de línea de la caja
        boxcol = 1   # Color del borde de la caja
        )

#######################
## Boxplot by groups ##--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#######################

sample.ds$ESTRATO = factor(sample.ds$ESTRATO, levels = c(1,2,3),
                           labels = c("bajo-bajo", "bajo","medio-bajo"))
boxplot(PUNTAJE ~ ESTRATO, data = sample.ds,
        ylab = "Puntaje SISBEN",
        xlab = "Estrato socioeconómico",
        main = "Puntaje SISBEN (2019) según estrato",
        col = c("lightblue",
                "skyblue3", 
                "skyblue4"),
        horizontal = T)

# Un resultado análogo se obtiene a partir de histogramas obtenidos según grupos
ses1.sample = sample.ds %>% filter(ESTRATO == "bajo-bajo")
ses2.sample = sample.ds %>% filter(ESTRATO == "bajo")
ses3.sample = sample.ds %>% filter(ESTRATO == "medio-bajo")


plot(density(ses1.sample$PUNTAJE), col = "lightblue",
     main = "Distribución del puntaje SISBEN según estrato (2019)")
lines(density(ses2.sample$PUNTAJE), col = "skyblue3")
lines(density(ses3.sample$PUNTAJE), col = "skyblue4")





