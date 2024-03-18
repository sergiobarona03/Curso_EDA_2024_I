
################################
## Preparación: base de datos ##
################################

library(tidyverse)

# Bases de datos 
Ocupados = read.csv("Datos/Diciembre/CSV/Ocupados.CSV", sep = ";")

Datos_del_hogar_y_la_vivienda = read.csv("Datos/Diciembre/CSV/Datos del hogar y la vivienda.CSV", sep = ";")

No_ocupados = read.csv("Datos/Diciembre/CSV/No ocupados.CSV",sep = ";")

Otros_ingresos_e_impuestos = read.csv("Datos/Diciembre/CSV/Otros ingresos e impuestos.CSV", sep = ";")

Caracteristicas_generales <- read.csv("Datos/Diciembre/CSV/Características generales, seguridad social en salud y educación.CSV", sep = ";")

# Áreas y ciudades 
datacarac <- Caracteristicas_generales %>% filter(AREA %in% c(05, 08, 11, 13, 17,
                                                            23, 73, 52, 54, 68,
                                                            50, 66, 76))
                                                 
dataing_imp <- Otros_ingresos_e_impuestos %>% filter(AREA %in% c(05, 08, 11, 13, 17,
                                                            23, 73, 52, 54, 68,
                                                            50, 66, 76))

datano_ocu <- No_ocupados %>% filter(AREA %in% c(05, 08, 11, 13, 17,23, 73, 52, 
                                               54, 68,50, 66, 76))
                                                              
dataocupa <- Ocupados %>% filter(AREA %in% c(05, 08, 11, 13, 17,23, 73, 52, 
                                            54, 68,50, 66, 76))

datahog_viv <- Datos_del_hogar_y_la_vivienda %>% filter(AREA %in% c(05, 08, 11, 13, 
                                                                 17,23, 73, 52, 
                                                                 54, 68, 50, 66, 76))                

#-------------#
# Variables   #
#-------------#

##########################
### Variables continuas ##
##########################

# P6800 ¿Cuántas horas a la semana trabaja normalmente ... en ese trabajo?
# P6040 Años cumplidos
# P6790 ¿Cuántos meses trabajó en los últimos 12 meses?
# P6960 ¿Cuántos años lleva cotizando al fondo de pensiones?
# P7100 ¿Cuántas horas adicionales puede trabajar a la semana?

# P6426 ¿Cuánto tiempo lleva trabajando en su empleo actual?
# P1882 Tiempo de desplazamiento a su trabajo
############################
### Variables categóricas ##
############################
# P3271 Sexo al nacer
# P6050 ¿Cuál es el parentesco de ... con el jefe o jefa del hogar?
# P3042 Mayor nivel educativo alcanzado y el último grado o semestre aprobado por
# P6510 El mes pasado recibió ingresos por concepto de horas extras?
# P6880 Dónde realiza principalmente su trabajo
# P6920 ¿Está ... cotizando actualmente a un fondo de pensiones?
# P7140S2 Desea mejorar sus ingresos?
# P7140S3 Desea trabajar menos horas?
# P7140S6 No le gusta su trabajo actual?
# RAMA4D_R4 Rama de actividad empleo principal (CIIU REV 4 a 4 números)
# RAMA2D_R4 Rama de actividad empleo principal (CIIU REV 4 a 2 números)
# P6430 ¿Qué posición ocupa en su trabajo?
# P6930 ¿A cuál fondo de pensiones cotiza?
# P7130 ¿Desea cambiar el trabajo que tiene?
# P514 ¿Considera que su empleo es estable?

#P7090 ¿Quiere trabajar más horas?
#P1881 Medio de transporte
#P7240 ¿Y si se queda sin trabajo?
#P3069 ¿Cuántas personas trabajan en su sitio?
#P6990 ¿ARL?
#P9450 ¿Afiliado a caja?

# AREA
# Dpto

#-------------------------------------#
# Selección de variables de interés   #
#-------------------------------------#
dataocupa$id <- paste0(dataocupa$DIRECTORIO, "-", dataocupa$SECUENCIA_P, "-", dataocupa$ORDEN)
ocup <- dataocupa[c("id", "P6800", "INGLABO", "P7140S6",
                    "P7140S3", "P7140S2", "P6920", "P6880", "P6510", "P6790", "P6960", "P7100", "RAMA4D_R4", "RAMA2D_R4",
                    "P6430", "P6930", "P7130", "P514", "P6426", "P1882", "P7090", "P1881", "P7240", "P3069", "P6990", "P9450",
                    "FEX_C18", "AREA", "DPTO")]
colnames(ocup) = c("id", "horas_semana", "ingreso", "satisf",
                   "menos_h", "mejor_y", "cotiza", "lugar", "h_extra", "meses",
                   "cotiza_t", "horas_ad", "rama_4", "rama_2", "posic", "fondo", "cambiar", "estable", "t_actual",
                   "t_viaje", "mas_h", "medio", "sintrab", "n_comp", "arl", "caja",
                   "factor_exp", "area", "dpto")

datacarac$id <- paste0(datacarac$DIRECTORIO, "-", datacarac$SECUENCIA_P, "-", datacarac$ORDEN)
Car_gen <- datacarac[c("id",
                       "P3271", "P6050", "P6040", "P3042")]
colnames(Car_gen) = c("id", "sexo", "parent", "edad", "edu")


# Selección de base de datos para imputar
dataset_final <- merge(ocup, Car_gen, all.x = TRUE)

# Orden de las variables
dataset_final <- dataset_final[c("id", "area", "dpto", "sexo", "parent", "edad", "edu",
                                 "ingreso", "horas_semana", "satisf",
                                 "menos_h", "mejor_y", "cotiza", "lugar", "h_extra", "meses",
                                 "cotiza_t", "horas_ad", "rama_4", "rama_2", "posic", "fondo", "cambiar", "estable", "t_actual",
                                 "t_viaje", "mas_h", "medio", "sintrab", "n_comp", "arl", "caja",
                                 "factor_exp")]

# Variables eliminadas por ser ramificadas
drop = c("satisf", "menos_h", "mejor_y", "h_extra",
         "cotiza_t", "horas_ad")

dataset_final <- dataset_final[setdiff(colnames(dataset_final), drop)]


#-----------------------#
# Recodificar variables #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------#

# Recodificar departamento
library(plyr)
dataset_final$dpto = revalue(factor(dataset_final$dpto), 
                             c("5"="Antioquia", "8"="Atlantico", "11"="Bogota", "13"="Bolivar"
                               , "17"="Caldas", "23"="Cordoba", "50"="Meta", "52"="Narino"
                               , "54"="Nte Santander", "66"="Risaralda", "68"="Santander",
                               "73"="Tolima", "76" = "Valle"))


# Recodificar area
dataset_final$area = revalue(factor(dataset_final$area), 
                             c("5"="Medellin", "8"="Barranquilla", "11"="Bogota", "13"="Cartagena"
                               , "17"="Manizales", "23"="Monteria", "50"="Villavicencio", "52"="Pasto"
                               , "54"="Cucuta", "66"="Pereira", "68"="Bucaramanga",
                               "73"="Ibague", "76" = "Cali"))

# Recodificar sexo
dataset_final$sexo = revalue(factor(dataset_final$sexo), 
                             c("1"="M", "2"="F"))

# Recodificar parentesco
dataset_final$parent = revalue(factor(dataset_final$parent), 
                             c("1"="Jefe", "2"="Pareja",
                               "3"="Hijo", "4"="Padre o madre",
                               "5"="Suegro", "6"="Hermano",
                               "7" = "Yerno o nuera", "8" = "Nieto",
                               "9" = "Otro pariente", "10" = "Empleado doméstico",
                               "11" = "Pensionista", "12" = "Trabajador",
                               "13" = "Otro no-pariente"))

# Recodificar educación
dataset_final$edu = revalue(factor(dataset_final$edu), 
                               c("1"="Ninguno",
                                 "3"="Primaria", "4"="Primaria",
                                 "5"="Secundaria", "6"="Secundaria",
                                 "7" = "Normalista", "8" = "Técnico o tecnológico",
                                 "9" = "Técnico o tecnológico", "10" = "Universitaria",
                                 "11" = "Especialización", "12" = "Maestría",
                                 "13" = "Doctorado"))

# Recodificar lugar de trabajo
dataset_final$lugar = revalue(factor(dataset_final$lugar), 
                            c("1"="En esta vivienda", "2" = "Otra vivienda",
                              "3"="Kiosko", "4"="Vehículo",
                              "5"="Puerta-puerta", "6"="En la calle",
                              "7" = "Local fijo u oficina", "8" = "Area rural",
                              "9" = "Obra en construccion", "10" = "Mina o cantera",
                              "11" = "Otro"))


# Recodificar código de actividad económica
ciuu <- readxl::read_excel("Datos/CIIU_REV_4.xls")
colnames(ciuu) <- c("rama_2", "grupo", "rama_4", "actividad")
ciuu$rama_2 <- as.numeric(ciuu$rama_2)
df <- merge(dataset_final[c("id", "rama_2")],
            ciuu[c("rama_2", "actividad")], by = "rama_2", all.x  = T)

dataset_final <- merge(dataset_final, df[c("id", "actividad")])

# Recodificar la posición
dataset_final$posic = revalue(factor(dataset_final$posic), 
                              c("1"="Obrero", "2" = "Funcionario",
                                "3"="Empleado domestico", "4"="Autonomo",
                                "5"="Empleador", "6"="Trabajador familiar no remunerado",
                                "7" = "Trabajador no remunerado en empresa", 
                                "8" = "Jornalero",
                                "9" = "Otro"))

# Recodificar medio de transporte
dataset_final$medio = revalue(factor(dataset_final$medio), 
                              c("1"="Bus intermunicipal",
                                "2" = "Bus urbano",
                                "3"="A pie", "4"="Metro",
                                "5"="SITP", "6"="Taxi",
                                "7" = "Transporte empresa", 
                                "8" = "Auto particular",
                                "9" = "Lancha o canoa",
                                "10" = "Caballo",
                                "11" = "Moto",
                                "12" = "Mototaxi",
                                "13" = "Bicicleta",
                                "14" = "No se desplaza",
                                "15" = "Otro"))

# Recodificar la variable "sin trabajo"
dataset_final$sintrab = revalue(factor(dataset_final$sintrab), 
                              c("1"="Cesantias",
                                "2" = "Ahorro",
                                "3"="Ayuda familiar", "4"="Indemnizacion",
                                "5"="No lo ha considerado", "6"="Vender bienes",
                                "7" = "Empenar", 
                                "8" = "No tendria recursos",
                                "9" = "Dinero prestado",
                                "10" = "Otros"))

# Recodificar ¿Cuántas personas trabajan en su empesa?
dataset_final$n_comp = revalue(factor(dataset_final$n_comp), 
                                c("1"="Solo",
                                  "2" = "2 - 3",
                                  "3"="4 - 5", "4"="6 - 10",
                                  "5"="11 - 19", "6"="20 - 30",
                                  "7" = "31 - 50", 
                                  "8" = "51 - 100",
                                  "9" = "101 - 200",
                                  "10" = "201 o mas"))

# Función para recodificar preguntas de sí y no

yes_no = c("cambiar", "estable", "arl", "caja", "mas_h")

for (k in yes_no) {
  index = which(colnames(dataset_final) == k)
  
  dataset_final[,index] = revalue(factor(dataset_final[,index]), 
                                  c("1"="Si", "2" = "No",
                                    "9" = "No sabe"))
}


# Recodificar las pensiones
dataset_final$cotiza_fondo = NA
for (i in 1:nrow(dataset_final)) {
  
  if (dataset_final$cotiza[i] == 1 &
      dataset_final$fondo[i] == 1) {
    dataset_final$cotiza_fondo[i] = "Fondo privado"
  }
  
  if (dataset_final$cotiza[i] == 1 &
      dataset_final$fondo[i] == 2) {
    dataset_final$cotiza_fondo[i] = "Colpensiones"
  }
  
  if (dataset_final$cotiza[i] == 1 &
      dataset_final$fondo[i] == 3) {
    dataset_final$cotiza_fondo[i] = "Regimen especial"
  } 
  
  if (dataset_final$cotiza[i] == 1 &
      dataset_final$fondo[i] == 4) {
    dataset_final$cotiza_fondo[i] = "Fondo subsidiado"
  } 
  
  if (dataset_final$cotiza[i] == 2) {
    dataset_final$cotiza_fondo[i] = "No cotiza"
  } 
  
  if (dataset_final$cotiza[i] == 3) {
    dataset_final$cotiza_fondo[i] = "Pensionado"
  }
  
}

#----------------------------------------------------#
# La base de datos es guardado en distintos formatos #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------#





write_csv(dataset_final, "Datos/Formatos/geih_dataset.csv")

write.table(dataset_final, file = "Datos/Formatos/geih_dataset.txt", 
            append = TRUE, sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE)

writexl::write_xlsx(dataset_final, "Datos/Formatos/geih_dataset.xlsx")

library(foreign)
haven::write_sav(dataset_final, "Datos/Formatos/geih_dataset.spss")

haven::read_sav("Datos/Formatos/geih_dataset.spss")

library(haven)
write_dta(dataset_final, "Datos/Formatos/geih_dataset.dta")

write_sas(dataset_final, "Datos/Formatos/geih_dataset.sas")

save(dataset_final, file='Datos/Formatos/geih_dataset.rda')

save(dataset_final, file='Datos/Formatos/geih_dataset.rds')


