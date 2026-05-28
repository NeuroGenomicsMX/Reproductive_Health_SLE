#History of preeclampsie (at least one pregnancy)
# Title: Reproductive Health Descriptive Analysis - Jaguar Fem
# Author: Nínive Rdz
# Date: 15/03/2027
# Description: 
# Usage:
# Input: Clean file obteined from 3.1 curatedDB_JaguarFem
# output: 
# Arguments

# install.packages("dplyr" and "tidyr")
library(dplyr)   # Manipulacion de datos
library(tidyr)   # Manipulacion de datos

# Working directory:

setwd("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/")
getwd()

dir.exists("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/") #Para verificar si existe el directorio


# Input Data
JaguarFem_clean <- read.csv("rawdata/3. Jaguar_Fem_Clean.csv", header=TRUE)
colnames(JaguarFem_clean)
glimpse(JaguarFem_clean) #Muestra un resumen compacto de la estructura del data frame: número de filas, columnas, tipo de cada variable y los primeros valores. Útil para una inspección rápida.

# QC: consistencia biológica 

library(lubridate)

JaguarFem_Qc <- JaguarFem_clean %>%
  mutate(
    
    # 1. Amenorrea inconsistente
    qc_amenorrea = if_else(
      age_at_survey < last_period_age |
        (age_at_survey <= 18 & last_period == 0),
      1, 0
    ),
    
    # 2. Menarca = última menstruación (sospechoso)
    qc_menarca_equal_last = if_else(
      !is.na(Menarca) & !is.na(last_period_age) &
        Menarca == last_period_age,
      1, 0
    ),
    
    # 3. Vida reproductiva negativa
    qc_rep_life_neg = if_else(
      !is.na(rep_life) & rep_life < 0,
      1, 0
    ),
    
    
    # 4. Embarazo actual + aborto (inconsistente)
    qc_pregnancy_conflict = if_else(
      pregnant == 1 & aborto == 1,
      1, 0
    ),
    
    # 5. Flag global (si tiene AL MENOS un error)
    qc_any = if_else(
      qc_amenorrea == 1 |
        qc_menarca_equal_last == 1 |
        qc_rep_life_neg == 1 |
        qc_pregnancy_conflict == 1,
      1, 0
    )
  )


# Función para variables continuas (Median + IQR)

median_iqr <- function(x){
  c(
    Median = median(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE)
  )
}

#Función para variables categoricas (Number + %)

freq_percent <- function(x){
  
  tab <- table(x, useNA = "no")
  pct <- prop.table(tab) * 100
  
  data.frame(
    Category = names(tab),
    Frequency = as.vector(tab),
    Percent = round(as.vector(pct),2)
  )
}


# Calcula Edad de participantes al  momento de responder los cuestionarios

median_iqr(JaguarFem_Qc$age_at_survey)


# Analysis of Menarca Age

median_iqr(JaguarFem_Qc$Menarca)

# Analysis of cicly regularity

freq_percent(JaguarFem_Qc$cycle_regularity)


#HISTORIAL DE EMBARAZO#


freq_percent(JaguarFem_Qc$pregnant) #Embarazo actual
freq_percent(JaguarFem_Qc$pregnancies) #History of pregnancies


#Subconjunto de embarazos
preg_history <- JaguarFem_Qc %>%
  filter(pregnancies == 1)

nrow(preg_history) #Verifico mis 39 registros

#Subconjunto de embarazos - ¿tuvo alguna complicación durante el embarazo? con la variable preeclampsie, ¿usted tuvo preeclampsia o alguna complicación durante alguno de sus embarazos?
freq_percent(preg_history$preeclampsie)
freq_percent(preg_history$pregnancies_number) #Calcular frecuencias de embarazos

table(preg_history$pregnancies_number, useNA = "ifany") #Verificar NAs


freq_percent(JaguarFem_Qc$aborto) #Calcular pérdidas gestacionnales / abortos en población general
freq_percent(preg_history$aborto) #Calcular pérdidas gestacionnales / abortos en personas con historial de embarazos
freq_percent(JaguarFem_Qc$diabetes_gestacional) #Calcular diabetes gestacional en población general
freq_percent(preg_history$diabetes_gestacional) #Calcular diabetes gestacional en personas con historial de embarazos
freq_percent(JaguarFem_Qc$enf_hipertensiva_embarazo) #Calcular Hipertensión gestacional en población general
freq_percent(preg_history$enf_hipertensiva_embarazo) #Calcular Hipertensión gestacional en personas con historial de embarazos
freq_percent(JaguarFem_Qc$ectopico) #Calcular embarazo ectópico en población general
freq_percent(preg_history$ectopico) #Calcular embarazo ectópico en personas con historial de embarazos
freq_percent(JaguarFem_Qc$histerectomy) #Calcular histerectomía en población general
freq_percent(preg_history$histerectomy) #Calcular histerectomía en personas con historial de embarazo
freq_percent(JaguarFem_Qc$ooforectomy) #Calcular Ooforectomía en población general
freq_percent(preg_history$ooforectomy) #Calcular Ooforectomía en personas con historial de embarazo
freq_percent(JaguarFem_Qc$sop) #Calcular SOP en población general
freq_percent(JaguarFem_Qc$contraceptives) #Calcular contraceptives en población general


# Sub-análisis de Amenorrea

Amenorrhea_base <- JaguarFem_Qc %>%
  filter(last_period %in% c(0,1)) #Esto permite tener un subconjunto con menstruantes (1) y no menstruantes (0); excluye 2 y NAs

#----Subgrupos----#

#Población menstruante
Menstruating <- Amenorrhea_base %>%
  filter(last_period == 1)

#Población No menstruante
Non_menstruating <- Amenorrhea_base %>%
  filter(last_period == 0)

#QC

nrow(Amenorrhea_base)
nrow(Menstruating)
nrow(Non_menstruating)

#Amenorrea natural (no quirurgica)
Amenorrhea_natural <- Non_menstruating %>%
  filter(
    (histerectomy == 0 | is.na(histerectomy)) &
      (ooforectomy == 0 | is.na(ooforectomy))
  )

nrow(Amenorrhea_natural)

#Media de edad en amenorrea natural no quirurgica
median_iqr(Amenorrhea_natural$last_period_age)


#Early amenorrea

Amenorrhea_early <- Amenorrhea_natural %>%
  mutate(
    early_amenorrhea = if_else(last_period_age < 40, 1, 0)
  )

# Frecuencia y porcentaje
freq_percent(Amenorrhea_early$early_amenorrhea)


# Early amenorrea en población general

Amenorrhea_general <- Amenorrhea_base %>%
  mutate(
    early_amenorrhea = if_else(
      last_period == 0 &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) &
        !is.na(last_period_age) &
        last_period_age < 40,
      1, 0
    )
  )

# Frecuencia y porcentaje
freq_percent(Amenorrhea_general$early_amenorrhea)

#Verificando amenorrea quirurgica 
JaguarFem_Qc %>%
  group_by(last_period, histerectomy, ooforectomy) %>%
  summarise(n = n(), .groups = "drop")

#Verificando ooforectomía y amenorrea
JaguarFem_Qc %>%
  filter(ooforectomy == 1) %>%
  count(last_period)

# Export datasets


#Guarda DB sin duplicados
write.csv(
  Registries_clean, 
  "./Resultados/Jaguar_Fem_Clean_Des_1503.csv",
  row.names = FALSE)

#Guarda reporte de duplicados
write.csv(
  Duplicated_ID, 
  "./Resultados/Jaguar_Fem_Duplicated IDs.csv",
  row.names = FALSE)


