# Title: Calculate age at survey
# Author: Nínive Rdz
# Date: 15/03/2027
# Description: Calculate age at survey completion using the REDCap signature date
# Usage:
# Input: File.csv de REDCap
# output: Dataset with corrected age variable
# Arguments

# install.packages("dplyr" and "tidyr")
library(dplyr)     # Manipulacion de datos
library(tidyr)     # Manipulacion de datos
library(stringr)   # Extracción de texto
library(lubridate) # Manejo de fechas

# Working directory:

setwd("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/")
getwd()

dir.exists("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/") #Para verificar si existe el directorio


# Input Data

Data_raw <- read.csv("rawdata/Jaguar_MexWomenRH_DATA_2026-03-20_0114.csv", header=TRUE)
colnames(Data_raw)
glimpse(Data_raw) #Muestra un resumen compacto de la estructura del data frame: número de filas, columnas, tipo de cada variable y los primeros valores. Útil para una inspección rápida.

# Signature date extraction

Data_survey_date <- Data_raw %>%
  mutate(
    survey_date = str_extract(signature,"\\d{4}-\\d{2}-\\d{2}"),
    survey_date = as.Date(survey_date))
  
# Calculate age at survey

Data_age_at_survey <- Data_survey_date %>%
  mutate(
    birth_date = as.Date(birth_date),
    age_at_survey = floor(interval(birth_date, survey_date) / years(1))
  )
  

# Check

Data_age_at_survey %>%
  select (record_id, birth_date, signature, survey_date, calculated_age, age_at_survey) %>%
  head()

# Ckeck missing ages

sum(is.na(Data_age_at_survey$age_at_survey))

# Ckeck differences between date at survey and calculated age

Data_age_at_survey$calculated_age - Data_age_at_survey$age_at_survey


# Export datasets
#Guarda DB sin duplicados

write.csv(
  Data_age_at_survey, 
  "./Resultados/JaguarFem_AgeSurvey.csv",
  row.names = FALSE)
