# Title: Curado de bases Jaguar o Lupus
# Author: Nínive Rdz
# Date: 14/03/2027
# Description: 
# Usage:
# Input: File.csv de REDCap o filtrado con criterios de inclusión/exclusión Jaguar o Fem
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
Registries_raw <- read.csv("rawdata/2. Jaguar_Fem_Healthy.csv", header=TRUE)
colnames(Registries_raw)
glimpse(Registries_raw) #Muestra un resumen compacto de la estructura del data frame: número de filas, columnas, tipo de cada variable y los primeros valores. Útil para una inspección rápida.


# QC: duplicated emails
Duplicated_emails_report <- Registries_raw %>%
  count(email) %>%
  filter(n > 1)


# QC: duplicated cellphone numbers
Duplicated_cellphone_report <- Registries_raw %>%
  count(cell_number) %>%
  filter(n > 1)


#Registros completos
Registries_completes <- Registries_raw %>%
  mutate(na_count = rowSums(is.na(.)))

#Identity of duplicated registries
Duplicated_ID <- Registries_completes %>%
  group_by(email, cell_number) %>%
  filter(n() > 1) %>%
  ungroup()

#Select the most complete registry
Best_registries <- Duplicated_ID %>%
  group_by(email, cell_number) %>%
  slice_min(na_count, n = 1, with_ties = FALSE) %>%
  ungroup()

#check
Best_registries %>%
  count(email, cell_number) %>%
  filter(n > 1)


#Remove worst registries
Registries_curated <- Registries_completes %>%
  filter(record_ID %in% Best_registries$record_ID |
  !(email %in% Duplicated_ID$email &
  cell_number %in% Duplicated_ID$cell_number)) %>%
  select(-na_count)

# Remove personal identifiers
Registries_clean <- Registries_curated %>%
  select (-c(names, country, maternal_surname,paternal_surname, cell_number, email)) #select(-names, -maternal_surname, -paternal_surname)


#QC report  

cat("Original records:", nrow(Registries_raw), "\n")
cat("Duplicated records:", nrow(Duplicated_ID), "\n")
cat("Final records:", nrow(Registries_clean), "\n")

# Export datasets
#Guarda DB sin duplicados
write.csv(
  Registries_clean, 
  "./Resultados/Jaguar_Fem_Clean.csv",
  row.names = FALSE)

#Guarda reporte de duplicados
write.csv(
  Duplicated_ID, 
  "./Resultados/Jaguar_Fem_Duplicated_IDs.csv",
  row.names = FALSE)
  
  
  
  
  