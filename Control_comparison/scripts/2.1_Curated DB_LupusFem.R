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
Registries_raw <- read.csv("rawdata/Lupus_WomenRH_AgeRepSurvey.csv", header=TRUE)

#Inspección rápida 
colnames(Registries_raw)
glimpse(Registries_raw) #Muestra un resumen compacto de la estructura del data frame: número de filas, columnas, tipo de cada variable y los primeros valores. Útil para una inspección rápida.


#QC DUPLICADOS

# Duplicated emails
Duplicated_emails_report <- Registries_raw %>%
  count(email) %>%
  filter(n > 1)


# Duplicated cellphone numbers
Duplicated_cellphone_report <- Registries_raw %>%
  count(cell_number) %>%
  filter(n > 1)


#Registros completos
Registries_completes <- Registries_raw %>%
  mutate(na_count = rowSums(is.na(.)))

#Identity duplicated registries
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
Registries_clean <- Registries_completes %>%
  filter(record_id %in% Best_registries$record_id |
  !(email %in% Duplicated_ID$email &
  cell_number %in% Duplicated_ID$cell_number)) %>%
  select(-na_count)


#Verificar los que cuenten con cuestionarios completos
Registries_surv_complete<- Registries_clean %>%
  filter(
    general_complete == 2,
    salud_reproductiva_complete == 2,
    cuestionario_medico_complete == 2,
    criterios_1b4563_complete == 2
  )

#Registros incompletos
Registries_surv_incomplete <- Registries_clean %>%
  filter(
    general_complete != 2 |
      salud_reproductiva_complete != 2 |
      cuestionario_medico_complete != 2 |
      criterios_1b4563_complete != 2
  )


# Remove personal identifiers
Registries_curated_complete <- Registries_surv_complete %>%
  select (-c(names, maternal_surname,paternal_surname, cell_number, email, email2,address)) #select(-names, -maternal_surname, -paternal_surname)


#QC report  

cat("Original records:", nrow(Registries_raw), "\n")
cat("Duplicated records:", nrow(Duplicated_ID), "\n")
cat("Registros limpios:", nrow(Registries_clean), "\n")
cat("Registros inmcompletos:", nrow(Registries_surv_incomplete), "\n")
cat("Registros incompletos:", nrow(Registries_surv_complete), "\n")
cat("Final Data:", nrow(Registries_curated_complete), "\n")



# Export datasets
#Guarda DB sin duplicados y completos
write.csv(
  Registries_curated_complete, 
  "./Resultados/LupusFem_Clean and complete.csv",
  row.names = FALSE)

#Guarda reporte de duplicados
#write.csv(
 # Duplicated_ID, 
  #"./Resultados/Lupus_Fem_Duplicated_IDs.csv",
  #row.names = FALSE)
  
  
  
  
  