# Title: Get list of healthy volunteers
# Author: Evelia Coss - Nínive Rdz
# Date: 27/02/2026
# Description: 
# Usage:
# Input: File.csv descargado de REDCap
# output: 
# Arguments

# Packages:
# install.packages("dplyr")
library(dplyr)   # Manipulacion de datos
library(tidyr)   # Manipulacion de datos

# Working directory:

setwd("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/")
getwd()

dir.exists("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/") #Para verificar si existe el directorio

# Input Data
Jaguar_db <- read.csv("rawdata/1. JaguarFem_AgeSurvey.csv", header=TRUE)
colnames(Jaguar_db)[1] <- "record_ID" 

print(names(Jaguar_db))
head(Jaguar_db[, c(13:25)])

# Functions
source("scripts/hp_code_funcion_Fem.R")

#---Dataset Only Mexico -----
Jaguar_healthy_db <- hp_code(Jaguar_db, 5) 

str(Jaguar_healthy_db)

# Select your country
# ID - Country 
# 1 - Argentina
# 2 - Brasil
# 3 - Chile
# 4 - Colombia
# 5 - Mexico
# 6 - Peru
# 7 - Uruguay

head(Jaguar_healthy_db)
tail(Jaguar_healthy_db)

# Total of participants
Jaguar_healthy_db %>% dplyr::select(record_ID) %>% n_distinct() #14 people
dim(Jaguar_healthy_db)

# Save dataset
write.csv(Jaguar_healthy_db, "./Resultados/Jaguar_Fem_Healthy.csv",
          row.names = FALSE)



