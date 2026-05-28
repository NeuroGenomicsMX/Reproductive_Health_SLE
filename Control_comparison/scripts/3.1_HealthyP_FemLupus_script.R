# Title: Get list of healthy volunteers
# Author: Nínive Rdz
# Date: 20/03/2026
# Description: 
# Usage:
# Input: File.csv from script 2.1
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
Lupus_db <- read.csv("rawdata/LupusFem_Clean and complete.csv", header=TRUE)
colnames(Lupus_db)[1] <- "record_ID" 

Lupus_controls <- Lupus_db %>%
  filter(lupus == 2) #Filtrando a los controles

# Functions - Aplicar criterios clínicos
source("scripts/hp_code_funcion_Lupus_Fem.R")

#Dataset Controles lupus Healthy
Lupus_healthy <- hp_code(Lupus_controls) 

str(Lupus_healthy)
head(Lupus_healthy)
tail(Lupus_healthy)


# QC Controls

QC_controls <- Lupus_controls %>%
  mutate(
    
    # 1. Edad
    pass_age = age_at_rep_survey >= 18,
    
    # 2. Sexo
    pass_sex = sex___1 == 1,
    
    # 3. Exclusión (trasplante)
    pass_exclusion = criterios_exclusion_4 == 2,
    
    # 4. Autoinmunes
    pass_autoimmune = autoimmunities == 0 | 
      (autoimmunities == 1 & 
         autoimmune_diseases___0 == 0 &
         autoimmune_diseases___1 == 0 &
         autoimmune_diseases___2 == 0 &
         autoimmune_diseases___3 == 0 &
         autoimmune_diseases___7 == 0 &
         autoimmune_diseases___10 == 0
      ),
    
    # 5. Metabólicas
    pass_metabolic = metabolica_yn == 2 | 
      (metabolica_yn == 1 & diabetes___1 == 0),
    
    # 6. Reumatológicas
    pass_rheum = reumatologica_yn == 2 | 
      (reumatologica_yn == 1 & 
         artritis_reumatoide___1 == 0 & 
         lupus_370998___1 == 0 & 
         esclerodermia___1 == 0 & 
         espondilitis_anquilosante___1 == 0
      ),
    
    # 🔴 GLOBAL (idéntico a hp_code)
    pass_all = coalesce(
      pass_age &
        pass_sex &
        pass_exclusion &
        pass_autoimmune &
        pass_metabolic &
        pass_rheum,
      FALSE
    ))

# QC Outputs
# Conteo
cat("Total controles (sin lupus):", nrow(Lupus_controls), "\n")
cat("Controles sanos:", nrow(Lupus_healthy), "\n")
cat("QC TRUE:", sum(QC_controls$pass_all), "\n")


# Not pass

QC_fail <- QC_controls %>%
  filter(!pass_all) %>%
  select(
    record_ID,
    pass_age,
    pass_sex,
    pass_exclusion,
    pass_autoimmune,
    pass_metabolic,
    pass_rheum
  )

# Summary fails
QC_summary <- QC_controls %>%
  summarise(
    fail_age = sum(!pass_age, na.rm = TRUE),
    fail_sex = sum(!pass_sex, na.rm = TRUE),
    fail_exclusion = sum(!pass_exclusion, na.rm = TRUE),
    fail_autoimmune = sum(!pass_autoimmune, na.rm = TRUE),
    fail_metabolic = sum(!pass_metabolic, na.rm = TRUE),
    fail_rheum = sum(!pass_rheum, na.rm = TRUE)
  )

print(QC_summary)

#Export (opcional)
write.csv(
  Lupus_healthy,
  "Resultados/Lupus_healthy_controls.csv",
  row.names = FALSE
)

write.csv(
  QC_fail,
  "Resultados/Lupus_controls_failed_QC.csv",
  row.names = FALSE
)
