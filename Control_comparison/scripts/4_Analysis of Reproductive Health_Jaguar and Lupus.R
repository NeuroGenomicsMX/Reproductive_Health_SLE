# Title: Reproductive Health Descriptive Analysis pf Controls
# Author: Nínive Rdz
# Date: 25/03/2027
# Description: 
# Usage:
# Input: Clean file obtained from merge of Cleand DB from Jaguar + Lupus
# output: 
# Arguments


# ---------------------------
# 1. Installing libraries  
# ---------------------------

# install.packages("dplyr" and "tidyr")
library(dplyr)   # Manipulacion de datos
library(tidyr)   # Manipulacion de datos
library(lubridate)


# -------------------------------------
# 2. Working directory and input data
# -------------------------------------

setwd("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/")
getwd()

dir.exists("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/") #Para verificar si existe el directorio

# Input Data
Control_Fem <- read.csv("rawdata/Controls_Fem_Clean.csv", header=TRUE)
colnames(Control_Fem)
glimpse(Control_Fem) #Muestra un resumen compacto de la estructura del data frame: número de filas, columnas, tipo de cada variable y los primeros valores. Útil para una inspección rápida.


# --------------------------------
# 3. Replace, rename + rep_life
# --------------------------------


#Cambiar 0 en FALSE
Control_Fem_clean <- Control_Fem %>%
  mutate(
    pregn_end_5 = as.numeric(
      replace_na(as.logical(pregn_end_5), FALSE)
    ),
    preeclampsie_5 = as.numeric(
      replace_na(as.logical(preeclampsie_5), FALSE)
    )
  )


#Renombra columna de period_age por Menarca
Control_Fem_Men <- Control_Fem_clean %>%
  rename(Menarca = period_age)


#Crea la variable de rep_life

Control_Fem_RH <- Control_Fem_Men %>%
  mutate(rep_life = last_period_age - Menarca)


# ------------------------------
# 4. QC: Biological consistence 
# ------------------------------

Control_Fem_Qc <- Control_Fem_RH %>%
  mutate(
    
    # 1. Edad inconsistente:
    # Inconsistencia: edad actual menor que edad de última menstruación
    # (biológicamente imposible)
    qc_age_last_period = if_else(
      !is.na(age_at_survey) & !is.na(last_period_age) &
        age_at_survey < last_period_age,
      1, 0
    ),
    
    # 2. Amenorrea inconsistente:
    # Definición clínica: amenorrea = ≥12 meses sin menstruación
    # Inconsistencia: reporta amenorrea (last_period == 0)
    # PERO su última menstruación coincide con la edad actual
    qc_amenorrea_conflict = if_else(
      last_period == 0 &
        !is.na(last_period_age) &
        !is.na(age_at_survey) &
        last_period_age == age_at_survey,
      1, 0
    ),
    
    # 3. Menarca igual a última menstruación:
    # Sospechoso: puede indicar error de captura o mala interpretación
    qc_menarca_equal_last = if_else(
      !is.na(Menarca) & 
        !is.na(last_period_age) &
        Menarca == last_period_age,
      1, 0
    ),
    
    # 4. Vida reproductiva negativa:
    # Definición: rep_life = última menstruación - menarca
    # Inconsistencia: valor negativo implica cronología imposible
    qc_rep_life_neg = if_else(
      !is.na(rep_life) & rep_life < 0,
      1, 0
    ),
    
    # 5. Embarazo actual + aborto simultáneo:
    # Inconsistencia: ambos eventos no pueden coexistir en el mismo momento
    qc_pregnancy_conflict = if_else(
      pregnant == 1 & aborto == 1,
      1, 0
    ),
    
    # 6. Flag global:
    # Marca registros con AL MENOS una inconsistencia biológica
    qc_any = if_else(
      qc_age_last_period == 1 |
        qc_amenorrea_conflict == 1 |
        qc_menarca_equal_last == 1 |
        qc_rep_life_neg == 1 |
        qc_pregnancy_conflict == 1,
      1, 0
    )
  )

# ------------------------------
# 5. Controls Dataset Clean
# ------------------------------

Control_Fem_clean_final <- Control_Fem_Qc %>%
  filter(qc_any == 0)

#-----------------------------------------
# 6.Funciones: median_iqr and freq_percent
#-----------------------------------------


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


#--------------------------
# 7. General description 
#--------------------------

median_iqr(Control_Fem_clean_final$age_at_survey) # Edad al  momento de responder los cuestionarios
median_iqr(Control_Fem_clean_final$Menarca) # Analysis of Menarca Age
median_iqr(Control_Fem_clean_final$last_period_age) # Analysis of Last period age 
median_iqr(Control_Fem_clean_final$rep_life) # Analysis of Reproductive life


freq_percent(Control_Fem_clean_final$cycle_regularity) # Analysis of cicly regularity

#-------------------------------
# 8. Sub-analysis: amenhnorrhea 
#-------------------------------

# Base: incluye solo registros con información válida de menstruación
# last_period: 1 = menstruando, 0 = no menstruando
Amenorrhea_base_controls <- Control_Fem_clean_final %>%
  filter(last_period %in% c(0, 1))


# Clasificación de estado menstrual (refinada)


Amenorrhea_classified_controls <- Amenorrhea_base_controls %>%
  mutate(
    
    # Estado menstrual:
    # - Menstruating: menstruación en los últimos 12 meses
    # - Early_Amenorrhea: ≥12 meses sin menstruación y <40 años (no quirúrgica)
    # - Amenorrhea_natural: ≥12 meses sin menstruación ≥40 años (no quirúrgica)
    # - Early_Amenorrhea_surgical: amenorrea <40 años asociada a cirugía
    # - Amenorrhea_surgical: amenorrea ≥40 años asociada a cirugía
    
    menstrual_status = case_when(
      
      # 1. Menstruación reciente
      last_period == 1 ~ "Menstruating",
      
      # 2. Early amenorrhea quirúrgica (<40 años)
      last_period == 0 &
        (histerectomy == 1 | ooforectomy == 1) &
        !is.na(last_period_age) &
        last_period_age < 40 ~ "Early_Amenorrhea_surgical",
      
      # 3. Amenorrea quirúrgica (≥40 años)
      last_period == 0 &
        (histerectomy == 1 | ooforectomy == 1) &
        !is.na(last_period_age) &
        last_period_age >= 40 ~ "Amenorrhea_surgical",
      
      # 4. Early amenorrhea (NO quirúrgica)
      last_period == 0 &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) &
        !is.na(last_period_age) &
        last_period_age < 40 ~ "Early_Amenorrhea",
      
      # 5. Amenorrea natural (NO quirúrgica)
      last_period == 0 &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) &
        !is.na(last_period_age) &
        last_period_age >= 40 ~ "Amenorrhea_natural",
      
      TRUE ~ NA_character_
    )
  )

# QC: Conteos de amenorrea

table(Amenorrhea_classified_controls$menstrual_status, useNA = "ifany")
freq_percent(Amenorrhea_classified_controls$menstrual_status) # Frecuencia y porcentaje


# Edad de amenorrea natural (mediana e IQR)

Amenorrhea_classified_controls %>%
  filter(menstrual_status == "Amenorrhea_natural") %>%
  pull(last_period_age) %>%
  median_iqr()

#--------------------------------------------------
# Integrar clasificación de amenorrea en la base de controles
#--------------------------------------------------

Control_Fem_clean_final <- Control_Fem_clean_final %>%
  left_join(
    Amenorrhea_classified_controls %>%
      select(record_ID, menstrual_status),
    by = "record_ID"
  )


#-------------------------------
# 9. Sub-analysis: pregnancy 
#-------------------------------

#Subconjunto de embarazos
preg_history_controls <- Control_Fem_clean_final %>%
  filter(pregnancies == 1) #1 = ha estado embarazada

nrow(preg_history_controls) #Verifico mis 33 registros

#agregar la columna de complications_pregn 
#complications_pregn: 0 = No complication, 1 = ≥1 pregnancy complication

preg_history_controls$complications_pregn <- ifelse(
  preg_history_controls$preeclampsie == 1 |
    preg_history_controls$diabetes_gestacional == 1 |
    preg_history_controls$ectopico == 1 |
    preg_history_controls$enf_hipertensiva_embarazo == 1,
  1, 0)

# Análisis población general

freq_percent(Control_Fem_clean_final$pregnant)#Embarazo actual
freq_percent(Control_Fem_clean_final$pregnancies) #History of pregnancies
freq_percent(Control_Fem_clean_final$aborto) #Pérdidas gestacionales en población general
freq_percent(Control_Fem_clean_final$diabetes_gestacional) #Diabetes gestacional en población general
freq_percent(Control_Fem_clean_final$enf_hipertensiva_embarazo) #Hipertensión gestacional en población general
freq_percent(Control_Fem_clean_final$ectopico) #Calcular embarazo ectópico en población general
freq_percent(Control_Fem_clean_final$histerectomy) #Calcular histerectomía en población general
freq_percent(Control_Fem_clean_final$ooforectomy) #Calcular Ooforectomía en población general
freq_percent(Control_Fem_clean_final$sop) #Calcular SOP en población general
freq_percent(Control_Fem_clean_final$contraceptives) #Calcular contraceptives en población general


# Análisis sub-conjunto de embarazos

freq_percent(preg_history_controls$preeclampsie) #Subconjunto de embarazos - ¿tuvo alguna complicación durante el embarazo? con la variable preeclampsie, ¿usted tuvo preeclampsia o alguna complicación durante alguno de sus embarazos?
freq_percent(preg_history_controls$pregnancies_number) #Calcular frecuencias de embarazos
freq_percent(preg_history_controls$aborto) #Pérdidas gestacionales en personas con historial de embarazos
freq_percent(preg_history_controls$diabetes_gestacional) #Diabetes gestacional en personas con historial de embarazos
freq_percent(preg_history_controls$enf_hipertensiva_embarazo) #Calcular Hipertensión gestacional en personas con historial de embarazos
freq_percent(preg_history_controls$ectopico) #Calcular embarazo ectópico en personas con historial de embarazos
freq_percent(preg_history_controls$histerectomy) #Calcular histerectomía en personas con historial de embarazo
freq_percent(preg_history_controls$ooforectomy) #Calcular Ooforectomía en personas con historial de embarazo
freq_percent(preg_history_controls$complications_pregn) #Calcular Complicaciones del embarazo en LES con historial de embarazo



# =========================================================
# LES: Reproductive Health Descriptive Analysis
# =========================================================

# ---------------------------
# 1. Input Data (LES)
# ---------------------------

#Base original

LES_raw <- read.csv(
  "rawdata/Lupus_rawdata.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Inspección de estructura
colnames(LES_raw)
glimpse(LES_raw)

# Filtrar solo pacientes con LES, lupus: 1 = LES, 2 = control
LES_base <- LES_raw %>%
  filter(lupus == 1)

# Verificación de filtrado
table(LES_raw$lupus, useNA = "ifany")
table(LES_base$lupus, useNA = "ifany")



#Variables clínicas derivadas (LES)

LES_desc <- LES_base %>%
  mutate(
    
    # Edad actual
    age_at_survey = calculated_age,
    
    # Edad al diagnóstico (años)
    # Definición: edad actual - duración de enfermedad
    age_dx = calculated_age - dx_time,
    
    # Duración de enfermedad (años)
    disease_duration = dx_time,
    
    # Actividad de enfermedad
    sledai = sledai_total,
    
    # Daño acumulado
    slicc = slicc_total
  )

# QC previo: variable rep_life

summary(LES_desc$rep_life) # Verificar distribución

# Verificar valores negativos (no esperados biológicamente)
LES_desc %>%
  filter(rep_life < 0)

# QC: Consistencia biológica

LES_Qc <- LES_desc %>%
  mutate(
    
    # 1. Edad inconsistente:
    # Edad actual menor que edad de última menstruación
    qc_age_last_period = if_else(
      !is.na(age_at_survey) & !is.na(last_period_age) &
        age_at_survey < last_period_age,
      1, 0
    ),
    
    # 2. Menarca igual a última menstruación:
    # Puede indicar error de captura
    qc_menarca_equal_last = if_else(
      !is.na(Menarca) & 
        !is.na(last_period_age) &
        Menarca == last_period_age,
      1, 0
    ),
    
    # 3. Vida reproductiva negativa:
    # Cronología imposible
    qc_rep_life_neg = if_else(
      !is.na(rep_life) & rep_life < 0,
      1, 0
    ),
    
    # 4. Inconsistencia en amenorrea:
    # Reporta menstruación pero edad sugiere >1 año sin menstruación
    qc_amenh_conflict = if_else(
      amenh == "Menstruating" &
        !is.na(last_period_age) &
        !is.na(age_at_survey) &
        last_period_age < (age_at_survey - 1),
      1, 0
    ),
    
    # 5. Flag global:
    # Identifica registros con al menos una inconsistencia
    qc_any = if_else(
      qc_age_last_period == 1 |
        qc_menarca_equal_last == 1 |
        qc_rep_life_neg == 1 |
        qc_amenh_conflict == 1,
      1, 0
    )
  )


# ---------------------------
# 5. Dataset limpio LES
# ---------------------------

LES_clean_final <- LES_Qc %>%
  filter(qc_any == 0)

# Save dataset
write.csv(LES_clean_final, "./Resultados/LES_clean_final_060426.csv")


# ---------------------------
# 6. Descriptivo clínico LES
# ---------------------------

# Variables continuas
median_iqr(LES_clean_final$age_at_survey) #Edad en años
median_iqr(LES_clean_final$age_dx) # edad de diagnóstico
median_iqr(LES_clean_final$disease_duration) #duración de la enfermedad
median_iqr(LES_clean_final$sledai) #Actividad de la enfermedad
median_iqr(LES_clean_final$slicc) #Daño acumulado SLICC
median_iqr(LES_clean_final$rep_life) #Vida reproductiva
median_iqr(LES_clean_final$last_period_age) #Edad última menstruación, no debería ser solo en las menstruantes?
median_iqr(LES_clean_final$Menarca) #Edad Menarca


# Variables categóricas
freq_percent(LES_clean_final$nephritis)
freq_percent(LES_clean_final$Antimalarials)
freq_percent(LES_clean_final$Corticosteroids)
freq_percent(LES_clean_final$cycle_regularity) #Regularidad del ciclo menstrual en pacientes LES
freq_percent(LES_clean_final$histerectomy) #Calcular histerectomía en población general
freq_percent(LES_clean_final$ooforectomy) #Calcular Ooforectomía en población general
freq_percent(LES_clean_final$sop) #Calcular SOP en población general
freq_percent(LES_clean_final$contraceptives) #Calcular contraceptives en población general



# ---------------------------
# Sub-análisis: Estado menstrual
# ---------------------------

# Base: registros con información válida de estado menstrual
LES_amenorrhea_base <- LES_clean_final %>%
  filter(!is.na(amenh))


# ---------------------------------------
# Clasificación de estado menstrual (LES)
# ----------------------------------------

LES_amenorrhea_classified <- LES_amenorrhea_base %>%
  mutate(
    
    # Clasificación:
    # - Menstruating
    # - Early_Amenorrhea (natural)
    # - Amenorrhea_natural
    # - Early_Amenorrhea_surgical
    # - Amenorrhea_surgical
    
    menstrual_status = case_when(
      
      # Menstruación reciente
      amenh == "Menstruating" ~ "Menstruating",
      
      # Early amenorrea quirúrgica (<40 años)
      amenh == "Early-Amenhorrea" &
        (histerectomy == 1 | ooforectomy == 1) ~ "Early_Amenorrhea_surgical",
      
      # Early amenorrea natural (<40 años)
      amenh == "Early-Amenhorrea" &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) ~ "Early_Amenorrhea",
      
      # Amenorrea quirúrgica (≥40 años)
      amenh == "Amenhorrea" &
        (histerectomy == 1 | ooforectomy == 1) ~ "Amenorrhea_surgical",
      
      # Amenorrea natural (≥40 años)
      amenh == "Amenhorrea" &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) ~ "Amenorrhea_natural",
      
      TRUE ~ NA_character_
    )
  )


#QC amenorrea
table(LES_amenorrhea_classified$menstrual_status, useNA = "ifany")

freq_percent(LES_amenorrhea_classified$menstrual_status) # Frecuencia y porcentaje de estado de amenorrea


# Edad de amenorrea natural (LES)

LES_amenorrhea_classified %>%
  filter(menstrual_status == "Amenorrhea_natural") %>%
  pull(last_period_age) %>%
  median_iqr()

#--------------------------------------------------
# Integrar clasificación de amenorrea en base LES
#--------------------------------------------------


LES_clean_final$menstrual_status <- LES_amenorrhea_classified$menstrual_status
nrow(LES_clean_final) == nrow(LES_amenorrhea_classified)


#----------------------------
#HISTORIAL DE EMBARAZO EN LES
#-----------------------------

#Subconjunto de embarazos
preg_history_LES <- LES_clean_final %>%
  filter(pregnancies == 1)

preg_history_LES$complications_pregn <- ifelse(
  preg_history_LES$preeclampsie == 1 |
    preg_history_LES$diabetes_gestacional == 1 |
    preg_history_LES$ectopico == 1 |
    preg_history_LES$enf_hipertensiva_embarazo == 1,
  1, 0)



nrow(preg_history_LES) #Verifico mis 100 registros en LES

# Variables categoricas relacionadas a embarazo en pacientes con LES

freq_percent(LES_clean_final$pregnant) #Embarazo actual
freq_percent(LES_clean_final$pregnancies) #History of pregnancies
freq_percent(LES_clean_final$aborto) #Calcular pérdidas gestacionnales LES general
freq_percent(LES_clean_final$diabetes_gestacional)#Calcular diabetes gestacional en población LES general
freq_percent(LES_clean_final$enf_hipertensiva_embarazo) #Calcular Hipertensión gestacional en población LES general
freq_percent(LES_clean_final$ectopico) #Calcular embarazo ectópico en población LES general

# Variables categoricas en el subconjunto de embarazo LES

freq_percent(preg_history_LES$preeclampsie) #Complicaciones en el embarazo
freq_percent(preg_history_LES$pregnancies_number) #Calcular frecuencias de embarazos
freq_percent(preg_history_LES$aborto)#Calcular pérdidas gestacionnales en personas con historial de embarazos
freq_percent(preg_history_LES$diabetes_gestacional) #Calcular diabetes gestacional en personas con historial de embarazos
freq_percent(preg_history_LES$enf_hipertensiva_embarazo) #Calcular Hipertensión gestacional en personas con historial de embarazos
freq_percent(preg_history_LES$ectopico) #Calcular embarazo ectópico en personas con historial de embarazos
freq_percent(preg_history_LES$complications_pregn) #Calcular embarazo ectópico en personas con historial de embarazos


table(preg_history_LES$pregnancies_number, useNA = "ifany") #Verificar NAs

# ---------------------------
# Export dataset Controls and LES
# ---------------------------

#Guardar base de controles Clean
write.csv(
  Control_Fem_clean_final,
  "./Resultados/Controles_Clean_final.csv",
  row.names = FALSE
)

#Guardar base de LES clean

write.csv(
  LES_clean_final,
  "./Resultados/DB_LES_Clean_final.csv",
  row.names = FALSE
)




# -----------------------------------------------------------
# HOMOLOGACIÓN DE VARIABLES ENTRE COHORTES
# -----------------------------------------------------------

# Objetivo:
# Asegurar que variables equivalentes entre controles y LES
# tengan el mismo nombre antes de fusionar las bases.
# Esto evita duplicación de columnas durante el bind_rows().

#Revisamos variables únicas en cada DB
setdiff(colnames(Control_Fem_clean_final), colnames(LES_clean_final))

setdiff(colnames(LES_clean_final), colnames(Control_Fem_clean_final))

# Limpieza de base LES

LES_clean_final <- LES_clean_final %>%
  select(-any_of(c("X", "pregnancy")))

# Homologar variable en controles


Control_Fem_hamonized <- Control_Fem_clean_final %>%
  mutate(cycle_regularity = as.character(cycle_regularity)) 

#Asignemos variables reproductivas con el nombre adecuado para evitar errores al realizar el merge

LES_harmonized <- LES_clean_final %>%
  mutate(aborto = aborto___1,
         diabetes_gestacional = diabetes_gestacional___1,
         ectopico = ectopico___1,
         enf_hipertensiva_embarazo = enf_hipertensiva_embarazo___1,
         sop = sop___1)
glimpse(LES_harmonized)
    
# -----------------------------------------------------------
# 1. Crear identificador de cohorte
# -----------------------------------------------------------
# Se añade una variable que indica si el registro pertenece
# a controles o pacientes con LES.

Controls_tagged <- Control_Fem_hamonized %>%
  mutate(group = "Control")

LES_tagged <- LES_harmonized %>%
  mutate(group = "SLE")

glimpse(Controls_tagged)
glimpse(LES_tagged)

# -----------------------------------------------------------
# 2. Generación de base combinada
# -----------------------------------------------------------
# bind_rows() combina ambas bases por filas.
# Las variables exclusivas de cada cohorte se rellenan
# automáticamente con NA en la otra.

Combined_data <- bind_rows(
  Controls_tagged,
  LES_tagged
)

# -----------------------------------------------------------
# 3. Verificación del merge
# -----------------------------------------------------------

# Número de registros por cohorte
table(Combined_data$group)


# Distribución de edad
summary(Combined_data$age_at_survey)

# Variables reproductivas clave
table(Combined_data$pregnant, Combined_data$group)
table(Combined_data$pregnancies, Combined_data$group)


#---------------------------------------------
# 4. Statistical Analysis
#---------------------------

median_iqr(Combined_data$age_at_survey) #datos de edad de ambos grupos

Combined_data %>%
  group_by(group) %>%
  summarise(
    Median = median(age_at_survey, na.rm = TRUE),
    Q1 = quantile(age_at_survey, 0.25, na.rm = TRUE),
    Q3 = quantile(age_at_survey, 0.75, na.rm = TRUE)
  )

#Calcula p value con test wilcox
wilcox.test(age_at_survey ~ group, data = Combined_data)


#CAlcula menarca combinado
median_iqr(Combined_data$Menarca)

#calcula menarca por grupo
Combined_data %>%
  group_by(group) %>%
  summarise(
    Median = median(Menarca, na.rm = TRUE),
    Q1 = quantile(Menarca, 0.25, na.rm = TRUE),
    Q3 = quantile(Menarca, 0.75, na.rm = TRUE)
  )

#Calcula p value
wilcox.test(Menarca ~ group, data = Combined_data)

#Calcula last period age overall
median_iqr(Combined_data$last_period_age)

#Calcula last period age por grupo
Combined_data %>%
  group_by(group) %>%
  summarise(
    Median = median(last_period_age, na.rm = TRUE),
    Q1 = quantile(last_period_age, 0.25, na.rm = TRUE),
    Q3 = quantile(last_period_age, 0.75, na.rm = TRUE)
  )

#Calcula p value
wilcox.test(last_period_age ~ group, data = Combined_data)


#Calcula rep_life overall y por grupo
median_iqr(Combined_data$rep_life)

Combined_data %>%
  group_by(group) %>%
  summarise(
    Median = median(rep_life, na.rm = TRUE),
    Q1 = quantile(rep_life, 0.25, na.rm = TRUE),
    Q3 = quantile(rep_life, 0.75, na.rm = TRUE)
  )

wilcox.test(rep_life ~ group, data = Combined_data)


# -----------------------------------
# 5. Menstrual cycle regularity
# -----------------------------------

# Crear tabla de contingencia por grupo
tab_cycle_regularity <- table(Combined_data$cycle_regularity, Combined_data$group)

# Convertir tabla a data.frame para manipulación
tab_cycle_df <- as.data.frame.matrix(tab_cycle_regularity)

# Calcular frecuencia total (Overall)
tab_cycle_df$Overall <- rowSums(tab_cycle_df)

# Calcular porcentaje respecto al total de la cohorte
total <- sum(tab_cycle_df$Overall)
tab_cycle_df$Percentage <- round((tab_cycle_df$Overall / total) * 100, 1)

# Visualizar tabla con conteos y porcentajes
tab_cycle_df

# -----------------------------------
# 6. Test estadístico
# -----------------------------------
# No se utiliza Chi-square porque hay frecuencias pequeñas
# en la categoría "Unknown". Se usa Fisher's exact test.

fisher.test(tab_cycle_regularity)



# -----------------------------------
# 7. Pregnancies Sub-analysis
# -----------------------------------

# Subgrupo: mujeres con historial de embarazo
preg_history_harmonized <- Combined_data %>%
  filter(pregnancies == 1)

# Verificación del tamaño del subgrupo
table(preg_history_harmonized$group)


chisq.test(table(Combined_data$pregnancies, Combined_data$group))

# -----------------------------------
# 8. Pregnancy complications
# -----------------------------------

Combined_data <- Combined_data %>%
  mutate(
    complications_pregn = ifelse(
      preeclampsie == 1 |
        diabetes_gestacional == 1 |
        ectopico == 1 |
        enf_hipertensiva_embarazo == 1,
      1, 0
    )
  )



preg_history_harmonized <- Combined_data %>%
  filter(pregnancies == 1)

table(preg_history_harmonized$group) #validación de datos

# Crear tabla de complicaciones por grupo
tab_complications <- table(
  preg_history_harmonized$complications_pregn,
  preg_history_harmonized$group
)

tab_complications

# Calcular frecuencia total (overall)
complications_df <- as.data.frame.matrix(tab_complications)

complications_df$Overall <- rowSums(complications_df)

total_comp <- sum(complications_df$Overall)

complications_df$Percentage <- round((complications_df$Overall / total_comp) * 100, 1)

complications_df

#Fisher porque tenemos frecuencias bajas
fisher.test(tab_complications)

#FRECUENCIAS DE EMBARAZOS

tab_n_preg <- table(
  preg_history_harmonized$pregnancies_number,
  preg_history_harmonized$group
)

n_preg_df <- as.data.frame.matrix(tab_n_preg)

n_preg_df$Overall <- rowSums(n_preg_df)

total <- sum(n_preg_df$Overall)

n_preg_df$Percentage <- round((n_preg_df$Overall / total) * 100, 1)

n_preg_df

#Calculo estadistico
fisher.test(table(preg_history_harmonized$pregnancies_number,
                  preg_history_harmonized$group))


# ----------------------------------------------
# 9. Pregnancy description (population overall)
# ----------------------------------------------

# Variables evaluadas en la población total
# N = 322 (SLE = 210, Controls = 112)
# Debido a frecuencias pequeñas se utiliza Fisher's exact test

pregnancy_outcome_vars <- c(
  "aborto",
  "diabetes_gestacional",
  "enf_hipertensiva_embarazo",
  "ectopico"
)

for (var in pregnancy_outcome_vars) {
  
  # Mostrar nombre de la variable analizada
  cat("\nPregnancy outcome variable:", var, "\n")
  
  # Crear tabla de contingencia por grupo
  pregnancy_outcome_table <- table(
    Combined_data[[var]],
    Combined_data$group
  )
  
  print(pregnancy_outcome_table)
  
  # Convertir tabla a data.frame
  pregnancy_outcome_df <- as.data.frame.matrix(pregnancy_outcome_table)
  
  # Calcular frecuencia total (Overall)
  pregnancy_outcome_df$Overall <- rowSums(pregnancy_outcome_df)
  
  # Calcular porcentaje respecto al total de la cohorte
  total_population <- sum(pregnancy_outcome_df$Overall)
  
  pregnancy_outcome_df$Percentage <- round(
    (pregnancy_outcome_df$Overall / total_population) * 100,
    1
  )
  
  print(pregnancy_outcome_df)
  
  # Test estadístico (Fisher)
  print(fisher.test(pregnancy_outcome_table))
}



# -----------------------------------------------------------------
# Pregnancy description (subset: mujeres con historial de embarazo)
# ------------------------------------------------------------------

#subconjunto de embarazos 
pregnancy_subset <- Combined_data %>%
  filter(pregnancies ==1)


# Variables evaluadas en el subgrupo
subgroup_pregnancies_outcome_vars <- c(
  "aborto",
  "diabetes_gestacional",
  "enf_hipertensiva_embarazo",
  "ectopico"
)

for (var in subgroup_pregnancies_outcome_vars) {
  
  cat("\nPregnancy outcome variable (pregnancy subgroup):", var, "\n")
  
  # Tabla de contingencia
  pregnancies_subgroup_outcome_table <- table(
    pregnancy_subset[[var]],
    pregnancy_subset$group
  )
  
  print(pregnancies_subgroup_outcome_table)
  
  # Convertir tabla a data.frame
  pregnancies_subgroup_outcome_df <- as.data.frame.matrix(
    pregnancies_subgroup_outcome_table
  )
  
  # Calcular frecuencia total (Overall)
  pregnancies_subgroup_outcome_df$Overall <- rowSums(
    pregnancies_subgroup_outcome_df
  )
  
  # Total del subconjunto con embarazo
  pregnancies_subgroup_total <- sum(
    pregnancies_subgroup_outcome_df$Overall
  )
  
  # Porcentaje
  pregnancies_subgroup_outcome_df$Percentage <- round(
    (pregnancies_subgroup_outcome_df$Overall / pregnancies_subgroup_total) * 100,
    1
  )
  
  print(pregnancies_subgroup_outcome_df)
  
  # Test estadístico
  print(fisher.test(pregnancies_subgroup_outcome_table))
}



#--------------------------
# 10. Variables ginecológicas
#---------------------------

gynecologic_vars_harmonized <- c(
  "histerectomy",
  "ooforectomy",
  "sop",
  "contraceptives"
)

for (var in gynecologic_vars_harmonized) {
  
  cat("\nVariable:", var, "\n")
  
  # Tabla de contingencia
  gynecologic_table_harmonized <- table(
    Combined_data[[var]],
    Combined_data$group
  )
  
  print(gynecologic_table_harmonized)
  
  # Convertir tabla a data.frame
  gynecologic_df <- as.data.frame.matrix(gynecologic_table_harmonized)
  
  # Calcular overall
  gynecologic_df$Overall <- rowSums(gynecologic_df)
  
  # Calcular porcentaje respecto a N total
  total_population <- sum(gynecologic_df$Overall)
  
  gynecologic_df$Percentage <- round(
    (gynecologic_df$Overall / total_population) * 100,
    1
  )
  
  print(gynecologic_df)
  
  # Test estadístico
  print(chisq.test(gynecologic_table_harmonized))
}


# 11. AMENORREA
# ---------------------------------------
# Clasificación del estado menstrual
# ---------------------------------------

Combined_data <- Combined_data %>%
  mutate(
    
    menstrual_status = case_when(
      
      # ---------------------------
      # CONTROLES
      # ---------------------------
      
      group == "Control" & last_period == 1 ~ "Menstruating",
      
      group == "Control" & last_period == 0 &
        (histerectomy == 1 | ooforectomy == 1) &
        !is.na(last_period_age) &
        last_period_age < 40 ~ "Early_Amenorrhea_surgical",
      
      group == "Control" & last_period == 0 &
        (histerectomy == 1 | ooforectomy == 1) &
        !is.na(last_period_age) &
        last_period_age >= 40 ~ "Amenorrhea_surgical",
      
      group == "Control" & last_period == 0 &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) &
        !is.na(last_period_age) &
        last_period_age < 40 ~ "Early_Amenorrhea",
      
      group == "Control" & last_period == 0 &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) &
        !is.na(last_period_age) &
        last_period_age >= 40 ~ "Amenorrhea_natural",
      
      
# -----
# LES
# -----
      
      group == "SLE" & amenh == "Menstruating" ~ "Menstruating",
      
      group == "SLE" & amenh == "Early-Amenhorrea" &
        (histerectomy == 1 | ooforectomy == 1) ~ "Early_Amenorrhea_surgical",
      
      group == "SLE" & amenh == "Early-Amenhorrea" &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) ~ "Early_Amenorrhea",
      
      group == "SLE" & amenh == "Amenhorrea" &
        (histerectomy == 1 | ooforectomy == 1) ~ "Amenorrhea_surgical",
      
      group == "SLE" & amenh == "Amenhorrea" &
        (histerectomy == 0 | is.na(histerectomy)) &
        (ooforectomy == 0 | is.na(ooforectomy)) ~ "Amenorrhea_natural",
      
      TRUE ~ NA_character_
    )
  )
#Derivamos variables de tabla 1, amenorrea y early amenorrea natural
Combined_data <- Combined_data %>%
  mutate(
    
    # Amenorrea natural ≥40
    amenh_natural = ifelse(menstrual_status == "Amenorrhea_natural", 1, 0),
    
    # Amenorrea temprana natural
    early_amenh = ifelse(menstrual_status == "Early_Amenorrhea", 1, 0)
    
  )

#QC
table(Combined_data$menstrual_status, Combined_data$group)

table(Combined_data$amenh_natural, Combined_data$group)
table(Combined_data$early_amenh, Combined_data$group)

#Overall calculating

# Tabla base
tab_amenh_natural <- table(Combined_data$amenh_natural, Combined_data$group)

# Convertir a data.frame
amenh_natural_df <- as.data.frame.matrix(tab_amenh_natural)

# Calcular overall
amenh_natural_df$Overall <- rowSums(amenh_natural_df)

# Total población
total <- sum(amenh_natural_df$Overall)

# Porcentaje overall
amenh_natural_df$Percentage <- round((amenh_natural_df$Overall / total) * 100, 1)

amenh_natural_df

# Tabla base early amenorrea
tab_early_amenh <- table(Combined_data$early_amenh, Combined_data$group)

# Convertir a data.frame
early_amenh_df <- as.data.frame.matrix(tab_early_amenh)

# Calcular overall
early_amenh_df$Overall <- rowSums(early_amenh_df)

# Total población
total <- sum(early_amenh_df$Overall)

# Porcentaje overall
early_amenh_df$Percentage <- round((early_amenh_df$Overall / total) * 100, 1)

early_amenh_df

#Test estadistico
fisher.test(table(Combined_data$amenh_natural, Combined_data$group))
fisher.test(table(Combined_data$early_amenh, Combined_data$group))


#Calcular edad de amenorrea_natural

# Edad de amenorrea (overall)

Combined_data %>%
  filter(menstrual_status %in% c("Amenorrhea_natural","Early_Amenorrhea")) %>%
  summarise(
    Median = median(last_period_age, na.rm = TRUE),
    Q1 = quantile(last_period_age, 0.25, na.rm = TRUE),
    Q3 = quantile(last_period_age, 0.75, na.rm = TRUE)
  )

# Edad de amenorrea natural por grupo

Combined_data %>%
  filter(menstrual_status == "Amenorrhea_natural") %>%
  group_by(group) %>%
  summarise(
    Median = median(last_period_age, na.rm = TRUE),
    Q1 = quantile(last_period_age, 0.25, na.rm = TRUE),
    Q3 = quantile(last_period_age, 0.75, na.rm = TRUE)
  )

#Test estadistico
wilcox.test(last_period_age ~ group,
            data = Combined_data %>%
              filter(menstrual_status == "Amenorrhea_natural"))
