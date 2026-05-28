# =========================================================
# Title: Calculate age at reproductive survey 
# Author: Nínive Rdz 
# Date: 20/03/2026 
# Description: Calculate age at reproductive questionnaire completion
#              using REDCap timestamp
# Input: CSV export from REDCap 
# Output: Dataset with age_at_rep_survey variable
# =========================================================


# Libraries

library(dplyr)
library(stringr)
library(lubridate)

# Working directory
setwd("C:/Users/niniv/OneDrive/POSTDOC/LIIGH/Proyectos R/Controls_Fem_RH/")

# Input Data

Data_raw <- read.csv(
  "rawdata/Lupus_WomenRH_DATA_2026-03-24_1759.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Extract reproductive survey date

Data_rep <- Data_raw %>%
  mutate(
    # Limpieza básica del timestamp
    salud_reproductiva_timestamp = str_trim(as.character(salud_reproductiva_timestamp)),
    salud_reproductiva_timestamp = na_if(salud_reproductiva_timestamp, ""),
    
    # Conversión a fecha-hora
    survey_date_rep = ymd_hms(salud_reproductiva_timestamp)
  )

# Calculate age at reproductive survey

Data_age_rep <- Data_rep %>%
  mutate(
    birth_date = as.Date(birth_date),
    
    # Edad en encuesta reproductiva
    age_at_rep_survey = floor(
      interval(birth_date, survey_date_rep) / years(1)
    )
  )

# QC 

Data_age_Qc <- Data_age_rep %>%
  mutate(
    no_consent = privacy_acceptance___1 != 1,
    missing_rep_date = is.na(survey_date_rep),
    missing_age_rep = is.na(age_at_rep_survey)
  )

# Exploración QC
sum(is.na(Data_age_Qc$age_at_rep_survey))

Data_age_Qc %>%
  filter(no_consent | missing_rep_date | missing_age_rep) %>%
  select(record_id, birth_date, survey_date_rep, age_at_rep_survey, privacy_acceptance___1)

# Trazabilidad de exclusiones

No_consent <- Data_age_Qc %>%
  filter(no_consent)

Missing_rep_date <- Data_age_Qc %>%
  filter(missing_rep_date)

Missing_age_rep <- Data_age_Qc %>%
  filter(missing_age_rep)

# Dataset elegible

Data_clean <- Data_age_Qc %>%
  filter(
    !no_consent,        # consentimiento
    !missing_rep_date,  # tiene encuesta reproductiva
    !missing_age_rep    # edad calculable
  )

# QC summary

cat("Total inicial:", nrow(Data_age_Qc), "\n")
cat("Sin consentimiento:", sum(Data_age_Qc$no_consent, na.rm = TRUE), "\n")
cat("Sin fecha reproductiva:", sum(Data_age_Qc$missing_rep_date), "\n")
cat("Sin edad calculable:", sum(Data_age_Qc$missing_age_rep), "\n")
cat("Total final:", nrow(Data_clean), "\n")

# Output

write.csv(
  Data_clean,
  "Resultados/Lupus_WomenRH_AgeRepSurvey.csv",
  row.names = FALSE
)
