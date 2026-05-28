# Title: Obtain healthy volunteers with general and reproductive health data
# Author: Nínive Rdz
# Date: 20/03/2026
# Description: Volunteers selected as healthy according to the inclusion criteria, including reproductive heatlh data in the file of results
# Usage: hp_code(dataframe)
# Arguments:
# - Input: 
# - output: Dataframe with all the information of the selected participants

hp_code <- function(df) {
  
  colnames(df)[1] <- "record_ID" 
 
 #group_by(criterios de inclusión) %>%
 data <- df %>% 
 # Select age (adults >= 18 años
 filter(age_at_rep_survey >= 18) %>%
 #Select women
 filter(sex___1 == 1) %>%
 # Trasplante de organos
 filter(criterios_exclusion_4 == 2) %>% # Sí = 1 y NO = 2
 # Sin enfermedades autoinmunes
 filter(autoimmunities == 0 | 
          (autoimmunities == 1 & 
             autoimmune_diseases___0 == 0 & #AR
             autoimmune_diseases___1 == 0 & #EM
             autoimmune_diseases___2 == 0 & #DM1
             autoimmune_diseases___3 == 0 & #Psoriasis
             autoimmune_diseases___7 == 0 & #Sjogren
             autoimmune_diseases___10 == 0   #Vasculitis
           )) %>%
 # Enfermedades metabolicas
 filter(metabolica_yn == 2 | 
          (metabolica_yn == 1 & 
             diabetes___1 ==0)) %>% #diabetes tipo 1 o 2????
 # Enfermedades reumatologicas / autoinmunes
 filter(reumatologica_yn == 2 | 
          (reumatologica_yn == 1 & 
             artritis_reumatoide___1 == 0 & 
             lupus_370998___1 ==0 & 
             esclerodermia___1 ==0 & 
             espondilitis_anquilosante___1 == 0)) %>%
 
  
  
 # Renombrar variable de sexo y asignar el valor de woman cuando exista el valor de 1 en esa variable
    mutate(
      sex = case_when(
        sex___1 == 1 ~ "woman",
        sex___1 == 0 ~ "not_woman",
        TRUE ~ NA_character_)) %>% 
    select(-sex___1) %>%
    
  #Seleccionar columnas  
  
  dplyr::select(record_ID, sex, 
                  calculated_age, age_at_rep_survey, period_age, 
                  last_period, last_period_age, cycle_regularity, pregnant, pregnancies, 
                  pregnancies_number, pregn_compl, pregn_end, pregn_end_2, pregn_end_3, pregn_end_4, 
                  pregn_end_5, preeclampsie, preeclampsie_2, preeclampsie_3, 
                  preeclampsie_4, preeclampsie_5, aborto___1, enf_hipertensiva_embarazo___1, 
                  diabetes_gestacional___1, ectopico___1, histerectomy, ooforectomy, sop___1, 
                  contraceptives) %>%
    
   
  # Modificar formato
    mutate_if(is.integer, ~replace_na(.,0))  %>% # Remplazar NA por cero
    mutate_if(is.logical, ~replace_na(.,0))  %>% # Remplazar NA por cero
    
    
  
  return(data)

}