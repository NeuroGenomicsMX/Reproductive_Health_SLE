# Title: Obtain healthy volunteers with general and reproductive health data
# Author: Evelia Coss - Nínive Rdz
# Date: 27/02/2026
# Description: Volunteers selected as healthy according to the inclusion criteria, including reproductive heatlh data in the file of results

# Usage: hp_code(dataframe, country)
# Arguments:
# - Input: 
#   - dataframe = File.csv downloaded from REDCap
#   - country = 
# ID - Country 
# 1 - Argentina
# 2 - Brasil
# 3 - Chile
# 4 - Colombia
# 5 - Mexico
# 6 - Peru
# 7 - Uruguay
# - output: Dataframe with all the information of the selected participants

hp_code <- function(df, Country) {
  
  colnames(df)[1] <- "record_ID" 
 
  
  #group_by(country) %>% # by countries
  data <- df %>% 
    filter(country == Country ) %>% #elegir pais
    # Select age (adults <= 18 años
    filter(calculated_age >= 18) %>%
    # native
    filter(inclusion_pais == "1") %>%
    # family
    filter(padres_pais_origen == "1") %>%
    # Transplante de organos
    filter(criterios_exclusion_4 == "2") %>% # NO = 2
    # Sin enfermedades autoinmunes
        filter(autoimmunities == "0" | autoimmunities == "1" & autoimmune_diseases___0 == "0" & autoimmune_diseases___1 == "0" & autoimmune_diseases___2 == "0" & autoimmune_diseases___3 == "0" & autoimmune_diseases___7 == "0" & autoimmune_diseases___10 == "0" & autoimmune_diseases___13 == "0") %>%
    # Enfermedades metabolicas
    filter(metabolica_yn == "2" | metabolica_yn == "1" & diabetes =="0") %>% #diabetes tipo 1 o 2????
    # Enfermedades reumatologicas / autoinmunes
    filter(reumatologica_yn == "2" | reumatologica_yn == "1" & artritis_reumatoide == "0" & lupus_370998 =="0" & esclerodermia =="0" & espondilitis_anquilosante == "0") %>%
        # Select Columns
    #dplyr::select(record_ID, country, names, paternal_surname, maternal_surname, sex, cell_number, email, calculated_age, place_of_birth_4, place_of_birth_5, pregnant, due_date, pregnancy_plan, pregnancies, pregnancies_number, pregnancies_2, pregn_end, pregn_end_date, pregn_end_2, pregn_end_date_2, pregn_end_3, pregn_end_date_3, pregn_end_4, pregn_end_date_4, pregn_end_5, pregn_end_date_5, 
                  #preeclampsie, pregn_compl, pregn_compl_time, preeclampsie_2, pregn_compl_2, pregn_compl_time_2, preeclampsie_3, pregn_compl_3, pregn_compl_time_3, preeclampsie_4, pregn_compl_4, pregn_compl_time_4, preeclampsie_5, pregn_compl_5, pregn_compl_time_5, smoking, pregn_smoke_habits___1, pregn_smoke_habits___2, pregn_smoke_habits___3, pregn_smokequit_month, 
                  #pregn_smokestart_time, smoking_2, pregn_smoke_habits_2___1, pregn_smoke_habits_2___2, pregn_smoke_habits_2___3, pregn_smokequit_month_2, pregn_smokestart_time_2, smoking_3, pregn_smoke_habits_3___1, pregn_smoke_habits_3___2,pregn_smoke_habits_3___3, pregn_smokequit_month_3,  pregn_smokestart_time_3, smoking_4, pregn_smoke_habits_4___1, pregn_smoke_habits_4___2, pregn_smoke_habits_4___3, 
                  #pregn_smokequit_month_4, pregn_smokestart_time_4, smoking_5, pregn_smoke_habits_5___1, pregn_smoke_habits_5___2, pregn_smoke_habits_5___3, pregn_smokequit_month_5, pregn_smokestart_time_5, breastfeeding, breasfeeding_type, breasfeeding_type_2, breasfeeding_type_3, breasfeeding_type_4, breasfeeding_type_5, period_age, last_period, last_period_age, cycle_regularity, histerectomy,
                  #histerectomy_date, ooforectomy, ooforectomy_ovaries, ooforectomy_date_2, contraceptives, contraceptive_type, contraceptive_type_others, contraceptive_date, contraceptives_past, contraceptive_past_age, contraceptives_past_time, contraceptives_past_months, contraceptives_past_years, contraceptive_past_type, contraceptivepast_other, menopause_tx, menopause_tx_date, menopause_tx_type, hormonal_repl_tx, 
                  #hormonal_repl_tx_age, hormonal_repl_tx_time, hormonal_repl_tx_months, hormonal_repl_tx_years, hormonal_repl_tx_type, enf_embarazo_yn, enf_hipertensiva_embarazo, diabetes_gestacional, aborto, ectopico, enf_hipertensiva_embarazo_dxmed, enf_hipertensiva_embarazo_edaddx, enf_hipertensiva_embarazo_edadsx, diabetes_gestacional_dxmed, diabetes_gestacional_edaddx, 
                  #diabetes_gestacional_edadsx, aborto_dxmed, aborto_edaddx, aborto_edadsx, aborto_n, ectopico_dxmed, ectopico_edaddx, ectopico_edadsx, ectopico_n, covid, neumonia, zika, hanta, dengue, criterios_exclusion_3, daily_medicine, daily_treatments___9, asma, ojo_seco, psoriasis, deformidad_columna, aborto) %>%
    dplyr::select(record_ID, country, names, paternal_surname, maternal_surname, sex, cell_number, email, calculated_age, age_at_survey, period_age, last_period, last_period_age, cycle_regularity, pregnant, pregnancies, pregnancies_number, pregn_compl, pregn_end, pregn_end_2, pregn_end_3, pregn_end_4, pregn_end_5, pregn_compl, preeclampsie, preeclampsie_2, preeclampsie_3, preeclampsie_4, preeclampsie_5, aborto, enf_hipertensiva_embarazo, diabetes_gestacional, ectopico, histerectomy, ooforectomy, sop, contraceptives) %>%
    
    # Rename Column
   # rename(C3_Antibiotics_Immunos = criterios_exclusion_3) %>%
    # rename(WithoutDailyTreatments = daily_treatments___9) %>%
    
    # Modificar formato
    mutate_if(is.integer, ~replace_na(.,0))  %>% # Remplazar NA por cero
    mutate_if(is.logical, ~replace_na(.,0))  %>% # Remplazar NA por cero
    
    
    #mutate(across(where(is.integer), ~replace_na(., 0)),  # Reemplaza NA en columnas enteras
          #across(where(is.logical), ~replace_na(., 0)))  # Reemplaza NA en columnas lógicas
    
    # Asignar sexo ----
  mutate(sex =if_else(sex==1,"woman", if_else(sex==2,"man", "other")))
  
  #data[,c(11:138)][data[,c(11:138)] == "2" | data[,c(11:138)] == "0"] <- "FALSE"
  #data[,c(11:138)][data[,c(11:138)] == "1"] <- "TRUE"
  
  
  # PLace
  PLACE <- select(df, record_ID, country, place_of_birth_4, place_of_birth_ar, place_of_birth_ch, place_of_birth_col, place_of_birth_br, place_of_birth_3, place_of_birth_uru)
  PLACE <- PLACE %>% mutate(PlaceOfBirth = 
                              if_else(country  ==1, place_of_birth_ar,
                                      if_else(country  ==2, place_of_birth_br, 
                                              if_else(country  ==3, place_of_birth_ch, 
                                                      if_else(country  ==4, place_of_birth_col,
                                                              if_else(country  ==5, place_of_birth_4,
                                                                      if_else(country  ==6, place_of_birth_3, 
                                                                              if_else(country  ==7, place_of_birth_uru, 
                                                                                      place_of_birth_4
                                                                              ))))))))
  PLACE <-  PLACE %>% select(record_ID, PlaceOfBirth) 
  data <- data %>% left_join(PLACE, by = join_by(record_ID))
  
  print(data)
  
  
}