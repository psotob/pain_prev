###************************************************************************
###************************************************************************
###                                                                     ***
###                               CODE 1:                               ***
###                           DATA CLEANING                             ***
###                                                                     ***
###************************************************************************
###************************************************************************

##*************************************************************************
##  @project   	Grunenthal Pain Survey in Lima, Peru                       *
##  @created   	February 14, 2022                                         *
##  @revised   	February 14, 2022                                         *
##  @category  	Importing and preparing dataset                           *
##  @R version  R version 4.1.2 (2021-11-01) -- 'Bird Hippie'       *
##  @OS system  MS Windows 11 Pro x 64 bits                               *
##  @author    	Percy Soto-Becerra <percys1991@gmail.com>                 *
##*************************************************************************

##****************************************************************
##  1. Configuration of environment and packages                **----
##****************************************************************

# Removing all objects including loaded libraries
rm(list = ls(all = TRUE))

# Installing and loading packages
if (!require("pacman")) install.packages("pacman")

p_unload() # Unloading all package except base

library(pacman)

p_load(dplyr, 
       tibble, 
       tidyr, 
       readr, 
       ggplot2, 
       haven, 
       labelled, 
       forcats, 
       skimr, 
       purrr, 
       survey) # Loading packages


# Session Info inspection
sessionInfo()

##***************************************************************
##  2. Data input and structuring                              **----
##***************************************************************

# Importing .sav data to tibble format
source_data <- haven::read_sav(file = "Data/Source/529a-0919 - OP Dolor - BD.sav")

# Labeling values of factor variables
source_data <- source_data %>% 
  unlabelled()

##***************************************************************
##  3. Data transformation                                     **----
##***************************************************************

# Helper list: 
#### List of comercial/generical names of NSAID:
nsaid_list <- c("Diclofenaco", "Ibuprofeno", "Celecoxib", "Naproxeno", 
                "Paracetamol", "Panadol", "Ketorolaco", "Metamizol", 
                "Dolocordralan", "Doloflan", "Doloneurobión", "Deflamat",
                "Apronax", "Antalgina", "Kitadol", "Doloral", "Celebrex", 
                "Dolo quimagésico", "Migralivia", "Miodel", "Norflex Plus", 
                "Redex", "Dioxaflex Plus", "Dorixina", "Excedrin Migraña", 
                "Voltarén", "Dolonet", "Migrapac", "Aspirina")

# Data cleaning:
derived_data <- source_data %>% 
  dplyr::select(
    MAPA, MANZANA, VIVIENDA, ENTREVISTADO, PON, 
    G, edad, H, I, J, L, K_O1, NSE, tnse, lima, Distrito, P01, P04, P07, P08, 
    P03_1, P03_2, P05, P06, P07, P08, P26, P47, T_P11_1_1:T_P11_1_6, 
    P02_O1:P02_O9, P10_O1:P10_O4, T_P32_1_1:T_P32_1_6  
  ) %>% 
  # 3.1. Renaming variables----
rename(
  distrit = Distrito, 
  place = lima, 
  econ = NSE, 
  econcat = tnse, 
  sex = G, 
  ager = edad, 
  age = H, 
  maritalstat = I, 
  educ = J, 
  ocup = L, 
  hinsurance = K_O1,
  pain3m = P01, 
  pain3m_1 = P03_1, 
  pain3m_2 = P03_2, 
  pain3m_1_t = P04, 
  pain_act1 = P05, 
  pain_act2 = P26, 
  pain_act1_int = P06, 
  pain3m_1_int_in = P07, 
  pain3m_1_freq = P08, 
  QoL = P47 
) %>% 
  # 3.2. Creation of new variables----
mutate(
  ### a) Pain sites between people that self-reported pain----
  dzon_face = case_when(P02_O1 == "Cara" | 
                          P02_O2 == "Cara" | 
                          P02_O3 == "Cara" | 
                          P02_O4 == "Cara" | 
                          P02_O5 == "Cara" | 
                          P02_O6 == "Cara" | 
                          P02_O7 == "Cara" | 
                          P02_O8 == "Cara" | 
                          P02_O9 == "Cara" ~ 1, 
                        pain3m == "No" ~ as.numeric(NA), 
                        TRUE ~ 0), 
  dzon_chest = case_when(P02_O1 == "Tórax" | 
                           P02_O2 == "Tórax" | 
                           P02_O3 == "Tórax" | 
                           P02_O4 == "Tórax" | 
                           P02_O5 == "Tórax" | 
                           P02_O6 == "Tórax" | 
                           P02_O7 == "Tórax" | 
                           P02_O8 == "Tórax" | 
                           P02_O9 == "Tórax" ~ 1, 
                         pain3m == "No" ~ as.numeric(NA), 
                         TRUE ~ 0), 
  dzon_shoulder = case_when(P02_O1 == "Hombro" | 
                              P02_O2 == "Hombro" | 
                              P02_O3 == "Hombro" | 
                              P02_O4 == "Hombro" | 
                              P02_O5 == "Hombro" | 
                              P02_O6 == "Hombro" | 
                              P02_O7 == "Hombro" | 
                              P02_O8 == "Hombro" | 
                              P02_O9 == "Hombro" ~ 1, 
                            pain3m == "No" ~ as.numeric(NA), 
                            TRUE ~ 0), 
  dzon_elbow = case_when(P02_O1 == "Codo" | 
                           P02_O2 == "Codo" | 
                           P02_O3 == "Codo" | 
                           P02_O4 == "Codo" | 
                           P02_O5 == "Codo" | 
                           P02_O6 == "Codo" | 
                           P02_O7 == "Codo" | 
                           P02_O8 == "Codo" | 
                           P02_O9 == "Codo" ~ 1, 
                         pain3m == "No" ~ as.numeric(NA), 
                         TRUE ~ 0), 
  dzon_abdomen = case_when(P02_O1 == "Abdomen" | 
                             P02_O2 == "Abdomen" | 
                             P02_O3 == "Abdomen" | 
                             P02_O4 == "Abdomen" | 
                             P02_O5 == "Abdomen" | 
                             P02_O6 == "Abdomen" | 
                             P02_O7 == "Abdomen" | 
                             P02_O8 == "Abdomen" | 
                             P02_O9 == "Abdomen" ~ 1, 
                           pain3m == "No" ~ as.numeric(NA), 
                           TRUE ~ 0), 
  dzon_hip = case_when(P02_O1 == "Cadera" | 
                         P02_O2 == "Cadera" | 
                         P02_O3 == "Cadera" | 
                         P02_O4 == "Cadera" | 
                         P02_O5 == "Cadera" | 
                         P02_O6 == "Cadera" | 
                         P02_O7 == "Cadera" | 
                         P02_O8 == "Cadera" | 
                         P02_O9 == "Cadera" ~ 1, 
                       pain3m == "No" ~ as.numeric(NA), 
                       TRUE ~ 0), 
  dzon_hand_wrist = case_when(P02_O1 == "Mano y muñeca" | 
                                P02_O2 == "Mano y muñeca" | 
                                P02_O3 == "Mano y muñeca" | 
                                P02_O4 == "Mano y muñeca" | 
                                P02_O5 == "Mano y muñeca" | 
                                P02_O6 == "Mano y muñeca" | 
                                P02_O7 == "Mano y muñeca" | 
                                P02_O8 == "Mano y muñeca" | 
                                P02_O9 == "Mano y muñeca" ~ 1, 
                              pain3m == "No" ~ as.numeric(NA), 
                              TRUE ~ 0), 
  dzon_thigh = case_when(P02_O1 == "Muslo" | 
                           P02_O2 == "Muslo" | 
                           P02_O3 == "Muslo" | 
                           P02_O4 == "Muslo" | 
                           P02_O5 == "Muslo" | 
                           P02_O6 == "Muslo" | 
                           P02_O7 == "Muslo" | 
                           P02_O8 == "Muslo" | 
                           P02_O9 == "Muslo" ~ 1, 
                         pain3m == "No" ~ as.numeric(NA), 
                         TRUE ~ 0), 
  dzon_knee = case_when(P02_O1 == "Rodilla" | 
                          P02_O2 == "Rodilla" | 
                          P02_O3 == "Rodilla" | 
                          P02_O4 == "Rodilla" | 
                          P02_O5 == "Rodilla" | 
                          P02_O6 == "Rodilla" | 
                          P02_O7 == "Rodilla" | 
                          P02_O8 == "Rodilla" | 
                          P02_O9 == "Rodilla" ~ 1, 
                        pain3m == "No" ~ as.numeric(NA), 
                        TRUE ~ 0), 
  dzon_leg = case_when(P02_O1 == "Pierna" | 
                         P02_O2 == "Pierna" | 
                         P02_O3 == "Pierna" | 
                         P02_O4 == "Pierna" | 
                         P02_O5 == "Pierna" | 
                         P02_O6 == "Pierna" | 
                         P02_O7 == "Pierna" | 
                         P02_O8 == "Pierna" | 
                         P02_O9 == "Pierna" ~ 1, 
                       pain3m == "No" ~ as.numeric(NA), 
                       TRUE ~ 0), 
  dzon_ankle_foots = case_when(P02_O1 == "Tobillo y pie" | 
                                 P02_O2 == "Tobillo y pie" | 
                                 P02_O3 == "Tobillo y pie" | 
                                 P02_O4 == "Tobillo y pie" | 
                                 P02_O5 == "Tobillo y pie" | 
                                 P02_O6 == "Tobillo y pie" | 
                                 P02_O7 == "Tobillo y pie" | 
                                 P02_O8 == "Tobillo y pie" | 
                                 P02_O9 == "Tobillo y pie" ~ 1, 
                               pain3m == "No" ~ as.numeric(NA), 
                               TRUE ~ 0), 
  dzon_head = case_when(P02_O1 == "Cabeza" | 
                          P02_O2 == "Cabeza" | 
                          P02_O3 == "Cabeza" | 
                          P02_O4 == "Cabeza" | 
                          P02_O5 == "Cabeza" | 
                          P02_O6 == "Cabeza" | 
                          P02_O7 == "Cabeza" | 
                          P02_O8 == "Cabeza" | 
                          P02_O9 == "Cabeza" ~ 1, 
                        pain3m == "No" ~ as.numeric(NA), 
                        TRUE ~ 0), 
  dzon_neck = case_when(P02_O1 == "Cuello" | 
                          P02_O2 == "Cuello" | 
                          P02_O3 == "Cuello" | 
                          P02_O4 == "Cuello" | 
                          P02_O5 == "Cuello" | 
                          P02_O6 == "Cuello" | 
                          P02_O7 == "Cuello" | 
                          P02_O8 == "Cuello" | 
                          P02_O9 == "Cuello" ~ 1, 
                        pain3m == "No" ~ as.numeric(NA), 
                        TRUE ~ 0), 
  dzon_back = case_when(P02_O1 == "Espalda" | 
                          P02_O2 == "Espalda" | 
                          P02_O3 == "Espalda" | 
                          P02_O4 == "Espalda" | 
                          P02_O5 == "Espalda" | 
                          P02_O6 == "Espalda" | 
                          P02_O7 == "Espalda" | 
                          P02_O8 == "Espalda" | 
                          P02_O9 == "Espalda" ~ 1, 
                        pain3m == "No" ~ as.numeric(NA), 
                        TRUE ~ 0), 
  dzon_lumbar = case_when(P02_O1 == "Lumbar" | 
                            P02_O2 == "Lumbar" | 
                            P02_O3 == "Lumbar" | 
                            P02_O4 == "Lumbar" | 
                            P02_O5 == "Lumbar" | 
                            P02_O6 == "Lumbar" | 
                            P02_O7 == "Lumbar" | 
                            P02_O8 == "Lumbar" | 
                            P02_O9 == "Lumbar" ~ 1, 
                          pain3m == "No" ~ as.numeric(NA), 
                          TRUE ~ 0), 
  ### b) Pain sites in overall population----
  dzon_faceTot = case_when(P02_O1 == "Cara" | 
                             P02_O2 == "Cara" | 
                             P02_O3 == "Cara" | 
                             P02_O4 == "Cara" | 
                             P02_O5 == "Cara" | 
                             P02_O6 == "Cara" | 
                             P02_O7 == "Cara" | 
                             P02_O8 == "Cara" | 
                             P02_O9 == "Cara" ~ 1, 
                           TRUE ~ 0), 
  dzon_chestTot = case_when(P02_O1 == "Tórax" | 
                              P02_O2 == "Tórax" | 
                              P02_O3 == "Tórax" | 
                              P02_O4 == "Tórax" | 
                              P02_O5 == "Tórax" | 
                              P02_O6 == "Tórax" | 
                              P02_O7 == "Tórax" | 
                              P02_O8 == "Tórax" | 
                              P02_O9 == "Tórax" ~ 1, 
                            TRUE ~ 0), 
  dzon_shoulderTot = case_when(P02_O1 == "Hombro" | 
                                 P02_O2 == "Hombro" | 
                                 P02_O3 == "Hombro" | 
                                 P02_O4 == "Hombro" | 
                                 P02_O5 == "Hombro" | 
                                 P02_O6 == "Hombro" | 
                                 P02_O7 == "Hombro" | 
                                 P02_O8 == "Hombro" | 
                                 P02_O9 == "Hombro" ~ 1, 
                               TRUE ~ 0), 
  dzon_elbowTot = case_when(P02_O1 == "Codo" | 
                              P02_O2 == "Codo" | 
                              P02_O3 == "Codo" | 
                              P02_O4 == "Codo" | 
                              P02_O5 == "Codo" | 
                              P02_O6 == "Codo" | 
                              P02_O7 == "Codo" | 
                              P02_O8 == "Codo" | 
                              P02_O9 == "Codo" ~ 1, 
                            TRUE ~ 0), 
  dzon_abdomenTot = case_when(P02_O1 == "Abdomen" | 
                                P02_O2 == "Abdomen" | 
                                P02_O3 == "Abdomen" | 
                                P02_O4 == "Abdomen" | 
                                P02_O5 == "Abdomen" | 
                                P02_O6 == "Abdomen" | 
                                P02_O7 == "Abdomen" | 
                                P02_O8 == "Abdomen" | 
                                P02_O9 == "Abdomen" ~ 1, 
                              TRUE ~ 0), 
  dzon_hipTot = case_when(P02_O1 == "Cadera" | 
                            P02_O2 == "Cadera" | 
                            P02_O3 == "Cadera" | 
                            P02_O4 == "Cadera" | 
                            P02_O5 == "Cadera" | 
                            P02_O6 == "Cadera" | 
                            P02_O7 == "Cadera" | 
                            P02_O8 == "Cadera" | 
                            P02_O9 == "Cadera" ~ 1, 
                          TRUE ~ 0), 
  dzon_hand_wristTot = case_when(P02_O1 == "Mano y muñeca" | 
                                   P02_O2 == "Mano y muñeca" | 
                                   P02_O3 == "Mano y muñeca" | 
                                   P02_O4 == "Mano y muñeca" | 
                                   P02_O5 == "Mano y muñeca" | 
                                   P02_O6 == "Mano y muñeca" | 
                                   P02_O7 == "Mano y muñeca" | 
                                   P02_O8 == "Mano y muñeca" | 
                                   P02_O9 == "Mano y muñeca" ~ 1, 
                                 TRUE ~ 0), 
  dzon_thighTot = case_when(P02_O1 == "Muslo" | 
                              P02_O2 == "Muslo" | 
                              P02_O3 == "Muslo" | 
                              P02_O4 == "Muslo" | 
                              P02_O5 == "Muslo" | 
                              P02_O6 == "Muslo" | 
                              P02_O7 == "Muslo" | 
                              P02_O8 == "Muslo" | 
                              P02_O9 == "Muslo" ~ 1, 
                            TRUE ~ 0), 
  dzon_kneeTot = case_when(P02_O1 == "Rodilla" | 
                             P02_O2 == "Rodilla" | 
                             P02_O3 == "Rodilla" | 
                             P02_O4 == "Rodilla" | 
                             P02_O5 == "Rodilla" | 
                             P02_O6 == "Rodilla" | 
                             P02_O7 == "Rodilla" | 
                             P02_O8 == "Rodilla" | 
                             P02_O9 == "Rodilla" ~ 1, 
                           TRUE ~ 0), 
  dzon_legTot = case_when(P02_O1 == "Pierna" | 
                            P02_O2 == "Pierna" | 
                            P02_O3 == "Pierna" | 
                            P02_O4 == "Pierna" | 
                            P02_O5 == "Pierna" | 
                            P02_O6 == "Pierna" | 
                            P02_O7 == "Pierna" | 
                            P02_O8 == "Pierna" | 
                            P02_O9 == "Pierna" ~ 1, 
                          TRUE ~ 0), 
  dzon_ankle_footsTot = case_when(P02_O1 == "Tobillo y pie" | 
                                    P02_O2 == "Tobillo y pie" | 
                                    P02_O3 == "Tobillo y pie" | 
                                    P02_O4 == "Tobillo y pie" | 
                                    P02_O5 == "Tobillo y pie" | 
                                    P02_O6 == "Tobillo y pie" | 
                                    P02_O7 == "Tobillo y pie" | 
                                    P02_O8 == "Tobillo y pie" | 
                                    P02_O9 == "Tobillo y pie" ~ 1, 
                                  TRUE ~ 0), 
  dzon_headTot = case_when(P02_O1 == "Cabeza" | 
                             P02_O2 == "Cabeza" | 
                             P02_O3 == "Cabeza" | 
                             P02_O4 == "Cabeza" | 
                             P02_O5 == "Cabeza" | 
                             P02_O6 == "Cabeza" | 
                             P02_O7 == "Cabeza" | 
                             P02_O8 == "Cabeza" | 
                             P02_O9 == "Cabeza" ~ 1, 
                           TRUE ~ 0), 
  dzon_neckTot = case_when(P02_O1 == "Cuello" | 
                             P02_O2 == "Cuello" | 
                             P02_O3 == "Cuello" | 
                             P02_O4 == "Cuello" | 
                             P02_O5 == "Cuello" | 
                             P02_O6 == "Cuello" | 
                             P02_O7 == "Cuello" | 
                             P02_O8 == "Cuello" | 
                             P02_O9 == "Cuello" ~ 1, 
                           TRUE ~ 0), 
  dzon_backTot = case_when(P02_O1 == "Espalda" | 
                             P02_O2 == "Espalda" | 
                             P02_O3 == "Espalda" | 
                             P02_O4 == "Espalda" | 
                             P02_O5 == "Espalda" | 
                             P02_O6 == "Espalda" | 
                             P02_O7 == "Espalda" | 
                             P02_O8 == "Espalda" | 
                             P02_O9 == "Espalda" ~ 1, 
                           TRUE ~ 0), 
  dzon_lumbarTot = case_when(P02_O1 == "Lumbar" | 
                               P02_O2 == "Lumbar" | 
                               P02_O3 == "Lumbar" | 
                               P02_O4 == "Lumbar" | 
                               P02_O5 == "Lumbar" | 
                               P02_O6 == "Lumbar" | 
                               P02_O7 == "Lumbar" | 
                               P02_O8 == "Lumbar" | 
                               P02_O9 == "Lumbar" ~ 1, 
                             TRUE ~ 0), 
  ### c) Pain sites according time----
  pain_1_type = case_when(pain3m_1_t == "Menos de 1 semana" | 
                            pain3m_1_t == "Entre 2 y 4 semanas" | 
                            pain3m_1_t == "Entre 1 y 3 meses" ~ "Acute (<=3 meses)", 
                          pain3m_1_t == "Más de 3 meses" ~ "Chronic (> 3 meses)", 
                          pain3m_1_t == "No recuerdo (E: No leer)" ~ as.character(NA)),
  pain_1_type = factor(pain_1_type), 
  
  ### d) Current pain level----
  pain_act1_niv = case_when(pain_act1_int <= 3 ~ "Mild (NRS 1-3)", 
                            pain_act1_int >= 4 & pain_act1_int <= 6 ~ "Moderate (NRS 4-6)", 
                            pain_act1_int >= 7 & pain_act1_int <= 10 ~ "Severe (NRS 7-10)"), 
  pain_act1_niv = factor(pain_act1_niv, 
                         levels = c("Mild (NRS 1-3)", 
                                    "Moderate (NRS 4-6)", 
                                    "Severe (NRS 7-10)"), 
                         ordered = TRUE), 
  
  # e) Level of start pain----
  pain_1_niv_in = case_when(pain3m_1_int_in <= 3 ~ "Mild (NRS 1-3)", 
                            pain3m_1_int_in >= 4 & pain3m_1_int_in <= 6 ~ "Moderate (NRS 4-6)", 
                            pain3m_1_int_in >= 7 & pain3m_1_int_in <= 10 ~ "Severe (NRS 7-10)"), 
  pain_1_niv_in = factor(pain_1_niv_in, 
                         levels = c("Mild (NRS 1-3)", 
                                    "Moderate (NRS 4-6)", 
                                    "Severe (NRS 7-10)"), 
                         ordered = TRUE)
) %>% 
  # i) Otros pain----
mutate(
  pain_n_sitesA = dzon_face + dzon_chest + dzon_shoulder + dzon_elbow + 
    dzon_abdomen + dzon_hip + dzon_hand_wrist + dzon_thigh + 
    dzon_knee + dzon_leg + dzon_ankle_foots + dzon_head + dzon_neck + 
    dzon_back + dzon_lumbar, 
  pain_n_sitesB = pain_n_sitesA*1, 
  pain_n_sites = case_when(is.na(pain_n_sitesA) ~ 0, 
                           TRUE ~ pain_n_sitesA)
) %>% 
  mutate(
    pain_n_sitescatA = case_when(pain_n_sites == 0 ~ "None", 
                                 pain_n_sites == 1 ~ "Only 1", 
                                 pain_n_sites == 2 ~ "Only 2", 
                                 pain_n_sites >= 3 ~ "Three or more"), 
    
    pain_n_sitescatB = case_when(pain_n_sites == 0 ~ "None", 
                                 pain_n_sites == 1 ~ "Only 1", 
                                 pain_n_sites >= 2 ~ "Two or more"),
    
    pain_n_sitescatC = case_when(pain_n_sites == 0 ~ as.character(NA), 
                                 pain_n_sites == 1 ~ "Only 1", 
                                 pain_n_sites == 2 ~ "Only 2", 
                                 pain_n_sites >= 3 ~ "Three or more"), 
    
    pain_n_sitescatD = case_when(pain_n_sites == 0 ~ as.character(NA), 
                                 pain_n_sites == 1 ~ "Only 1", 
                                 pain_n_sites >= 2 ~ "Two or more"), 
    
    pain_solo2 = case_when(!is.na(pain3m_2) ~ 1, 
                           TRUE ~ 0),
    pain_solo1 = case_when(!is.na(pain3m_1) &  pain_solo2 == 0 ~ 1, 
                           TRUE ~ 0), 
    pain_actual = case_when(pain_act1 == "Sí" | pain_act2 == "Sí" ~ 1, 
                            TRUE ~ 0), 
    pain_actual2 = case_when(is.na(pain_act2) | pain_act2 == "No" ~ 0, 
                             TRUE ~ 1), 
    pain_actual1 = case_when(is.na(pain_act1) | pain_act1 == "No" ~ 0, 
                             TRUE ~ 1), 
    pain_actualboth = case_when(pain_act1 == "Sí" & pain_act2 == "Sí" ~ 1, 
                                TRUE ~ 0) 
  ) %>% 
  mutate(
    # Tipo de tratamiento 1
    tto_medic1 = case_when(P10_O1 == "Medicamentos o fármacos" |  
                             P10_O2 == "Medicamentos o fármacos" | 
                             P10_O3 == "Medicamentos o fármacos" | 
                             P10_O4 == "Medicamentos o fármacos" ~ 1, 
                           pain3m == "No" ~ as.numeric(NA), 
                           TRUE ~ 0), 
    tto_surg1 = case_when(P10_O1 == "Cirugía o tratamiento Quirúrgico" | 
                            P10_O2 == "Cirugía o tratamiento Quirúrgico" | 
                            P10_O3 == "Cirugía o tratamiento Quirúrgico" | 
                            P10_O4 == "Cirugía o tratamiento Quirúrgico" ~ 1, 
                          pain3m == "No" ~ as.numeric(NA), 
                          TRUE ~ 0), 
    tto_infilt1 = case_when(P10_O1 == "Infiltraciones de medicamentos en la articulación" | 
                              P10_O2 == "Infiltraciones de medicamentos en la articulación" | 
                              P10_O3 == "Infiltraciones de medicamentos en la articulación" | 
                              P10_O4 == "Infiltraciones de medicamentos en la articulación" ~ 1, 
                            pain3m == "No" ~ as.numeric(NA), 
                            TRUE ~ 0), 
    tto_physio1 = case_when(P10_O1 == "Fisioterapia o rehabilitación" | P10_O1 == "Masajes" | 
                              P10_O2 == "Fisioterapia o rehabilitación" | P10_O2 == "Masajes" | 
                              P10_O3 == "Fisioterapia o rehabilitación" | P10_O3 == "Masajes" | 
                              P10_O4 == "Fisioterapia o rehabilitación" | P10_O1 == "Masajes" ~ 1, 
                            pain3m == "No" ~ as.numeric(NA), 
                            TRUE ~ 0), 
    tto_altermed1 = case_when(P10_O1 == "Medicina alternativa (Ej. plantas medicinales, acupuntura, medicina china, homeopatía, etc.)" | 
                                P10_O2 == "Medicina alternativa (Ej. plantas medicinales, acupuntura, medicina china, homeopatía, etc.)" | 
                                P10_O3 == "Medicina alternativa (Ej. plantas medicinales, acupuntura, medicina china, homeopatía, etc.)" | 
                                P10_O4 == "Medicina alternativa (Ej. plantas medicinales, acupuntura, medicina china, homeopatía, etc.)" ~ 1, 
                              pain3m == "No" ~ as.numeric(NA), 
                              TRUE ~ 0), 
    tto_homemed1 = case_when(P10_O1 == "Remedios caseros" | 
                               P10_O2 == "Remedios caseros" | 
                               P10_O3 == "Remedios caseros" | 
                               P10_O4 == "Remedios caseros" ~ 1, 
                             pain3m == "No" ~ as.numeric(NA), 
                             TRUE ~ 0), 
    tto_massage1 = case_when(P10_O1 == "Masajes" | 
                               P10_O2 == "Masajes" | 
                               P10_O3 == "Masajes" | 
                               P10_O4 == "Masajes" ~ 1, 
                             pain3m == "No" ~ as.numeric(NA), 
                             TRUE ~ 0), 
    tto_none1 = case_when(P10_O1 == "Ninguno, ningún tratamiento" | 
                            P10_O2 == "Ninguno, ningún tratamiento" | 
                            P10_O3 == "Ninguno, ningún tratamiento" | 
                            P10_O4 == "Ninguno, ningún tratamiento" ~ 1, 
                          pain3m == "No" ~ as.numeric(NA), 
                          TRUE ~ 0), 
    # j) Familia de medicamento 1----
    dtto_nsaid1 = case_when(T_P11_1_1 %in% nsaid_list | 
                              T_P11_1_2 %in% nsaid_list |
                              T_P11_1_3 %in% nsaid_list |
                              T_P11_1_4 %in% nsaid_list | 
                              T_P11_1_5 %in% nsaid_list |
                              T_P11_1_6 %in% nsaid_list ~ 1, 
                            pain3m == "No" ~ as.numeric(NA), 
                            TRUE ~ 0),
    dtto_opio_debil1 = case_when(T_P11_1_1 == "Tramadol" | 
                                   T_P11_1_2 == "Tramadol" | 
                                   T_P11_1_3 == "Tramadol" | 
                                   T_P11_1_4 == "Tramadol" | 
                                   T_P11_1_5 == "Tramadol" | 
                                   T_P11_1_6 == "Tramadol" ~ 1, 
                                 pain3m == "No" ~ as.numeric(NA), 
                                 TRUE ~ 0), 
    dtto_tto_comb1 = case_when(dtto_nsaid1 == 0 & dtto_opio_debil1 == 0 & tto_medic1 == 0 ~ "No medication", 
                               dtto_nsaid1 == 0 & dtto_opio_debil1 == 0 & tto_medic1 == 1 ~ "Other drugs, no NSAID neither opioid", 
                               dtto_nsaid1 == 1 & dtto_opio_debil1 == 0 ~ "Only NSAID",
                               dtto_nsaid1 == 0 & dtto_opio_debil1 == 1 ~ "Only weak opioid",
                               dtto_nsaid1 == 1 & dtto_opio_debil1 == 1 ~ "NSAID + weak opioid"), 
    # k) Familia de medicamento 2----
    dtto_nsaid2 = case_when(T_P32_1_1 %in% nsaid_list | 
                              T_P32_1_2 %in% nsaid_list | 
                              T_P32_1_3 %in% nsaid_list | 
                              T_P32_1_4 %in% nsaid_list |
                              T_P32_1_5 %in% nsaid_list | 
                              T_P32_1_6 %in% nsaid_list ~ 1, 
                            pain3m == "No" ~ as.numeric(NA), 
                            TRUE ~ 0),
    dtto_opio_debil2 = case_when(T_P32_1_1 == "Tramadol" | 
                                   T_P32_1_2 == "Tramadol" | 
                                   T_P32_1_3 == "Tramadol" | 
                                   T_P32_1_4 == "Tramadol" | 
                                   T_P32_1_5 == "Tramadol" | 
                                   T_P32_1_6 == "Tramadol" ~ 1, 
                                 pain3m == "No" ~ as.numeric(NA), 
                                 TRUE ~ 0), 
    # l) Relative to Quality of Life----
    scoreQoL = as.integer(QoL) - 1, 
    QoL_cat = case_when(scoreQoL <= 3 ~ 1, 
                        scoreQoL >= 4 & scoreQoL <= 5 ~ 2, 
                        scoreQoL >= 6 & scoreQoL <= 7 ~ 3, 
                        scoreQoL >= 8 & scoreQoL <= 10 ~ 4, 
                        TRUE ~ as.numeric(NA)), 
    QoL_cat2 = case_when(scoreQoL <= 5 ~ 1, 
                         scoreQoL >= 6 & scoreQoL <= 7 ~ 2, 
                         scoreQoL >= 8 & scoreQoL <= 10 ~ 3, 
                         TRUE ~ as.numeric(NA)), 
    # m) Collapsing values of variables with few cases per cell----
    maritalstatcat = case_when(maritalstat == "Soltero" ~ "Single", 
                               maritalstat == "Casado, conviviente" ~ 
                                 "Married/cohabiting", 
                               maritalstat == "Viudo" | 
                                 maritalstat == "Separado, divorciado" ~ 
                                 "Divorced/separated/widowed", 
                               TRUE ~ as.character(NA)), 
    educat = case_when(educ == "No ha estudiado" | 
                         educ == "Primaria (completa, incompleta)" ~ 
                         "No study/Primary", 
                       educ == "Secundaria (completa, incompleta)" ~ 
                         "High school",
                       educ == "Técnica (completa, incompleta)" ~ 
                         "Technical superior", 
                       educ == "Universitaria (completa, incompleta)" | 
                         educ == "Maestría, Doctorado (completa, incompleta)" ~ 
                         "University superior", 
                       TRUE ~ as.character(NA)), 
    ocupcat1 = case_when(ocup == "Desocupado, desempleado, no trabaja" | 
                           ocup == "Jubilado / cesante" ~ "No studies nor works", 
                         ocup == "Ama de casa" ~ "Homeworker", 
                         ocup == "Estudiante" ~ "Only studies", 
                         ocup == "Trabaja tiempo completo (incluye independiente)" ~ 
                           "Full time work", 
                         ocup == "Trabaja a medio tiempo" ~ 
                           "TPart time work", 
                         TRUE ~ as.character(NA)), 
    ocupcat2 = case_when(ocup == "Desocupado, desempleado, no trabaja" | 
                           ocup == "Jubilado / cesante" ~ "No studies nor works", 
                         ocup == "Estudiante" ~ "Only studies", 
                         ocup == "Trabaja tiempo completo (incluye independiente)" | 
                           ocup == "Ama de casa" | ocup == "Trabaja a medio tiempo" ~ 
                           "Works", 
                         TRUE ~ as.character(NA)), 
    hinsurancecat = case_when(hinsurance == "SIS, Sistema Integrado de Salud" ~ 
                                "Comprenhensive Health Insurance", 
                              hinsurance == "EsSalud" ~ 
                                "Social Health Security", 
                              hinsurance == "Fuerzas Armadas y Policiales" ~ 
                                "Armed Forces and Police health insurance", 
                              hinsurance == "EPS, Entidad Privada de Salud" | 
                                hinsurance == "Privado, particular" ~ 
                                "Private Health Insurance", 
                              hinsurance == "No tiene seguro" ~ 
                                "No insurance", 
                              TRUE ~ as.character(NA)), 
    # n) Extra edits----
    ager2 = case_when(
      age >= 18 & age <= 40 ~ "18-40 years", 
      age >= 41 & age <= 60 ~ "41-60 years", 
      age > 60 ~ "61+ years"
    ), 
    ager2 = factor(ager2), 
    tto_remed_alter = case_when(
      tto_homemed1 == 0 & tto_altermed1 == 0 ~ 0, 
      (tto_homemed1 == 1 & tto_altermed1 == 0) | 
        (tto_homemed1 == 0 & tto_altermed1 == 1) | 
        (tto_homemed1 == 1 & tto_altermed1 == 1) ~ 1, 
      TRUE ~ as.numeric(NA)
    ), 
    pain_cronic = case_when(
      pain_1_type == "Chronic (> 3 meses)" ~ 1, 
      (pain_1_type == "Acute (<=3 meses)" |  is.na(pain_1_type)) & !is.na(pain3m) ~ 0, 
      TRUE ~ as.numeric(NA)
    ), 
    pain_acute = case_when(
      pain_1_type == "Acute (<=3 meses)" ~ 1, 
      (pain_1_type == "Chronic (> 3 meses)" |  is.na(pain_1_type)) & !is.na(pain3m) ~ 0, 
      TRUE ~ as.numeric(NA)
    ),
    rtotal = 1, 
    tto_medic1a = case_when(
      dtto_tto_comb1 == "No medication" ~ 0, 
      dtto_tto_comb1 %in% c("Other drugs, no NSAID neither opioid", 
                            "Only NSAID", 
                            "Only weak opioid", 
                            "NSAID + weak opioid") ~ 1, 
      TRUE ~ as.numeric(NA)
    )
  ) %>% 
  # 3.3. Recoding values of variables----
mutate(
  place = fct_recode(place, 
                     "North" = "CONO NORTE", 
                     "East" = "CONO ESTE", 
                     "South" = "CONO SUR", 
                     "Residential Lima" = "LIMA RESIDENCIAL",
                     "Center Lima" = "LIMA CENTRO"), 
  pain3m_1_freq = fct_recode(pain3m_1_freq, 
                             "Intermitent" = "Intermitente (que se interrumpe y prosigue cada cierto tiempo de manera reiterada)", 
                             "Sporadic" = "Esporádico (que se hace o sucede con poca frecuencia, con intervalos de tiempos irregulares y de forma aislada, sin rel", 
                             "Frequent" = "Frecuente (que ocurre, se hace o se repite a menudo, con intervalos más o menos cercanos", 
                             "Constant" = "Constante (persistente y duradero y se reitera continuamente"), 
  sex = fct_recode(sex, 
                   "Male" = "Hombre", 
                   "Female" = "Mujer"), 
  pain3m = fct_recode(pain3m, 
                      "Yes" = "Sí", 
                      "No" = "No"), 
  pain3m = fct_relevel(pain3m, c("No", "Yes")), 
  pain_act1 = fct_recode(pain_act1, 
                         "No" = "No", 
                         "Yes" = "Sí"), 
  pain_act1 = fct_relevel(pain_act1, c("No", "Yes")), 
  pain3m_1_t = fct_recode(pain3m_1_t, 
                          "< 1 week" = "Menos de 1 semana", 
                          "Between 2 to 4 weeks" = "Entre 2 y 4 semanas", 
                          "Between 1 to 3 months" = "Entre 1 y 3 meses", 
                          ">3 months" = "Más de 3 meses")
) %>%
  mutate(
    across(
      c(maritalstat, educ, ocup, hinsurance, pain3m_1_t, pain3m_1_freq), 
      fct_recode,  
      NULL = "Nc (E: No leer)", 
      NULL = "Nc (E. No leer)", 
      NULL = "No recuerdo (E: No leer)", 
      NULL = "Ns (E: No leer)", 
      NULL = "Nc (E: No leer)"
    )
  ) %>% 
  # 3.4. Converting in factor----
mutate(
  educ = factor(educ, ordered = TRUE), 
  educat = factor(educat, 
                  levels = c("No study/Primary", "High school", 
                             "Technical superior", "University superior"), 
                  ordered = TRUE), 
  hinsurancecat = fct_relevel(hinsurancecat, 
                              c("Comprenhensive Health Insurance", 
                                "Social Health Security", 
                                "Armed Forces and Police health insurance", 
                                "Private Health Insurance", 
                                "No insurance")), 
  maritalstatcat = fct_relevel(maritalstatcat, 
                               c("Single", "Married/cohabiting", 
                                 "Divorced/separated/widowed")), 
  econ = factor(econ, level = c("A", "B", "C", "D", "E"), 
                ordered = TRUE), 
  econcat = factor(econcat, levels = c("A/B", "C", "D", "E"), 
                   labels = c("A/B (richest level)", "C", "D", "E (poorest level)"), 
                   ordered = TRUE), 
  ager = fct_recode(ager, 
                    "18-24 years" = "18/24", "25-34 years" = "25/34", 
                    "35-44 years" = "35/44", "45-54 years" = "45/54", 
                    "55-70 years" = "55/70"), 
  ager = factor(ager, ordered = TRUE), 
  QoL = factor(QoL, ordered  = TRUE), 
  QoL_cat = factor(QoL_cat, levels = c(1, 2, 3, 4), 
                   labels = c("0-3", "4-5", "6-7", "8-10"), 
                   ordered = TRUE), 
  QoL_cat2 = factor(QoL_cat2, levels = c(1, 2, 3), 
                    labels = c("0-5 (Low)", "6-7 (Middle)", "8-10 (High)"), 
                    ordered = TRUE), 
  pain_n_sitescatA = factor(pain_n_sitescatA, 
                            level = c("None", "Only 1", 
                                      "Only 2", "Three or more"), 
                            ordered = TRUE), 
  pain_n_sitescatB = factor(pain_n_sitescatB, 
                            level = c("None", "Only 1", 
                                      "Two or more"), ordered = TRUE), 
  pain_n_sitescatC = factor(pain_n_sitescatA, 
                            level = c("Only 1", 
                                      "Only 2", "Three or more"), 
                            ordered = TRUE), 
  pain_n_sitescatD = factor(pain_n_sitescatB, 
                            level = c("Only 1", 
                                      "Two or more"), ordered = TRUE), 
  dtto_tto_comb1 = factor(dtto_tto_comb1, 
                          levels = c("No medication", 
                                     "Only NSAID", 
                                     "Only weak opioid", 
                                     "NSAID + weak opioid", 
                                     "Other drugs, no NSAID neither opioid"), 
                          ordered = TRUE)
) %>% 
  # 3.5. Labeling variables----
set_variable_labels(
  # a) Variables de diseño muestral----
  MAPA = "Primary Sampling Unit", 
  MANZANA = "Secondary Sampling Unit", 
  VIVIENDA = "Tertiary Sampling Unit", 
  ENTREVISTADO = "Quaternary sampling unit", 
  PON = "Sampling Weight", 
  
  # b) Variables sociodemográficas----
  distrit = "District", 
  place = "Lima sector", 
  econ = "Socioeconomic level", 
  econcat = "Socioeconomic level", 
  sex = "Sex", 
  ager = "Age group", 
  age = "Age, years", 
  maritalstat = "Marital Status", 
  maritalstatcat = "Marital Status", 
  educ = "Educational level", 
  educat = "Educational level", 
  ocup = "Occupational status", 
  ocupcat1 = "Occupational status", 
  ocupcat2 = "Occupational status", 
  hinsurance = "Health's insurance",
  hinsurancecat = "Health's insurance",
  
  # c) Relativas al pain----
  pain3m = "Pain at last 3 months",  
  pain3m_1 = "Zona de primer pain que más preocupa", 
  pain3m_2 = "Zona de segundo pain que más preocupa ", 
  pain_n_sites = "Number of body sites with pain", 
  pain_n_sitesB = "Number of body sites with pain", 
  pain_n_sitescatA = "Number of body sites with pain", 
  pain_n_sitescatB = "Number of body sites with pain", 
  pain_n_sitescatC = "Number of body sites with pain", 
  pain_n_sitescatD = "Number of body sites with pain", 
  pain_solo1 = "Only have one body site with pain", 
  pain_solo2 = "Tiene al menos dos sites apainidas que le preocupan en últimos 3 meses", 
  pain_actual = "Have current pain", 
  pain_actual1 = "Have the current pain of greatest concern", 
  pain_actual2 = "Segundo pain que más le preocupa actualmente activo",
  pain_actualboth = "Dos pnsaid que más le preocupan actualmente activos", 
  pain3m_1_t = "Onset of pain", 
  pain_1_type = "Type of pain", 
  pain_act1 = "Actualmente tiene primer pain que más preocupa", 
  pain_act2 = "Actualmente tiene segundo pain que más preocupa", 
  pain_act1_int = "Intensidad actual de primer pain que más preocupa",
  pain_act1_niv = "Current level of pain of greatest concern", 
  pain3m_1_int_in = "Initial intensity of pain in NRS", 
  pain_1_niv_in = "Initial intensity level of pain", 
  pain3m_1_freq = "Pain frquency", 
  dzon_face = "Face", 
  dzon_chest = "Chest", 
  dzon_shoulder = "Shoulder", 
  dzon_elbow = "Elbow", 
  dzon_abdomen = "Abdomen", 
  dzon_hip = "Hip", 
  dzon_hand_wrist = "Hand and wrist", 
  dzon_thigh = "Thigh", 
  dzon_knee = "Knee", 
  dzon_leg = "Leg", 
  dzon_ankle_foots = "Ankle and foot", 
  dzon_head = "Head", 
  dzon_neck = "Neck", 
  dzon_back = "Upper back", 
  dzon_lumbar = "Lower back", 
  dzon_faceTot = "Face", 
  dzon_chestTot = "Chest", 
  dzon_shoulderTot = "Shoulder", 
  dzon_elbowTot = "Elbow", 
  dzon_abdomenTot = "Abdomen", 
  dzon_hipTot = "Hip", 
  dzon_hand_wristTot = "Hand and wrist", 
  dzon_thighTot = "Tigh", 
  dzon_kneeTot = "Knee", 
  dzon_legTot = "Leg", 
  dzon_ankle_footsTot = "Ankle and foot", 
  dzon_headTot = "Head", 
  dzon_neckTot = "Neck", 
  dzon_backTot = "Back", 
  dzon_lumbarTot = "Lumbar", 
  
  # Tipo de tratamiento----
  tto_medic1 = "Received / used medications / drugs", 
  tto_medic1a = "Received / used medications / drugs", 
  tto_surg1 = "Received / used surgery", 
  tto_infilt1 = "Received / used infiltrations in the joint", 
  tto_physio1 = "Received / used Physiotherapy / Rehabilitation", 
  tto_altermed1 = "Received / used alternative medicine", 
  tto_homemed1 = "Received / used home remedy ", 
  tto_massage1 = "Received / used massages", 
  tto_none1 = "Did not receive any treatment", 
  
  # f) Tratamiento----
  dtto_nsaid1 = "nsaid para primer pain que más preocupa", 
  dtto_opio_debil1 = "Opiáceo débil para primer pain que más preocupa", 
  dtto_tto_comb1 = "Type of drug treatment received", 
  dtto_nsaid2 = "nsaid para segundo pain que más preocupa", 
  dtto_opio_debil2 = "Opiáceo débil para segundo pain que más preocupa", 
  
  # g) Calidad de vida----
  QoL = "Escala de Calidad de vidad ACPA", 
  QoL_cat = "Escala de Calidad de vida ACPA", 
  QoL_cat2 = "Escala de Calidad de vida ACPA", 
  scoreQoL = "Puntaje de Calidad de vida ACPA",
  
  # h) extra
  ager2 = "Age group", 
  tto_remed_alter = "Received/used home remedy or alternative medicine",
  rtotal = "Total"
) %>%
  
  # 3.3. Selecting prepared variables for analysis----
dplyr::select(
  MAPA, 
  MANZANA, 
  VIVIENDA, 
  ENTREVISTADO, 
  PON, 
  distrit, 
  place, 
  econ, 
  econcat, 
  sex, 
  ager, 
  ager2, 
  age, 
  maritalstat, 
  maritalstatcat, 
  educ, 
  educat, 
  ocup, 
  ocupcat1, 
  ocupcat2, 
  hinsurance,
  hinsurancecat, 
  pain3m, 
  pain_n_sites, 
  pain_n_sitesB,
  pain_n_sitescatA, 
  pain_n_sitescatB, 
  pain_n_sitescatC, 
  pain_n_sitescatD, 
  pain_actual, 
  pain3m_1, 
  pain3m_2,
  pain_solo1, 
  pain_solo2,
  pain_actual1, 
  pain_actual2, 
  pain_actualboth, 
  pain3m_1_t, 
  pain_1_type, 
  pain_act1, 
  pain_act2 , 
  pain_act1_int,
  pain_act1_niv, 
  pain3m_1_int_in, 
  pain_1_niv_in, 
  pain3m_1_freq, 
  pain_cronic, 
  pain_acute, 
  dzon_face, 
  dzon_chest, 
  dzon_shoulder, 
  dzon_elbow, 
  dzon_abdomen, 
  dzon_hip, 
  dzon_hand_wrist, 
  dzon_thigh, 
  dzon_knee, 
  dzon_leg, 
  dzon_ankle_foots, 
  dzon_head, 
  dzon_neck, 
  dzon_back, 
  dzon_lumbar, 
  dzon_faceTot, 
  dzon_chestTot, 
  dzon_shoulderTot, 
  dzon_elbowTot, 
  dzon_abdomenTot, 
  dzon_hipTot, 
  dzon_hand_wristTot, 
  dzon_thighTot, 
  dzon_kneeTot, 
  dzon_legTot, 
  dzon_ankle_footsTot, 
  dzon_headTot, 
  dzon_neckTot, 
  dzon_backTot, 
  dzon_lumbarTot, 
  tto_medic1, 
  tto_medic1a, 
  tto_surg1, 
  tto_infilt1, 
  tto_physio1, 
  tto_altermed1, 
  tto_homemed1, 
  tto_massage1, 
  tto_none1,
  tto_remed_alter, 
  dtto_nsaid1, 
  dtto_opio_debil1, 
  dtto_tto_comb1, 
  dtto_nsaid2, 
  dtto_opio_debil2, 
  QoL,
  QoL_cat, 
  QoL_cat2, 
  scoreQoL, 
  rtotal
) 

# Save derived dataset
saveRDS(object = derived_data, file = "Data/Derived/derived_data.rds")

##***************************************************************
##  4. Complex Survye Setting                                  **----
##***************************************************************

# Eploring dataset
skim(derived_data)
glimpse(derived_data)

# Seting complex survey data: Total sample for estimates of total population
svydes <- svydesign(id = ~ MAPA + MANZANA + VIVIENDA + ENTREVISTADO, 
                    weights = ~ PON, data = derived_data)

svydes_pain <- subset(svydes, pain3m == "Yes")
orig <- svydes$variables
final <- svydes_pain$variables
final <- final %>% copy_labels_from(orig)
svydes_pain$variables <- final # Recuperando labels

# Save all final datasets for analysis
save(svydes, svydes_pain, derived_data, file = "Data/Derived/data_final.RData")