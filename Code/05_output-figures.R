###************************************************************************
###************************************************************************
###                                                                     ***
###                               CODE 5:                               ***
###                          PREPARING FIGURES                          ***
###                                                                     ***
###************************************************************************
###************************************************************************

##*************************************************************************
##  @project   	Grunenthal Encuesta de pain                              *
##  @created   	15 de abril, 2021                                         *
##  @revised   	15 de abril, 2021                                         *
##  @category  	Coding tables for report                                  *
##  @R version  R version 4.0.4 (2021-02-15) -- 'Lost Library Book'       *
##  @OS system  MS Windows 10 Pro x 64 bits                               *
##  @author    	Percy Soto-Becerra <percys1991@gmail.com>                 *
##*************************************************************************

##****************************************************************
##  1. Configuration of environment and packages                **----
##****************************************************************

# Removing all objects including loaded libraries
rm(list = ls(all = TRUE))
gc()

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
       skimr,
       gtsummary, 
       flextable, 
       survey, 
       srvyr, 
       broom, 
       gt, 
       tab, 
       purrr, 
       formattable, 
       gdtools, 
       officer) # Loading packages

# Session Info inspection
sessionInfo()


##***************************************************************
##  2. Complex Survye Setting                                  **----
##***************************************************************

# Importing .sav data to tibble format
load("Data/Derived/data_final.RData")

##***************************************************************
##  3. Figures                                            **----
##***************************************************************

# Figure 1----

table3b <- 
  svydes_pain %>% 
  tbl_svysummary(
    include = c(dzon_cabezaTot, dzon_lumbTot, dzon_espaldTot, dzon_rodillTot, 
                dzon_hombroTot, dzon_cuelloTot, dzon_abdomenTot,dzon_tobill_pieTot, 
                dzon_mano_munecaTot, dzon_piernaTot, dzon_caderaTot, dzon_codoTot, 
                dzon_toraxTot, dzon_caraTot, dzon_musloTot, pain_1_type), 
    by = "pain_1_type", 
    digits = list(all_categorical() ~ c(0, 1)), 
    missing_text = "Datos faltantes"
  ) %>%  
  add_p() %>% 
  modify_header(
    update = list(
      label ~ "**Características**", 
      p.value ~ "**Valor p**"
    )
  ) %>% 
  modify_spanning_header(
    c("stat_1", "stat_2") ~ "**Tipo de pain que más preocupa en últimos 3 meses**"
  ) %>% 
  bold_labels() %>% 
  bold_p(t = 0.05)

table3b