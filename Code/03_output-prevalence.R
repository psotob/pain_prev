###************************************************************************
###************************************************************************
###                                                                     ***
###                               CODE 3:                               ***
###                       PREVALENCE ESTIMATION                         ***
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


## Prevalence of pain, chronic pain and acute pain:
svyciprop(formula = I(pain3m == "Yes") ~ 1, design = svydes, level = 0.95)
svyciprop(formula = I(pain_cronic == 1) ~ 1, design = svydes, level = 0.95)
svyciprop(formula = I(pain_acute == 1) ~ 1, design = svydes, level = 0.95)

## Prevalence of received treatment
svyciprop(formula = I(tto_medic1 == 1) ~ 1, design = svydes, level = 0.95)
svyciprop(formula = I(tto_none1 == 1) ~ 1, design = svydes, level = 0.95)
svyciprop(formula = I(tto_physio1 == 1) ~ 1, design = svydes, level = 0.95)
svyciprop(formula = I(tto_surg1 == 1) ~ 1, design = svydes, level = 0.95)
svyciprop(formula = I(tto_remed_alter == 1) ~ 1, design = svydes, level = 0.95)