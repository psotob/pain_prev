###************************************************************************
###************************************************************************
###                                                                     ***
###                               CODE 2:                               ***
###                        EXPLORATORY ANALYSIS                         ***
###                                                                     ***
###************************************************************************
###************************************************************************

##*************************************************************************
##  @project   	Grunenthal Encuesta de pain                              *
##  @created   	15 de abril, 2021                                         *
##  @revised   	15 de abril, 2021                                         *
##  @category  	Exploratory Data Analysis                                 *
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

# Installing and loading packages
if (!require("pacman")) install.packages("pacman")

library(pacman)

p_load(dplyr, 
        tibble, 
        tidyr, 
        readr, 
        ggplot2, 
        haven, 
        labelled, 
        skimr, 
        survey, 
        srvyr, 
        e1071) # Loading packages


# Session Info inspection
sessionInfo()

##***************************************************************
##  2. Complex Survye Setting                                  **----
##***************************************************************

# Importing .sav data to tibble format
load("Data/Derived/data_final.RData")

# Eploring dataset
skim(derived_data)
glimpse(derived_data)

# Seting complex survey data
svydes <- derived_data %>% 
  as_survey_design(
    ids = c(MAPA, MANZANA, VIVIENDA, ENTREVISTADO), 
    weights = PON
    )

##***************************************************************
##  3. Exploratory Data Analysis                               **----
##***************************************************************

# Exploring pweights----
hist(derived_data$PON)
boxplot(derived_data$PON)

derived_data %>% 
  summarize_at(vars(PON), 
               list(
                 Min = min, 
                 Max = max, 
                 Media = mean, 
                 Mediana = median, 
                 DE = sd, 
                 Asimetria = skewness, 
                 Kurtosis = kurtosis, 
                 p25 = ~ quantile(.x, probs = .25), 
                 p75 = ~ quantile(.x, probs = .75),
                 IQR = ~ IQR(.x), 
                 Max_Box = ~ IQR(.x) * 1.5 + quantile(.x, probs = .75)
                 )
               )

derived_data %>% 
  filter(PON > 6) %>% 
  count()

derived_data %>% 
  filter(PON > 2.08) %>% 
  count()

derived_data %>% 
  filter(PON < 6) %>% 
  summarize_at(vars(PON), 
               list(
                 Min = min, 
                 Max = max, 
                 Media = mean, 
                 Mediana = median, 
                 DE = sd, 
                 Asimetria = skewness, 
                 Kurtosis = kurtosis, 
                 p25 = ~ quantile(.x, probs = .25), 
                 p75 = ~ quantile(.x, probs = .75),
                 IQR = ~ IQR(.x), 
                 Max_Box = ~ IQR(.x) * 1.5 + quantile(.x, probs = .75)
                 )
               )

# Comparing weighted versus unweighted numerical variables
cdf_w <- survey::svycdf(~ age + pain_act1_int + pain3m_1_int_in + scoreQoL, 
                        svydes)
par(mfrow = c(2, 2))

# age----
cdf_uw <- ecdf(derived_data$age)
par(mar = c(4.1, 4.1, 1.1, 2.1))
plot(cdf_uw, do.points = FALSE, xlab = "age", 
     ylab = "Probabilidad acumulada", main = "", lwd = 1, verticals = TRUE, 
     col = "red")
lines(cdf_w[[1]], lwd = 2, col.vert = "green", col.hor = "green", 
      do.points = FALSE)
legend("bottomright", lwd = c(1, 2), bty = "n", col = c("red", "green"), 
       legend = c("No ponderado", "Ponderado"))

# Intensidad actual de primer pain que más preocupa----
cdf_uw <- ecdf(derived_data$pain_act1_int)
par(mar = c(4.1, 4.1, 1.1, 2.1))
plot(cdf_uw, do.points = FALSE, xlab = "Intensidad actual de PDQMP", 
     ylab = "Probabilidad acumulada", main = "", lwd = 1, verticals = TRUE, 
     col = "red")
lines(cdf_w[[2]], lwd = 2, col.vert = "green", col.hor = "green", 
      do.points = FALSE)
legend("bottomright", lwd = c(1, 2), bty = "n", col = c("red", "green"), 
       legend = c("No ponderado", "Ponderado"))

# Intesidad inicial de primer pain que más preocupa----
cdf_uw <- ecdf(derived_data$pain3m_1_int_in)
par(mar = c(4.1, 4.1, 1.1, 2.1))
plot(cdf_uw, do.points = FALSE, xlab = "Intensidad actual de IIPDQMP", 
     ylab = "Probabilidad acumulada", main = "", lwd = 1, verticals = TRUE, 
     col = "red")
lines(cdf_w[[4]], lwd = 2, col.vert = "green", col.hor = "green", 
      do.points = FALSE)
legend("bottomright", lwd = c(1, 2), bty = "n", col = c("red", "green"), 
       legend = c("No ponderado", "Ponderado"))


# Score of Quality of Life----
cdf_uw <- ecdf(derived_data$scoreQoL)
par(mar = c(4.1, 4.1, 1.1, 2.1))
plot(cdf_uw, do.points = FALSE, xlab = "Puntaje de calidad de vida", 
     ylab = "Probabilidad acumulada", main = "", lwd = 1, verticals = TRUE, 
     col = "red")
lines(cdf_w[[4]], lwd = 2, col.vert = "green", col.hor = "green", 
      do.points = FALSE)
legend("bottomright", lwd = c(1, 2), bty = "n", col = c("red", "green"), 
       legend = c("No ponderado", "Ponderado"))

# Exploring distributions of numerical variables using Graphs----
par(mfrow = c(2, 2))
svyhist(~ age, main = "", col = "grey80", 
        xlab = "age", design = svydes)
svyhist(~ pain_act1_int, main = "", col = "grey80", 
        xlab = "Intensidad actual de PDQMP", design = svydes)
svyhist(~ pain3m_1_int_in, main = "", col = "grey80", 
        xlab = "Intensidad actual de IIPDQMP", design = svydes)
svyhist(~ scoreQoL, main = "", col = "grey80", 
        xlab = "Puntaje de calidad de vida", design = svydes)

par(mfrow = c(2, 2))
svyboxplot(age ~ 1, col = "gray80", varwidth = TRUE, 
           ylab = "age", design = svydes)
svyboxplot(pain_act1_int ~ 1, col = "gray80", varwidth = TRUE, 
           ylab = "Intensidad actual de PDQMP", design = svydes)
svyboxplot(pain3m_1_int_in ~ 1, col = "gray80", varwidth = TRUE, 
           ylab = "Intensidad actual de IIPDQMP", design = svydes)
svyboxplot(scoreQoL ~ 1, col = "gray80", varwidth = TRUE, 
           ylab = "Puntaje de calidad de vida", design = svydes)

# Exploring distributions of numerical variables using Analytics values----
## age
svydes %>% 
  summarise(
    Media = survey_mean(age, vartype = "cv", na.rm = TRUE), 
    Mediana = survey_quantile(age, c(0.5), vartype = "cv", na.rm = TRUE)
  )
## Intensidad actual de PDQMP
svydes %>% 
  summarise(
    Media = survey_mean(pain_act1_int, vartype = "cv", na.rm = TRUE), 
    Mediana = survey_quantile(pain_act1_int, c(0.5), vartype = "cv", na.rm = TRUE)
  )

## "Intensidad actual de IIPDQMP"
svydes %>% 
  summarise(
    Media = survey_mean(pain3m_1_int_in, vartype = "cv", na.rm = TRUE), 
    Mediana = survey_quantile(pain3m_1_int_in, c(0.5), vartype = "cv", na.rm = TRUE)
  )

## Puntaje de calidad de vida
svydes %>% 
  summarise(
    Media = survey_mean(scoreQoL, vartype = "cv", na.rm = TRUE), 
    Mediana = survey_quantile(scoreQoL, c(0.5), vartype = "cv", na.rm = TRUE)
  )

