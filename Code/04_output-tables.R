###************************************************************************
###************************************************************************
###                                                                     ***
###                               CODE 4:                               ***
###                           PREPARING TABLES                          ***
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
       officer, 
       officedown) # Loading packages

# Session Info inspection
sessionInfo()

##***************************************************************
##  2. Complex Survye Setting                                  **----
##***************************************************************

# Importing .sav data to tibble format
load("Data/Derived/data_final.RData")


##***************************************************************
##  3. Main Tables                                             **----
##***************************************************************

# Set themes for tables
theme_gtsummary_language(
  language = "en",
  decimal.mark = ".",
  big.mark = " ",
  iqr.sep = "-",
  ci.sep = "-",
  set_theme = TRUE
)

#*******************************************************************************
# Translating labels of values and variables from Spanish to English for reporting

#*******************************************************************************
# Tabla 1----
# Tabla 1: Características de los participantes

## Estimados Puntuales
table1 <- 
  svydes %>% 
  tbl_svysummary(
    include = c(sex, age, ager2, maritalstatcat, educat, ocupcat1, 
                hinsurancecat, econcat, place), 
    by = "pain3m", 
    type = list(all_continuous() ~ "continuous2"), 
    statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})")), 
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), 
    missing = "ifany",
    missing_text = "Missing data", 
    percent = "row"
  ) %>% 
  add_overall(
    col_lab = "**Total**\n**n (%)**"
  ) %>% 
  add_p() %>% 
  modify_header(
    update = list(
      label ~ "**Characteristics**", 
      p.value ~ "**P-value**", 
      stat_1 ~ "**No**\n**n (%)**", 
      stat_2 ~ "**Yes**\n**n (%)**"
    )
  ) %>% 
  bold_labels() %>% 
  bold_p(t = 0.05) %>% 
  modify_footnote(update = everything() ~ NA)

show_header_names(table1)

table1 %>%
  as_flex_table(return_calls = TRUE) 
  
table1 <-   
  table1 %>%
    as_flex_table() %>%
    add_header_row(
      values = c("Characteristics", "Total\nn (%)", "Pain", "P-value"), 
      colwidths = c(1L, 1L, 2L, 1L)
    ) %>% 
    merge_v(part = "header", combine = F) %>% 
    add_footer_lines(
      values = "n: Observations estimated by weighting real observations so that sums could differ up to ± 1 due to approximation error; %: column weighted percentage; IQR: interquartile range; NA: Does not apply."
    ) %>% 
    footnote(
      i = 1:2, j = 5, 
      value = as_paragraph(
        c("Unless otherwise stated, Chi-square test with Rao and Scott second-order correction.")
      ), 
      ref_symbols = c("1"), 
      part = "header"
    ) %>% 
    footnote(
      i = 4, j = 5, 
      value = as_paragraph(
        c("Wilcoxon signed-rank test for complex samples."
          )
      ), 
      ref_symbols = c("2"), 
      part = "body"
    ) %>% 
    set_caption(
      "Table 1. Sociodemographic characteristics of study participants", 
      autonum = TRUE
    ) %>% 
    bold(bold = TRUE, part = "header") %>%
    font(fontname = "Calibri", part = "all") %>% 
    fontsize(size = 9, part = "all") 

table1

# font_family_exists("Calibri")
# sys_fonts()


#*******************************************************************************
# Tabla 2----
## Tabla 2. Características de primer pain que más preocupa

# put the CI in a tibble with the variable name
# first create a data frame with each variable and it's values
df_result <- 
  tibble(variable = c("pain3m_1_t", "pain_1_type",  
                      "pain_1_niv_in", "pain3m_1_freq")) %>%
  # get the levels of each variable in a new column
  # adding them as a list to allow for different variable classes
  rowwise() %>%
  mutate(
    # level to be used to construct call
    level = unique(svydes_pain$variables[[variable]]) %>% as.list() %>% list(),
    # character version to be merged into table
    label = unique(svydes_pain$variables[[variable]]) %>% as.character() %>% as.list() %>% list()
  ) %>%
  unnest(c(level, label)) %>%
  mutate(
    label = unlist(label)
  ) %>% 
  filter(!is.na(label))

# construct call to svyciprop
df_result$svyciprop <-
  map2(
    df_result$variable, df_result$label,
    function(variable, level) rlang::inject(survey::svyciprop(~I(!!rlang::sym(variable) == !!level), svydes_pain))
  )


df_result 

# round/format the 95% CI
df_result <-
  df_result %>%
  rowwise() %>%
  mutate(
    ci = 
      svyciprop %>%
      attr("ci") %>%
      style_sigfig(digits = 3, scale = 100) %>%
      paste0("%", collapse = " - ")
  ) %>% 
  ungroup() %>%
  # keep variables needed in tbl
  select(variable, label, ci)

df_result 

# construct gtsummary table with CI
table2a <- 
  svydes_pain %>% 
  tbl_svysummary(
    include = c(pain3m_1_t, pain_1_type, pain3m_1_int_in, pain_1_niv_in,
                pain3m_1_freq), 
    type = list(all_continuous() ~ "continuous2"), 
    statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
                                          "{min} - {max}")), 
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), 
    missing = "ifany",
    missing_text = "Missing data"
  ) %>%   
  modify_table_body(
    ~.x %>%
      left_join(
        df_result, 
        by = c("variable", "label")
      )
  ) %>%
  # add a header
  modify_header(
    update = list(
      label = "**Pain history**", 
      ci = "**95% CI**", 
      stat_0 ~ "**n (%)**"
    )
  ) %>%
  bold_labels()

table2a

#********

table2b <- 
  svydes_pain %>% 
  tbl_svysummary(
    include = c(pain3m_1_t, pain_1_type, pain3m_1_int_in, pain_1_niv_in,
                pain3m_1_freq), 
    by = "sex", 
    type = list(all_continuous() ~ "continuous2"), 
    statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
                                          "{min} - {max}")), 
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), 
    missing = "ifany",
    missing_text = "Missing data"
  ) %>%   
  add_p() %>% 
  modify_header(
    update = list(
      label = "**Pain history**", 
      p.value = "**P-value**",
      stat_1 ~ "**Male**\n**n (%)**", 
      stat_2 ~ "**Female**\n**n (%)**"
    )
  ) %>%
  bold_labels() %>% 
  bold_p(t = 0.05)

table2b

table2 <- 
  tbl_merge(
    tbls = list(table2a, table2b), 
    tab_spanner = FALSE
  ) %>% 
  modify_footnote(update = everything() ~ NA)


table2 %>%
  as_flex_table(return_calls = TRUE)

table2 %>%
  as_flex_table() %>% 
  add_header_row(
    values = c("Pain history", "Total", "Sex"), 
    colwidths = c(1L, 2L, 3L)
  ) %>% 
  merge_v(part = "header", combine = F) %>% 
  add_footer_lines(
    values = "n: Observations estimated by weighting real observations so that sums can differ up to ± 1 due to approximation error; %: column weighted percentage; IQR: interquartile range; NRS: Numerical rating scale."
  ) %>% 
  footnote(
    i = 2, j = 6, 
    value = as_paragraph(
      c("Unless otherwise stated, Chi-square test with Rao and Scott second-order correction.")
    ), 
    ref_symbols = c("1"), 
    part = "header"
  ) %>% 
  footnote(
    i = 11, j = 6, 
    value = as_paragraph(
      c("Wilcoxon signed-rank test for complex samples."
      )
    ), 
    ref_symbols = c("2"), 
    part = "body"
  ) %>% 
  set_caption(
    "Table 2. Characteristics of the pain of greatest concern in the population that reported having pain in the last 3 months", 
    autonum = TRUE
  ) %>% 
  bold(bold = TRUE, part = "header") %>%
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 9, part = "all") -> table2

table2

  
show_header_names(table2)

#*******************************************************************************
# Tabla 3----
## Tabla 3: Características sociodemográficas y tipo de pain
table3 <- 
  svydes_pain %>% 
  tbl_svysummary(
    include = c(sex, age, ager2, maritalstatcat, educat, ocupcat1, 
                hinsurancecat, place, econcat, pain3m_1_freq, 
                pain_1_niv_in, dtto_tto_comb1, pain_1_type), 
    by = "pain_1_type", 
    type = age ~ "continuous2", 
    statistic = age ~ c("{median} ({p25} - {p75})", "{min} - {max}"), 
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), 
    missing = "ifany",
    missing_text = "Missing data"
  ) %>%  
  add_p() %>% 
  modify_header(
    update = list(
      label ~ "**Characteristics**", 
      p.value = "**P-value**",
      stat_1 ~ "**Acute (<3 mo)**\n**n (%)**", 
      stat_2 ~ "**Chronic (>= 3 mo)**\n**n (%)**"
    )
  ) %>% 
  bold_labels() %>% 
  bold_p(t = 0.05) %>% 
  modify_footnote(update = everything() ~ NA)

table3

show_header_names(table3)

table3 %>%
  as_flex_table(return_calls = TRUE) 

table3 <-   
  table3 %>%
  as_flex_table() %>%
  add_header_row(
    values = c("Characteristics", "Type of the pain", "P-value"), 
    colwidths = c(1L, 2L, 1L)
  ) %>% 
  merge_v(part = "header", combine = F) %>% 
  add_footer_lines(
    values = "n: Observations estimated by weighting real observations so that sums could differ up to ± 1 due to approximation error; %: column weighted percentage; mo: months; IQR: interquartile range; NA: Does not apply."
  ) %>% 
  footnote(
    i = 1, j = 2:3, 
    value = as_paragraph(
      c("11 participants (weighted n = 10) did not response data to determine chronicity of pain."
      )
    ), 
    ref_symbols = c("1"), 
    part = "header"
  ) %>% 
  footnote(
    i = 1:2, j = 4, 
    value = as_paragraph(
      c("Unless otherwise stated, Chi-square test with Rao and Scott second-order correction.")
    ), 
    ref_symbols = c("2"), 
    part = "header"
  ) %>% 
  footnote(
    i = 4, j = 4, 
    value = as_paragraph(
      c("Wilcoxon signed-rank test for complex samples."
      )
    ), 
    ref_symbols = c("3"), 
    part = "body"
  ) %>% 
  set_caption(
    "Table 3. Type of the pain of greatest concern in the population that reported having pain in the last 3 months", 
    autonum = TRUE
  ) %>% 
  bold(bold = TRUE, part = "header") %>%
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 9, part = "all") 

table3

#*******************************************************************************
# Tabla 4----
## Tabla 4. Características de primer pain que más preocupa
table4 <- 
  svydes_pain %>% 
  tbl_svysummary(
    include = c(pain_actual,  pain_n_sitescatC, pain_solo1, pain_actual1, 
                pain_1_type, pain3m_1_t, pain_1_niv_in, pain3m_1_freq,  
                pain_act1_niv, QoL_cat2), 
    by = "QoL_cat2", 
    type = all_continuous() ~ "continuous2", 
    statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
                                          "{min} - {max}")), 
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), 
    missing = "ifany",
    missing_text = "Missing data",
    percent = "row"
  ) %>%   
  add_p() %>% 
  modify_header(
    update = list(
      label = "**Pain history of greatest concern**", 
      p.value = "**P-value**", 
      stat_1  = "**0-5 (Low)**\n**n (%)**",                
      stat_2  = "**6-7 (Middle)**\n**n (%)**",             
      stat_3  = "**8-10 (High)**\n**n (%)**" 
    )
  ) %>%
  bold_labels() %>% 
  bold_p(t = 0.05) %>% 
  modify_footnote(update = everything() ~ NA)

table4

show_header_names(table4)

table4 %>%
  as_flex_table(return_calls = TRUE) 

table4 <-   
  table4 %>%
  as_flex_table() %>%
  add_header_row(
    values = c("Pain history of greatest concern", "Quality of Life Level", "P-value"), 
    colwidths = c(1L, 3L, 1L)
  ) %>% 
  merge_v(part = "header", combine = F) %>% 
  add_footer_lines(
    values = "n: Observations estimated by weighting real observations so that sums could differ up to ± 1 due to approximation error; %: column weighted percentage; NRS: Numerical rating scale."
  ) %>% 
  footnote(
    i = 1, j = 2:4, 
    value = as_paragraph(
      c("Five participants had missing data in quality of life score")
    ), 
    ref_symbols = c("1"), 
    part = "header"
  ) %>% 
  footnote(
    i = 1:2, j = 5, 
    value = as_paragraph(
      c("Unless otherwise stated, Chi-square test with Rao and Scott second-order correction.")
    ), 
    ref_symbols = c("2"), 
    part = "header"
  ) %>% 
  set_caption(
    "Table 4. Relationship between quality of life level and pain characteristics in participants who had pain in the las 3 month", 
    autonum = TRUE
  ) %>% 
  bold(bold = TRUE, part = "header") %>%
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 9, part = "all") 

table4

#*******************************************************************************
# Tabla 5----
## Tabla 5. Tipo de tratamiento y nivel inicial de pain
table5 <- 
  svydes_pain %>% 
  tbl_svysummary(
    include = c(tto_medic1, tto_none1, tto_physio1, tto_surg1, # tto_infilt1 tiene 0
                tto_remed_alter, dtto_tto_comb1, pain_1_niv_in), 
    by = "pain_1_niv_in", 
    type = list(all_continuous() ~ "continuous2"), 
    statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
                                          "{min} - {max}")), 
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)), 
    missing_text = "Datos faltantes", 
  ) %>%   
  add_p() %>% 
  modify_header(
    update = list(
      label = "**Received treatment**", 
      p.value = "**P-value**", 
      stat_1 = "**Mild (NRS 1-3)**\n**n (%)**",       
      stat_2 = "**Moderate (NRS 4-6)**\n**n (%)**",  
      stat_3 = "**Severe (NRS 7-10)**\n**n (%)**"  
    )
  ) %>%
  bold_labels() %>% 
  bold_p(t = 0.05) %>% 
  modify_footnote(update = everything() ~ NA)

table5 

show_header_names(table5)

table5 %>%
  as_flex_table(return_calls = TRUE) 

table5 <-   
  table5 %>%
  as_flex_table() %>%
  add_header_row(
    values = c("Received treatment", "Intensity of pain", "P-value"), 
    colwidths = c(1L, 3L, 1L)
  ) %>% 
  merge_v(part = "header", combine = F) %>% 
  add_footer_lines(
    values = "n: Observations estimated by weighting real observations so that sums could differ up to ± 1 due to approximation error; %: column weighted percentage; NRS: Numerical rating scale."
  ) %>% 
  footnote(
    i = 1:2, j = 5, 
    value = as_paragraph(
      c("Unless otherwise stated, Chi-square test with Rao and Scott second-order correction.")
    ), 
    ref_symbols = c("1"), 
    part = "header"
  ) %>% 
  set_caption(
    "Table 5. Comparison of management of pain of greatest concern according to level of pain intensity", 
    autonum = TRUE
  ) %>% 
  bold(bold = TRUE, part = "header") %>%
  font(fontname = "Calibri", part = "all") %>% 
  fontsize(size = 9, part = "all")

table5

#*******************************************************************************
# Creating word with reproducible tables

doc <- read_docx()

doc2 <- 
  doc %>% 
  set_table_properties(layout = "autofit") %>% 
  body_add_flextable(table1) %>% 
  body_add_break() %>%
  body_add_flextable(table2) %>% 
  body_add_break() %>%
  body_add_flextable(table3) %>% 
  body_add_break() %>%
  body_add_flextable(table4) %>% 
  body_add_break() %>%
  body_add_flextable(table5) 
  
print(doc2, target = "Table/FinalTable.docx")