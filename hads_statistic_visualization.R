#Date: 29-11-2024
#Author: Romy Klein Kranenbarg
#Description: 
# this script is for analysis as part of the manuscript 'Symptoms of depression and anxiety are early biomarkers of multi-domain disability progression in progressive MS'
# in this script, we perform statistical analysis as described in the manuscript and visualization for the manuscript

#### SETTING UP THE ENVIRONMENT ####
##### 1. Install and load packages #####
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("gtsummary")
# install.packages("emmeans")
# install.packages("MASS")
# install.packages("car")
# install.packages("forecast")
# install.packages("DHARMa")
# install.packages("ggVennDiagram")

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(gtsummary)
library(emmeans)
library(MASS)
library(performance)
library(car)
library(forecast)
library(DHARMa)
library(knitr)
library(ggVennDiagram)
library(scales)
library(openxlsx)

##### 2. Set up Working directory #####
setwd("/path/to/your/work/directory")

#### Visualizations and descriptive statistics analysis 1F ####        
merged_data_1F = read.xlsx(file = "merged_data_1F.xlsx")
merged_data_1F_2y = read.xlsx(file = "merged_data_1F_2y.xlsx")

# Determine descriptive statistics for number of days between prev_visit and next_visit
days_between_prev_and_next_visit_stats <- summary(merged_data_1F$days_between_prev_and_next_visit)

# Determine median, minimum en maximum 
median_prev_next <- median(merged_data_1F$days_between_prev_and_next_visit, na.rm = TRUE)
min_prev_next <- min(merged_data_1F$days_between_prev_and_next_visit, na.rm = TRUE)
max_prev_next <- max(merged_data_1F$days_between_prev_and_next_visit, na.rm = TRUE)

# Determine Q1 and Q3 without interpolation (type = 1)
Q1_prev_next <- quantile(merged_data_1F$days_between_prev_and_next_visit, probs = 0.25, type = 1, na.rm = TRUE)
Q3_prev_next <- quantile(merged_data_1F$days_between_prev_and_next_visit, probs = 0.75, type = 1, na.rm = TRUE)

# Print these descriptive statistics
cat("Statistieken voor het aantal dagen tussen prev_visit en next_visit:\n")
cat("Minimale waarde:", min_prev_next, "\n")
cat("1e kwartiel (Q1):", Q1_prev_next, "\n")
cat("Mediaan:", median_prev_next, "\n")
cat("3e kwartiel (Q3):", Q3_prev_next, "\n")
cat("Maximale waarde:", max_prev_next, "\n")

# Determine the descriptive statistics for the number of days between survey_date and prev_visit_date
survey_prev_stats <- summary(as.numeric(difftime(merged_data_1F$survey_date, merged_data_1F$prev_visit_date, units = "days")))
median_survey_prev <- median(as.numeric(difftime(merged_data_1F$survey_date, merged_data_1F$prev_visit_date, units = "days")), na.rm = TRUE)
IQR_survey_prev <- IQR(as.numeric(difftime(merged_data_1F$survey_date, merged_data_1F$prev_visit_date, units = "days")), na.rm = TRUE)
min_survey_prev <- min(as.numeric(difftime(merged_data_1F$survey_date, merged_data_1F$prev_visit_date, units = "days")), na.rm = TRUE)
max_survey_prev <- max(as.numeric(difftime(merged_data_1F$survey_date, merged_data_1F$prev_visit_date, units = "days")), na.rm = TRUE)

# Print these  descriptive statistics 
cat("\nStatistieken voor het aantal dagen tussen survey_date en prev_visit_date:\n")
cat("Minimale waarde:", min_survey_prev, "\n")
cat("1e kwartiel (Q1):", survey_prev_stats[2], "\n")
cat("Mediaan:", median_survey_prev, "\n")
cat("3e kwartiel (Q3):", survey_prev_stats[5], "\n")
cat("Maximale waarde:", max_survey_prev, "\n")

##### Table 1. Baseline tables #####

###### YEAR 1. Baseline tables ######
# Two manual annotation of the dataframe here due to changes in database (detected for privacy reason)

# Baseline table EDSS progression vs no EDSS progression 
baseline_table <- merged_data_1F %>%
  select(
    geslacht,  
    age_at_prev_visit,
    disease_duration_firstsymptoms,
    prev_edss,  
    prev_immuunMSmedicatie_type,
    prev_psychofarmaca,
    prev_antidepressiva,
    prev_anxiolytica,
    hads_angst,  
    hads_depressie,  
    hads_totaal,  
    edss_progression  
  ) %>%
  tbl_summary(
    by = edss_progression,  
    missing = "ifany",  
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p}%)",  
      all_continuous() ~ "{median} ({p25}, {p75})"  
    )
  ) %>%
  add_overall() %>%  
  add_p(
    test = list(
      all_continuous() ~ kruskal.test,  
      all_categorical() ~ fisher.test  
    ),
    test.args = all_categorical() ~ list(workspace = 2e8, simulate.p.value = TRUE, B = 1e5)  
  ) %>%
  modify_table_body(~ .x %>%
                      relocate(label, stat_0, stat_1, stat_2, p.value)  
  ) %>%
  modify_header(label = "**Variabele**") %>%
  modify_header(
    stat_0 = "**Overall (N = {N})**",  
    stat_1 = "**No EDSS progression (N = {n})**",  
    stat_2 = "**EDSS progression (N = {n})**"  
  ) %>%
  bold_labels()  

# View baseline table
baseline_table


# Baseline table EDSS or T25FW or AMSQ progression vs no EDSS or T25FW or AMSQ progression 
baseline_table_progression_edss_or_T25FW_or_AMSQ <- merged_data_1F %>%
  select(
    geslacht,  
    age_at_prev_visit,
    disease_duration_firstsymptoms,
    prev_edss,  
    prev_immuunMSmedicatie_type,
    prev_psychofarmaca,
    prev_antidepressiva,
    prev_anxiolytica,
    hads_angst,  
    hads_depressie,  
    hads_totaal,  
    progression_edss_or_T25FW_or_AMSQ  
  ) %>%
  tbl_summary(
    by = progression_edss_or_T25FW_or_AMSQ,
    missing = "ifany",
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p}%)",
      all_continuous() ~ "{median} ({p25}, {p75})"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ kruskal.test,
      all_categorical() ~ fisher.test
    ),
    test.args = all_categorical() ~ list(workspace = 2e8, simulate.p.value = TRUE, B = 1e5)
  ) %>%
  modify_table_body(~ .x %>%
                      relocate(label, stat_0, stat_1, stat_2, p.value)
  ) %>%
  modify_header(label = "**Variabele**") %>%
  modify_header(
    stat_0 = "**Overall (N = {N})**",
    stat_1 = "**No progression (N = {n})**",
    stat_2 = "**Progression (N = {n})**"
  ) %>%
  bold_labels()

# View baseline table
baseline_table_progression_edss_or_T25FW_or_AMSQ

# Baseline table EDSS or T25FW or AMSQ or SDMT or PDDS progression vs no EDSS or T25FW or AMSQ or SDMT or PDDS progression
# First extra step to use quantile type=1 so that only existing EDSS scores are chosen as median
.edss_str_t1 <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("—")
  q <- function(p) stats::quantile(x, probs = p, na.rm = TRUE, type = 1)
  m  <- q(0.5); q1 <- q(0.25); q3 <- q(0.75)
  sprintf("%s (%s, %s)",
          gtsummary::style_number(m,  digits = 1),
          gtsummary::style_number(q1, digits = 1),
          gtsummary::style_number(q3, digits = 1))
}

# Baseline table 
baseline_table_progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS <- merged_data_1F %>%
  select(
    geslacht,  
    age_at_prev_visit,
    disease_duration_firstsymptoms,
    prev_edss,  
    prev_immuunMSmedicatie_type,
    prev_psychofarmaca,
    prev_antidepressiva,
    prev_anxiolytica,
    hads_angst,  
    hads_depressie,  
    hads_totaal,  
    progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS
  ) %>%
  tbl_summary(
    by = progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS,
    missing = "ifany",
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p}%)",
      all_continuous()  ~ "{median} ({p25}, {p75})"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ kruskal.test,
      all_categorical() ~ fisher.test
    ),
    test.args = all_categorical() ~ list(workspace = 2e8, simulate.p.value = TRUE, B = 1e5)
  ) %>%
  modify_table_body(~ .x %>% relocate(label, stat_0, stat_1, stat_2, p.value)) %>%
  modify_header(label = "**Variabele**") %>%
  modify_header(
    stat_0 = "**Overall (N = {N})**",
    stat_1 = "**No progression (N = {n})**",
    stat_2 = "**Progression (N = {n})**"
  ) %>%
  bold_labels()

# Replace only the display cells for prev_edss and next_edss with type=1 result
# (determines order of groups based on the data)
.by_levels <- merged_data_1F %>%
  filter(!is.na(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS)) %>%
  distinct(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS) %>%
  arrange(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS) %>%
  pull()

.prev_overall <- .edss_str_t1(merged_data_1F$prev_edss)
.next_overall <- .edss_str_t1(merged_data_1F$next_edss)

.prev_g1 <- .edss_str_t1(merged_data_1F$prev_edss[merged_data_1F$progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS == .by_levels[1]])
.prev_g2 <- .edss_str_t1(merged_data_1F$prev_edss[merged_data_1F$progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS == .by_levels[2]])

.next_g1 <- .edss_str_t1(merged_data_1F$next_edss[merged_data_1F$progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS == .by_levels[1]])
.next_g2 <- .edss_str_t1(merged_data_1F$next_edss[merged_data_1F$progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS == .by_levels[2]])

baseline_table_progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS <-
  baseline_table_progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS %>%
  modify_table_body(~ .x %>%
                      mutate(
                        stat_0 = dplyr::case_when(
                          variable == "prev_edss" ~ .prev_overall,
                          variable == "next_edss" ~ .next_overall,
                          TRUE ~ stat_0
                        ),
                        stat_1 = dplyr::case_when(
                          variable == "prev_edss" ~ .prev_g1,
                          variable == "next_edss" ~ .next_g1,
                          TRUE ~ stat_1
                        ),
                        stat_2 = dplyr::case_when(
                          variable == "prev_edss" ~ .prev_g2,
                          variable == "next_edss" ~ .next_g2,
                          TRUE ~ stat_2
                        )
                      )
  )

# View
baseline_table_progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS

###### YEAR 2. Baseline tables ######
# Calculate descriptive statistics for number of days between prev_visit and next_visit_2

# Median, minimum and maximum
median_prev_next_2 <- median(merged_data_1F_2y$days_between_prev_and_next_visit_2, na.rm = TRUE)
min_prev_next_2 <- min(merged_data_1F_2y$days_between_prev_and_next_visit_2, na.rm = TRUE)
max_prev_next_2 <- max(merged_data_1F_2y$days_between_prev_and_next_visit_2, na.rm = TRUE)

# Determine Q1 and Q3 without interpolation (type = 1)
Q1_prev_next_2 <- quantile(merged_data_1F_2y$days_between_prev_and_next_visit_2, probs = 0.25, type = 1, na.rm = TRUE)
Q3_prev_next_2 <- quantile(merged_data_1F_2y$days_between_prev_and_next_visit_2, probs = 0.75, type = 1, na.rm = TRUE)

# Print the descriptive statistics 
cat("Statistieken voor het aantal dagen tussen prev_visit en next_visit_2:\n")
cat("Minimale waarde:", min_prev_next_2, "\n")
cat("1e kwartiel (Q1):", Q1_prev_next_2, "\n")
cat("Mediaan:", median_prev_next_2, "\n")
cat("3e kwartiel (Q3):", Q3_prev_next_2, "\n")
cat("Maximale waarde:", max_prev_next_2, "\n")
   
# Baseline table EDSS progression vs no EDSS progression 2 years after HADS
baseline_table <- merged_data_1F_2y %>%
  select(
    geslacht,  
    age_at_prev_visit,
    disease_duration_firstsymptoms,
    prev_edss,  
    prev_immuunMSmedicatie_type,
    prev_psychofarmaca,
    prev_antidepressiva,
    prev_anxiolytica,
    hads_angst,  
    hads_depressie,  
    hads_totaal,  
    edss_progression_2  
  ) %>%
  tbl_summary(
    by = edss_progression_2,  
    missing = "ifany",  
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p}%)",  
      all_continuous() ~ "{median} ({p25}, {p75})"  
    )
  ) %>%
  add_overall() %>%  
  add_p(
    test = list(
      all_continuous() ~ kruskal.test,  
      all_categorical() ~ fisher.test  
    ),
    test.args = all_categorical() ~ list(workspace = 2e8, simulate.p.value = TRUE, B = 1e5)  
  ) %>%
  modify_table_body(~ .x %>%
                      relocate(label, stat_0, stat_1, stat_2, p.value)  
  ) %>%
  modify_header(label = "**Variabele**") %>%
  modify_header(
    stat_0 = "**Overall (N = {N})**",  
    stat_1 = "**No EDSS progression (N = {n})**",  
    stat_2 = "**EDSS progression (N = {n})**"  
  ) %>%
  bold_labels()  

baseline_table

# Baseline table EDSS or T25FW or AMSQ progression vs no EDSS or T25FW or AMSQ progression 2 years after HADS
# First extra step to use quantile type=1 so only existing EDSS values and HADS_angst values are chosen as median
.q_str_t1 <- function(x, digits) {
  x <- x[!is.na(x)]
  if (!length(x)) return("—")
  q <- function(p) stats::quantile(x, probs = p, na.rm = TRUE, type = 1)
  sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
          q(0.5), q(0.25), q(0.75))
}
.edss_str_t1 <- function(x) .q_str_t1(x, digits = 1)  
.hads_str_t1 <- function(x) .q_str_t1(x, digits = 0)  

# Create baseline table - EDSS OR T25FW OR AMSQ 2Y
baseline_table_progression_edss_or_T25FW_or_AMSQ_2y <- merged_data_1F_2y %>%
  select(
    geslacht, age_at_prev_visit, 
    disease_duration_firstsymptoms, 
    prev_edss, 
    prev_immuunMSmedicatie_type, prev_psychofarmaca, prev_antidepressiva, prev_anxiolytica,
    hads_angst, hads_depressie, hads_totaal,
    progression_edss_or_T25FW_or_AMSQ_2y
  ) %>%
  tbl_summary(
    by = progression_edss_or_T25FW_or_AMSQ_2y,
    missing = "ifany",
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p}%)",
      all_continuous()  ~ "{median} ({p25}, {p75})"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(all_continuous() ~ kruskal.test, all_categorical() ~ fisher.test),
    test.args = all_categorical() ~ list(workspace = 2e8, simulate.p.value = TRUE, B = 1e5)
  ) %>%
  modify_table_body(~ .x %>% relocate(label, stat_0, stat_1, stat_2, p.value)) %>%
  modify_header(label = "**Variabele**") %>%
  modify_header(
    stat_0 = "**Overall (N = {N})**",
    stat_1 = "**No EDSS or T25FW or AMSQ progression (N = {n})**",
    stat_2 = "**EDSS or T25FW or AMSQ progression (N = {n})**"
  ) %>%
  bold_labels()

# Replace only the cells for prev_edss, next_edss (1 dec) and hads_angst (0 dec) 
.by <- "progression_edss_or_T25FW_or_AMSQ_2y"
.by_levels <- sort(unique(na.omit(merged_data_1F_2y[[.by]])))

fmt <- list(
  prev_edss = .edss_str_t1,
  next_edss = .edss_str_t1,
  hads_angst = .hads_str_t1
)

vars_to_fix <- names(fmt)

fix_rows <- lapply(vars_to_fix, function(v) {
  tibble::tibble(
    variable = v,
    stat_0 = fmt[[v]](merged_data_1F_2y[[v]]),
    stat_1 = fmt[[v]](merged_data_1F_2y[[v]][merged_data_1F_2y[[.by]] == .by_levels[1]]),
    stat_2 = fmt[[v]](merged_data_1F_2y[[v]][merged_data_1F_2y[[.by]] == .by_levels[2]])
  )
}) |> dplyr::bind_rows()

baseline_table_progression_edss_or_T25FW_or_AMSQ_2y <-
  baseline_table_progression_edss_or_T25FW_or_AMSQ_2y %>%
  modify_table_body(~ .x %>%
                      dplyr::left_join(fix_rows, by = "variable", suffix = c("", ".new")) %>%
                      dplyr::mutate(
                        stat_0 = dplyr::coalesce(stat_0.new, stat_0),
                        stat_1 = dplyr::coalesce(stat_1.new, stat_1),
                        stat_2 = dplyr::coalesce(stat_2.new, stat_2)
                      ) %>%
                      dplyr::select(-dplyr::ends_with(".new"))
  )

baseline_table_progression_edss_or_T25FW_or_AMSQ_2y   

# Baseline table EDSS or T25FW or AMSQ or SDMT or PDDS progression vs no EDSS or T25FW or AMSQ or SDMT or PDDS progression at 2 years after HADS 
# Create baseline table - EDSS OR T25FW OR AMSQ OR SDMT OR PDDS 2Y
baseline_table_progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y <- merged_data_1F_2y %>%
  select(
    geslacht,  
    age_at_prev_visit,
    disease_duration_firstsymptoms,
    prev_edss,  
    prev_immuunMSmedicatie_type,
    prev_psychofarmaca,
    prev_antidepressiva,
    prev_anxiolytica,
    hads_angst,  
    hads_depressie,  
    hads_totaal,  
    progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y 
  ) %>%
  tbl_summary(
    by = progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y,
    missing = "ifany",
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p}%)",
      all_continuous()  ~ "{median} ({p25}, {p75})"
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ kruskal.test,
      all_categorical() ~ fisher.test
    ),
    test.args = all_categorical() ~ list(workspace = 2e8, simulate.p.value = TRUE, B = 1e5)
  ) %>%
  modify_table_body(~ .x %>% relocate(label, stat_0, stat_1, stat_2, p.value)) %>%
  modify_header(label = "**Variabele**") %>%
  modify_header(
    stat_0 = "**Overall (N = {N})**",
    stat_1 = "**No EDSS or T25FW or AMSQ or SDMT or PDDS progression (N = {n})**",
    stat_2 = "**EDSS or T25FW or AMSQ or SDMT or PDDS progression (N = {n})**"
  ) %>%
  bold_labels()

# Only replace cells for EDSS and HADS anxiety and depression 
.by <- "progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y"
.by_levels <- sort(unique(na.omit(merged_data_1F_2y[[.by]])))

fmt <- list(
  prev_edss    = .edss_str_t1,
  next_edss    = .edss_str_t1,
  hads_angst   = .hads_str_t1,
  hads_depressie = .hads_str_t1
)

vars_to_fix <- names(fmt)

fix_rows <- lapply(vars_to_fix, function(v) {
  tibble::tibble(
    variable = v,
    stat_0 = fmt[[v]](merged_data_1F_2y[[v]]),
    stat_1 = fmt[[v]](merged_data_1F_2y[[v]][merged_data_1F_2y[[.by]] == .by_levels[1]]),
    stat_2 = fmt[[v]](merged_data_1F_2y[[v]][merged_data_1F_2y[[.by]] == .by_levels[2]])
  )
}) |> dplyr::bind_rows()

baseline_table_progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y <-
  baseline_table_progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y %>%
  modify_table_body(~ .x %>%
                      dplyr::left_join(fix_rows, by = "variable", suffix = c("", ".new")) %>%
                      dplyr::mutate(
                        stat_0 = dplyr::coalesce(stat_0.new, stat_0),
                        stat_1 = dplyr::coalesce(stat_1.new, stat_1),
                        stat_2 = dplyr::coalesce(stat_2.new, stat_2)
                      ) %>%
                      dplyr::select(-dplyr::ends_with(".new"))
  )

baseline_table_progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y

##### Supplementary Table 2.CREATE BASELINE TABLE TO COMPARE PEOPLE WITH 2-3 AVAILABLE VARIABLES TO THOSE WITH 4-5 AVAILABLE VARIABLES WITHIN THE NON-PROGRESSION GROUP #####
###### YEAR 1. Supplementary Table 2 ######
# Step 1: Create a clean dataset of people without progression 
merged_data_nonprogression_base <- merged_data_1F %>%
  filter(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS == 0) %>%
  mutate(
    n_progressie_available = 
      1 +  # edss_progression always available
      1 +  # PDDS_progression always available
      as.integer(T25FW_boxplotgroep != "Ontbrekend") +
      as.integer(AMSQ_progressie_boxplot != "Ontbrekend") +
      as.integer(SDMT_progressie_boxplot != "Ontbrekend")
  )

# Step 2: Analysis 1: 2–3 vs 4–5 variables available
merged_data_subgroup_2_3_vs_4_5 <- merged_data_nonprogression_base %>%
  mutate(
    subgroup_progressiecompleetheid = case_when(
      n_progressie_available %in% 2:3 ~ "2–3 variabelen available",
      n_progressie_available %in% 4:5 ~ "4–5 variabelen available",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(subgroup_progressiecompleetheid))

# Step 2a: Custom functions for HADS-scores with type=1 quantiles and rounding off
.make_hads_str_t1 <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("—")
  q <- function(p) stats::quantile(x, probs = p, na.rm = TRUE, type = 1)
  m <- round(q(0.5))
  q1 <- round(q(0.25))
  q3 <- round(q(0.75))
  sprintf("%d (%d, %d)", m, q1, q3)
}

# Stap 2b: Defining group order 
merged_data_subgroup_2_3_vs_4_5 <- merged_data_subgroup_2_3_vs_4_5 %>%
  mutate(subgroup_progressiecompleetheid = factor(
    subgroup_progressiecompleetheid,
    levels = c("2–3 variabelen beschikbaar", "4–5 variabelen beschikbaar")
  ))

.by_levels_hads <- merged_data_subgroup_2_3_vs_4_5 %>%
  distinct(subgroup_progressiecompleetheid) %>%
  arrange(subgroup_progressiecompleetheid) %>%
  pull()

# Stap 2c: Calculate custom summaries for multiple HADS variables
.get_grouped_hads_summaries <- function(varname) {
  var <- merged_data_subgroup_2_3_vs_4_5[[varname]]
  g <- merged_data_subgroup_2_3_vs_4_5$subgroup_progressiecompleetheid
  
  list(
    overall = .make_hads_str_t1(var),
    g1 = .make_hads_str_t1(var[g == .by_levels_hads[1]]),
    g2 = .make_hads_str_t1(var[g == .by_levels_hads[2]])
  )
}

.hads_totaal_vals <- .get_grouped_hads_summaries("hads_totaal")
.hads_angst_vals  <- .get_grouped_hads_summaries("hads_angst")


##  Stap 3. Make the baseline table 2-3 vs 4-5 available variables and adjust hads_totaal to the custom rounding off
baseline_table_2_3_vs_4_5 <- merged_data_subgroup_2_3_vs_4_5 %>%
  select(
    geslacht, age_at_prev_visit, 
    disease_duration_firstsymptoms, 
    prev_edss, prev_immuunMSmedicatie_type,
    prev_psychofarmaca, prev_antidepressiva, prev_anxiolytica,
    hads_angst, hads_depressie, hads_totaal,
    subgroup_progressiecompleetheid
  ) %>%
  tbl_summary(
    by = subgroup_progressiecompleetheid,
    missing = "ifany",
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p}%)",
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    type = list(prev_edss ~ "continuous")
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ kruskal.test,
      all_categorical() ~ fisher.test
    ),
    test.args = all_categorical() ~ list(workspace = 2e8, simulate.p.value = TRUE, B = 1e5)
  ) %>%
  modify_table_body(~ .x %>%
                      relocate(label, stat_0, stat_1, stat_2, p.value) %>%
                      mutate(across(
                        starts_with("stat_"),
                        ~ case_when(
                          variable == "hads_totaal" ~ case_when(
                            cur_column() == "stat_0" ~ .hads_totaal_vals$overall,
                            cur_column() == "stat_1" ~ .hads_totaal_vals$g1,
                            cur_column() == "stat_2" ~ .hads_totaal_vals$g2,
                            TRUE ~ .
                          ),
                          variable == "hads_angst" ~ case_when(
                            cur_column() == "stat_0" ~ .hads_angst_vals$overall,
                            cur_column() == "stat_1" ~ .hads_angst_vals$g1,
                            cur_column() == "stat_2" ~ .hads_angst_vals$g2,
                            TRUE ~ .
                          ),
                          TRUE ~ .
                        )
                      ))
  ) %>%
  modify_header(label = "**Variabele**") %>%
  modify_header(
    stat_0 = "**Overall (N = {N})**",
    stat_1 = "**2–3 beschikbaar (N = {n})**",
    stat_2 = "**4–5 beschikbaar (N = {n})**"
  ) %>%
  bold_labels()

baseline_table_2_3_vs_4_5

###### YEAR 2. Supplementary Table 2 ######
# Step 1: code type=1 for variables which showed a 'non-existing' value in the table 
.make_t1_str <- function(x, digits = 0) {
  x <- x[!is.na(x)]
  if (!length(x)) return("—")
  q <- function(p) stats::quantile(x, probs = p, na.rm = TRUE, type = 1)
  m <- round(q(0.5), digits)
  q1 <- round(q(0.25), digits)
  q3 <- round(q(0.75), digits)
  fmt <- paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)")
  sprintf(fmt, m, q1, q3)
}

.get_grouped_t1_summaries <- function(data, varname, groupvar, levels_vec, digits = 0) {
  var <- data[[varname]]
  g <- data[[groupvar]]
  list(
    overall = .make_t1_str(var, digits),
    g1 = if (length(levels_vec) >= 1) .make_t1_str(var[g == levels_vec[1]], digits) else "—",
    g2 = if (length(levels_vec) >= 2) .make_t1_str(var[g == levels_vec[2]], digits) else "—"
  )
}

# Step 2: Analysis 1: 2–3 vs 4–5 variabeles available
.by_levels_2y_a <- merged_data_subgroup_2_3_vs_4_5_2y %>%
  distinct(subgroup_progressiecompleetheid) %>%
  arrange(subgroup_progressiecompleetheid) %>%
  pull()

.vars_t1_a <- c("prev_edss", "next_edss_2", "hads_angst", "hads_depressie", "hads_totaal")

.vals_list_a <- lapply(
  setNames(.vars_t1_a, .vars_t1_a),
  function(v) {
    .get_grouped_t1_summaries(
      merged_data_subgroup_2_3_vs_4_5_2y, v,
      "subgroup_progressiecompleetheid", .by_levels_2y_a,
      digits = if (v %in% c("prev_edss", "next_edss_2", "hads_angst", "hads_depressie", "hads_totaal")) 1 else 0
    )
  }
)

baseline_table_2_3_vs_4_5_2y <- merged_data_subgroup_2_3_vs_4_5_2y %>%
  select(
    geslacht, age_at_prev_visit, 
    disease_duration_firstsymptoms,
    prev_edss, prev_immuunMSmedicatie_type,
    prev_psychofarmaca, prev_antidepressiva, prev_anxiolytica,
    hads_angst, hads_depressie, hads_totaal,
    subgroup_progressiecompleetheid
  ) %>%
  tbl_summary(
    by = subgroup_progressiecompleetheid,
    missing = "ifany",
    statistic = list(
      all_categorical() ~ "{n}/{N} ({p}%)",
      all_continuous() ~ "{median} ({p25}, {p75})"
    ),
    type = list(prev_edss ~ "continuous", next_edss_2 ~ "continuous")
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_continuous() ~ kruskal.test,
      all_categorical() ~ fisher.test
    ),
    test.args = all_categorical() ~ list(workspace = 2e8, simulate.p.value = TRUE, B = 1e5)
  ) %>%
  modify_table_body(~ {
    tb <- .x
    for (v in names(.vals_list_a)) {
      rows <- which(tb$variable == v & tb$row_type == "label")
      if (length(rows) > 0) {
        tb$stat_0[rows] <- .vals_list_a[[v]]$overall
        tb$stat_1[rows] <- .vals_list_a[[v]]$g1
        tb$stat_2[rows] <- .vals_list_a[[v]]$g2
      }
    }
    tb %>% relocate(label, stat_0, stat_1, stat_2, p.value)
  }) %>%
  modify_header(label = "**Variabele**") %>%
  modify_header(
    stat_0 = "**Overall (N = {N})**",
    stat_1 = "**2–3 beschikbaar (N = {n})**",
    stat_2 = "**4–5 beschikbaar (N = {n})**"
  ) %>%
  bold_labels()

# Result
baseline_table_2_3_vs_4_5_2y


##### Figure 2 & Supplementary Figure 1 & Figure 2. - boxplots and statistics of HADS scores by 1-year and 2-year progression  #####

###### 1. year 1 visualization and statistics ######

#### HADS vs EDSS progression 1 year after completing HADS
# Check if there are NAs in edss_progression --> no NAs
table(merged_data_1F$prev_edss, useNA = "always")
table(merged_data_1F$next_edss, useNA = "always")


### Figure 2K - HADS Angst vs EDSS progression 1 year after completing HADS
boxedss1 <- ggplot(merged_data_1F, aes(x = factor(edss_progression), y = hads_angst, fill = factor(edss_progression))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen EDSS Progressie", "EDSS Progressie")) +  
  labs(x = "EDSS Progressie (1 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())  
boxedss1

### Figure 2Q - HADS-depression score vs EDSS progression after 1 year 
boxedss2 <- ggplot(merged_data_1F, aes(x = factor(edss_progression), y = hads_depressie, fill = factor(edss_progression))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen EDSS Progressie", "EDSS Progressie")) +  
  labs(x = "EDSS Progressie (1 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())  
boxedss2


### Figure 2E - total HADS score vs EDSS progression after 1 year 
boxedss3 <- ggplot(merged_data_1F, aes(x = factor(edss_progression), y = hads_totaal, fill = factor(edss_progression))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen EDSS Progressie", "EDSS Progressie")) +  
  labs(x = "EDSS Progressie (1 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())  
boxedss3

#### statistics - HADS vs EDSS progression 1 year after completing HADS
# Determine p-values of significance in HADS scores using post-hoc test on HADS in linear regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb <- glm.nb(hads_angst ~ factor(edss_progression), data = merged_data_1F)
m_depr_nb  <- glm.nb(hads_depressie ~ factor(edss_progression), data = merged_data_1F)
m_tot_nb   <- glm.nb(hads_totaal ~ factor(edss_progression), data = merged_data_1F)

### 2. Check model assumptions
check_model(m_angst_nb)
check_model(m_depr_nb)
check_model(m_tot_nb)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb <- emmeans(m_angst_nb, pairwise ~ edss_progression, adjust = "tukey")
emm_angst_nb$contrasts

# HADS Depression
emm_depr_nb <- emmeans(m_depr_nb, pairwise ~ edss_progression, adjust = "tukey")
emm_depr_nb$contrasts

# HADS Total
emm_tot_nb <- emmeans(m_tot_nb, pairwise ~ edss_progression, adjust = "tukey")
emm_tot_nb$contrasts

# Print group size
table(merged_data_1F$edss_progression)

# Save with ggsave
ggsave("figuren/boxplot_hads_angst_vs_edss_19092025.png", plot = boxedss1, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_edss_19092025.png", plot = boxedss2, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_edss_19092025.png", plot = boxedss3, width = 5, height = 4, dpi = 300)     

# Summarizing descriptive statistics per group with explicit Q1 and Q3
stats_summary_edss <- merged_data_1F %>%
  group_by(edss_progression) %>%
  summarise(
    n = n(),
    
    median_hads_angst = median(hads_angst, na.rm = TRUE),
    Q1_hads_angst = quantile(hads_angst, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_angst = quantile(hads_angst, probs = 0.75, na.rm = TRUE, type = 1),
    
    median_hads_depressie = median(hads_depressie, na.rm = TRUE),
    Q1_hads_depressie = quantile(hads_depressie, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_depressie = quantile(hads_depressie, probs = 0.75, na.rm = TRUE, type = 1),
    
    median_hads_totaal = median(hads_totaal, na.rm = TRUE),
    Q1_hads_totaal = quantile(hads_totaal, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_totaal = quantile(hads_totaal, probs = 0.75, na.rm = TRUE, type = 1)
  )

print(stats_summary_edss, width = Inf)

#### HADS vs T25FW progression 1 year after completing HADS
# Create a separate variable so that NAs are displayed separately (instead of NA = not progressive)
merged_data_1F <- merged_data_1F %>%
  mutate(
    # Recalculate what the progress status *would be* based on the raw measurements
    T25FW_progressie_boxplot = case_when(
      !is.na(prev_T25FW) & !is.na(next_T25FW) & ((next_T25FW - prev_T25FW) / prev_T25FW >= 0.20) ~ "T25FW Progressie",
      !is.na(prev_T25FW) & !is.na(next_T25FW) ~ "Geen T25FW Progressie",
      TRUE ~ "Ontbrekend"
    ),
    
    # Overwrite with ‘T25FW Progression’ for the two manually coded patients (T25FW no longer possible due to progression resulting in loss of walking ability)
    T25FW_boxplotgroep = case_when(
      `Participant Id` %in% c("SPINP10039", "SPINP10001") ~ "T25FW Progressie",
      TRUE ~ T25FW_progressie_boxplot
    ),
    
    # Put in factor form for control over order in box plot
    T25FW_boxplotgroep = factor(T25FW_boxplotgroep, levels = c(
      "Geen T25FW Progressie", "T25FW Progressie", "Ontbrekend"
    ))
  )

### Supplementary Figure 1E - HADS anxiety score vs T25FW progression 1 year after completing HADS (NAs SHOWN SEPARATELY)
boxT25FW1a <- ggplot(merged_data_1F, aes(x = T25FW_boxplotgroep, y = hads_angst, fill = T25FW_boxplotgroep)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen T25FW Progressie" = "#0072B2",
    "T25FW Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "T25FW Progressie (1 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxT25FW1a

### Supplementary Figure 1I -HADS depression score vs T25FW progression after 1 year (NAs SHOWN SEPARATELY) 
boxT25FW2a <- ggplot(merged_data_1F, aes(x = T25FW_boxplotgroep, y = hads_depressie, fill = T25FW_boxplotgroep)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen T25FW Progressie" = "#0072B2",
    "T25FW Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "T25FW Progressie (1 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxT25FW2a

### Supplementary Figure 1A - HADS total score vs T25FW progression after 1 year (NAs SHOWN SEPARATELY) 
boxT25FW3a <- ggplot(merged_data_1F, aes(x = T25FW_boxplotgroep, y = hads_totaal, fill = T25FW_boxplotgroep)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen T25FW Progressie" = "#0072B2",
    "T25FW Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "T25FW Progressie (1 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxT25FW3a

#### statistics - HADS scores vs T25FW progression 1 year after completing HADS
# Filter only the 2 relevant groups (no progression vs progression) for calculating p-value (using post-hoc testing)
subset_data <- merged_data_1F %>%
  filter(T25FW_boxplotgroep %in% c("Geen T25FW Progressie", "T25FW Progressie"))

# Determine p-values of significance in HADS scores using post-hoc test on HADS in linear regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_T25FW <- glm.nb(hads_angst ~ factor(T25FW_boxplotgroep), data = merged_data_1F)
m_depr_nb_T25FW  <- glm.nb(hads_depressie ~ factor(T25FW_boxplotgroep), data = merged_data_1F)
m_tot_nb_T25FW   <- glm.nb(hads_totaal ~ factor(T25FW_boxplotgroep), data = merged_data_1F)

### 2. Check model assumptions
check_model(m_angst_nb_T25FW)
check_model(m_depr_nb_T25FW)
check_model(m_tot_nb_T25FW)

### 3. Post-hoc test using emmeans (pairwise, Tukey correctie)
# HADS Anxiety
emm_angst_nb_T25FW <- emmeans(m_angst_nb_T25FW, pairwise ~ T25FW_boxplotgroep, adjust = "tukey")
emm_angst_nb_T25FW$contrasts

# HADS Depression
emm_depr_nb_T25FW <- emmeans(m_depr_nb_T25FW, pairwise ~ T25FW_boxplotgroep, adjust = "tukey")
emm_depr_nb_T25FW$contrasts

# HADS Total
emm_tot_nb_T25FW <- emmeans(m_tot_nb_T25FW, pairwise ~ T25FW_boxplotgroep, adjust = "tukey")
emm_tot_nb_T25FW$contrasts

# Group size including NAs
table(merged_data_1F$T25FW_boxplotgroep)

# Save using ggsave
# Boxplots with NAs shown separately
ggsave("figuren/boxplot_hads_angst_vs_T25FW_wNAs_19092025.png", plot = boxT25FW1a, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_T25FW_wNAs_19092025.png", plot = boxT25FW2a, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_T25FW_wNAs_19092025.png", plot = boxT25FW3a, width = 6, height = 4, dpi = 300)     

#### HADS vs AMSQ progression 1 year after completing HADS 
# Create a separate variable so that NAs are displayed separately (instead of NA = not progressive)        
merged_data_1F <- merged_data_1F %>%
  mutate(
    AMSQ_progressie_boxplot = case_when(
      !is.na(prev_AMSQ_totaal) & !is.na(next_AMSQ_totaal) & (next_AMSQ_totaal - prev_AMSQ_totaal) >= 18 ~ "AMSQ Progressie",
      !is.na(prev_AMSQ_totaal) & !is.na(next_AMSQ_totaal) ~ "Geen AMSQ Progressie",
      TRUE ~ "Ontbrekend"
    ),
    AMSQ_progressie_boxplot = factor(AMSQ_progressie_boxplot, levels = c(
      "Geen AMSQ Progressie", "AMSQ Progressie", "Ontbrekend"
    ))
  )

### Supplementary Figure 2F - HADS anxiety score vs AMSQ progression (NAS shown separately)
boxAMSQ1a <- ggplot(merged_data_1F, aes(x = AMSQ_progressie_boxplot, y = hads_angst, fill = AMSQ_progressie_boxplot)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen AMSQ Progressie" = "#0072B2",
    "AMSQ Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "AMSQ Progressie (1 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxAMSQ1a

### Supplementary Figure 2J - HADS depression score vs AMSQ progression (NAS shown separately)
boxAMSQ2a <- ggplot(merged_data_1F, aes(x = AMSQ_progressie_boxplot, y = hads_depressie, fill = AMSQ_progressie_boxplot)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen AMSQ Progressie" = "#0072B2",
    "AMSQ Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "AMSQ Progressie (1 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxAMSQ2a

### Supplementary Figure 2B - HADS score vs AMSQ progression (NAS shown separately)
boxAMSQ3a <- ggplot(merged_data_1F, aes(x = AMSQ_progressie_boxplot, y = hads_totaal, fill = AMSQ_progressie_boxplot)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen AMSQ Progressie" = "#0072B2",
    "AMSQ Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "AMSQ Progressie (1 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxAMSQ3a

#### statistics - HADS vs AMSQ progression 1 year after completing HADS 
# Filter only the 2 relevante groups (no progression vs progression) for calculating p-value (using post-hoc testing)
subset_data_AMSQ <- merged_data_1F %>%
  filter(AMSQ_progressie_boxplot %in% c("Geen AMSQ Progressie", "AMSQ Progressie"))

# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_AMSQ <- glm.nb(hads_angst ~ factor(AMSQ_progressie_boxplot), data = merged_data_1F)
m_depr_nb_AMSQ  <- glm.nb(hads_depressie ~ factor(AMSQ_progressie_boxplot), data = merged_data_1F)
m_tot_nb_AMSQ   <- glm.nb(hads_totaal ~ factor(AMSQ_progressie_boxplot), data = merged_data_1F)

### 2. Check model assumptions
check_model(m_angst_nb_AMSQ)
check_model(m_depr_nb_AMSQ)
check_model(m_tot_nb_AMSQ)

### 3. Post-hoc test using emmeans (pairwise, Tukey correctie)
# HADS Anxiety
emm_angst_nb_AMSQ <- emmeans(m_angst_nb_AMSQ, pairwise ~ AMSQ_progressie_boxplot, adjust = "tukey")
emm_angst_nb_AMSQ$contrasts

# HADS Depression
emm_depr_nb_AMSQ <- emmeans(m_depr_nb_AMSQ, pairwise ~ AMSQ_progressie_boxplot, adjust = "tukey")
emm_depr_nb_AMSQ$contrasts

# HADS Total
emm_tot_nb_AMSQ <- emmeans(m_tot_nb_AMSQ, pairwise ~ AMSQ_progressie_boxplot, adjust = "tukey")
emm_tot_nb_AMSQ$contrasts


# Group size including NAs
table(merged_data_1F$AMSQ_progressie_boxplot)

# Save using ggsave
# Boxplots with NAs shown separately
ggsave("figuren/boxplot_hads_angst_vs_AMSQ_wNAs_19092025.png", plot = boxAMSQ1a, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_AMSQ_wNAs_19092025.png", plot = boxAMSQ2a, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_AMSQ_wNAs_19092025.png", plot = boxAMSQ3a, width = 6, height = 4, dpi = 300)     

#### HADS vs SDMT progression 1 year after completing HADS     
# Maak separate variabele so that NAs are displayed separately (instead of NA = not progresssive)     
merged_data_1F <- merged_data_1F %>%
  mutate(
    SDMT_progressie_boxplot = case_when(
      !is.na(prev_SDMT) & !is.na(next_SDMT) & (prev_SDMT - next_SDMT >= 8) ~ "SDMT Progressie",
      !is.na(prev_SDMT) & !is.na(next_SDMT) ~ "Geen SDMT Progressie",
      TRUE ~ "Ontbrekend"
    ),
    SDMT_progressie_boxplot = factor(SDMT_progressie_boxplot, levels = c(
      "Geen SDMT Progressie", "SDMT Progressie", "Ontbrekend"
    ))
  )

### Supplementary Figure 2G - HADS anxiety score vs SDMT progression (NAS = shown separately)
boxSDMT1a <- ggplot(merged_data_1F, aes(x = SDMT_progressie_boxplot, y = hads_angst, fill = SDMT_progressie_boxplot)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen SDMT Progressie" = "#0072B2",
    "SDMT Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "SDMT Progressie (1 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxSDMT1a

### Supplementary Figure 2K -  HADS depression score vs SDMT progression (NAS = shown separately)
boxSDMT2a <- ggplot(merged_data_1F, aes(x = SDMT_progressie_boxplot, y = hads_depressie, fill = SDMT_progressie_boxplot)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen SDMT Progressie" = "#0072B2",
    "SDMT Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "SDMT Progressie (1 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxSDMT2a

### Supplementary Figure 2C - HADS total score vs SDMT progression (NAS = shown separately)
boxSDMT3a <- ggplot(merged_data_1F, aes(x = SDMT_progressie_boxplot, y = hads_totaal, fill = SDMT_progressie_boxplot)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen SDMT Progressie" = "#0072B2",
    "SDMT Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "SDMT Progressie (1 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxSDMT3a

#### statistics - HADS vs SDMT progression 1 year after completing HADS 
# Filter only the 2 relevant groups (no progression vs progression) for calculating p-values
subset_data_sdmt <- merged_data_1F %>%
  filter(SDMT_progressie_boxplot %in% c("Geen SDMT Progressie", "SDMT Progressie"))

# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_SDMT <- glm.nb(hads_angst ~ factor(SDMT_progressie_boxplot), data = merged_data_1F)
m_depr_nb_SDMT  <- glm.nb(hads_depressie ~ factor(SDMT_progressie_boxplot), data = merged_data_1F)
m_tot_nb_SDMT   <- glm.nb(hads_totaal ~ factor(SDMT_progressie_boxplot), data = merged_data_1F)

### 2. Check model assumptions
check_model(m_angst_nb_SDMT)
check_model(m_depr_nb_SDMT)
check_model(m_tot_nb_SDMT)

### 3. Post-hoc test using emmeans (pairwise, Tukey correctie)
# HADS Anxiety
emm_angst_nb_SDMT <- emmeans(m_angst_nb_SDMT, pairwise ~ SDMT_progressie_boxplot, adjust = "tukey")
emm_angst_nb_SDMT$contrasts

# HADS Depression
emm_depr_nb_SDMT <- emmeans(m_depr_nb_SDMT, pairwise ~ SDMT_progressie_boxplot, adjust = "tukey")
emm_depr_nb_SDMT$contrasts

# HADS Total
emm_tot_nb_SDMT <- emmeans(m_tot_nb_SDMT, pairwise ~ SDMT_progressie_boxplot, adjust = "tukey")
emm_tot_nb_SDMT$contrasts

# Print group size
table(merged_data_1F$SDMT_progressie_boxplot)

# Save using ggsave (NAs = shown separately)
ggsave("figuren/boxplot_hads_angst_vs_SDMT_wNAs_19092025.png", plot = boxSDMT1a, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_SDMT_wNAs_19092025.png", plot = boxSDMT2a, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_SDMT_wNAs_19092025.png", plot = boxSDMT3a, width = 6, height = 4, dpi = 300)     


#### HADS vs PDDS progression 1 year after completing HADS      
# Check if there are NA's in PDDS_progression --> no NAs
table(merged_data_1F$prev_PDDS, useNA = "always")
table(merged_data_1F$next_PDDS, useNA = "always")

### Supplementary Figure 1H - HADS anxiety score vs PDDS progression
boxPDDS1 <- ggplot(merged_data_1F, aes(x = factor(PDDS_progression), y = hads_angst, fill = factor(PDDS_progression))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen PDDS Progressie", "PDDS Progressie")) +
  labs(x = "PDDS Progressie (1 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxPDDS1

### Supplementary Figure 1L - HADS-depression score vs PDDS progression
boxPDDS2 <- ggplot(merged_data_1F, aes(x = factor(PDDS_progression), y = hads_depressie, fill = factor(PDDS_progression))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen PDDS Progressie", "PDDS Progressie")) +
  labs(x = "PDDS Progressie (1 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxPDDS2

### Supplementary Figure 1D - total HADS score vs PDDS progression
boxPDDS3 <- ggplot(merged_data_1F, aes(x = factor(PDDS_progression), y = hads_totaal, fill = factor(PDDS_progression))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen PDDS Progressie", "PDDS Progressie")) +
  labs(x = "PDDS Progressie (1 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxPDDS3

#### statistics - HADS vs PDDS progression 1 year after completing HADS 
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_PDDS <- glm.nb(hads_angst ~ factor(PDDS_progression), data = merged_data_1F)
m_depr_nb_PDDS  <- glm.nb(hads_depressie ~ factor(PDDS_progression), data = merged_data_1F)
m_tot_nb_PDDS   <- glm.nb(hads_totaal ~ factor(PDDS_progression), data = merged_data_1F)

### 2. Check model assumptions
check_model(m_angst_nb_PDDS)
check_model(m_depr_nb_PDDS)
check_model(m_tot_nb_PDDS)

### 3. Post-hoc test using emmeans (pairwise, Tukey correctie)
# HADS Anxiety
emm_angst_nb_PDDS <- emmeans(m_angst_nb_PDDS, pairwise ~ PDDS_progression, adjust = "tukey")
emm_angst_nb_PDDS$contrasts

# HADS Depression
emm_depr_nb_PDDS <- emmeans(m_depr_nb_PDDS, pairwise ~ PDDS_progression, adjust = "tukey")
emm_depr_nb_PDDS$contrasts

# HADS Total
emm_tot_nb_PDDS <- emmeans(m_tot_nb_PDDS, pairwise ~ PDDS_progression, adjust = "tukey")
emm_tot_nb_PDDS$contrasts

# Print group size 
table(merged_data_1F$PDDS_progression, useNA = "always")

# Save using ggsave
ggsave("figuren/boxplot_hads_angst_vs_PDDS_19092025.png", plot = boxPDDS1, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_PDDS_19092025.png", plot = boxPDDS2, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_PDDS_19092025.png", plot = boxPDDS3, width = 5, height = 4, dpi = 300)     


#### HADS scores vs EDSS or T25FW or AMSQ progression 1 year after completing HADS  

### Figure 2L - HADS anxiety score vs EDSS or T25FW or AMSQ progressie after 1 year
boxedss_T25FW_AMSQ1 <- ggplot(merged_data_1F, aes(x = factor(progression_edss_or_T25FW_or_AMSQ), y = hads_angst, fill = factor(progression_edss_or_T25FW_or_AMSQ))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie (3var)")) +  
  labs(x = "Progressie (1 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())  
boxedss_T25FW_AMSQ1

### Figure 2R - HADS-depression score vs EDSS or T25FW or AMSQ progression after 1 year
boxedss_T25FW_AMSQ2 <- ggplot(merged_data_1F, aes(x = factor(progression_edss_or_T25FW_or_AMSQ), y = hads_depressie, fill = factor(progression_edss_or_T25FW_or_AMSQ))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie (3var)")) +
  labs(x = "Progressie (1 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxedss_T25FW_AMSQ2

### Figure 2F - total HADS score vs EDSS or T25FW or AMSQ progression after 1 year
boxedss_T25FW_AMSQ3 <- ggplot(merged_data_1F, aes(x = factor(progression_edss_or_T25FW_or_AMSQ), y = hads_totaal, fill = factor(progression_edss_or_T25FW_or_AMSQ))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie (3var)")) +
  labs(x = "Progressie (1 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxedss_T25FW_AMSQ3


#### statistics - HADS vs EDSS or T25FW or AMSQ progression 1 year after completing HADS 
# Number of people in each group
table(merged_data_1F$progression_edss_or_T25FW_or_AMSQ)

# Summarizing statsitics per group 
stats_summary <- merged_data_1F %>%
  group_by(progression_edss_or_T25FW_or_AMSQ) %>%
  summarise(
    n = n(),
    median_hads_angst = median(hads_angst, na.rm = TRUE),
    Q1_hads_angst = quantile(hads_angst, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_angst = quantile(hads_angst, probs = 0.75, na.rm = TRUE, type = 1),
    
    median_hads_depressie = median(hads_depressie, na.rm = TRUE),
    Q1_hads_depressie = quantile(hads_depressie, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_depressie = quantile(hads_depressie, probs = 0.75, na.rm = TRUE, type = 1),
    
    median_hads_totaal = median(hads_totaal, na.rm = TRUE),
    Q1_hads_totaal = quantile(hads_totaal, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_totaal = quantile(hads_totaal, probs = 0.75, na.rm = TRUE, type = 1)
  )

print(stats_summary, width = Inf)

# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_3varcomp <- glm.nb(hads_angst ~ factor(progression_edss_or_T25FW_or_AMSQ), data = merged_data_1F)
m_depr_nb_3varcomp  <- glm.nb(hads_depressie ~ factor(progression_edss_or_T25FW_or_AMSQ), data = merged_data_1F)
m_tot_nb_3varcomp   <- glm.nb(hads_totaal ~ factor(progression_edss_or_T25FW_or_AMSQ), data = merged_data_1F)

### 2. Check model assumptions
check_model(m_angst_nb_3varcomp)
check_model(m_depr_nb_3varcomp)
check_model(m_tot_nb_3varcomp)

### 3. Post-hoc test using emmeans (pairwise, Tukey correctie)
# HADS Anxiety
emm_angst_nb_3varcomp <- emmeans(m_angst_nb_3varcomp, pairwise ~ progression_edss_or_T25FW_or_AMSQ, adjust = "tukey")
emm_angst_nb_3varcomp$contrasts

# HADS Depression
emm_depr_nb_3varcomp <- emmeans(m_depr_nb_3varcomp, pairwise ~ progression_edss_or_T25FW_or_AMSQ, adjust = "tukey")
emm_depr_nb_3varcomp$contrasts

# HADS Total
emm_tot_nb_3varcomp <- emmeans(m_tot_nb_3varcomp, pairwise ~ progression_edss_or_T25FW_or_AMSQ, adjust = "tukey")
emm_tot_nb_3varcomp$contrasts


# Save using ggsave
ggsave("figuren/boxplot_hads_angst_vs_edss_T25FW_AMSQ_19092025.png", plot = boxedss_T25FW_AMSQ1, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_edss_T25FW_AMSQ_19092025.png", plot = boxedss_T25FW_AMSQ2, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_edss_T25FW_AMSQ_19092025.png", plot = boxedss_T25FW_AMSQ3, width = 5, height = 4, dpi = 300)  


#### HADS scores vs EDSS, T25FW, AMSQ, SDMT or PDDS progression 1 year after completing HADS  

### Figure 2M - HADS anxiety score vs progression after 1 year
boxedss_T25FW_AMSQ_SDMT_PDDS1 <- ggplot(merged_data_1F, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS), y = hads_angst, fill = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie (5var)")) +
  labs(x = "Progressie (1 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxedss_T25FW_AMSQ_SDMT_PDDS1

### Figure 2S - HADS depression score vs progression after 1 year
boxedss_T25FW_AMSQ_SDMT_PDDS2 <- ggplot(merged_data_1F, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS), y = hads_depressie, fill = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie (5var)")) +
  labs(x = "Progressie (1 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxedss_T25FW_AMSQ_SDMT_PDDS2 

### Figure 2G - total HADS score vs progression after 1 year
boxedss_T25FW_AMSQ_SDMT_PDDS3 <- ggplot(merged_data_1F, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS), y = hads_totaal, fill = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie (5var)")) +
  labs(x = "Progressie (1 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
boxedss_T25FW_AMSQ_SDMT_PDDS3

#### statistics - EDSS, T25FW, AMSQ, SDMT or PDDS progression 1 year after completing HADS 
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_5varcomp <- glm.nb(hads_angst ~ factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS), data = merged_data_1F)
m_depr_nb_5varcomp  <- glm.nb(hads_depressie ~ factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS), data = merged_data_1F)
m_tot_nb_5varcomp   <- glm.nb(hads_totaal ~ factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS), data = merged_data_1F)

### 2. Check model assumptions
check_model(m_angst_nb_5varcomp)
check_model(m_depr_nb_5varcomp)
check_model(m_tot_nb_5varcomp)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb_5varcomp <- emmeans(m_angst_nb_5varcomp, pairwise ~ progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS, adjust = "tukey")
emm_angst_nb_5varcomp$contrasts

# HADS Depression
emm_depr_nb_5varcomp <- emmeans(m_depr_nb_5varcomp, pairwise ~ progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS, adjust = "tukey")
emm_depr_nb_5varcomp$contrasts

# HADS Total
emm_tot_nb_5varcomp <- emmeans(m_tot_nb_5varcomp, pairwise ~ progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS, adjust = "tukey")
emm_tot_nb_5varcomp$contrasts

# Number of people in each group
table(merged_data_1F$progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS)

# Summarizing statistics per group with explicit Q1 en Q3
stats_summary <- merged_data_1F %>%
  group_by(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS) %>%
  summarise(
    n = n(),
    
    median_hads_angst = median(hads_angst, na.rm = TRUE),
    Q1_hads_angst = quantile(hads_angst, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_angst = quantile(hads_angst, probs = 0.75, na.rm = TRUE, type = 1),
    
    median_hads_depressie = median(hads_depressie, na.rm = TRUE),
    Q1_hads_depressie = quantile(hads_depressie, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_depressie = quantile(hads_depressie, probs = 0.75, na.rm = TRUE, type = 1),
    
    median_hads_totaal = median(hads_totaal, na.rm = TRUE),
    Q1_hads_totaal = quantile(hads_totaal, probs = 0.25, na.rm = TRUE, type = 1),
    Q3_hads_totaal = quantile(hads_totaal, probs = 0.75, na.rm = TRUE, type = 1)
  )

print(stats_summary, width = Inf)  

# Save using ggsave
ggsave("figuren/boxplot_hads_angst_vs_edss_T25FW_AMSQ_SDMT_PDDS_19092025.png", plot = boxedss_T25FW_AMSQ_SDMT_PDDS1, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_edss_T25FW_AMSQ_SDMT_PDDS_19092025.png", plot = boxedss_T25FW_AMSQ_SDMT_PDDS2, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_edss_T25FW_AMSQ_SDMT_PDDS_19092025.png", plot = boxedss_T25FW_AMSQ_SDMT_PDDS3, width = 5, height = 4, dpi = 300)      

###### 2. year 2 visualization and statistics ######

#### HADS vs EDSS progression 2 year after completing HADS
# Check if there are any NAs in edss_progression_2 --> no NAs
table(merged_data_1F_2y$edss_progression_2, useNA = "always")
table(merged_data_1F_2y$prev_edss, useNA = "always")
table(merged_data_1F_2y$next_edss_2, useNA = "always")

### Figure 2N - HADS angst vs EDSS progression 2 years after completing HADS
boxedss1_2y <- ggplot(merged_data_1F_2y, aes(x = factor(edss_progression_2), y = hads_angst, fill = factor(edss_progression_2))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen EDSS Progressie", "EDSS Progressie")) +  
  labs(x = "EDSS Progressie (2 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())  
boxedss1_2y

### Figure 2T - HADS depression score vs EDSS progression after 2  years
boxedss2_2y <- ggplot(merged_data_1F_2y, aes(x = factor(edss_progression_2), y = hads_depressie, fill = factor(edss_progression_2))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen EDSS Progressie", "EDSS Progressie")) +  
  labs(x = "EDSS Progressie (2 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())  
boxedss2_2y

### Figure 2H - total HADS score vs EDSS progression after 2 years
boxedss3_2y <- ggplot(merged_data_1F_2y, aes(x = factor(edss_progression_2), y = hads_totaal, fill = factor(edss_progression_2))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen EDSS Progressie", "EDSS Progressie")) +  
  labs(x = "EDSS Progressie (2 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())        
boxedss3_2y

#### statistics - HADS vs EDSS progression 2 year after completing HADS
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_EDSS_2y <- glm.nb(hads_angst ~ factor(edss_progression_2), data = merged_data_1F_2y)
m_depr_nb_EDSS_2y  <- glm.nb(hads_depressie ~ factor(edss_progression_2), data = merged_data_1F_2y)
m_tot_nb_EDSS_2y   <- glm.nb(hads_totaal ~ factor(edss_progression_2), data = merged_data_1F_2y)

### 2. Check model assumptions
check_model(m_angst_nb_EDSS_2y)
check_model(m_depr_nb_EDSS_2y)
check_model(m_tot_nb_EDSS_2y)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb_EDSS_2y <- emmeans(m_angst_nb_EDSS_2y, pairwise ~ edss_progression_2, adjust = "tukey")
emm_angst_nb_EDSS_2y$contrasts

# HADS Depression
emm_depr_nb_EDSS_2y <- emmeans(m_depr_nb_EDSS_2y, pairwise ~ edss_progression_2, adjust = "tukey")
emm_depr_nb_EDSS_2y$contrasts

# HADS Total
emm_tot_nb_EDSS_2y <- emmeans(m_tot_nb_EDSS_2y, pairwise ~ edss_progression_2, adjust = "tukey")
emm_tot_nb_EDSS_2y$contrasts


# Print group size including NAs
table(merged_data_1F_2y$edss_progression_2, useNA = "always")

# Save using ggsave
ggsave("figuren/boxplot_hads_angst_vs_edss1_2y.png", plot = boxedss1_2y, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_edss2_2y.png", plot = boxedss2_2y, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_edss3_2y.png", plot = boxedss3_2y, width = 5, height = 4, dpi = 300)

#### HADS scores vs T25FW progression 2 years after completing HADS  

# Make separate variable so that NA's are shown separately (instead of NA = not progressive)
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(
    # Recalculate progression status based on raw values 
    T25FW_progressie_2y_box = case_when(
      !is.na(prev_T25FW) & !is.na(next_T25FW_2) & ((next_T25FW_2 - prev_T25FW) / prev_T25FW >= 0.20) ~ "T25FW Progressie",
      !is.na(prev_T25FW) & !is.na(next_T25FW_2) ~ "Geen T25FW Progressie",
      TRUE ~ "Ontbrekend"
    ),
    
    # Manually overwrite TRUEs
    T25FW_boxplotgroep_2y = case_when(
      `Participant Id` %in% c("SPINP012003", "SPINP070005", "SPINP10001", "SPINP10039") ~ "T25FW Progressie",
      TRUE ~ T25FW_progressie_2y_box
    ),
    T25FW_boxplotgroep_2y = factor(T25FW_boxplotgroep_2y,
                                   levels = c("Geen T25FW Progressie", "T25FW Progressie", "Ontbrekend"))
  )

### Supplementary 2E - HADS angst score vs T25FW progression after 2 years (NAs = shown separately)
box_T25FW_2y <- ggplot(merged_data_1F_2y, aes(x = T25FW_boxplotgroep_2y, y = hads_angst, fill = T25FW_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen T25FW Progressie" = "#0072B2",
    "T25FW Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "T25FW Progressie (2 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
box_T25FW_2y

### Supplementary 2I - HADS depression score vs T25FW progression after 2 years (NAs = shown separately)
box_T25FW_2y2 <- ggplot(merged_data_1F_2y, aes(x = T25FW_boxplotgroep_2y, y = hads_depressie, fill = T25FW_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen T25FW Progressie" = "#0072B2",
    "T25FW Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "T25FW Progressie (2 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
box_T25FW_2y2

### Supplementary 2A - total HADS score vs T25FW progression after 2 years (NAs = shown separately)
box_T25FW_2y3 <- ggplot(merged_data_1F_2y, aes(x = T25FW_boxplotgroep_2y, y = hads_totaal, fill = T25FW_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen T25FW Progressie" = "#0072B2",
    "T25FW Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "T25FW Progressie (2 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())
box_T25FW_2y3

#### statistics - HADS scores vs T25FW progression 2 years after completing HADS  
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_T25FW_2y <- glm.nb(hads_angst ~ factor(T25FW_boxplotgroep_2y), data = merged_data_1F_2y)
m_depr_nb_T25FW_2y  <- glm.nb(hads_depressie ~ factor(T25FW_boxplotgroep_2y), data = merged_data_1F_2y)
m_tot_nb_T25FW_2y   <- glm.nb(hads_totaal ~ factor(T25FW_boxplotgroep_2y), data = merged_data_1F_2y)

### 2. Check model assumptions
check_model(m_angst_nb_T25FW_2y)
check_model(m_depr_nb_T25FW_2y)
check_model(m_tot_nb_T25FW_2y)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb_T25FW_2y <- emmeans(m_angst_nb_T25FW_2y, pairwise ~ T25FW_boxplotgroep_2y, adjust = "tukey")
emm_angst_nb_T25FW_2y$contrasts

# HADS Depression
emm_depr_nb_T25FW_2y <- emmeans(m_depr_nb_T25FW_2y, pairwise ~ T25FW_boxplotgroep_2y, adjust = "tukey")
emm_depr_nb_T25FW_2y$contrasts

# HADS Total
emm_tot_nb_T25FW_2y <- emmeans(m_tot_nb_T25FW_2y, pairwise ~ T25FW_boxplotgroep_2y, adjust = "tukey")
emm_tot_nb_T25FW_2y$contrasts

# Show group size including NAs
table(merged_data_1F_2y$T25FW_boxplotgroep_2y, useNA = "always")

# Save using ggsave
# Boxplots with NAs shown separately
ggsave("figuren/boxplot_hads_angst_vs_T25FW2y_wNAs.png", plot = box_T25FW_2y, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_T25FW2y_wNAs.png", plot = box_T25FW_2y2, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_T25FW2y_wNAs.png", plot = box_T25FW_2y3, width = 6, height = 4, dpi = 300)     


#### HADS scores vs AMSQ progression 2 years after HADS
# Make distinct variable so that NA's are shown separately (instead of NA = not progressive)
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(
    AMSQ_progressie_boxplot_2y = case_when(
      !is.na(prev_AMSQ_totaal) & !is.na(next_AMSQ_2_totaal) & (next_AMSQ_2_totaal - prev_AMSQ_totaal) >= 18 ~ "AMSQ Progressie",
      !is.na(prev_AMSQ_totaal) & !is.na(next_AMSQ_2_totaal) ~ "Geen AMSQ Progressie",
      TRUE ~ "Ontbrekend"
    ),
    AMSQ_boxplotgroep_2y = factor(AMSQ_progressie_boxplot_2y, levels = c(
      "Geen AMSQ Progressie", "AMSQ Progressie", "Ontbrekend"
    ))
  )

### Supplementary 2F - HADS anxiety score vs AMSQ progression after 2 years (NAs = shown separately)
box_AMSQ_2y <- ggplot(merged_data_1F_2y, aes(x = AMSQ_boxplotgroep_2y, y = hads_angst, fill = AMSQ_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen AMSQ Progressie" = "#0072B2",
    "AMSQ Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "AMSQ Progressie (2 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Supplementary 2J - HADS depression score vs AMSQ progression after 2 years (NAs = shown separately)
box_AMSQ_2y2 <- ggplot(merged_data_1F_2y, aes(x = AMSQ_boxplotgroep_2y, y = hads_depressie, fill = AMSQ_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen AMSQ Progressie" = "#0072B2",
    "AMSQ Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "AMSQ Progressie (2 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Supplementary 2B - HADS total score vs AMSQ progression after 2 years (NAs = shown separately)
box_AMSQ_2y3 <- ggplot(merged_data_1F_2y, aes(x = AMSQ_boxplotgroep_2y, y = hads_totaal, fill = AMSQ_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen AMSQ Progressie" = "#0072B2",
    "AMSQ Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "AMSQ Progressie (2 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())


#### statistics - HADS scores vs AMSQ progression 2 years after HADS
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_AMSQ_2y <- glm.nb(hads_angst ~ factor(AMSQ_boxplotgroep_2y), data = merged_data_1F_2y)
m_depr_nb_AMSQ_2y  <- glm.nb(hads_depressie ~ factor(AMSQ_boxplotgroep_2y), data = merged_data_1F_2y)
m_tot_nb_AMSQ_2y   <- glm.nb(hads_totaal ~ factor(AMSQ_boxplotgroep_2y), data = merged_data_1F_2y)

### 2. Check model assumptions
check_model(m_angst_nb_AMSQ_2y)
check_model(m_depr_nb_AMSQ_2y)
check_model(m_tot_nb_AMSQ_2y)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb_AMSQ_2y <- emmeans(m_angst_nb_AMSQ_2y, pairwise ~ AMSQ_boxplotgroep_2y, adjust = "tukey")
emm_angst_nb_AMSQ_2y$contrasts

# HADS Depression
emm_depr_nb_AMSQ_2y <- emmeans(m_depr_nb_AMSQ_2y, pairwise ~ AMSQ_boxplotgroep_2y, adjust = "tukey")
emm_depr_nb_AMSQ_2y$contrasts

# HADS Total
emm_tot_nb_AMSQ_2y <- emmeans(m_tot_nb_AMSQ_2y, pairwise ~ AMSQ_boxplotgroep_2y, adjust = "tukey")
emm_tot_nb_AMSQ_2y$contrasts


# Print group size with NAs separately
table(merged_data_1F_2y$AMSQ_boxplotgroep_2y, useNA = "always")

# Save using ggsave
# Boxplots with NAs shown separately
ggsave("figuren/boxplot_hads_angst_vs_AMSQ2y_wNAs.png", plot = box_AMSQ_2y, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_AMSQ2y_wNAs.png", plot = box_AMSQ_2y2, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_AMSQ2y_wNAs.png", plot = box_AMSQ_2y3, width = 6, height = 4, dpi = 300)     


#### HADS scores vs SDMT progression 2 years after completing HADS   

# Make separate variable so that NA's are shown separately (instead of NA = not progressive)
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(
    SDMT_progressie_boxplot_2y = case_when(
      !is.na(prev_SDMT) & !is.na(next_SDMT_2) & (prev_SDMT - next_SDMT_2 >= 8) ~ "SDMT Progressie",
      !is.na(prev_SDMT) & !is.na(next_SDMT_2) ~ "Geen SDMT Progressie",
      TRUE ~ "Ontbrekend"
    ),
    SDMT_boxplotgroep_2y = factor(SDMT_progressie_boxplot_2y, levels = c(
      "Geen SDMT Progressie", "SDMT Progressie", "Ontbrekend"
    ))
  )

### Supplementary 2G - HADS anxiety score vs SDMT progression after 2 years (NAs = shown separately)
box_SDMT_2y <- ggplot(merged_data_1F_2y, aes(x = SDMT_boxplotgroep_2y, y = hads_angst, fill = SDMT_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen SDMT Progressie" = "#0072B2",
    "SDMT Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "SDMT Progressie (2 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Supplementary 2K - HADS depression score vs SDMT progression after 2 years (NAs = shown separately)
box_SDMT_2y2 <- ggplot(merged_data_1F_2y, aes(x = SDMT_boxplotgroep_2y, y = hads_depressie, fill = SDMT_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen SDMT Progressie" = "#0072B2",
    "SDMT Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "SDMT Progressie (2 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Supplementary 2C - total HADS score vs SDMT progression after 2 years (NAs = shown separately)
box_SDMT_2y3 <- ggplot(merged_data_1F_2y, aes(x = SDMT_boxplotgroep_2y, y = hads_totaal, fill = SDMT_boxplotgroep_2y)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Geen SDMT Progressie" = "#0072B2",
    "SDMT Progressie" = "#E69F00",
    "Ontbrekend" = "grey70"
  )) +
  labs(x = "SDMT Progressie (2 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

#### statistics - HADS scores vs SDMT progression 2 years after completing HADS   
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_SDMT_2y <- glm.nb(hads_angst ~ factor(SDMT_boxplotgroep_2y), data = merged_data_1F_2y)
m_depr_nb_SDMT_2y  <- glm.nb(hads_depressie ~ factor(SDMT_boxplotgroep_2y), data = merged_data_1F_2y)
m_tot_nb_SDMT_2y   <- glm.nb(hads_totaal ~ factor(SDMT_boxplotgroep_2y), data = merged_data_1F_2y)

### 2. Check model assumptions
check_model(m_angst_nb_SDMT_2y)
check_model(m_depr_nb_SDMT_2y)
check_model(m_tot_nb_SDMT_2y)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb_SDMT_2y <- emmeans(m_angst_nb_SDMT_2y, pairwise ~ SDMT_boxplotgroep_2y, adjust = "tukey")
emm_angst_nb_SDMT_2y$contrasts

# HADS Depression
emm_depr_nb_SDMT_2y <- emmeans(m_depr_nb_SDMT_2y, pairwise ~ SDMT_boxplotgroep_2y, adjust = "tukey")
emm_depr_nb_SDMT_2y$contrasts

# HADS Total
emm_tot_nb_SDMT_2y <- emmeans(m_tot_nb_SDMT_2y, pairwise ~ SDMT_boxplotgroep_2y, adjust = "tukey")
emm_tot_nb_SDMT_2y$contrasts


# Print group size with NAs separately
table(merged_data_1F_2y$SDMT_boxplotgroep_2y, useNA = "always")

# Save using ggsave
# Boxplots with NAs shown separately
ggsave("figuren/boxplot_hads_angst_vs_SDMT2y_wNAs.png", plot = box_SDMT_2y, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_SDMT2y_wNAs.png", plot = box_SDMT_2y2, width = 6, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_SDMT2y_wNAs.png", plot = box_SDMT_2y3, width = 6, height = 4, dpi = 300)     


#### HADS scores vs PDDS progression 2 years after completing HADS  
# Check if there are NAs in PDDS_progression_2 --> no NA's
table(merged_data_1F_2y$prev_PDDS, useNA = "always")
table(merged_data_1F_2y$next_PDDS_2, useNA = "always")

### Supplementary 2H - HADS anxiety score vs PDDS progression after 2 years
box_PDDS_2y <- ggplot(merged_data_1F_2y, aes(x = factor(PDDS_progression_2), y = hads_angst, fill = factor(PDDS_progression_2))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"),
                    labels = c("Geen PDDS Progressie", "PDDS Progressie")) +
  labs(x = "PDDS Progressie (2 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Supplementary 2L - HADS depression score vs PDDS progression after 2 years
box_PDDS_2y2 <- ggplot(merged_data_1F_2y, aes(x = factor(PDDS_progression_2), y = hads_depressie, fill = factor(PDDS_progression_2))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"),
                    labels = c("Geen PDDS Progressie", "PDDS Progressie")) +
  labs(x = "PDDS Progressie (2 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Supplementary 2D - total HADS score vs PDDS progression after 2 years
box_PDDS_2y3 <- ggplot(merged_data_1F_2y, aes(x = factor(PDDS_progression_2), y = hads_totaal, fill = factor(PDDS_progression_2))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"),
                    labels = c("Geen PDDS Progressie", "PDDS Progressie")) +
  labs(x = "PDDS Progressie (2 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

#### statistics - HADS scores vs PDDS progression 2 years after completing HADS  
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_PDDS_2y <- glm.nb(hads_angst ~ factor(PDDS_progression_2), data = merged_data_1F_2y)
m_depr_nb_PDDS_2y  <- glm.nb(hads_depressie ~ factor(PDDS_progression_2), data = merged_data_1F_2y)
m_tot_nb_PDDS_2y   <- glm.nb(hads_totaal ~ factor(PDDS_progression_2), data = merged_data_1F_2y)

### 2. Check model assumptions
check_model(m_angst_nb_PDDS_2y)
check_model(m_depr_nb_PDDS_2y)
check_model(m_tot_nb_PDDS_2y)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb_PDDS_2y <- emmeans(m_angst_nb_PDDS_2y, pairwise ~ PDDS_progression_2, adjust = "tukey")
emm_angst_nb_PDDS_2y$contrasts

# HADS Depression
emm_depr_nb_PDDS_2y <- emmeans(m_depr_nb_PDDS_2y, pairwise ~ PDDS_progression_2, adjust = "tukey")
emm_depr_nb_PDDS_2y$contrasts

# HADS Total
emm_tot_nb_PDDS_2y <- emmeans(m_tot_nb_PDDS_2y, pairwise ~ PDDS_progression_2, adjust = "tukey")
emm_tot_nb_PDDS_2y$contrasts


# Print group size 
merged_data_1F_2y %>%
  count(PDDS_progression_2, sort = TRUE, name = "Aantal") %>%
  mutate(
    PDDS_progression_2 = as.character(PDDS_progression_2),
    PDDS_progression_2 = ifelse(is.na(PDDS_progression_2), "NA", PDDS_progression_2)
  ) %>%
  rename(Groep = PDDS_progression_2) %>%
  print()

# Save using ggsave
ggsave("figuren/boxplot_hads_angst_vs_PDDS2y_wNAs.png", plot = box_PDDS_2y, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_PDDS2y_wNAs.png", plot = box_PDDS_2y2, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_PDDS2y_wNAs.png", plot = box_PDDS_2y3, width = 5, height = 4, dpi = 300)     


#### HADS scores vs EDSS or T25FW or AMSQ progression 2 years after completing HADS (NAs = not progressive)
### Figure 2O - HADS anxiety vs EDSS or T25FW or AMSQ progression 2 years after completing HADS 
boxedss_T25FW_AMSQ1_2y <- ggplot(merged_data_1F_2y, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_2y), y = hads_angst, fill = factor(progression_edss_or_T25FW_or_AMSQ_2y))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie")) +
  labs(x = "Progressie (EDSS of T25FW of AMSQ, 2 jaar)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Figure 2U - HADS depression vs EDSS or T25FW or AMSQ progression 2 years after completing HADS 
boxedss_T25FW_AMSQ2_2y <- ggplot(merged_data_1F_2y, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_2y), y = hads_depressie, fill = factor(progression_edss_or_T25FW_or_AMSQ_2y))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie")) +
  labs(x = "Progressie (EDSS of T25FW of AMSQ, 2 jaar)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Figure 2I - HADS total vs EDSS or T25FW or AMSQ progression 2 years after completing HADS 
boxedss_T25FW_AMSQ3_2y <- ggplot(merged_data_1F_2y, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_2y), y = hads_totaal, fill = factor(progression_edss_or_T25FW_or_AMSQ_2y))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    labels = c("Geen Progressie", "Progressie")) +
  labs(x = "Progressie (EDSS of T25FW of AMSQ, 2 jaar)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

#### statistics - HADS scores vs EDSS or T25FW or AMSQ progression 2 years after completing HADS 
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_3varcomp_2y <- glm.nb(hads_angst ~ factor(progression_edss_or_T25FW_or_AMSQ_2y), data = merged_data_1F_2y)
m_depr_nb_3varcomp_2y  <- glm.nb(hads_depressie ~ factor(progression_edss_or_T25FW_or_AMSQ_2y), data = merged_data_1F_2y)
m_tot_nb_3varcomp_2y   <- glm.nb(hads_totaal ~ factor(progression_edss_or_T25FW_or_AMSQ_2y), data = merged_data_1F_2y)

### 2. Check model assumptions
check_model(m_angst_nb_3varcomp_2y)
check_model(m_depr_nb_3varcomp_2y)
check_model(m_tot_nb_3varcomp_2y)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb_3varcomp_2y <- emmeans(m_angst_nb_3varcomp_2y, pairwise ~ progression_edss_or_T25FW_or_AMSQ_2y, adjust = "tukey")
emm_angst_nb_3varcomp_2y$contrasts

# HADS Depression
emm_depr_nb_3varcomp_2y <- emmeans(m_depr_nb_3varcomp_2y, pairwise ~ progression_edss_or_T25FW_or_AMSQ_2y, adjust = "tukey")
emm_depr_nb_3varcomp_2y$contrasts

# HADS Total
emm_tot_nb_3varcomp_2y <- emmeans(m_tot_nb_3varcomp_2y, pairwise ~ progression_edss_or_T25FW_or_AMSQ_2y, adjust = "tukey")
emm_tot_nb_3varcomp_2y$contrasts


# Print group size
table(merged_data_1F_2y$progression_edss_or_T25FW_or_AMSQ_2y)

merged_data_1F_2y %>%
  count(progression_edss_or_T25FW_or_AMSQ_2y, sort = TRUE, name = "Aantal") %>%
  rename(Groep = progression_edss_or_T25FW_or_AMSQ_2y) %>%
  print()

# Save using ggsave
ggsave("figuren/boxplot_hads_angst_vs_edss_T25FW_AMSQ_2y.png", plot = boxedss_T25FW_AMSQ1_2y, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_edss_T25FW_AMSQ_2y.png", plot = boxedss_T25FW_AMSQ2_2y, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_edss_T25FW_AMSQ_2y.png", plot = boxedss_T25FW_AMSQ3_2y, width = 5, height = 4, dpi = 300)      

#### HADS scores vs progression EDSS or T25FW or AMSQ or SDMT or PDDS 2 years after completing HADS (NAs = not progressive)

### Figure 2P - Boxplot HADS anxiety
boxedss_T25FW_AMSQ_SDMT_PDDS1_2y <- ggplot(merged_data_1F_2y, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y), y = hads_angst, fill = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"),
                    labels = c("Geen Progressie", "Progressie (5vars)")) +
  labs(x = "Progressie (5vars)", y = "HADS Angst Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Figure 2V - Boxplot HADS depression
boxedss_T25FW_AMSQ_SDMT_PDDS2_2y <- ggplot(merged_data_1F_2y, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y), y = hads_depressie, fill = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"),
                    labels = c("Geen Progressie", "Progressie (5vars)")) +
  labs(x = "Progressie (5vars)", y = "HADS Depressie Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

### Figure 2J - Boxplot HADS total
boxedss_T25FW_AMSQ_SDMT_PDDS3_2y <- ggplot(merged_data_1F_2y, aes(x = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y), y = hads_totaal, fill = factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0072B2", "#E69F00"),
                    labels = c("Geen Progressie", "Progressie (5vars)")) +
  labs(x = "Progressie (5vars)", y = "HADS Totaal Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

#### statistics - HADS scores vs progression EDSS or T25FW or AMSQ or SDMT or PDDS 2 years after completing HADS 
# Determine p-values of significance of differences in HADS scores using post-hoc test on HADS in LINEAIR regression model (glm nb)
### 1. Fit Negative Binomial GLM
m_angst_nb_5varcomp_2y <- glm.nb(hads_angst ~ factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y), data = merged_data_1F_2y)
m_depr_nb_5varcomp_2y  <- glm.nb(hads_depressie ~ factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y), data = merged_data_1F_2y)
m_tot_nb_5varcomp_2y   <- glm.nb(hads_totaal ~ factor(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y), data = merged_data_1F_2y)

### 2. Check model assumptions
check_model(m_angst_nb_5varcomp_2y)
check_model(m_depr_nb_5varcomp_2y)
check_model(m_tot_nb_5varcomp_2y)

### 3. Post-hoc test using emmeans (pairwise, Tukey correction)
# HADS Anxiety
emm_angst_nb_5varcomp_2y <- emmeans(m_angst_nb_5varcomp_2y, pairwise ~ progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y, adjust = "tukey")
emm_angst_nb_5varcomp_2y$contrasts

# HADS Depression
emm_depr_nb_5varcomp_2y <- emmeans(m_depr_nb_5varcomp_2y, pairwise ~ progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y, adjust = "tukey")
emm_depr_nb_5varcomp_2y$contrasts

# HADS Total
emm_tot_nb_5varcomp_2y <- emmeans(m_tot_nb_5varcomp_2y, pairwise ~ progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y, adjust = "tukey")
emm_tot_nb_5varcomp_2y$contrasts

# Print group size
table(merged_data_1F_2y$progression_edss_or_T25FW_or_SDMT_or_PDDS_2y)

# Save using ggsave
ggsave("figuren/boxplot_hads_angst_vs_edss_T25FW_AMSQ_SDMT_PDDS_2y.png", plot = boxedss_T25FW_AMSQ_SDMT_PDDS1_2y, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_depressie_vs_edss_T25FW_AMSQ_SDMT_PDDS_2y.png", plot = boxedss_T25FW_AMSQ_SDMT_PDDS2_2y, width = 5, height = 4, dpi = 300)
ggsave("figuren/boxplot_hads_totaal_vs_edss_T25FW_AMSQ_SDMT_PDDS_2y.png", plot = boxedss_T25FW_AMSQ_SDMT_PDDS3_2y, width = 5, height = 4, dpi = 300)      


### Convert progression variables to 1 and 0 instead of TRUE and FALSE ###
merged_data_1F <- merged_data_1F %>%
  mutate(
    edss_progression = as.integer(edss_progression),
    T25FW_progression = as.integer(T25FW_progression),
    progression_AMSQ = as.integer(progression_AMSQ),
    SDMT_progression = as.integer(SDMT_progression),
    PDDS_progression = as.integer(PDDS_progression),
    progression_edss_or_T25FW_or_AMSQ = as.integer(progression_edss_or_T25FW_or_AMSQ),
    progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS = as.integer(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS)
  )

merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(
    edss_progression_2 = as.integer(edss_progression_2),
    T25FW_progression_2 = as.integer(T25FW_progression_2),
    progression_AMSQ_2 = as.integer(progression_AMSQ_2),
    SDMT_progression_2 = as.integer(SDMT_progression_2),
    PDDS_progression_2 = as.integer(PDDS_progression_2),
    progression_edss_or_T25FW_or_AMSQ_2y = as.integer(progression_edss_or_T25FW_or_AMSQ_2y),
    progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y = as.integer(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y)
  )

# Add 2y-progression variables to merged_data_1F based on Participant Id
merged_data_1F <- merged_data_1F %>%
  left_join(
    merged_data_1F_2y %>%
      select(`Participant Id`, 
             progression_edss_or_T25FW_or_AMSQ_2y, 
             progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y),
    by = "Participant Id"
  )

# The variabeles 'progression_edss_or_T25FW_or_AMSQ_2y' and 'progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y' are now duplicated in merged_data_1F, so remove them and delete '.x' and '.y' 
merged_data_1F <- merged_data_1F %>%
  mutate(
    progression_edss_or_T25FW_or_AMSQ_2y = progression_edss_or_T25FW_or_AMSQ_2y.x,
    progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y = progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y.x
  ) %>%
  select(
    -progression_edss_or_T25FW_or_AMSQ_2y.x,
    -progression_edss_or_T25FW_or_AMSQ_2y.y,
    -progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y.x,
    -progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y.y
  )


##### Figure 2 - Venndiagrams of overlaps between different progression measurements #####

###### 1. year 1 visualization ######
# Make sets per progression outcome (only use TRUE values)
venn_list_1y <- list(
  EDSS = merged_data_1F %>% filter(edss_progression == 1) %>% pull(`Participant Id`),
  T25FW = merged_data_1F %>% filter(T25FW_progression == 1) %>% pull(`Participant Id`),
  SDMT = merged_data_1F %>% filter(SDMT_progression == 1) %>% pull(`Participant Id`),
  AMSQ = merged_data_1F %>% filter(progression_AMSQ == 1) %>% pull(`Participant Id`),
  PDDS = merged_data_1F %>% filter(PDDS_progression == 1) %>% pull(`Participant Id`)
)

# Show number of people with progression per criterion
sapply(venn_list_1y, length)

### Figure 2B - Plot Venndiagram
venndiagram1y5var <- ggVennDiagram(
  venn_list_1y, 
  label = "none"   
) +
  scale_fill_gradient(
    low = "#f2f2f2", 
    high = "#3a5f4b",
    breaks = scales::pretty_breaks(),         
    labels = scales::label_number(accuracy=1) 
  ) +
  labs(
    title = "Overlap tussen progressiecriteria na 1 jaar",
    subtitle = "EDSS, T25FW, SDMT, PDDS en AMSQ",
    fill = "Aantal mensen met progressie"  
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12)) +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

venndiagram1y5var

# Make a new list of only EDSS, T25FW and AMSQ
venn_list_3vars <- venn_list_1y[c("EDSS", "T25FW", "AMSQ")]

# Show number of people with progression per criterion
sapply(venn_list_3vars, length)

### Figure 2A - Plot  Venndiagram using only these 3 variables
venndiagram1y3var <- ggVennDiagram(
  venn_list_3vars, 
  label = "none"   
) +
  scale_fill_gradient(
    low = "#f2f2f2", 
    high = "#3a5f4b",
    breaks = scales::pretty_breaks(),         
    labels = scales::label_number(accuracy=1) 
  ) +
  labs(
    title = "Overlap tussen progressiecriteria na 1 jaar",
    subtitle = "EDSS, T25FW en AMSQ",
    fill = "Aantal mensen met progressie"   
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12)) +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

venndiagram1y3var

###### 2. year 2 visualization ######
# Make sets per progression outcome (only use TRUE values)
venn_list_2y <- list(
  EDSS = merged_data_1F_2y %>% filter(edss_progression_2 == 1) %>% pull(`Participant Id`),
  T25FW = merged_data_1F_2y %>% filter(T25FW_progression_2 == 1) %>% pull(`Participant Id`),
  SDMT = merged_data_1F_2y %>% filter(SDMT_progression_2 == 1) %>% pull(`Participant Id`),
  AMSQ = merged_data_1F_2y %>% filter(progression_AMSQ_2 == 1) %>% pull(`Participant Id`),
  PDDS = merged_data_1F_2y %>% filter(PDDS_progression_2 == 1) %>% pull(`Participant Id`)
)

# Show number of people with progression per criterion
sapply(venn_list_2y, length)

### Figure 2D - Plot Venndiagram
venndiagram2y5var <- ggVennDiagram(
  venn_list_2y, 
  label = "none"   
) +
  scale_fill_gradient(
    low = "#f2f2f2", 
    high = "#3a5f4b",
    breaks = scales::pretty_breaks(),         
    labels = scales::label_number(accuracy=1) 
  ) +
  labs(
    title = "Overlap tussen progressiecriteria na 2 jaar",
    subtitle = "EDSS, T25FW, SDMT, PDDS en AMSQ",
    fill = "Aantal mensen met progressie"     
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12)) +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

venndiagram2y5var


# Make a new list using only EDSS, T25FW and AMSQ
venn_list_3vars_2y <- venn_list_2y[c("EDSS", "T25FW", "AMSQ")]

### Figure 2C - Venndiagram with only these 3 variables
venndiagram2y3var <- ggVennDiagram(
  venn_list_3vars_2y, 
  label = "none"   
) +
  scale_fill_gradient(
    low = "#f2f2f2", 
    high = "#3a5f4b",
    breaks = scales::pretty_breaks(),         
    labels = scales::label_number(accuracy=1) 
  ) +
  labs(
    title = "Overlap tussen progressiecriteria na 2 jaar",
    subtitle = "EDSS, T25FW en AMSQ",
    fill = "Aantal mensen met progressie"     
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12)) +
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

venndiagram2y3var

# Save all venndiagrams using ggsave
ggsave("figuren/venndiagram1y5var_19092025_nolabels.png", plot = venndiagram1y5var, width = 8, height = 8, dpi = 300)
ggsave("figuren/venndiagram1y3var_19092025_nolabels.png", plot = venndiagram1y3var, width = 8, height = 8, dpi = 300)
ggsave("figuren/venndiagram2y5var_19092025_nolabels.png", plot = venndiagram2y5var, width = 8, height = 8, dpi = 300)  
ggsave("figuren/venndiagram2y3var_19092025_nolabels.png", plot = venndiagram2y3var, width = 8, height = 8, dpi = 300) 

##### Extra tables with counts and percentages 
make_venn_table <- function(venn_list){
  all_ids <- unique(unlist(venn_list))
  df <- data.frame(Participant = all_ids)
  
  for(name in names(venn_list)){
    df[[name]] <- df$Participant %in% venn_list[[name]]
  }
  
  df %>%
    group_by(across(all_of(names(venn_list)))) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = round(count / length(all_ids) * 100, 1))
}

# 1 year
venn_table_1y3 <- make_venn_table(venn_list_3vars)
venn_table_1y5 <- make_venn_table(venn_list_1y)

# 2 years
venn_table_2y3 <- make_venn_table(venn_list_3vars_2y)
venn_table_2y5 <- make_venn_table(venn_list_2y)

# View results
venn_table_1y3
venn_table_1y5
venn_table_2y3
venn_table_2y5

##### Figure 3. LOGISTIC REGRESSION ANALYSIS and visualization #####      

###### 1. year 1 visualization and statistics ######

# Convert prev_edss to an ordinal factor with specified levels
merged_data_1F$prev_edss <- factor(merged_data_1F$prev_edss,
                                   levels = c( 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,
                                               5.5, 6.0, 6.5, 7.0, 7.5, 8.0),
                                   ordered = TRUE)

# Create logistic regression models
model1F1a <- glm(edss_progression ~ log10(hads_angst + 0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())
model1F1b <- glm(edss_progression ~ log10(hads_depressie + 0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())
model1F1c <- glm(edss_progression ~ log10(hads_totaal+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())

model1F2a <- glm(progression_edss_or_T25FW_or_AMSQ ~ log10(hads_angst+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())
model1F2b <- glm(progression_edss_or_T25FW_or_AMSQ ~ log10(hads_depressie+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())
model1F2c <- glm(progression_edss_or_T25FW_or_AMSQ ~ log10(hads_totaal+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())

model1F3a <- glm(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS ~ log10(hads_angst+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())
model1F3b <- glm(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS ~ log10(hads_depressie+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())
model1F3c <- glm(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS ~ log10(hads_totaal+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F, family = binomial())

# Check model assumptions and compare models
AIC(model1F3a, model1F3b, model1F3c)
anova(model1F1, model1F2, model1F3, model1F4)
vif(model1F4)
summary(model1F3c)

checkresiduals(model1F3c)
residualPlots(model1F3c)

simulationoutput = simulateResiduals(fittedModel = model1F3c)
plot(simulationoutput)

##### Create forest plot with OR's and 95% CI's 

##### Forest plot multivariabel logistische regressiemodellen van log10 hads + 0.1 modellen 1y 

# Function to extract OR + 95% CI from a model
extract_or <- function(model, variable_name, hads_type, progression_type) {
  coef_summary <- summary(model)$coefficients
  logOR <- coef_summary[variable_name, "Estimate"]
  SE <- coef_summary[variable_name, "Std. Error"]
  p_value <- coef_summary[variable_name, "Pr(>|z|)"]  # haal p-waarde op
  
  OR <- exp(logOR)
  CI_lower <- exp(logOR - 1.96 * SE)
  CI_upper <- exp(logOR + 1.96 * SE)
  
  data.frame(
    HADS = hads_type,
    Progression = progression_type,
    OR = OR,
    CI_lower = CI_lower,
    CI_upper = CI_upper,
    p_value = p_value  
  )
}

# Create data frame with ORs from all models
df_forest <- bind_rows(
  # HADS Total
  extract_or(model1F1c, "log10(hads_totaal + 0.1)", "HADS Total", "EDSS progression"),
  extract_or(model1F2c, "log10(hads_totaal + 0.1)", "HADS Total", "EDSS or T25FW or AMSQ progression"),
  extract_or(model1F3c, "log10(hads_totaal + 0.1)", "HADS Total", "EDSS or T25FW or AMSQ or SDMT or PDDS progression"),
  # HADS Anxiety
  extract_or(model1F1a, "log10(hads_angst + 0.1)", "HADS Anxiety", "EDSS progression"),
  extract_or(model1F2a, "log10(hads_angst + 0.1)", "HADS Anxiety", "EDSS or T25FW or AMSQ progression"),
  extract_or(model1F3a, "log10(hads_angst + 0.1)", "HADS Anxiety", "EDSS or T25FW or AMSQ or SDMT or PDDS progression"),
  
  # HADS Depression
  extract_or(model1F1b, "log10(hads_depressie + 0.1)", "HADS Depression", "EDSS progression"),
  extract_or(model1F2b, "log10(hads_depressie + 0.1)", "HADS Depression", "EDSS or T25FW or AMSQ progression"),
  extract_or(model1F3b, "log10(hads_depressie + 0.1)", "HADS Depression", "EDSS or T25FW or AMSQ or SDMT or PDDS progression")
  
)

# Print p-values separately
df_forest %>%
  dplyr::select(HADS, Progression, OR, CI_lower, CI_upper, p_value) %>%
  mutate(
    OR        = round(OR, 2),
    CI_lower  = round(CI_lower, 2),
    CI_upper  = round(CI_upper, 2),
    p_value   = round(p_value, 2)   # altijd 2 decimalen
  )

df_export <- df_forest %>%
  dplyr::select(HADS, Progression, OR, CI_lower, CI_upper, p_value) %>%
  mutate(
    OR        = round(OR, 2),
    CI_lower  = round(CI_lower, 2),
    CI_upper  = round(CI_upper, 2),
    p_value   = round(p_value, 2)
  )

kable(df_export, format = "simple") 

# Put factor order of HADS right
df_forest$HADS <- factor(df_forest$HADS, levels = c("HADS Depression", "HADS Anxiety", "HADS Total"))

# And of progression outcomes
df_forest$Progression <- factor(df_forest$Progression, levels = rev(c(
  "EDSS progression",
  "EDSS or T25FW or AMSQ progression",
  "EDSS or T25FW or AMSQ or SDMT or PDDS progression"
)))

# Add column for right labels 
df_forest <- df_forest %>%
  mutate(label = sprintf("OR %.2f [%.2f–%.2f]", OR, CI_lower, CI_upper))

### Figure 3A - Plot
forestplot1y <- ggplot(df_forest, aes(x = Progression, y = OR, color = HADS)) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                position = position_dodge(width = 0.6), width = 0.2) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  #geom_text(aes(y = CI_upper, label = label),  
  #        position = position_dodge(width = 0.6),
  #        hjust = -0.1, size = 3.5, show.legend = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30") +
  coord_flip(clip = "off") +
  ylab("Odds Ratio (95% CI)") +
  xlab("") +
  ggtitle("Forest plot of log10(HADS) scales per progression outcome") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.margin = margin(5.5, 110, 5.5, 5.5),
        axis.text.y = element_blank()) +
  scale_color_manual(values = c(
    "HADS Total" = "#008080",    
    "HADS Anxiety" = "#6a3d9a",  
    "HADS Depression" = "#5ab4d0" 
  ))

forestplot1y

# Save using ggsave
ggsave("figuren/forestplot1yv3_19092025.png", plot = forestplot1y, width = 5, height = 8, dpi = 900)

###### 2. year 2 visualization and statistics ######
# Convert prev_edss to an ordinal factor with specified levels
merged_data_1F_2y$prev_edss <- factor(merged_data_1F_2y$prev_edss,
                                      levels = c( 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,
                                                  5.5, 6.0, 6.5, 7.0, 7.5, 8.0),
                                      ordered = TRUE)

# Create logistic regression models
model1F5a <- glm(edss_progression_2 ~ log10(hads_angst+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())
model1F5b <- glm(edss_progression_2 ~ log10(hads_depressie+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())
model1F5c <- glm(edss_progression_2 ~ log10(hads_totaal+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())

model1F6a <- glm(progression_edss_or_T25FW_or_AMSQ_2y ~ log10(hads_angst+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())
model1F6b <- glm(progression_edss_or_T25FW_or_AMSQ_2y ~ log10(hads_depressie+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())
model1F6c <- glm(progression_edss_or_T25FW_or_AMSQ_2y ~ log10(hads_totaal+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())

model1F7a <- glm(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y ~ log10(hads_angst+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())
model1F7b <- glm(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y ~ log10(hads_depressie+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())
model1F7c <- glm(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y ~ log10(hads_totaal+0.1) + offset(log10(age_at_prev_visit)), 
                 data = merged_data_1F_2y, family = binomial())

##### Create forest plot with OR's and 95% CI's

##### Forest plot multivariabel logistische regressiemodellen van log10 hads + 0.1 modellen 2y

# Function to extract OR + 95% CI from a model
extract_or <- function(model, variable_name, hads_type, progression_type) {
  coef_summary <- summary(model)$coefficients
  logOR <- coef_summary[variable_name, "Estimate"]
  SE <- coef_summary[variable_name, "Std. Error"]
  p_value <- coef_summary[variable_name, "Pr(>|z|)"]
  
  OR <- exp(logOR)
  CI_lower <- exp(logOR - 1.96 * SE)
  CI_upper <- exp(logOR + 1.96 * SE)
  
  data.frame(
    HADS = hads_type,
    Progression = progression_type,
    OR = OR,
    CI_lower = CI_lower,
    CI_upper = CI_upper,
    p_value = p_value
  )
}

# Make dataframe with ORs from all 9 models predicting progression at 2 years
df_forest_2y <- bind_rows(
  # HADS Total
  extract_or(model1F5c, "log10(hads_totaal + 0.1)", "HADS Total", "EDSS progression (2y)"),
  extract_or(model1F6c, "log10(hads_totaal + 0.1)", "HADS Total", "EDSS or T25FW or AMSQ progression (2y)"),
  extract_or(model1F7c, "log10(hads_totaal + 0.1)", "HADS Total", "EDSS or T25FW or AMSQ or SDMT or PDDS progression (2y)"),
  
  # HADS Anxiety
  extract_or(model1F5a, "log10(hads_angst + 0.1)", "HADS Anxiety", "EDSS progression (2y)"),
  extract_or(model1F6a, "log10(hads_angst + 0.1)", "HADS Anxiety", "EDSS or T25FW or AMSQ progression (2y)"),
  extract_or(model1F7a, "log10(hads_angst + 0.1)", "HADS Anxiety", "EDSS or T25FW or AMSQ or SDMT or PDDS progression (2y)"),
  
  # HADS Depression
  extract_or(model1F5b, "log10(hads_depressie + 0.1)", "HADS Depression", "EDSS progression (2y)"),
  extract_or(model1F6b, "log10(hads_depressie + 0.1)", "HADS Depression", "EDSS or T25FW or AMSQ progression (2y)"),
  extract_or(model1F7b, "log10(hads_depressie + 0.1)", "HADS Depression", "EDSS or T25FW or AMSQ or SDMT or PDDS progression (2y)")
  
)

# Print p-values separately
df_forest_2y %>%
  select(HADS, Progression, OR, CI_lower, CI_upper, p_value) %>%
  mutate(p_value = signif(p_value, 3)) %>%
  print()

# Put factor order of HADS right
df_forest_2y$HADS <- factor(df_forest_2y$HADS, levels = c("HADS Depression", "HADS Anxiety", "HADS Total"))


# Put factor order of progression outcomes right 
df_forest_2y$Progression <- factor(df_forest_2y$Progression, levels = rev(c(
  "EDSS progression (2y)",
  "EDSS or T25FW or AMSQ progression (2y)",
  "EDSS or T25FW or AMSQ or SDMT or PDDS progression (2y)"
)))

# Add column for right labels
df_forest_2y <- df_forest_2y %>%
  mutate(label = sprintf("OR %.2f [%.2f–%.2f]", OR, CI_lower, CI_upper))

### Figure 3B - Plot
forestplot2y <- ggplot(df_forest_2y, aes(x = Progression, y = OR, color = HADS)) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                position = position_dodge(width = 0.6), width = 0.2) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  # geom_text(aes(y = CI_upper, label = label),  
  # position = position_dodge(width = 0.6),
  #  hjust = -0.1, size = 3.5, show.legend = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30") +
  coord_flip(clip = "off") +
  ylab("Odds Ratio (95% CI)") +
  xlab("") +
  ggtitle("Forest plot of log10(HADS) scales per progression outcome (2 years)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.margin = margin(5.5, 110, 5.5, 5.5),
        axis.text.y = element_blank()) +
  scale_color_manual(values = c(
    "HADS Total" = "#008080",    
    "HADS Anxiety" = "#6a3d9a",  
    "HADS Depression" = "#5ab4d0" 
  ))

forestplot2y

# Save using ggsave
ggsave("figuren/forestplot2yv2.png", plot = forestplot2y, width = 5, height = 8, dpi = 900)


#### EXPORT ENVIRONMENT ####
# remove everything from environment at the end
rm(list=ls())

# save session info
sessionInfo()

