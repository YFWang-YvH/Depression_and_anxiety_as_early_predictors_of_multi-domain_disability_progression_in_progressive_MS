#Date: 29-11-2024
#Author: Romy Klein Kranenbarg
#Description: 
# this script is for analysis as part of the manuscript 'Symptoms of depression and anxiety are early predictors of multi-domain disability progression in progressive MS'
# in this script, we load and transform all data for analysis

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

#### LOADING AND TRANSFORMING SPIN-P DATA FOR ANALYSIS ####
##### 1. Read data and Filter out test patients and adjust unclear column names #####
data <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Study results", na = "NULL")

data_clean <- data %>% 
  filter(`Site Abbreviation` != "TES")

colnames(data_clean)[colnames(data_clean) == "datumpoliFU1_1"] <- "datumpoliFU2"
colnames(data_clean)[colnames(data_clean) == "datumpoliFU1_1_1"] <- "datumpoliFU3"
colnames(data_clean)[colnames(data_clean) == "datumpoliFU1_1_1_1"] <- "datumpoliFU4"
colnames(data_clean)[colnames(data_clean) == "datumpoliFU1_1_1_1_1"] <- "datumpoliFU5"
colnames(data_clean)[colnames(data_clean) == "beloopFU1_1"] <- "beloopFU2"
colnames(data_clean)[colnames(data_clean) == "beloopFU1_1_1"] <- "beloopFU3"
colnames(data_clean)[colnames(data_clean) == "beloopFU1_1_1_1"] <- "beloopFU4"
colnames(data_clean)[colnames(data_clean) == "beloopFU1_1_1_1_1"] <- "beloopFU5"

##### 2. Extract numerical values of PDDS from text labels and set them as ordinal factors #####

## Define the columns of PDDS 
pdds_columns <- c("PDDS", "PDDS_FU1", "PDDS_FU2", "PDDS_FU3", "PDDS_FU4", "PDDS_FU5")

## Transform all PDDS columns 
data_transformed <- data_clean %>%
  mutate(across(all_of(pdds_columns), ~ {
    # Extract numerical values from text
    numeric_values <- as.numeric(str_extract(., "^[0-8]"))
    # Convert to ordinal factor
    factor(numeric_values, levels = 0:8, ordered = TRUE)
  }))

##### 3. Assign other appropriate data types to relevant variables 'Study results' #####

## Categorical variables as factor 

# Define all categorical variables in a vector 
categorische_variabelen <- c(
  "geslacht", "immuunMSmedicatie")

# Apply 'as.factor' to all categorical variables 
data_transformed <- data_transformed %>%
  mutate(across(all_of(categorische_variabelen), as.factor))

## Ordinal variables as ordened factors 
factor_levels = c(0,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10)
data_transformed$EDSS <- factor(data_transformed$EDSS, levels = factor_levels, ordered = TRUE)
data_transformed$EDSS_FU1 <- factor(data_transformed$EDSS_FU1, levels = factor_levels, ordered = TRUE)
data_transformed$EDSS_FU2 <- factor(data_transformed$EDSS_FU2, levels = factor_levels, ordered = TRUE)
data_transformed$EDSS_FU3 <- factor(data_transformed$EDSS_FU3, levels = factor_levels, ordered = TRUE)
data_transformed$EDSS_FU4 <- factor(data_transformed$EDSS_FU4, levels = factor_levels, ordered = TRUE)
data_transformed$EDSS_FU5 <- factor(data_transformed$EDSS_FU5, levels = factor_levels, ordered = TRUE)

## Dates as date
# Convert to dates and convert any invalid dates to NA
data_transformed$geboortedatum <- as.Date(data_transformed$geboortedatum, format = "%d-%m-%Y")
data_transformed$datumontstaan <- as.Date(data_transformed$datumontstaan, format = "%d-%m-%Y")
data_transformed$datumbaselinevisit <- as.Date(data_transformed$datumbaselinevisit, format = "%d-%m-%Y")
data_transformed$datumpoliFU1 <- as.Date(data_transformed$datumpoliFU1, format = "%d-%m-%Y")
data_transformed$datumpoliFU2 <- as.Date(data_transformed$datumpoliFU2, format = "%d-%m-%Y")
data_transformed$datumpoliFU3 <- as.Date(data_transformed$datumpoliFU3, format = "%d-%m-%Y")
data_transformed$datumpoliFU4 <- as.Date(data_transformed$datumpoliFU4, format = "%d-%m-%Y")
data_transformed$datumpoliFU5 <- as.Date(data_transformed$datumpoliFU5, format = "%d-%m-%Y")

## Numerical variables as numerical
data_transformed$gemT25FW <- as.numeric(data_transformed$gemT25FW)
data_transformed$gemT25FW_FU1 <- as.numeric(data_transformed$gemT25FW_FU1)
data_transformed$gemT25FW_FU2 <- as.numeric(data_transformed$gemT25FW_FU2)
data_transformed$gemT25FW_FU3 <- as.numeric(data_transformed$gemT25FW_FU3)
data_transformed$gemT25FW_FU4 <- as.numeric(data_transformed$gemT25FW_FU4)
data_transformed$gemT25FW_FU5 <- as.numeric(data_transformed$gemT25FW_FU5)
data_transformed$SDMT <- as.numeric(data_transformed$SDMT)
data_transformed$SDMT_FU1 <- as.numeric(data_transformed$SDMT_FU1)
data_transformed$SDMT_FU2 <- as.numeric(data_transformed$SDMT_FU2)
data_transformed$SDMT_FU3 <- as.numeric(data_transformed$SDMT_FU3)
data_transformed$SDMT_FU4 <- as.numeric(data_transformed$SDMT_FU4)
data_transformed$SDMT_FU5 <- as.numeric(data_transformed$SDMT_FU5)

##### 4. Create filtered table with relevant variables "study results"

## Create a new table containing only the relevant parameters
data_transformed_filtered <- data_transformed %>%
  select(
    "Participant Id", "geslacht", "geboortedatum", "datumontstaan", "datumbaselinevisit", "datumpoliFU1", "datumpoliFU2", "datumpoliFU3", "datumpoliFU4", "datumpoliFU5", 
    "EDSS", "EDSS_FU1", "EDSS_FU2", "EDSS_FU3", "EDSS_FU4", "EDSS_FU5",  
    "PDDS", "PDDS_FU1", "PDDS_FU2", "PDDS_FU3", "PDDS_FU4", "PDDS_FU5",  
    "SDMT", "SDMT_FU1", "SDMT_FU2", "SDMT_FU3", "SDMT_FU4", "SDMT_FU5",  
    "gemT25FW", "gemT25FW_FU1", "gemT25FW_FU2", "gemT25FW_FU3", "gemT25FW_FU4", "gemT25FW_FU5", 
    "immuunMSmedicatie", "specmedicatiehuidig", "immuunMSmedicatie_FU1", 
    "specmedicatiehuidig_FU1", "stopimmuunMSmedicatie_FU1", "immuunMSmedicatie_FU2", 
    "specmedicatiehuidig_FU2", "stopimmuunMSmedicatie_FU2", "immuunMSmedicatie_FU3", 
    "specmedicatiehuidig_FU3", "stopimmuunMSmedicatie_FU3","immuunMSmedicatie_FU4", 
    "specmedicatiehuidig_FU4", "stopimmuunMSmedicatie_FU4", "immuunMSmedicatie_FU5", 
    "specmedicatiehuidig_FU5", "stopimmuunMSmedicatie_FU5"
  )

##### 4A. Add new variables after new Castor export after adding psychotropic drugs and disease duration #####
# 4A1. Load new export
new_export <- read_excel("SPIN-P_excel_export_20250728031807.xlsx", sheet = "Study results", na = "NULL")

# 4A2. Define vector with variables to be added + variables to be replaced + Participant ID
nieuwe_variabelen <- c(
  "Participant Id", "datumontstaan",
  "psychofarmaca_BL", "antidepressiva_BL", "anxiolytica_BL",
  paste0("psychofarmaca_FU", 1:5),
  paste0("antidepressiva_FU", 1:5),
  paste0("anxiolytica_FU", 1:5)
)

# 4A3. Select only these columns from the new export
new_subset <- new_export[, nieuwe_variabelen]

# 4A4. Remove existing versions of these columns from the old dataset (except Participant Id)
data_transformed_filtered <- data_transformed_filtered %>%
  select(-any_of(setdiff(nieuwe_variabelen, "Participant Id")))

# 4A5. Merge everything by Participant ID 
data_transformed_filtered <- data_transformed_filtered %>%
  left_join(new_subset, by = "Participant Id") 

## following patients criteria were applied: PIF signed, PPMS diagnosis, had BL visit, correct information, incorrect/missing values are replaced with correct values or NA
## This process of code is deducted here to hide patient id due to privacy reason

#### LOADING AND TRANSFORMING/CONVERTING HADS DATA FOR ANALYSIS ####

##### 1. Load HADS data and interpret NA as NULL #####
dataHADSBL <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Angst_en_stemming_BL", na = "NULL")
dataHADSFU1 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Angst_en_stemming_FU1", na = "NULL")
dataHADSFU2 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Angst_en_stemming_FU2", na = "NULL")
dataHADSFU3 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Angst_en_stemming_FU3", na = "NULL")
dataHADSFU4 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Angst_en_stemming_FU4", na = "NULL")
dataHADSFU5 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Angst_en_stemming_FU5", na = "NULL")

##### 2. Filter out 
## following patients criteria were applied: PIF signed, PPMS diagnosis, had BL visit, correct information, incorrect/missing values are replaced with correct values or NA
## This process of code is deducted here to hide patient id due to privacy reason

##### 3. Filter out columns with 'Survey Instance Id', 'Survey Package Id' en 'Castor Participant Status' #####
dataHADSBL_analysis <- dataHADSBL %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataHADSFU1_analysis <- dataHADSFU1 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataHADSFU2_analysis <- dataHADSFU2 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataHADSFU3_analysis <- dataHADSFU3 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataHADSFU4_analysis <- dataHADSFU4 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataHADSFU5_analysis <- dataHADSFU5 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)

##### 4. Assigning correct data types to relevant variables HADS data #####

## Dates as dates (ignore time)
dataHADSBL_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataHADSBL_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataHADSBL_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataHADSBL_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataHADSFU1_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataHADSFU1_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataHADSFU1_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataHADSFU1_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataHADSFU2_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataHADSFU2_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataHADSFU2_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataHADSFU2_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataHADSFU3_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataHADSFU3_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataHADSFU3_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataHADSFU3_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataHADSFU4_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataHADSFU4_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataHADSFU4_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataHADSFU4_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataHADSFU5_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataHADSFU5_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataHADSFU5_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataHADSFU5_analysis$`Survey Sent Date`), format = "%d-%m-%Y")

##### 5. Converting HADS responses to scores #####
score_types = list(
  type_1_scores = c("Meestal" = 3, "Vaak" = 2, "Af en toe, soms" = 1, "Helemaal niet" = 0),
  type_2_scores = c("Zeker zo veel" = 0, "Wel wat minder" = 1, "Duidelijk minder" = 2, "Eigenlijk nauwelijks nog" = 3),
  type_3_scores = c("Jazeker, en vrij erg" = 3, "Ja, maar niet zo erg" = 2, "Een beetje, maar het hindert me niet" = 1, "Helemaal niet" = 0),
  type_4_scores = c("Net zoveel als vroeger" = 0, "Nu wel wat minder" = 1, "Duidelijk wat minder" = 2, "Helemaal niet" = 3),
  type_5_scores = c("Heel erg vaak" = 3, "Vaak" = 2, "Af en toe, maar niet zo vaak" = 1, "Heel soms" = 0),
  type_6_scores = c("Helemaal niet" = 3, "Heel af en toe" = 2, "Soms" = 1, "Meestal" = 0),
  type_7_scores = c("Jazeker" = 0, "Meestal" = 1, "Af en toe" = 2, "Helemaal niet" = 3),
  type_8_scores = c("Bijna altijd" = 3, "Heel vaak" = 2, "Soms" = 1, "Helemaal niet" = 0),
  type_9_scores = c("Helemaal niet" = 0, "Soms" = 1, "Vrij vaak" = 2, "Heel Vaak" = 3),
  type_10_scores = c("Inderdaad, helemaal niet meer" = 3, 
                     "Niet meer zoveel als eigenlijk zou moeten" = 2, 
                     "Het interesseert me wel, maar iets minder dan vroeger" = 1, 
                     "Het interesseert me nog net zoveel als vroeger" = 0),
  type_11_scores = c("Inderdaad, heel duidelijk" = 3, "Duidelijk" = 2, "Enigszins" = 1, "Helemaal niet" = 0),
  type_12_scores = c("Net zoveel als vroeger" = 0, "Een beetje minder dan vroeger" = 1, "Veel minder dan vroeger" = 2, "Bijna nooit" = 3),
  type_13_scores = c("Inderdaad, zeer vaak" = 3, "Tamelijk vaak" = 2, "Soms" = 1, "Helemaal nooit" = 0),
  type_14_scores = c("Vaak" = 0, "Tamelijk vaak" = 1, "Af en toe" = 2, "Heel zelden" = 3))

convert_hads_scores <- function(data) {
  # loop over question numbers and timepoints
  for (i in 1:14) {
    
    type = paste0("type_", i, "_scores")
    mapping = score_types[[type]]
    
    for (prefix in c("", "FU1_", "FU2_", "FU3_", "FU4_", "FU5_")) {
    
      colname = paste0(prefix, "hads_", i)
      
      if (colname %in% names(data)) {
        data[[colname]] <- as.numeric(recode(data[[colname]], !!!mapping))
      }
    }
  }
  return(data)
}

# Apply the function to all _analysis datasets
dataHADSBL_analysis <- convert_hads_scores(dataHADSBL_analysis)
dataHADSFU1_analysis <- convert_hads_scores(dataHADSFU1_analysis)
dataHADSFU2_analysis <- convert_hads_scores(dataHADSFU2_analysis)
dataHADSFU3_analysis <- convert_hads_scores(dataHADSFU3_analysis)
dataHADSFU4_analysis <- convert_hads_scores(dataHADSFU4_analysis)
dataHADSFU5_analysis <- convert_hads_scores(dataHADSFU5_analysis)

### Add ANXIETY and DEPRESSION and TOTAL HADS scores to the datasets

calculate_hads_scores <- function(data, prefix = "BL") {
  # If the prefix is ‘BL’, add it to the column names of the baseline dataset
  if (prefix == "BL") {
    # Add “BL_” to the column names for the baseline dataset
    colnames(data)[grepl("^hads_", colnames(data))] <- paste0("BL_", colnames(data)[grepl("^hads_", colnames(data))])
    
    angst_columns <- paste0("BL_hads_", c(1, 3, 5, 7, 9, 11, 13))
    depressie_columns <- paste0("BL_hads_", c(2, 4, 6, 8, 10, 12, 14))
  } else {
    # Use the existing prefix for follow-up datasets
    angst_columns <- paste0(prefix, "_hads_", c(1, 3, 5, 7, 9, 11, 13))
    depressie_columns <- paste0(prefix, "_hads_", c(2, 4, 6, 8, 10, 12, 14))
  }
  
  # Add the scores to the dataset
  data[[paste0(prefix, "_hads_angst")]] <- rowSums(data[, angst_columns], na.rm = TRUE)
  data[[paste0(prefix, "_hads_depressie")]] <- rowSums(data[, depressie_columns], na.rm = TRUE)
  
  # Calculate and add the total HADS score
  data[[paste0(prefix, "_hads_totaal")]] <- 
    data[[paste0(prefix, "_hads_angst")]] + 
    data[[paste0(prefix, "_hads_depressie")]]
  
  return(data)
}

# Apply the function to baseline and FU1 to FU5
dataHADSBL_analysis <- calculate_hads_scores(dataHADSBL_analysis)  
dataHADSFU1_analysis <- calculate_hads_scores(dataHADSFU1_analysis, "FU1")
dataHADSFU2_analysis <- calculate_hads_scores(dataHADSFU2_analysis, "FU2")
dataHADSFU3_analysis <- calculate_hads_scores(dataHADSFU3_analysis, "FU3")
dataHADSFU4_analysis <- calculate_hads_scores(dataHADSFU4_analysis, "FU4")
dataHADSFU5_analysis <- calculate_hads_scores(dataHADSFU5_analysis, "FU5")


##### 6. Merge all HADS data in 1 dataset ##### 

# Make a list of all HADS datasets and the corresponding visit names
hads_datasets <- list(
  dataHADSBL_analysis,
  dataHADSFU1_analysis,
  dataHADSFU2_analysis,
  dataHADSFU3_analysis,
  dataHADSFU4_analysis,
  dataHADSFU5_analysis
)

# Function to remove prefixes
clean_colnames <- function(df) {
  colnames(df) <- str_replace_all(colnames(df), "^(BL_|FU[1-5]_)", "")  
  return(df)
}

# Apply the function and combine the datasets
data_HADS_combined <- bind_rows(lapply(hads_datasets, clean_colnames))

# Rename 'Castor Participant ID' to 'Participant Id'
data_HADS_combined <- data_HADS_combined %>%
  rename('Participant Id' = `Castor Participant ID`)

##### 7. further filter and calculation with merged HADS data #####
## Keep only completed questionnaires
# Filter only rows where all hads_1 to hads_14 columns are complete (no NA)
data_HADS_complete <- data_HADS_combined %>%
  filter(if_all(starts_with("hads_"), ~ !is.na(.)))


## select HADS questionnaire with visit at least 10 months (304 days) later, with the previous visit +/- one month (31 days) to completing the HADS 
# Add the visit dates to the data_HADS_complete dataset 
visit_dates <- data_transformed_filtered %>%
  select(`Participant Id`, 
         datumbaselinevisit, datumpoliFU1, datumpoliFU2, datumpoliFU3, datumpoliFU4, datumpoliFU5)

data_HADS_complete <- data_HADS_complete %>%
  left_join(visit_dates, by = "Participant Id")

# Add the column 'survey_date' and convert it to Date-format
data_HADS_complete <- data_HADS_complete %>%
  mutate(survey_date = as.Date(coalesce(`Survey Completed On`, `Survey Sent Date`), format="%Y-%m-%d"))

# Add the new column 'Criteria_HADS'
data_HADS_complete <- data_HADS_complete %>%
  mutate(Criteria_HADS = mapply(function(survey_date, baseline, fu1, fu2, fu3, fu4, fu5) {
    # Criterion 1: The date in Survey_Date must be within 1 month (31 days) of a visit
    criteria_1 <- any(abs(difftime(survey_date, c(baseline, fu1, fu2, fu3, fu4, fu5), units = "days")) <= 31, na.rm = TRUE)
    # Criterion 2: There must be a follow-up visit at least 10 months (304 days) later
    criteria_2 <- any(difftime(c(baseline, fu1, fu2, fu3, fu4, fu5), survey_date, units = "days") >= 304, na.rm = TRUE)
    return(criteria_1 & criteria_2)
  }, 
  data_HADS_complete$survey_date, 
  data_HADS_complete$datumbaselinevisit, 
  data_HADS_complete$datumpoliFU1, 
  data_HADS_complete$datumpoliFU2, 
  data_HADS_complete$datumpoliFU3, 
  data_HADS_complete$datumpoliFU4, 
  data_HADS_complete$datumpoliFU5))

# Create a new dataset containing only the rows for which Criteria_HADS is TRUE
data_HADS_complete_1F <- data_HADS_complete %>%
  filter(Criteria_HADS == TRUE)

# Add column with the name of the visit that meets criterion 1
data_HADS_complete_1F <- data_HADS_complete_1F %>%
  rowwise() %>%
  mutate(
    prev_visit = {
      visits <- c(datumbaselinevisit, datumpoliFU1, datumpoliFU2, datumpoliFU3, datumpoliFU4, datumpoliFU5)
      visit_names <- c("BL", "FU1", "FU2", "FU3", "FU4", "FU5")
      diffs <- abs(difftime(survey_date, visits, units = "days"))
      diffs[diffs > 31 | is.na(diffs)] <- NA
      if (all(is.na(diffs))) NA else visit_names[which.min(diffs)]
    }
  ) %>%
  ungroup()

# Add column with the name of the first next visit that meets criterion 2
data_HADS_complete_1F <- data_HADS_complete_1F %>%
  rowwise() %>%
  mutate(
    next_visit = {
      visits <- c(datumbaselinevisit, datumpoliFU1, datumpoliFU2, datumpoliFU3, datumpoliFU4, datumpoliFU5)
      visit_names <- c("BL", "FU1", "FU2", "FU3", "FU4", "FU5")
      diffs <- difftime(visits, survey_date, units = "days")
      valid <- which(diffs >= 304)
      if (length(valid) == 0) NA else visit_names[valid[which.min(diffs[valid])]]
    }
  ) %>%
  ungroup()

# Keep the first (earliest) visit per patient
visit_volgorde <- c("BL", "FU1", "FU2", "FU3", "FU4", "FU5")

data_HADS_complete_1F_filtered <- data_HADS_complete_1F %>%
  mutate(prev_visit = factor(prev_visit, levels = visit_volgorde)) %>%
  group_by(`Participant Id`) %>%
  arrange(prev_visit, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# Merge with clinical data
merged_data_1F <- data_HADS_complete_1F_filtered %>%
  left_join(
    select(data_transformed_filtered, -datumbaselinevisit, -datumpoliFU1, -datumpoliFU2, -datumpoliFU3, -datumpoliFU4, -datumpoliFU5),
    by = "Participant Id"
  )

# Add column ‘next_visit_2’ based on ‘next_visit’, to also view progression after 2 years instead of 1 year
merged_data_1F <- merged_data_1F %>%
  mutate(
    next_visit_2 = case_when(
      next_visit == "BL"  ~ "FU1",
      next_visit == "FU1" ~ "FU2",
      next_visit == "FU2" ~ "FU3",
      next_visit == "FU3" ~ "FU4",
      next_visit == "FU4" ~ "FU5",
      TRUE ~ NA_character_
    )
  )

# Add column 'next_visit_2_date'
merged_data_1F <- merged_data_1F %>%
  mutate(
    next_visit_2_date = case_when(
      next_visit_2 == "FU1" ~ datumpoliFU1,
      next_visit_2 == "FU2" ~ datumpoliFU2,
      next_visit_2 == "FU3" ~ datumpoliFU3,
      next_visit_2 == "FU4" ~ datumpoliFU4,
      next_visit_2 == "FU5" ~ datumpoliFU5,
      TRUE ~ as.Date(NA)
    )
  )

# Add visit dates to calculate the number of days between visits
merged_data_1F <- merged_data_1F %>%
  mutate(
    prev_visit_date = case_when(
      prev_visit == "BL"  ~ datumbaselinevisit,
      prev_visit == "FU1" ~ datumpoliFU1,
      prev_visit == "FU2" ~ datumpoliFU2,
      prev_visit == "FU3" ~ datumpoliFU3,
      prev_visit == "FU4" ~ datumpoliFU4,
      prev_visit == "FU5" ~ datumpoliFU5,
      TRUE ~ as.Date(NA)
    ),
    next_visit_date = case_when(
      next_visit == "FU1" ~ datumpoliFU1,
      next_visit == "FU2" ~ datumpoliFU2,
      next_visit == "FU3" ~ datumpoliFU3,
      next_visit == "FU4" ~ datumpoliFU4,
      next_visit == "FU5" ~ datumpoliFU5,
      TRUE ~ as.Date(NA)
    ),
    days_between_prev_and_next_visit = as.numeric(difftime(next_visit_date, prev_visit_date, units = "days"))
  )

# Calculate age at prev_visit and add the extra column
merged_data_1F <- merged_data_1F %>%
  mutate(
    age_at_prev_visit = as.numeric(difftime(prev_visit_date, geboortedatum, unit = "weeks")) / 52.25
  ) %>%
  mutate(age_at_prev_visit = floor(age_at_prev_visit))

# Calculate disease duration (time since first symptoms) and add the extra column
merged_data_1F$disease_duration_firstsymptoms <- as.numeric(
  difftime(merged_data_1F$prev_visit_date, 
           merged_data_1F$datumontstaan, 
           units = "days")
) / 365.25


#### LOADING AND TRANSFORMING/CONVERTING AMSQ DATA FOR ANALYSIS ####

##### 1. Load AMSQ data and interpret NA as NULL #####
dataAMSQBL <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Armfunctie_BL", na = "NULL")
dataAMSQFU1 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Armfunctie_FU1", na = "NULL")
dataAMSQFU2 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Armfunctie_FU2", na = "NULL")
dataAMSQFU3 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Armfunctie_FU3", na = "NULL")
dataAMSQFU4 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Armfunctie_FU4", na = "NULL")
dataAMSQFU5 <- read_excel("SPIN-P_excel_export_20250611113137.xlsx", sheet = "Armfunctie_FU5", na = "NULL")

##### 2. Filter out 
## following patients criteria were applied: PIF signed, PPMS diagnosis, had BL visit, correct information, incorrect/missing values are replaced with correct values or NA
## This process of code is deducted here to hide patient id due to privacy reason

##### 3. Filter out columns with ‘Survey Instance Id’, ‘Survey Package Id’, and 'Castor Participant Status' #####
dataAMSQBL_analysis <- dataAMSQBL %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataAMSQFU1_analysis <- dataAMSQFU1 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataAMSQFU2_analysis <- dataAMSQFU2 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataAMSQFU3_analysis <- dataAMSQFU3 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataAMSQFU4_analysis <- dataAMSQFU4 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)
dataAMSQFU5_analysis <- dataAMSQFU5 %>% select(-`Survey Instance Id`, -`Survey Package Id`, -`Castor Participant Status`)

##### 4. Assign correct data types to relevant variables HADS data #####

## Dates as dates (ignore time)
dataAMSQBL_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataAMSQBL_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataAMSQBL_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataAMSQBL_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataAMSQFU1_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataAMSQFU1_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataAMSQFU1_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataAMSQFU1_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataAMSQFU2_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataAMSQFU2_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataAMSQFU2_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataAMSQFU2_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataAMSQFU3_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataAMSQFU3_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataAMSQFU3_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataAMSQFU3_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataAMSQFU4_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataAMSQFU4_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataAMSQFU4_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataAMSQFU4_analysis$`Survey Sent Date`), format = "%d-%m-%Y")
dataAMSQFU5_analysis$`Survey Completed On` <- as.Date(sub(" .*", "", dataAMSQFU5_analysis$`Survey Completed On`), format = "%d-%m-%Y")
dataAMSQFU5_analysis$`Survey Sent Date` <- as.Date(sub(" .*", "", dataAMSQFU5_analysis$`Survey Sent Date`), format = "%d-%m-%Y")

##### 5. Converting AMSQ responses to scores #####
convert_AMSQ_scores <- function(data) {
  
  mapping = c(c("Helemaal niet" = 1, "Een beetje" = 2, "Matig" = 3, "Tamelijk veel" = 4, "Heel erg" = 5, "Kan ik niet meer" = 6))
  
  for (i in 1:31) {
    for (prefix in c("", "FU1", "FU2_", "FU3_", "FU4_", "FU5_")) {
      
      colname = paste0(prefix, "AMSQ_", i)
      
      if (colname %in% names(data)) {
        data[[colname]] <- as.numeric(recode(data[[colname]], !!!mapping))
      }
    }
  }
  return(data)
}

# Apply the function to all _analysis datasets
dataAMSQBL_analysis <- convert_AMSQ_scores(dataAMSQBL_analysis)
dataAMSQFU1_analysis <- convert_AMSQ_scores(dataAMSQFU1_analysis)
dataAMSQFU2_analysis <- convert_AMSQ_scores(dataAMSQFU2_analysis)
dataAMSQFU3_analysis <- convert_AMSQ_scores(dataAMSQFU3_analysis)
dataAMSQFU4_analysis <- convert_AMSQ_scores(dataAMSQFU4_analysis)
dataAMSQFU5_analysis <- convert_AMSQ_scores(dataAMSQFU5_analysis)

### Add total AMSQ score to the datasets
calculate_AMSQ_total_score <- function(data, prefix = "BL") {
  if (prefix == "BL") {
    # Rename columns for BL (without prefix)
    colnames(data)[grepl("^AMSQ_", colnames(data))] <- paste0("BL_", colnames(data)[grepl("^AMSQ_", colnames(data))])
    AMSQ_columns <- paste0("BL_AMSQ_", 1:31)
    
  } else if (prefix == "FU1") {
    # Rename columns of FU1 without underscore to with underscore 
    colnames(data)[grepl("^FU1AMSQ_", colnames(data))] <-
      sub("^FU1AMSQ_", "FU1_AMSQ_", colnames(data)[grepl("^FU1AMSQ_", colnames(data))])
    
    AMSQ_columns <- paste0("FU1_AMSQ_", 1:31)
    
  } else {
    # For FU2 up to and including FU5
    AMSQ_columns <- paste0(prefix, "_AMSQ_", 1:31)
  }
  
  
  # Calculate total score
  data[[paste0(prefix, "_AMSQ_totaal")]] <- rowSums(data[, AMSQ_columns], na.rm = TRUE)
  
  return(data)
}

# Add the scores to the datasets BL t/m FU5
dataAMSQBL_analysis <- calculate_AMSQ_total_score(dataAMSQBL_analysis, prefix = "BL")
dataAMSQFU1_analysis <- calculate_AMSQ_total_score(dataAMSQFU1_analysis, prefix = "FU1")
dataAMSQFU2_analysis <- calculate_AMSQ_total_score(dataAMSQFU2_analysis, prefix = "FU2")
dataAMSQFU3_analysis <- calculate_AMSQ_total_score(dataAMSQFU3_analysis, prefix = "FU3")
dataAMSQFU4_analysis <- calculate_AMSQ_total_score(dataAMSQFU4_analysis, prefix = "FU4")
dataAMSQFU5_analysis <- calculate_AMSQ_total_score(dataAMSQFU5_analysis, prefix = "FU5")

##### 6. Merge all AMSQ data in 1 dataset ##### 
# Make a list of all AMSQ datasets and the corresponding visit names
AMSQ_datasets <- list(
  dataAMSQBL_analysis,
  dataAMSQFU1_analysis,
  dataAMSQFU2_analysis,
  dataAMSQFU3_analysis,
  dataAMSQFU4_analysis,
  dataAMSQFU5_analysis
)

# Function to remove prefixes
clean_colnames <- function(df) {
  colnames(df) <- str_replace_all(colnames(df), "^(BL_|FU[1-5]_)", "")  # Verwijdert BL_, FU1_, FU2_, etc.
  return(df)
}

# Apply the function and combine the datasets
data_AMSQ_combined <- bind_rows(lapply(AMSQ_datasets, clean_colnames))

# Rename 'Castor Participant ID' to 'Participant Id'
data_AMSQ_combined <- data_AMSQ_combined %>%
  rename('Participant Id' = `Castor Participant ID`)


##### 7. Only keep fully completed questionnaires #####

## Filter only rows where all AMSQ_1 to AMSQ_31 columns are complete (no NA)
data_AMSQ_complete <- data_AMSQ_combined %>%
  filter(if_all(starts_with("AMSQ_"), ~ !is.na(.)))

# Add the new column 'survey_date' and convert it to Date-format
data_AMSQ_complete <- data_AMSQ_complete %>%
  mutate(survey_date = as.Date(coalesce(`Survey Completed On`, `Survey Sent Date`), format="%Y-%m-%d"))

# Add prev_visit_date, next_visit_date and next_visit_2_date from merged_data_1F to AMSQ-data based on Participant Id
data_AMSQ_complete <- data_AMSQ_complete %>%
  left_join(
    merged_data_1F %>%
      select(`Participant Id`, prev_visit_date, next_visit_date, next_visit_2_date),
    by = "Participant Id"
  )

##### 8. Calculate the criteria-columns and further filtering #####
# Criterion 1 = AMSQ within one month of prev_visit HADS
# Criterion 2 = AMSQ within one month of next_visit HADS
# Criterion 3 = AMSQ within one month of next_visit_2_date HADS 
data_AMSQ_complete <- data_AMSQ_complete %>%
  mutate(
    criterion_1_AMSQ = !is.na(prev_visit_date) &
      abs(as.numeric(difftime(survey_date, prev_visit_date, units = "days"))) <= 31,
    criterion_2_AMSQ = !is.na(next_visit_date) &
      abs(as.numeric(difftime(survey_date, next_visit_date, units = "days"))) <= 31,
    criterion_3_AMSQ = !is.na(next_visit_2_date) &
      abs(as.numeric(difftime(survey_date, next_visit_2_date, units = "days"))) <= 31
  )


# Create sub-datasets for each criterion
data_AMSQ_complete_criterion_1 <- data_AMSQ_complete %>%
  filter(criterion_1_AMSQ)

data_AMSQ_complete_criterion_2 <- data_AMSQ_complete %>%
  filter(criterion_2_AMSQ)

data_AMSQ_complete_criterion_3 <- data_AMSQ_complete %>%
  filter(criterion_3_AMSQ)

# View duplicate patients under criterion 1 (so with multiple complete AMSQ questionnaires at time of prev_visit_date)
# Step 1: save the IDs of patients with double rows 
duplicates_AMSQ_ids_criterion_1 <- data_AMSQ_complete_criterion_1 %>%
  count(`Participant Id`) %>%
  filter(n > 1) %>%
  pull(`Participant Id`)

# Step 2: filter the original dataset at these IDs
duplicates_AMSQ_criterion_1 <- data_AMSQ_complete_criterion_1 %>%
  filter(`Participant Id` %in% duplicates_AMSQ_ids_criterion_1)

# View the result
View(duplicates_AMSQ_criterion_1)

# And at criterion 2:
# Step 1: save the IDs of patients with double rows at criterion 2 
duplicates_AMSQ_ids_criterion_2 <- data_AMSQ_complete_criterion_2 %>%
  count(`Participant Id`) %>%
  filter(n > 1) %>%
  pull(`Participant Id`)

# Step 2: filter the original dataset on these IDs
duplicates_AMSQ_criterion_2 <- data_AMSQ_complete_criterion_2 %>%
  filter(`Participant Id` %in% duplicates_AMSQ_ids_criterion_2)

# View the result
View(duplicates_AMSQ_criterion_2)

# For duplicate patients, select the complete questionnaire that was completed closest to the visit
## So for criterion 1
data_AMSQ_complete_criterion_1_filtered <- data_AMSQ_complete_criterion_1 %>%
  group_by(`Participant Id`) %>%
  # Calculate absolute difference in days between survey_date and prev_visit_date
  mutate(diff_days = abs(as.numeric(difftime(survey_date, prev_visit_date, units = "days")))) %>%
  # Select row with smallest diff_days per participant
  slice_min(order_by = diff_days, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-diff_days)  # delete column

## And for criterion 2
data_AMSQ_complete_criterion_2_filtered <- data_AMSQ_complete_criterion_2 %>%
  group_by(`Participant Id`) %>%
  # Calculate absolute difference in days between survey_date and next_visit_date
  mutate(diff_days = abs(as.numeric(difftime(survey_date, next_visit_date, units = "days")))) %>%
  # Select row with smallest diff_days per participant
  slice_min(order_by = diff_days, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-diff_days)

# Check whether correct duplicates have been removed -> yes
View(data_AMSQ_complete_criterion_1_filtered)
View(data_AMSQ_complete_criterion_2_filtered)

# NB There are no duplicates for criterion 3

# Select relevant columns and rename column names so that both AMSQ datasets can be merged into one dataset in wide format
data_AMSQ_complete_criterion_1_filtered_merge <- data_AMSQ_complete_criterion_1_filtered %>%
  select(`Participant Id`, starts_with("AMSQ_"), AMSQ_totaal, survey_date)

data_AMSQ_complete_criterion_1_filtered_merge <- data_AMSQ_complete_criterion_1_filtered_merge %>%
  rename_with(~ str_replace(., "^AMSQ_", "prev_AMSQ_"), starts_with("AMSQ_")) %>% 
  rename(
    prev_AMSQ_survey_date = survey_date
  )

View(data_AMSQ_complete_criterion_1_filtered_merge)

# Same for criterion 2 questionnaire
data_AMSQ_complete_criterion_2_filtered_merge <- data_AMSQ_complete_criterion_2_filtered %>%
  select(`Participant Id`, starts_with("AMSQ_"), AMSQ_totaal, survey_date)

data_AMSQ_complete_criterion_2_filtered_merge <- data_AMSQ_complete_criterion_2_filtered_merge %>%
  rename_with(~ str_replace(., "^AMSQ_", "next_AMSQ_"), starts_with("AMSQ_")) %>% 
  rename(
    next_AMSQ_survey_date = survey_date
  )

View(data_AMSQ_complete_criterion_2_filtered_merge)

# And for criterion 3 quetionnaire
data_AMSQ_complete_criterion_3_filtered <- data_AMSQ_complete_criterion_3

data_AMSQ_complete_criterion_3_filtered_merge <- data_AMSQ_complete_criterion_3_filtered %>%
  select(`Participant Id`, starts_with("AMSQ_"), AMSQ_totaal, survey_date) %>%
  rename_with(~ str_replace(., "^AMSQ_", "next_AMSQ_2_"), starts_with("AMSQ_")) %>%
  rename(next_AMSQ_2_survey_date = survey_date)

View(data_AMSQ_complete_criterion_3_filtered_merge)

# Now add the AMSQ questionnaires to the dataset merged_data_1F
merged_data_1F <- merged_data_1F %>%
  left_join(data_AMSQ_complete_criterion_1_filtered_merge, by = "Participant Id") %>%
  left_join(data_AMSQ_complete_criterion_2_filtered_merge, by = "Participant Id") %>%
  left_join(data_AMSQ_complete_criterion_3_filtered_merge, by = "Participant Id")

# Add extra columns to merged_data_1F with information whether AMSQ is present at both prev_ and next_ visit and at both prev_ and next_visit_2 
merged_data_1F <- merged_data_1F %>%
  mutate(
    AMSQ_prev_and_next_available = !is.na(prev_AMSQ_totaal) & !is.na(next_AMSQ_totaal),
    AMSQ_prev_and_next_2_available = !is.na(prev_AMSQ_totaal) & !is.na(next_AMSQ_2_totaal)
  )

# Print number of TRUE for both combinations 
aantal_prev_next <- sum(merged_data_1F$AMSQ_prev_and_next_available, na.rm = TRUE)
aantal_prev_next_2 <- sum(merged_data_1F$AMSQ_prev_and_next_2_available, na.rm = TRUE)

print(aantal_prev_next)
print(aantal_prev_next_2)

#### FINAL DATA TRANSFORMATION ON MERGED DATASET FOR ANALYSIS ####        
##### 1. Add EDSS progression data (NA = not progressive) #####
merged_data_1F <- merged_data_1F %>%
  mutate(
    prev_edss = case_when(
      prev_visit == "BL"  ~ EDSS_BL_numeric,
      prev_visit == "FU1" ~ EDSS_FU1_numeric,
      prev_visit == "FU2" ~ EDSS_FU2_numeric,
      prev_visit == "FU3" ~ EDSS_FU3_numeric,
      prev_visit == "FU4" ~ EDSS_FU4_numeric,
      prev_visit == "FU5" ~ EDSS_FU5_numeric,
      TRUE ~ NA_real_
    ),
    next_edss = case_when(
      next_visit == "BL"  ~ EDSS_BL_numeric,
      next_visit == "FU1" ~ EDSS_FU1_numeric,
      next_visit == "FU2" ~ EDSS_FU2_numeric,
      next_visit == "FU3" ~ EDSS_FU3_numeric,
      next_visit == "FU4" ~ EDSS_FU4_numeric,
      next_visit == "FU5" ~ EDSS_FU5_numeric,
      TRUE ~ NA_real_
    ),
    edss_progression = case_when(
      !is.na(prev_edss) & !is.na(next_edss) & prev_edss <= 5.5 & (next_edss - prev_edss >= 1.0) ~ TRUE,
      !is.na(prev_edss) & !is.na(next_edss) & prev_edss > 5.5  & (next_edss - prev_edss >= 0.5) ~ TRUE,
      TRUE ~ FALSE
    )
  )

##### 2. Add T25FW progression data ##### 
merged_data_1F <- merged_data_1F %>%
  mutate(
    prev_T25FW = case_when(
      prev_visit == "BL"  ~ gemT25FW,
      prev_visit == "FU1" ~ gemT25FW_FU1,
      prev_visit == "FU2" ~ gemT25FW_FU2,
      prev_visit == "FU3" ~ gemT25FW_FU3,
      prev_visit == "FU4" ~ gemT25FW_FU4,
      prev_visit == "FU5" ~ gemT25FW_FU5,
      TRUE ~ NA_real_
    ),
    next_T25FW = case_when(
      next_visit == "BL"  ~ gemT25FW,
      next_visit == "FU1" ~ gemT25FW_FU1,
      next_visit == "FU2" ~ gemT25FW_FU2,
      next_visit == "FU3" ~ gemT25FW_FU3,
      next_visit == "FU4" ~ gemT25FW_FU4,
      next_visit == "FU5" ~ gemT25FW_FU5,
      TRUE ~ NA_real_
    ),
    T25FW_progression = case_when(
      !is.na(prev_T25FW) & !is.na(next_T25FW) & ((next_T25FW - prev_T25FW) / prev_T25FW >= 0.20) ~ TRUE,
      !is.na(prev_T25FW) & !is.na(next_T25FW) ~ FALSE,
      TRUE ~ FALSE
    )
  ) # some patients info were manually annotated here due to loss of walking ability at the next_visit_2 and therefore progression, code reducted here due to privacy reason

##### 3. Add AMSQ progression data #####
merged_data_1F <- merged_data_1F %>%
  mutate(
    # Progression at next_visit compared to prev_visit 
    progression_AMSQ = !is.na(prev_AMSQ_totaal) & !is.na(next_AMSQ_totaal) & 
      (next_AMSQ_totaal - prev_AMSQ_totaal) >= 18
  )

##### 4. Add SDMT progression data #####
merged_data_1F <- merged_data_1F %>%
  mutate(
    prev_SDMT = case_when(
      prev_visit == "BL"  ~ SDMT,
      prev_visit == "FU1" ~ SDMT_FU1,
      prev_visit == "FU2" ~ SDMT_FU2,
      prev_visit == "FU3" ~ SDMT_FU3,
      prev_visit == "FU4" ~ SDMT_FU4,
      prev_visit == "FU5" ~ SDMT_FU5,
      TRUE ~ NA_real_
    ),
    next_SDMT = case_when(
      next_visit == "BL"  ~ SDMT,
      next_visit == "FU1" ~ SDMT_FU1,
      next_visit == "FU2" ~ SDMT_FU2,
      next_visit == "FU3" ~ SDMT_FU3,
      next_visit == "FU4" ~ SDMT_FU4,
      next_visit == "FU5" ~ SDMT_FU5,
      TRUE ~ NA_real_
    ),
    SDMT_progression = case_when(
      !is.na(prev_SDMT) & !is.na(next_SDMT) & (prev_SDMT - next_SDMT >= 8) ~ TRUE,  # Aangepaste conditie voor afname
      !is.na(prev_SDMT) & !is.na(next_SDMT) ~ FALSE,
      TRUE ~ FALSE
    )
  )

##### 5. Add PDDS progression data #####
merged_data_1F <- merged_data_1F %>%
  mutate(
    prev_PDDS = case_when(
      prev_visit == "BL"  ~ PDDS_numeric,
      prev_visit == "FU1" ~ PDDS_FU1_numeric,
      prev_visit == "FU2" ~ PDDS_FU2_numeric,
      prev_visit == "FU3" ~ PDDS_FU3_numeric,
      prev_visit == "FU4" ~ PDDS_FU4_numeric,
      prev_visit == "FU5" ~ PDDS_FU5_numeric,
      TRUE ~ NA_real_
    ),
    next_PDDS = case_when(
      next_visit == "BL"  ~ PDDS_numeric,
      next_visit == "FU1" ~ PDDS_FU1_numeric,
      next_visit == "FU2" ~ PDDS_FU2_numeric,
      next_visit == "FU3" ~ PDDS_FU3_numeric,
      next_visit == "FU4" ~ PDDS_FU4_numeric,
      next_visit == "FU5" ~ PDDS_FU5_numeric,
      TRUE ~ NA_real_
    ),
    PDDS_progression = case_when(
      !is.na(prev_PDDS) & !is.na(next_PDDS) & (next_PDDS - prev_PDDS >= 1) ~ TRUE,
      !is.na(prev_PDDS) & !is.na(next_PDDS) ~ FALSE,
      TRUE ~ FALSE
    )
  )

## View people with missing progression values for EDSS, T25FW, SDMT, and PDDS
missing_progression_values <- merged_data_1F %>%
  filter(
    is.na(prev_edss) |
      is.na(next_edss) |
      is.na(prev_T25FW) |
      is.na(next_T25FW) |
      is.na(prev_SDMT) |
      is.na(next_SDMT) |
      is.na(prev_PDDS) |
      is.na(next_PDDS)
  )

View(missing_progression_values)

### There are no 'unjustified' missing progression values 

# Make combined progression variable of EDSS or T25FW or AMSQ
merged_data_1F <- merged_data_1F %>%
  mutate(progression_edss_or_T25FW_or_AMSQ = edss_progression | T25FW_progression | progression_AMSQ)

# Make combined progression variable of EDSS or T25FW or AMSQ or SDMT or PDDS
merged_data_1F <- merged_data_1F %>%
  mutate(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS = edss_progression | T25FW_progression | progression_AMSQ | SDMT_progression | PDDS_progression)


##### 6. add psychotropic medication and immunomodulatory MS medication for baseline characteristics #####
## Add psychotropic medication  
merged_data_1F <- merged_data_1F %>%
  mutate(
    prev_psychofarmaca = case_when(
      prev_visit == "BL"  ~ psychofarmaca_BL,
      prev_visit == "FU1" ~ psychofarmaca_FU1,
      prev_visit == "FU2" ~ psychofarmaca_FU2,
      prev_visit == "FU3" ~ psychofarmaca_FU3,
      prev_visit == "FU4" ~ psychofarmaca_FU4,
      prev_visit == "FU5" ~ psychofarmaca_FU5,
      TRUE ~ NA_character_
    ),
    prev_antidepressiva = case_when(
      prev_visit == "BL"  ~ antidepressiva_BL,
      prev_visit == "FU1" ~ antidepressiva_FU1,
      prev_visit == "FU2" ~ antidepressiva_FU2,
      prev_visit == "FU3" ~ antidepressiva_FU3,
      prev_visit == "FU4" ~ antidepressiva_FU4,
      prev_visit == "FU5" ~ antidepressiva_FU5,
      TRUE ~ NA_character_
    ),
    prev_anxiolytica = case_when(
      prev_visit == "BL"  ~ anxiolytica_BL,
      prev_visit == "FU1" ~ anxiolytica_FU1,
      prev_visit == "FU2" ~ anxiolytica_FU2,
      prev_visit == "FU3" ~ anxiolytica_FU3,
      prev_visit == "FU4" ~ anxiolytica_FU4,
      prev_visit == "FU5" ~ anxiolytica_FU5,
      TRUE ~ NA_character_
    )
  )

## Add immunomodulatory MS medication               
merged_data_1F <- merged_data_1F %>%
  rowwise() %>%
  mutate(
    # IMT at baseline
    medicatie_BL = immuunMSmedicatie,
    
    # IMT at FU1
    medicatie_FU1 = case_when(
      is.na(medicatie_BL) ~ NA_character_,
      !is.na(immuunMSmedicatie_FU1) & immuunMSmedicatie_FU1 != "Geen" ~ immuunMSmedicatie_FU1,
      !is.na(stopimmuunMSmedicatie_FU1) & stopimmuunMSmedicatie_FU1 == medicatie_BL ~ "Geen",
      TRUE ~ medicatie_BL
    ),
    
    # IMT at FU2
    medicatie_FU2 = case_when(
      is.na(medicatie_FU1) ~ NA_character_,
      !is.na(immuunMSmedicatie_FU2) & immuunMSmedicatie_FU2 != "Geen" ~ immuunMSmedicatie_FU2,
      !is.na(stopimmuunMSmedicatie_FU2) & stopimmuunMSmedicatie_FU2 == medicatie_FU1 ~ "Geen",
      TRUE ~ medicatie_FU1
    ),
    
    # IMT at FU3
    medicatie_FU3 = case_when(
      is.na(medicatie_FU2) ~ NA_character_,
      !is.na(immuunMSmedicatie_FU3) & immuunMSmedicatie_FU3 != "Geen" ~ immuunMSmedicatie_FU3,
      !is.na(stopimmuunMSmedicatie_FU3) & stopimmuunMSmedicatie_FU3 == medicatie_FU2 ~ "Geen",
      TRUE ~ medicatie_FU2
    ),
    
    # IMT at FU4 
    medicatie_FU4 = case_when(
      is.na(medicatie_FU3) ~ NA_character_,
      !is.na(immuunMSmedicatie_FU4) & immuunMSmedicatie_FU4 != "Geen" ~ immuunMSmedicatie_FU4,
      !is.na(stopimmuunMSmedicatie_FU4) & stopimmuunMSmedicatie_FU4 == medicatie_FU3 ~ "Geen",
      TRUE ~ medicatie_FU3
    ),
    
    # IMT at FU5
    medicatie_FU5 = case_when(
      is.na(medicatie_FU4) ~ NA_character_,
      !is.na(immuunMSmedicatie_FU5) & immuunMSmedicatie_FU5 != "Geen" ~ immuunMSmedicatie_FU5,
      !is.na(stopimmuunMSmedicatie_FU5) & stopimmuunMSmedicatie_FU5 == medicatie_FU4 ~ "Geen",
      TRUE ~ medicatie_FU4
    ),
    
    # Choose right medication per prev_visit 
    prev_immuunMSmedicatie_type = case_when(
      prev_visit == "BL"  ~ medicatie_BL,
      prev_visit == "FU1" ~ medicatie_FU1,
      prev_visit == "FU2" ~ medicatie_FU2,
      prev_visit == "FU3" ~ medicatie_FU3,
      prev_visit == "FU4" ~ medicatie_FU4,
      prev_visit == "FU5" ~ medicatie_FU5,
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()


##### 7. prepare dataframe for EDSS progression at 2 years #####
# Add column 'next_edss_2' 
merged_data_1F <- merged_data_1F %>%
  mutate(
    next_edss_2 = case_when(
      next_visit_2 == "FU1" ~ EDSS_FU1_numeric,
      next_visit_2 == "FU2" ~ EDSS_FU2_numeric,
      next_visit_2 == "FU3" ~ EDSS_FU3_numeric,
      next_visit_2 == "FU4" ~ EDSS_FU4_numeric,
      next_visit_2 == "FU5" ~ EDSS_FU5_numeric,
      TRUE ~ NA_real_
    )
  )

# Add column 'edss_progression_2' (compared to prev_edss; NA = not progressive)
merged_data_1F <- merged_data_1F %>%
  mutate(
    edss_progression_2 = case_when(
      !is.na(prev_edss) & !is.na(next_edss_2) & prev_edss <= 5.5 & (next_edss_2 - prev_edss >= 1.0) ~ TRUE,
      !is.na(prev_edss) & !is.na(next_edss_2) & prev_edss > 5.5  & (next_edss_2 - prev_edss >= 0.5) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Calculate number of days between prev_visit and next_visit_2
merged_data_1F <- merged_data_1F %>%
  mutate(
    days_between_prev_and_next_visit_2 = as.numeric(difftime(next_visit_2_date, prev_visit_date, units = "days"))
  )

# Create a separate dataset containing only patients who also have a next_visit_2 
merged_data_1F_2y <- merged_data_1F %>%
  filter(!is.na(next_visit_2_date))

## View people with missing progression values for EDSS, T25FW, SDMT and PDDS in this dataset
missing_progression_values_2y <- merged_data_1F_2y %>%
  filter(
    is.na(prev_edss) |
      is.na(next_edss) |
      is.na(prev_T25FW) |
      is.na(next_T25FW) |
      is.na(prev_SDMT) |
      is.na(next_SDMT) |
      is.na(prev_PDDS) |
      is.na(next_PDDS)
  )

View(missing_progression_values_2y)

### There are no 'unjustified' missing progression values of EDSS/T25FW/SDMT/PDDS at 2y (there are only missing values which are truly unknown)

# Add data T25FW progression at 2 years in column 'next_T25FW_2' (NA = not progressive)
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(
    next_T25FW_2 = case_when(
      next_visit_2 == "FU1" ~ gemT25FW_FU1,
      next_visit_2 == "FU2" ~ gemT25FW_FU2,
      next_visit_2 == "FU3" ~ gemT25FW_FU3,
      next_visit_2 == "FU4" ~ gemT25FW_FU4,
      next_visit_2 == "FU5" ~ gemT25FW_FU5,
      TRUE ~ NA_real_
    ),
    T25FW_progression_2 = case_when(
      !is.na(prev_T25FW) & !is.na(next_T25FW_2) & ((next_T25FW_2 - prev_T25FW) / prev_T25FW >= 0.20) ~ TRUE,
      !is.na(prev_T25FW) & !is.na(next_T25FW_2) ~ FALSE,
      TRUE ~ FALSE
    )
  ) # some patients info were manually annotated here due to changes in disease progression, code reducted here due to privacy reason

# Add data AMSQ progression at 2 years in column 'next_AMSQ_2_totaal' (NA = not progressive)
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(
    progression_AMSQ_2 = !is.na(prev_AMSQ_totaal) & !is.na(next_AMSQ_2_totaal) & 
      (next_AMSQ_2_totaal - prev_AMSQ_totaal) >= 18
  )


# Add data SDMT progression at 2 years in column 'next_SDMT_2' (NA = not progressive)
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(
    next_SDMT_2 = case_when(
      next_visit_2 == "FU1" ~ SDMT_FU1,
      next_visit_2 == "FU2" ~ SDMT_FU2,
      next_visit_2 == "FU3" ~ SDMT_FU3,
      next_visit_2 == "FU4" ~ SDMT_FU4,
      next_visit_2 == "FU5" ~ SDMT_FU5,
      TRUE ~ NA_real_
    ),
    SDMT_progression_2 = case_when(
      !is.na(prev_SDMT) & !is.na(next_SDMT_2) & (prev_SDMT - next_SDMT_2 >= 8) ~ TRUE,
      !is.na(prev_SDMT) & !is.na(next_SDMT_2) ~ FALSE,
      TRUE ~ FALSE
    )
  )

# Add data PDDS progression at 2 years in column 'next_PDDS_2' (NA = not progressive)
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(
    next_PDDS_2 = case_when(
      next_visit_2 == "FU1" ~ PDDS_FU1_numeric,
      next_visit_2 == "FU2" ~ PDDS_FU2_numeric,
      next_visit_2 == "FU3" ~ PDDS_FU3_numeric,
      next_visit_2 == "FU4" ~ PDDS_FU4_numeric,
      next_visit_2 == "FU5" ~ PDDS_FU5_numeric,
      TRUE ~ NA_real_
    ),
    PDDS_progression_2 = case_when(
      !is.na(prev_PDDS) & !is.na(next_PDDS_2) & (next_PDDS_2 - prev_PDDS >= 1) ~ TRUE,
      !is.na(prev_PDDS) & !is.na(next_PDDS_2) ~ FALSE,
      TRUE ~ FALSE
    )
  )

# Create combined progression variable for EDSS or T25FW or AMSQ at 2 years 
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(progression_edss_or_T25FW_or_AMSQ_2y = edss_progression_2 | T25FW_progression_2 | progression_AMSQ_2)

# Create combined progression variable for EDSS or T25FW or AMSQ or SDMT or PDDS at 2 years 
merged_data_1F_2y <- merged_data_1F_2y %>%
  mutate(progression_edss_or_T25FW_or_AMSQ_or_SDMT_or_PDDS_2y = edss_progression_2 | T25FW_progression_2 | progression_AMSQ_2 | SDMT_progression_2 | PDDS_progression_2)


#### EXPORT FINAL DATASET FOR ANALYSIS AND ENVIRONMENT ####
write.xlsx(merged_data_1F, file = "merged_data_1F.xlsx")
write.xlsx(merged_data_1F_2y, file = "merged_data_1F_2y.xlsx")

# remove everything from environment at the end
rm(list=ls())

# save session info
sessionInfo()


