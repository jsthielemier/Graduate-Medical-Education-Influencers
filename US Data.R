library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

# Load data
HOSP10_2022_NMRC <- read_csv("HOSP10_2022_NMRC.CSV")
HOSP10_2022_ALPHA <- read_csv("HOSP10_2022_ALPHA.CSV")
HOSP10_2022_RPT <- read_csv("HOSP10_2022_RPT.CSV")

# Rename columns
colnames(HOSP10_2022_NMRC) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_VAL_NUM")
colnames(HOSP10_2022_ALPHA) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_ALPHNMRC_ITM_TXT")
colnames(HOSP10_2022_RPT) <- c("RPT_REC_NUM", "PRVDR_CTRL_TYPE_CD", "PROVIDER_NUMBER", "NPI", "RPT_STUS_CD", "FY_BGN_DT", "FY_END_DT", "PROC_DT", "INITL_RPT_SW", "LAST_RPT_SW", "TRNSMTL_NUM", "FI_NUM", "ADR_VNDR_CD", "FI_CREAT_DT", "UTIL_CD", "NPR_DT", "SPEC_IND", "FI_RCPT_DT")

# Provider info
US_providers <- HOSPITAL10_PROVIDER_ID_INFO

# Truncate ZIP code to first 5 digits
US_providers <- US_providers %>%
  mutate(Zip_Code = substr(Zip_Code, 1, 5))

# Link report numbers
US_RPT_REC_NUM <- HOSP10_2022_RPT %>%
  filter(PROVIDER_NUMBER %in% US_providers$PROVIDER_NUMBER)

# Build hospital dataset
US_Hospitals <- US_providers %>%
  left_join(US_RPT_REC_NUM %>% select(PROVIDER_NUMBER, RPT_REC_NUM), by = "PROVIDER_NUMBER") %>%
  select(-FYB, -FYE, -STATUS, -CTRL_TYPE, -PO_Box) %>%
  select(RPT_REC_NUM, PROVIDER_NUMBER, everything())

# Pull helper functions
pull_value <- function(wksht_cd, line_num, clmn_num, new_col_name) {
  HOSP10_2022_NMRC %>%
    filter(
      WKSHT_CD == wksht_cd,
      LINE_NUM == str_pad(line_num, 5, pad = "0"),
      CLMN_NUM == str_pad(clmn_num, 5, pad = "0")
    ) %>%
    select(RPT_REC_NUM, ITM_VAL_NUM) %>%
    rename(!!new_col_name := ITM_VAL_NUM)
}

pull_alpha_value <- function(wksht_cd, line_num, clmn_num, new_col_name) {
  HOSP10_2022_ALPHA %>%
    filter(
      WKSHT_CD == wksht_cd,
      LINE_NUM == str_pad(line_num, 5, pad = "0"),
      CLMN_NUM == str_pad(clmn_num, 5, pad = "0")
    ) %>%
    select(RPT_REC_NUM, ITM_ALPHNMRC_ITM_TXT) %>%
    rename(!!new_col_name := ITM_ALPHNMRC_ITM_TXT)
}

pull_gme_paid <- function() {
  HOSP10_2022_NMRC %>%
    filter(WKSHT_CD == "E40A180",
           LINE_NUM == "01700",
           CLMN_NUM %in% c("00100", "00200")) %>%
    group_by(RPT_REC_NUM) %>%
    summarise(GME_Paid = sum(ITM_VAL_NUM, na.rm = TRUE), .groups = "drop")
}


# Join data
US_Hospitals <- US_Hospitals %>%
  left_join(pull_value("S300001", "01400", "00200", "Number_of_Beds"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("E40A180", "00100", "00100", "Federal_GME_Cap"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("E40A180", "00600", "00100", "FTE_Residents"), by = "RPT_REC_NUM") %>%
  left_join(pull_alpha_value("S200001", "14100", "00100", "System_Affiliation"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("S300001", "01400", "00600", "Medicare_Days"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("S300001", "01400", "00700", "Medicaid_Days"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("A000000", "02100", "00500", "Resident_Salary"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("A000000", "02200", "00500", "Faculty_Cost"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("G300000", "00100", "00100", "Hospital_Revenue"), by = "RPT_REC_NUM") %>%
  left_join(pull_gme_paid(), by = "RPT_REC_NUM") %>%
  
  
  filter(!is.na(FTE_Residents) & FTE_Residents > 0) %>%
  mutate(
    Medicare_Pct = ifelse((Medicare_Days + Medicaid_Days) > 0,
                          Medicare_Days / (Medicare_Days + Medicaid_Days), NA),
    Medicaid_Pct = ifelse((Medicare_Days + Medicaid_Days) > 0,
                          Medicaid_Days / (Medicare_Days + Medicaid_Days), NA),
    Academic_Medical_Center = "Yes",
    Hospital_System_Affiliation = case_when(
      !is.na(System_Affiliation) ~ "System Member",
      is.na(System_Affiliation) ~ "Stand-Alone"
    ),
    Medicare_GME_Per_Resident = GME_Paid / FTE_Residents,
    Faculty_Cost_Per_Resident = Faculty_Cost / FTE_Residents,
    Fill_Rate = FTE_Residents / Federal_GME_Cap
  )



# Export
write.csv(US_Hospitals, 
          "C:/Users/Jacob Thielemier/OneDrive - Hull Property Group/Desktop/National Research/US_Hospitals_final.csv", 
          row.names = FALSE)
