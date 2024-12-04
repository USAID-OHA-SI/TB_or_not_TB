# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TB/HIV Dashboard QC
# REF ID:   f50503e6 
# LICENSE:  MIT
# DATE:     2022-11-29
# UPDATED:  2024-03-01

#FUNCTIONALIZE THIS EVEN MORE!

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)



# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

path <- si_path() %>% return_latest("OU_IM_FY22")

# Grab metadata
metadata <- get_metadata(path)

#manually override period for Q1 and Q3 (for example, if we are in Q3 and we need to see Q2 numbers, change this to FY23Q2)
metadata$curr_pd <- "FY23Q4"
metadata$curr_fy <- 2023

ref_id <- "f50503e6"

# IMPORT ------------------------------------------------------------------

#download MSD from Pano using Pano API
pano_extract_msd(operatingunit = NULL,
                 #version = "clean",
                 fiscal_year = 2024,
                 #quarter = 2,
                 level = "ou",
                 dest_path = si_path())


df <- si_path() %>% return_latest("OU_IM_FY22") %>% 
  read_psd()

df <- df %>% 
  bind_rows(df %>% mutate(funding_agency = "PEPFAR")) %>% 
  filter(operatingunit %ni% c("Ukraine"))


df <- df %>% 
  resolve_knownissues()

# 2. 2. TB Screening Coverage -----------------------------------------------

# FIX THIS FUNCTION LATER - numbers inflated

tab2_tb_screen_qc <- function(df, agency = NULL, group_var = NULL) {
  
  #filter
  df_filter <- df %>% 
    clean_indicator() %>%
    filter(indicator %in% c("TX_TB_D", "TX_CURR"),
           fiscal_year == metadata$curr_fy,
           if (!is.null(agency)) funding_agency == agency else TRUE,
           #operatingunit == "Zambia",
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) 
  
  df_final <- df_filter %>% 
    group_by(fiscal_year, indicator, across(all_of(group_var))) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd() %>% 
    filter(period == metadata$curr_pd) %>% 
    pivot_wider(names_from = "indicator", values_from = "value") %>% 
    mutate(pct_screen = `TX_TB_D`/`TX_CURR`) %>% 
    arrange(desc(pct_screen))
  
  return(df_final)
  
}

#check PEPFAR global
tab2_tb_screen_qc(df, "HHS/CDC")
#check USAID by OU
tab2_tb_screen_qc(df, group_var = c("operatingunit", "funding_agency")) %>% View()
#check by agency
tab2_tb_screen_qc(df, group_var = "funding_agency")

# 3. TB Screening Outcomes by Age/Sex/ART Status ---------------------------

tab3_tb_screen_group <- function(df, agency, group_var = NULL) {
  
  #BAR chart - TB Screen results
  df_final <- df %>% 
    clean_indicator() %>%
    filter(indicator %in% c("TX_TB_D"),
           fiscal_year == metadata$curr_fy,
           funding_agency == agency,
           standardizeddisaggregate  %in% c("Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus", "Age/Sex/HIVStatus")) %>% 
    mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already on ART", "New on ART")) %>% 
    group_by(fiscal_year, indicator,
             otherdisaggregate,
             across(all_of(group_var))
    ) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(status = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
    group_by(fiscal_year, indicator, across(all_of(group_var)), status) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(period == metadata$curr_pd)
  
  return(df_final) 
}

tab3_tb_screen_group(df, "PEPFAR", group_var = "trendscoarse")
tab3_tb_screen_group(df, "USAID", group_var = "sex")
tab3_tb_screen_group(df, "USAID", group_var = "art_status")

tab_tb_screen_cov_group <- function(df, agency, group_var = NULL) {
  
  df_final <-  df %>% 
    clean_indicator() %>%
    filter(indicator %in% c("TX_TB_D", "TX_CURR"),
           fiscal_year == metadata$curr_fy,
           funding_agency == agency,
           standardizeddisaggregate  %in% c("Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus", "Age/Sex/HIVStatus")) %>% 
    group_by(fiscal_year, indicator, funding_agency, across(all_of(group_var))) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = "indicator", values_from = "value") %>% 
    mutate(pct_screen = `TX_TB_D`/`TX_CURR`) %>% 
    filter(period == metadata$curr_pd)
  
  return(df_final)
  
}


tab_tb_screen_cov_group(df, "PEPFAR", group_var = "trendscoarse")
tab_tb_screen_cov_group(df, "USAID", group_var = "sex")



# 4. % Pos TB screening by HIV status ---------------------------------------

tab4_tb_screen_pos_artstatus <- function(df, agency, group_var = NULL) {
  
  #grab total screening of already vs new on art
  df_total <- df %>% 
    clean_indicator() %>%
    filter(indicator %in% c("TX_TB_D"),
           funding_agency == agency,
           fiscal_year == metadata$curr_fy,
           standardizeddisaggregate  %in% c("Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus")) %>% 
    group_by(fiscal_year, indicator,
             otherdisaggregate, across(all_of(group_var))) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(status_tb = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
    mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already", "New")) %>% 
    group_by(fiscal_year, indicator, art_status, across(all_of(group_var))) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(value !=0,
           period == metadata$curr_pd) %>% 
    rename(total_screen = value)
  
  #grab pos and bind
  df_final <- df %>% 
    clean_indicator() %>%
    filter(indicator %in% c("TX_TB_D"),
           fiscal_year ==metadata$curr_fy,
           funding_agency == agency,
           standardizeddisaggregate  %in% c("Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus")) %>% 
    group_by(fiscal_year, indicator,
             otherdisaggregate, across(all_of(group_var))) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(status_tb = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
    mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already", "New")) %>% 
    filter(status_tb == "Positive") %>% 
    group_by(fiscal_year, indicator, art_status, across(all_of(group_var))) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(value !=0,
           period == metadata$curr_pd) %>% 
    rename(pos_screen = value) %>% 
    left_join(df_total, by = c("indicator", "period", "period_type", "art_status", group_var)) %>% 
    mutate(screen_rate = pos_screen/total_screen)
  
  return(df_final)
}


#grab pos and bind
df_final <- df %>% 
  clean_indicator() %>%
  filter(indicator %in% c("TX_TB_D"),
         fiscal_year ==metadata$curr_fy,
         funding_agency == "PEPFAR",
         standardizeddisaggregate  %in% c("Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus")) %>% 
  group_by(fiscal_year, indicator,
           otherdisaggregate, operatingunit) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  mutate(status_tb = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
  mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already", "New")) %>% 
  filter(status_tb == "Positive") %>% 
  group_by(fiscal_year, indicator, art_status, operatingunit) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(value !=0,
         period == metadata$curr_pd) %>% 
  rename(pos_screen = value) %>% 
  left_join(df_total, by = c("indicator", "period", "period_type", "art_status")) %>% 
  mutate(screen_rate = pos_screen/total_screen)

tab4_tb_screen_pos_artstatus(df, "PEPFAR")
tab4_tb_screen_pos_artstatus(df, "PEPFAR", group_var = "operatingunit")  %>% View()


# 5a. Trends in TB Treatment Initiation --------------------------------------

#numerator can be cumulative

df %>% 
  # resolve_knownissues() %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "N",
          funding_agency == "PEPFAR",
         # operatingunit == "Zambia",
         standardizeddisaggregate %in%c("Age Aggregated/Sex/NewExistingArt/HIVStatus",
                                        "Age/Sex/NewExistingArt/HIVStatus")) %>% 
  #(standardizeddisaggregate)
  group_by(fiscal_year, indicator,
        # operatingunit,
         otherdisaggregate
  ) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  mutate(total_val = qtr2 + qtr4)

# 5b. Trends in TB Treatment Initiation --------------------------------------

#numerator can be cumulative

df %>% 
  # resolve_knownissues() %>% 
  filter(indicator %in% c("TX_CURR", "TX_TB"),
         fiscal_year == metadata$curr_fy,
         numeratordenom == "N",
         funding_agency == "PEPFAR",
         # operatingunit == "Zambia",
         standardizeddisaggregate %in%c("Age Aggregated/Sex/NewExistingArt/HIVStatus",
                                        "Age/Sex/NewExistingArt/HIVStatus",
                                        "Age/Sex/HIVStatus")) %>% 
  #(standardizeddisaggregate)
  group_by(fiscal_year, indicator,
           operatingunit
           
           # operatingunit,
           #otherdisaggregate
  ) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  pivot_wider(names_from = "indicator") %>% 
  mutate(prop_txcurr = TX_TB/TX_CURR) %>% 
  arrange(desc(prop_txcurr))


# 4a. TB Screen Positive Cascade ---------------------------------------------

#get TX_TB D POS
tx_tb_denom_pos <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "D",
         # operatingunit == "Zambia",
         standardizeddisaggregate == "Age/Sex/TBScreen/NewExistingART/HIVStatus",
         funding_agency == "PEPFAR") %>% 
  group_by(fiscal_year, indicator,
           funding_agency, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  filter(str_detect(otherdisaggregate, "Positive")) %>% 
  group_by(fiscal_year, indicator, funding_agency) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  mutate(indicator = "TX_TB_D_Pos")

#Get Specimen sent
specimen_sent <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "D",
         #operatingunit == "Zambia",
         standardizeddisaggregate == "Specimen Sent/HIVStatus",
         funding_agency == "PEPFAR") %>% 
  group_by(fiscal_year, indicator,funding_agency,
           standardizeddisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  mutate(indicator = "Specimen Sent") %>% 
  select(-standardizeddisaggregate)

#get specimen return
specimen_return <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "D",
         # operatingunit == "Zambia",
         standardizeddisaggregate == "Specimen Return/HIVStatus",
         funding_agency == "PEPFAR") %>% 
  group_by(fiscal_year, indicator,
           funding_agency, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  mutate(indicator = "Specimen Returned Positive") %>% 
  select(-otherdisaggregate)

#get total numerator
tx_tb_num <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "N",
         funding_agency == "PEPFAR",
         # operatingunit == "Zambia",
         standardizeddisaggregate == "Total Numerator") %>% 
  #(standardizeddisaggregate)
  group_by(fiscal_year, indicator, funding_agency) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  mutate(indicator = "TX_TB_N")

df_bar <- rbind(tx_tb_denom_pos, specimen_sent, specimen_return, tx_tb_num)

# use TX_TB D and TX_TB_D POS for TB % Screen Pos
tx_tb_d_val <- df %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         numeratordenom == "D",
         # operatingunit == "Zambia",
         standardizeddisaggregate == "Age/Sex/TBScreen/NewExistingART/HIVStatus",
         funding_agency == "PEPFAR") %>% 
  group_by(fiscal_year, indicator,
           funding_agency) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  pull(qtr4)

tx_tb_denom_pos %>% 
  mutate(pct_screen_pos = value/tx_tb_d_val,
         pct_screen_pos = scales::percent(pct_screen_pos)) %>% 
  pull(pct_screen_pos)


# get screen pos with specimen sent (Specimen sent / TX_TB D Pos)

tx_tb_denom_pos_val <- tx_tb_denom_pos %>% 
  pull(value)

specimen_sent %>% 
  mutate(pct_pos_specimen_sent = value/tx_tb_denom_pos_val,
         pct_pos_specimen_sent = scales::percent(pct_pos_specimen_sent)) %>% 
  pull(pct_pos_specimen_sent)


#get type % breakdown
df %>% 
  # resolve_knownissues() %>% 
  filter(indicator == "TX_TB",
         fiscal_year == metadata$curr_fy,
         # operatingunit == "Zambia",
         standardizeddisaggregate == "TB Test Type/HIVStatus",
         funding_agency == "PEPFAR") %>%
  group_by(fiscal_year, indicator,
           funding_agency, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  pivot_wider(names_from = "otherdisaggregate", values_from = "value") %>% 
  mutate(total = `Other - TB Test Type` + Smear + Xpert,
         pct_xpert = Xpert / total,
         pct_smear = Smear / total,
         pct_other = `Other - TB Test Type` / total) %>% 
  arrange(desc(pct_xpert))


# 4b. TB Test Type Comparison ------------------------------------------------

tab9_tb_testtype_comp <- function(df, agency = NULL, group_var = NULL) {
  
  df_final <- df %>% 
    filter(indicator == "TX_TB",
           fiscal_year == metadata$curr_fy,
           if (!is.null(agency)) funding_agency == agency else TRUE,
           standardizeddisaggregate == "TB Test Type/HIVStatus"
    ) %>%
    group_by(fiscal_year, indicator, otherdisaggregate,
             across(all_of(group_var))) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd() %>% 
    filter(period == metadata$curr_pd) %>% 
    pivot_wider(names_from = "otherdisaggregate", values_from = "value") %>% 
    mutate(across(c(`Other - TB Test Type`:Xpert), ~ ifelse(is.na(.), 0, .))) %>% 
    mutate(total = `Other - TB Test Type` + Smear + Xpert,
           pct_xpert = Xpert / total,
           pct_xpert = scales::percent(pct_xpert),
           pct_smear = Smear / total,
           pct_smear = scales::percent(pct_smear),
           pct_other = `Other - TB Test Type` / total,
           pct_other = scales::percent(pct_other)) %>% 
    arrange(desc(pct_xpert))
  
  return(df_final)
}

tab9_tb_testtype_comp(df, "PEPFAR", group_var = "operatingunit") %>% View()
tab9_tb_testtype_comp(df, group_var = "funding_agency")


# 6. Trends: TPT Completion -------------------------------------------------


#total denominator and TPT Completion
df %>% 
  # resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == metadata$curr_fy,
         numeratordenom %in% c("N", "D"),
         funding_agency == "PEPFAR",
         standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
  group_by(fiscal_year, indicator, numeratordenom) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  pivot_wider(names_from = "numeratordenom", values_from = "cumulative") %>% 
  mutate(tpt_completion = `N`/`D`)

#ART status 
df %>% 
  #resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == metadata$curr_fy,
         numeratordenom %in% c("N", "D"),
         #  operatingunit == "Mozambique",
         funding_agency == "PEPFAR",
         standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
  group_by(fiscal_year, indicator, numeratordenom, otherdisaggregate) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  pivot_wider(names_from = "numeratordenom", values_from = "cumulative") %>% 
  mutate(tpt_completion = `N`/`D`)

#coarse age
df %>% 
  # resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == metadata$curr_fy,
         numeratordenom %in% c("N", "D"),
         #operatingunit == "Mozambique",
         funding_agency == "PEPFAR",
         standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
  group_by(fiscal_year, indicator, numeratordenom, trendscoarse) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  pivot_wider(names_from = "numeratordenom", values_from = "cumulative") %>% 
  mutate(tpt_completion = `N`/`D`)


#sex
df %>% 
  # resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == metadata$curr_fy,
         numeratordenom %in% c("N", "D"),
         #operatingunit == "Mozambique",
         funding_agency == "PEPFAR",
         standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
  group_by(fiscal_year, indicator, numeratordenom, sex) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  pivot_wider(names_from = "numeratordenom", values_from = "cumulative") %>% 
  mutate(tpt_completion = `N`/`D`)

# 7. TPT Age Sex HIV Status ----------------------------------------------------

# age sex TPT completion
df %>% 
  #resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == metadata$curr_fy,
         numeratordenom %in% c("N", "D"),
         #operatingunit == "Mozambique",
         funding_agency == "PEPFAR",
         standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
  group_by(fiscal_year, indicator, numeratordenom, trendscoarse, sex) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  pivot_wider(names_from = "numeratordenom", values_from = "value") %>% 
  mutate(tpt_completion = `N`/`D`)

# age ART Status TPT completion
df %>% 
  #  resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == metadata$curr_fy,
         numeratordenom %in% c("N", "D"),
         # operatingunit == "Mozambique",
         funding_agency == "PEPFAR",
         standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
  group_by(fiscal_year, indicator, numeratordenom, trendscoarse, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  pivot_wider(names_from = "numeratordenom", values_from = "value") %>% 
  mutate(tpt_completion = `N`/`D`)

# 12. % TPT Completion Comparison ------------------------------------------

#by OU
df %>% 
  # resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == metadata$curr_fy,
         #funding_agency == "PEPF",
         standardizeddisaggregate %in% c("Total Numerator",
                                         "Total Denominator")) %>% 
  group_by(fiscal_year, indicator, standardizeddisaggregate, funding_agency) %>% #change to funding agency if you want
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>%
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>% 
  rename(total_num = `Total Numerator`,
         total_den = `Total Denominator`) %>% 
  mutate(tpt_completion = total_num / total_den) %>% 
  arrange(desc(tpt_completion))

# 13. TPT Completion Trends by Age ------------------------------------------

#globbal
df %>% 
  #resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
       #  fiscal_year == metadata$curr_fy,
         numeratordenom %in% c("N"),
         # operatingunit == "Mozambique",
         funding_agency == "PEPFAR",
         standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
  group_by(fiscal_year, indicator, sex
           #,operatingunit
         #  ,trendscoarse
  ) %>% #change to funding agency if you want
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") 

#by OU
df %>% 
  #resolve_knownissues() %>% 
  filter(indicator == "TB_PREV",
         fiscal_year == 2022,
         numeratordenom %in% c("N"),
         # operatingunit == "Mozambique",
         funding_agency == "USAID",
         standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
  group_by(fiscal_year, indicator, operatingunit, trendscoarse) %>% #change to funding agency if you want
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>%
  pivot_wider(names_from = "trendscoarse", values_from = "value") %>% 
  rename(total_num = `Total Numerator`,
         total_den = `Total Denominator`) %>% 
  mutate(tpt_completion = total_num / total_den) %>% 
  arrange(desc(tpt_completion))

# NEW ORG FOR DASHBOARD-----------------------------------------------------

## 11. Trends: TB_STAT ------------------------------------------

tab11_tb_stat <- function(df, agency, group_var = NULL, result_type) {
  
  df_tbstat <- df %>% 
    gophr::clean_indicator() %>%
    filter(indicator %in% c("TB_STAT", "TB_STAT_D"),
           funding_agency == agency,
           standardizeddisaggregate  %in% c("Age/Sex/KnownNewPosNeg","Age/Sex/KnownNewPosNegRecentNeg",
                                            "Age/Sex"))
  
  if (result_type == "qtr") {
    df_final <- df_tbstat %>% 
      group_by(fiscal_year, indicator,
               across(all_of(group_var))
      ) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
      reshape_msd() 
  } else if (result_type == "cumulative") {
    df_final <- df_tbstat %>% 
      group_by(fiscal_year, indicator,
               across(all_of(group_var))
      ) %>%
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      rename(value = cumulative)
  }
  
  
  return(df_final)
}

tab11_tb_stat(df, "USAID", group_var = c("otherdisaggregate"), result_type = "cumulative")
tab11_tb_stat(df, "USAID", group_var = c("sex"), result_type = "cumulative")
tab11_tb_stat(df, "USAID", group_var = c("trendscoarse"), result_type = "cumulative")

tab11_tb_stat_coverage <-function(df) {
  df_final <- df %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(tbstat_cov = TB_STAT/TB_STAT_D)
  
  return(df_final)
}

tab11_tb_stat(df, "USAID", result_type = "qtr") %>% 
  tab11_tb_stat_coverage()


## 13. % TB_STAT Dissaggregates ------------------------------------------

tbstat_disagg <- tab11_tb_stat(df, "USAID",  group_var = c("operatingunit","otherdisaggregate", "statushiv"), result_type = "qtr") %>% 
  filter(period == metadata$curr_pd) %>% 
  mutate(group = str_c(otherdisaggregate, "-", statushiv)) %>% 
  select(-c("otherdisaggregate", "statushiv")) %>% 
  pivot_wider(names_from = "group", values_fn = min) %>% 
  mutate(total = `Known at Entry-Positive` + `Newly Identified-Positive` +`Newly Identified-Negative`+
           `Recent-Negative`,
         share_new_neg = `Newly Identified-Negative` / total,
         share_new_pos = `Newly Identified-Positive`/total,
         share_known = `Known at Entry-Positive`/total,
         share_recent =`Recent-Negative`/total)

tbstat_disagg %>% 
  select(operatingunit, share_new_pos, share_known, share_new_neg, share_recent) %>% 
  arrange(desc(share_new_pos)) %>% View()

### TB_STAT_POS
df %>% 
  gophr::clean_indicator() %>%
  filter(indicator %in% c("TB_STAT", "TB_STAT_POS"),
         funding_agency == 'USAID',
         standardizeddisaggregate  %in% c("Total Numerator")) %>% 
  group_by(fiscal_year, indicator, operatingunit) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  pivot_wider(names_from= indicator) %>% 
  mutate(tb_stat_pos = TB_STAT_POS / TB_STAT) %>% 
  arrange(desc(tb_stat_pos))

## 14. Trends: TB_ART ------------------------------------------

tab14_tb_art <- function(df, agency, group_var = NULL, result_type) {
  
  df_art<- df %>% 
    gophr::clean_indicator() %>%
    filter(str_detect(indicator, "TB_ART"),
           funding_agency == agency,
           standardizeddisaggregate  %in% c("Age/Sex/NewExistingArt/HIVStatus", "Age/Sex/KnownNewPosNeg"))
  
  if (result_type == "qtr") {
    df_final <- df_art %>% 
      group_by(fiscal_year, indicator,
               across(all_of(group_var))
      ) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
      reshape_msd() 
  } else if (result_type == "cumulative") {
    df_final <- df_art %>% 
      group_by(fiscal_year, indicator,
               across(all_of(group_var))
      ) %>%
      summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
      rename(value = cumulative)
  }
  
  
  return(df_final)
}

tab14_tb_art(df, "USAID", result_type = "cumulative") %>% 
  pivot_wider(names_from = "indicator") %>% mutate(cov = `TB_ART`/`TB_ART_D`)


tab14_tb_art(df, "USAID", group_var = "trendscoarse", result_type = "cumulative") %>% 
  pivot_wider(names_from = "indicator") %>% mutate(cov = `TB_ART`/`TB_ART_D`)

tab14_tb_art(df, "USAID", group_var = "sex", result_type = "cumulative") %>% 
  pivot_wider(names_from = "indicator") %>% mutate(cov = `TB_ART`/`TB_ART_D`)



## 14. Results vs Targets ------------------------------------------


df %>% 
  clean_indicator() %>% 
  filter(indicator == "TB_STAT_POS",
         funding_agency == "USAID",
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         fiscal_year == 2024) %>% 
  group_by(operatingunit, indicator, fiscal_year) %>% 
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(achv = cumulative/targets) %>% 
  arrange(desc(achv))

## 15. OU HTS_TST pos in TB clinics ------------------------------------------

df %>% 
  clean_indicator() %>% 
  filter(indicator == c("HTS_TST", "HTS_TST_POS"),
         funding_agency == "USAID",
         modality =="TBClinic",
         #  otherdisaggregate != "Recent",
         indicatortype != "CS",
         #use_for_age == TRUE,
         standardizeddisaggregate =="Modality/Age/Sex/Result",
         fiscal_year == 2024) %>% 
  group_by(operatingunit, indicator, fiscal_year, modality) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  pivot_wider(names_from = indicator) %>% 
  mutate(pos = HTS_TST_POS/HTS_TST) %>% 
  arrange(desc(pos)) 
