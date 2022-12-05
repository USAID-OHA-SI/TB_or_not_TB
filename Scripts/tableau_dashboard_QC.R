# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TB/HIV Dashboard QC
# REF ID:   f50503e6 
# LICENSE:  MIT
# DATE:     2022-11-29
# UPDATED: 

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
  
      
  # Grab metadata
   get_metadata()
  
  ref_id <- "f50503e6"

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% return_latest("OU_IM") %>% 
    read_msd()



# 7. Trends in TB Treatment Initiation --------------------------------------
  
  df %>% 
    filter(indicator == "TX_TB",
           fiscal_year == 2022,
           numeratordenom == "N",
          # funding_agency == "USAID",
          # operatingunit == "Zambia",
           standardizeddisaggregate == "Age Aggregated/Sex/NewExistingArt/HIVStatus") %>% 
    #(standardizeddisaggregate)
    group_by(fiscal_year, indicator,
             #operatingunit,
             trendscoarse) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")
  
# 8. TB Screen Positive Cascade ---------------------------------------------
  
  #get TX_TB D POS
  tx_tb_denom_pos <- df %>% 
    filter(indicator == "TX_TB",
           fiscal_year == 2022,
           numeratordenom == "D",
          # operatingunit == "Zambia",
           standardizeddisaggregate == "Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus",
          funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator,
             funding_agency, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    filter(str_detect(otherdisaggregate, "Positive")) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")  
  
  #Get Specimen sent
  specimen_sent <- df %>% 
    filter(indicator == "TX_TB",
           fiscal_year == 2022,
           numeratordenom == "D",
           #operatingunit == "Zambia",
           standardizeddisaggregate == "Specimen Sent/HIVStatus",
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator,
             standardizeddisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") 
  
  #get specimen return
 specimen_return <- df %>% 
    filter(indicator == "TX_TB",
           fiscal_year == 2022,
           numeratordenom == "D",
           # operatingunit == "Zambia",
           standardizeddisaggregate == "Specimen Return/HIVStatus",
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator,
             funding_agency, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") 
 
 #get total numerator
 tx_tb_num <- df %>% 
   filter(indicator == "TX_TB",
          fiscal_year == 2022,
          numeratordenom == "N",
           funding_agency == "USAID",
          # operatingunit == "Zambia",
          standardizeddisaggregate == "Total Numerator") %>% 
   #(standardizeddisaggregate)
   group_by(fiscal_year, indicator) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")
 
 # use TX_TB D and TX_TB_D POS for TB % Screen Pos
 tx_tb_d_val <- df %>% 
   filter(indicator == "TX_TB",
          fiscal_year == 2022,
          numeratordenom == "D",
          # operatingunit == "Zambia",
          standardizeddisaggregate == "Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus",
          funding_agency == "USAID") %>% 
   group_by(fiscal_year, indicator,
            funding_agency) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   pull(qtr4)
 
 tx_tb_denom_pos %>% 
   mutate(pct_screen_pos = qtr4/tx_tb_d_val,
          pct_screen_pos = scales::percent(pct_screen_pos)) %>% 
   pull(pct_screen_pos)
 
 
 # get screen pos with specimen sent (Specimen sent / TX_TB D Pos)
 
 tx_tb_denom_pos_val <- tx_tb_denom_pos %>% 
   pull(qtr4)
 
 specimen_sent %>% 
   mutate(pct_pos_specimen_sent = qtr4/tx_tb_denom_pos_val,
          pct_pos_specimen_sent = scales::percent(pct_pos_specimen_sent)) %>% 
   pull(pct_pos_specimen_sent)
 
 
 #get type % breakdown
 test_type <- df %>% 
  # resolve_knownissues() %>% 
   filter(indicator == "TX_TB",
          fiscal_year == 2022,
          # operatingunit == "Zambia",
          standardizeddisaggregate == "TB Test Type/HIVStatus",
          funding_agency == "USAID") %>%
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
    
  
# 9. TB Test Type Comparison ------------------------------------------------
 
  df %>% 
    resolve_knownissues() %>% 
    filter(indicator == "TX_TB",
           fiscal_year == 2022,
          # funding_agency == "USAID",
           standardizeddisaggregate == "TB Test Type/HIVStatus"
           ) %>%
    group_by(fiscal_year, indicator, otherdisaggregate, operatingunit) %>% 
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
 
# 10. Trends: TPT Completion -------------------------------------------------
 
 #total numerator
 tb_prev_numer_val <- df %>% 
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
          #funding_agency == "USAID",
          standardizeddisaggregate == "Total Numerator") %>% 
   group_by(fiscal_year, indicator) %>% 
   summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
   pull(cumulative)
 
 #total denominator and TPT Completion
 df %>% 
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
          #funding_agency == "USAID",
          standardizeddisaggregate == "Total Denominator") %>% 
   group_by(fiscal_year, indicator) %>% 
   summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
   mutate(tpt_completion = tb_prev_numer_val / cumulative)

 #ART status 
 df %>% 
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
          numeratordenom %in% c("N", "D"),
          #funding_agency == "USAID",
          standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
   group_by(fiscal_year, indicator, numeratordenom, otherdisaggregate) %>% 
   summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
   pivot_wider(names_from = "numeratordenom", values_from = "cumulative") %>% 
   mutate(tpt_completion = `N`/`D`)
 
 #coarse age
 df %>% 
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
          numeratordenom %in% c("N", "D"),
          #funding_agency == "USAID",
          standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
   group_by(fiscal_year, indicator, numeratordenom, trendscoarse) %>% 
   summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
   pivot_wider(names_from = "numeratordenom", values_from = "cumulative") %>% 
   mutate(tpt_completion = `N`/`D`)
 
 
 #sex
 df %>% 
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
          numeratordenom %in% c("N", "D"),
          #funding_agency == "USAID",
          standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
   group_by(fiscal_year, indicator, numeratordenom, sex) %>% 
   summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
   pivot_wider(names_from = "numeratordenom", values_from = "cumulative") %>% 
   mutate(tpt_completion = `N`/`D`)
 
 # 11. TPT Age Sex HIV Status ----------------------------------------------------
 
 # age sex TPT completion
 df %>% 
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
          numeratordenom %in% c("N", "D"),
          #operatingunit == "Mozambique",
          #funding_agency == "USAID",
          standardizeddisaggregate == "Age/Sex/NewExistingArt/HIVStatus") %>% 
   group_by(fiscal_year, indicator, numeratordenom, trendscoarse, sex) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   reshape_msd() %>% 
   filter(period == metadata$curr_pd) %>% 
   pivot_wider(names_from = "numeratordenom", values_from = "value") %>% 
   mutate(tpt_completion = `N`/`D`)
 
 # age ART Status TPT completion
 df %>% 
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
          numeratordenom %in% c("N", "D"),
         # operatingunit == "Mozambique",
         # funding_agency == "USAID",
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
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
         # funding_agency == "USAID",
          standardizeddisaggregate %in% c("Total Numerator",
                                          "Total Denominator")) %>% 
   group_by(fiscal_year, indicator, standardizeddisaggregate, operatingunit) %>% #change to funding agency if you want
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   reshape_msd() %>% 
   filter(period == metadata$curr_pd) %>%
   pivot_wider(names_from = "standardizeddisaggregate", values_from = "value") %>% 
   rename(total_num = `Total Numerator`,
          total_den = `Total Denominator`) %>% 
   mutate(tpt_completion = total_num / total_den) %>% 
   arrange(desc(tpt_completion))

# 13. TPT Completion Trends by Age ------------------------------------------
 
 #by OU
 df %>% 
   resolve_knownissues() %>% 
   filter(indicator == "TB_PREV",
          fiscal_year == 2022,
          numeratordenom %in% c("N"),
          # operatingunit == "Mozambique",
          # funding_agency == "USAID",
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
 
  
  
  