# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  AHD coverage + prevalence
# REF ID:   2b889569 
# LICENSE:  MIT
# DATE:     2024-09-30
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

path <- si_path() %>% return_latest("OU_IM_FY22")

# Grab metadata
metadata <- get_metadata(path)
  
  ref_id <- "2b889569"

# IMPORT ------------------------------------------------------------------
  
 df_msd <- read_psd(path)

# MUNGE -------------------------------------------------------------------
  
  df_cd4 <- df_msd %>% 
    filter(indicator == "TX_RTT",
           fiscal_year == metadata$curr_fy,
           age_2019 %ni% c("01-04", "<01"),
           standardizeddisaggregate == "Age/Sex/CD4/HIVStatus") %>% 
    group_by(fiscal_year, indicator, operatingunit,
             otherdisaggregate) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = "otherdisaggregate", values_from = "cumulative") %>% 
    ungroup() %>% 
    mutate(`<200 CD4` = ifelse(is.na(`<200 CD4`), 0, `<200 CD4`),
           `>=200 CD4` = ifelse(is.na(`>=200 CD4`), 0, `>=200 CD4`),
           `CD4 Unknown` = ifelse(is.na(`CD4 Unknown`), 0, `CD4 Unknown`)) %>% 
    mutate(total = `<200 CD4`+`>=200 CD4`+`CD4 Unknown`,
           pct_completeness = (`<200 CD4`+`>=200 CD4`) / total,
           pct_ahd = (`<200 CD4`)/(`<200 CD4`+`>=200 CD4`)) %>% 
    mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     operatingunit == "Dominican Republic" ~ "DR",
                                     TRUE ~ operatingunit)) %>% 
    mutate(pct_ahd = ifelse(`<200 CD4` == 0 & `>=200 CD4` == 0, 0, pct_ahd)) %>% 
    filter(operatingunit != "South Sudan")
  
  
#FY24Q3 TX_NEW  
df_cd4 <- df_msd %>% 
    filter(indicator == "TX_NEW",
          # funding_agency == "USAID",
           fiscal_year == metadata$curr_fy,
          age_2019 %ni% c("01-04", "<01"),
           standardizeddisaggregate == "Age/Sex/CD4/HIVStatus") %>% 
    group_by(fiscal_year, indicator, operatingunit,
             otherdisaggregate) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = "otherdisaggregate", values_from = "cumulative") %>% 
    ungroup() %>% 
    mutate(`<200 CD4` = ifelse(is.na(`<200 CD4`), 0, `<200 CD4`),
           `>=200 CD4` = ifelse(is.na(`>=200 CD4`), 0, `>=200 CD4`),
           `CD4 Unknown` = ifelse(is.na(`CD4 Unknown`), 0, `CD4 Unknown`)) %>% 
    mutate(total = `<200 CD4`+`>=200 CD4`+`CD4 Unknown`,
           pct_completeness = (`<200 CD4`+`>=200 CD4`) / total,
           pct_ahd = (`<200 CD4`)/(`<200 CD4`+`>=200 CD4`)) %>% 
    mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     operatingunit == "Dominican Republic" ~ "DR",
                                     TRUE ~ operatingunit)) %>% 
    mutate(pct_ahd = ifelse(`<200 CD4` == 0 & `>=200 CD4` == 0, 0, pct_ahd)) %>% 
    filter(operatingunit != "South Sudan")

 
  
  # VIZ --------------------------------------------------------------------------
  
  df_cd4 %>% 
    mutate(fill_color = case_when(pct_completeness <= 0.5 ~ hw_viking,
                                  pct_completeness > 0.5 & pct_ahd >=.2 ~ hw_midnight_blue,
                                  TRUE ~ trolley_grey)) %>% 
    ggplot(aes(x=pct_completeness, y=pct_ahd, color = fill_color)) +
    geom_point(size=3) +
    geom_hline(yintercept = .2, colour = "#D3D3D3") +
    geom_vline(xintercept = .50, colour = "#D3D3D3") +
    scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0,.6)) +
    #c(2004, seq(1990, 2020, by = 10))
    scale_color_identity() +
    ggrepel::geom_text_repel(aes(label = operatingunit), size = 4, na.rm = TRUE, family = "Source Sans Pro",
                             max.overlaps = 60) +
    si_style_nolines() +
    labs(x = "Reported CD4 Coverage",
         y = "Percent AHD",
         title = "CD4 Reporting Completeness and Advanced HIV Disease Rates for Patients Newly initiated on treatment" %>% toupper(),
        # title = "<span style= 'color:#5BB5D5;'>CD4 COVERAGE GAPS</span> STILL EXIST - EVEN WHERE <span style= 'color:#15478A;'>CD4 COVERAGE IS HIGH, MANY COUNTRIES HAVE HIGH AHD PREVALENCE</span>",
         subtitle = "FY24Q3 cumulative reported results at PEPFAR-supported sites",
         caption = glue("Source: {metadata$source} | Ref id: {ref_id}
                        Note: Results for <5 ommitted from plot")) +
    theme(plot.title = element_markdown())
  
  si_save("Graphics/AHD_by_OU_PEPFAR.svg")
  si_save("Images/AHD_by_OU_PEPFAR.png")
  
  
  #CD4 coverage gaps still exist and 2) Even where coverage is high, a large proportion of patients are still presenting with AHD
