# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  re-work TB visuals for TB/HIV website
# REF ID:   8c0b07a5 
# LICENSE:  MIT
# DATE:     2024-05-10
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
library(mindthegap)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
  
  ref_id <- "8c0b07a5"

# IMPORT ------------------------------------------------------------------
  
  #Current MSD
  filepath <- si_path() %>% 
    return_latest("MER_Structured_Datasets_OU_IM_FY22-24")
  
  metadata_msd <- get_metadata()
  
  #read MSD
  df_msd <- read_psd(filepath)
  
  #read NGA genie file
  df_nga <- si_path() %>% 
    return_latest("All indicators_FY23Q4_Nigeria") %>% 
    read_csv()
  
  #select only names in MSD
  names_to_keep <- names(df_msd)
  setdiff(names(df_msd), names(df_nga)) #only qtrs
  setdiff(names(df_nga), names(df_msd))
  
  #select all names from NGA file that match MSD (minus qtr1-qtr4)
  df_nga_new <- df_nga %>% 
    select(any_of(names_to_keep))
  
  #rbind together removing TZ and NGA for FY23 ONLY
  df <- df_msd %>% 
    filter(!(operatingunit=="Nigeria" & fiscal_year=="2023")
           # ,
           # !(operatingunit=="Tanzania" & fiscal_year=="2023")
    ) %>% 
    select(-c(qtr1:qtr4)) %>% 
    rbind(df_nga_new)
  
  
  #Current MSD
  arch_filepath <- si_path() %>% 
    return_latest("MER_Structured_Datasets_OU_IM_FY15")
  
  df_arch <- read_psd(arch_filepath)

# MUNGE -------------------------------------------------------------------
