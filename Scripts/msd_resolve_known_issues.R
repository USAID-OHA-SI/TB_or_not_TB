# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  export MSD with known issues resolved for TB dashboard QC
# REF ID:   89ca5f1b 
# LICENSE:  MIT
# DATE:     2022-11-28
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(gophr)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  #set up folders if you havent
  # folder_setup()

  # SI specific paths/functions  
    load_secrets()

    ref_id <- "89ca5f1b"

# IMPORT ------------------------------------------------------------------
  
 df_msd <- si_path() %>%
    return_latest("MER_Structured_Datasets_OU_IM_FY20-23") %>%
    read_msd() %>% 
    resolve_knownissues()
    
    
write_tsv(df_msd, "Dataout/MER_Structured_Datasets_OU_IM_FY20-23_20221114_known_issues_resolved.txt", na = "")



