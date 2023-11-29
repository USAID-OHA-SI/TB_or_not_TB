# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  export MSD with known issues resolved for TB dashboard QC
# REF ID:   89ca5f1b 
# LICENSE:  MIT
# DATE:     2022-11-28
# UPDATED:  2023-11-29

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(gophr)
  library(grabr)
  library(glue)
  

# GLOBAL VARIABLES --------------------------------------------------------

  # SI specific paths/functions  
    load_secrets()

    ref_id <- "89ca5f1b"
    
    today <- lubridate::today()
    
    #establish Pano session
    user <- pano_user()
    pass <- pano_pwd()
    
    sess <- pano_session(username = user, password = pass)

# IMPORT ------------------------------------------------------------------
    
    #download MSD from Pano using Pano API
    pano_extract_msd(operatingunit = NULL,
                     #version = "clean",
                     fiscal_year = 2023,
                     #quarter = 2,
                     level = "ou",
                     dest_path = si_path())
  
 df_msd <- si_path() %>%
    return_latest("MER_Structured_Datasets_OU_IM_FY21-24") %>%
    read_psd() %>% 
    resolve_knownissues()
    
 df_msd_no_nga <- si_path() %>%
   return_latest("MER_Structured_Datasets_OU_IM_FY21-24") %>%
   read_psd() %>% 
   resolve_knownissues() %>% 
   filter(operatingunit != "Nigeria")
    
write_tsv(df_msd, glue("Dataout/MER_Structured_Datasets_OU_IM_FY21-24_{today}_known_issues_resolved.txt"), na = "")
write_tsv(df_msd_no_nga, glue("Dataout/MER_Structured_Datasets_OU_IM_FY21-24_{today}_known_issues_resolved_NO_NIGERIA.txt"), na = "")


