# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TB WHO Missed case data processing
# REF ID:   0cf38479 
# LICENSE:  MIT
# DATE:     2023-10-31
# UPDATED:  2024-11-21

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
  
  ref_id <- "0cf38479"
  
  data_folder <- "Data/"

# IMPORT ------------------------------------------------------------------

  #TB notifs dataset
 df_notif <- data_folder %>% 
    return_latest("notifications") %>% 
    read_csv()

  #TB country indics
df_country <- data_folder %>% 
    return_latest("countries") %>% 
    read_csv()

#TB age/sex dataset
df_age_sex <- data_folder %>% 
  return_latest("age_sex") %>% 
  read_csv()

#UNAIDS 2023 Estimates
df_est <- pull_estimates(FALSE)
df_tt <- pull_testtreat(FALSE)

# MSD

df_msd_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_psd()

df_msd <- si_path() %>% 
  return_latest("OU_IM_FY22") %>% 
  read_psd()


metadata_msd <-  si_path() %>% 
  return_latest("OU_IM_FY22") %>% 
  get_metadata()
# FUNCTIONS -------------------------------------------------------------------

#clean up country names
rename_countries <- function(df) {
  df_rename <- df %>% 
    mutate(country = case_when(country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
                               country == "Democratic Republic of the Congo" ~ "DRC",
                               country == "Lao People's Democratic Republic" ~ "Laos",
                               country == "Papua New Guinea" ~ "PNG",
                               country == "United Republic of Tanzania" ~ "Tanzania",
                               country == "Viet Nam" ~ "Vietnam",
                               country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                               country == "Myanmar" ~ "Burma",
                               country == "Türkiye" ~ "Turkey",
                               country == "Curaçao" ~ "Curazao",
                               country == "Czechia" ~ "Czech Republic",
                               country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                               country == "Democratic People Republic of Korea"~ "Democratic People's Republic of Korea",
                               country == "Netherlands (Kingdom of the)" ~ "Netherlands",
                               country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                               TRUE ~ country))
  
  return(df_rename)
}

#Tidy df
tidy_data <- function(df) {
  
 df_tidy <-  df %>% 
   filter(year >= 2015) %>% 
    mutate(year = as.character(year),
           iso_numeric = as.character(iso_numeric)) %>% 
   tidyr::pivot_longer(where(is.double),
                       names_to = "indicator") %>% 
    select(country,iso3, g_whoregion, year, indicator, value)
   
 
 return(df_tidy)
}

# MUNGE -------------------------------------------------------------------

#apply rename and tidy function to country df
df_cntry_clean <- df_country %>%
  rename_countries() %>% 
  tidy_data() %>% 
  filter(indicator %in% c("e_pop_num", "e_inc_100k","e_inc_num", "e_tbhiv_prct", "e_inc_tbhiv_100k",
                          "e_inc_tbhiv_num", "e_mort_exc_tbhiv_100k", "e_mort_exc_tbhiv_num", "e_mort_tbhiv_100k",
                          "e_mort_tbhiv_num","e_mort_100k", "e_mort_num")) %>% 
  mutate(table_name = "TB burden countries")

# #list of cols to remove from notifs df
# rm_cols <- c("new_sp", "new_sn", "new_su", "new_oth", "ret_rel", "ret_taf", "ret_tad", "ret_oth", "newret_oth",
#              "new_sp_m04", "new_sp_m514", "new_sp_m014", "new_sp_m1524", "new_sp_m2534","new_sp_m3544","new_sp_m4554",
#              "new_sp_m5564","new_sp_m65", "new_sp_mu","new_sp_f04","new_sp_f514","new_sp_f014","new_sp_f1524",
#              "new_sp_f2534","new_sp_f3544","new_sp_f4554","new_sp_f5564","new_sp_f65", "new_sp_fu","new_sn_m04",
#              "new_sn_m514","new_sn_m014","new_sn_m1524", "new_sn_m2534","new_sn_m3544", "new_sn_m4554","new_sn_m5564","new_sn_m65",
#              "new_sn_m15plus", "new_sn_mu", "new_sn_f04", "new_sn_f514","new_sn_f014", "new_sn_f1524", "new_sn_f2534", "new_sn_f3544",
#              "new_sn_f4554","new_sn_f5564", "new_sn_f65", "new_sn_f15plus", "new_sn_fu", "new_sn_sexunk04", "new_sn_sexunk514",
#              "new_sn_sexunk014","new_sn_sexunk15plus","new_ep_m04","new_ep_m514","new_ep_m014", "new_ep_m1524","new_ep_m2534","new_ep_m3544",
#              "new_ep_m4554","new_ep_m5564", "new_ep_m65","new_ep_m15plus","new_ep_mu","new_ep_f04","new_ep_f514", "new_ep_f014","new_ep_f1524",
#              "new_ep_f2534","new_ep_f3544","new_ep_f4554", "new_ep_f5564","new_ep_f65","new_ep_f15plus",
#              "mdr_tx_adverse_events", "hivtest", "hivtest_pos", "hiv_cpt", "hiv_art", "hiv_tbscr", "hiv_reg",
#              "new_ep_fu", "new_ep_sexunk04", "new_ep_sexunk514","new_ep_sexunk014","new_ep_sexunk15plus", "new_ep_sexunkageunk",
#              "newrel_f514", "unconf_mdr_tx", "conf_mdr_tx")

#apply functions and remove cols
df_notif_clean <- df_notif %>% 
  rename_countries() %>% 
 # select(-c(all_of(rm_cols))) %>% 
 # select(country, iso2, iso3, iso_numeric, g_whoregion, year, newrel_hivpos, newrel_hivpos_014) %>% 
  tidy_data() %>% 
  filter(indicator %in% c("c_newinc", "hiv_new", "hiv_reg", "hiv_reg_all", "hiv_reg_new",
                          "hivtest_pos","newrel_hivpos", "newrel_hivpos_014", "c_tbhiv_tsr", "hiv_tbscr", "hiv_tbdetect")) %>% 
  mutate(table_name = "TB notifications")


df_age_sex_clean <- df_age_sex %>% 
  rename_countries() %>% 
  filter(age_group == "0-14",
         sex == "a", 
         risk_factor == "all") %>% 
  mutate(indicator = str_c("e_", measure, "_", unit, "_", age_group)) %>% 
  select(country, year, indicator, best, lo, hi) %>% 
  mutate(year = as.character(year)) %>% 
  tidyr::pivot_longer(where(is.double),
                      names_to = "type") %>% 
  unite(indicator, c(indicator, type), sep = "_", remove = TRUE) %>% 
  mutate(indicator = str_remove(indicator, "_best")) %>% 
  mutate(table_name = "TB Burden age sex") 



# UNAIDS ------------------------------------------------

#indics: PLHIV on ART, UNAIDS PLHIV, PLHIV on ART 0-14, UNAIDS PLHIV on ART %, PLHIV Children 0-14

#PLHIV (adults / peds)
df_est_tidy <- df_est %>%
  filter(indicator %in% c("Number PLHIV", "Total deaths to HIV Population"),
         sex == "All",
         age %in% c("All", "0-14"),
         year >= 2018,
         country != region) %>% 
  mutate(year = as.character(year)) %>%
  rename_countries()

#clean up to get into Tableau format
df_est_final <- df_est_tidy %>% 
  select(-c(region, estimate_flag, sheet, indic_type, epi_control,achv_95_plhiv, achv_95_relative, epi_ratio_2023)) %>% 
  tidyr::pivot_longer(where(is.double),
                      names_to = "est_type") %>% 
  mutate(est_type = case_when(est_type == "estimate" ~ "val", #remove this for the tidy data - this is just to match TB workflow
                              est_type == "lower_bound" ~ "lo",
                              est_type == "upper_bound" ~ "hi",
                              TRUE ~ est_type)) %>% 
  mutate(indicator = case_when(indicator == "Number PLHIV" ~ "PLHIV",
                               indicator == "Total deaths to HIV Population" ~ "Total_Deaths")) %>% 
  #mutate(indicator = "PLHIV") %>% 
  unite(indicator, c(indicator, age, est_type), sep = "_", remove = FALSE) %>% 
  select(country, year, indicator, value) %>% 
  mutate(indicator = str_remove(indicator, "_val")) %>% 
  mutate(table_name = "UNAIDS 2023 Estimates")


#PLHIV on ART, PLHIV on ART 0-14, UNAIDS PLHIV on ART %
df_tt_tidy <- df_tt %>%
 # count(indicator)
  filter(indicator %in% c("Number on ART of PLHIV", "Percent on ART of PLHIV"),
         sex == "All",
         age %in% c("All", "0-14"),
         year >= 2018,
         country != region) %>% 
  mutate(year = as.character(year)) %>%
  rename_countries()
  
#clean up to get into Tableau format
df_tt_final <- df_tt_tidy %>% 
  select(-c(region, estimate_flag, sheet, indic_type, epi_control, achv_95_plhiv, achv_95_relative, epi_ratio_2023)) %>% 
  tidyr::pivot_longer(where(is.double),
                      names_to = "est_type") %>% 
  mutate(est_type = case_when(est_type == "estimate" ~ "val", #remove this for the tidy data - this is just to match TB workflow
                              est_type == "lower_bound" ~ "lo",
                              est_type == "upper_bound" ~ "hi",
                              TRUE ~ est_type)) %>% 
  mutate(indicator = case_when(indicator == "Number on ART of PLHIV" ~ "Number_PLHIV_on_ART",
                               indicator == "Percent on ART of PLHIV" ~ "Percent_PLHIV_on_ART")) %>% 
  unite(indicator, c(indicator, age, est_type), sep = "_", remove = FALSE) %>% 
  select(country, year, indicator, value) %>% 
  mutate(indicator = str_remove(indicator, "_val")) %>% 
  mutate(table_name = "UNAIDS 2023 Estimates")


# MSD data ----------------------------------------------------------------

# TX_CURR every year (Q1 cumulative) - all ages and <15
# TX_TB_N cumulative - all ages and <15

df_msd_combined <- df_msd %>% 
  rbind(df_msd_arch) 

# TX_CURR Q1 - <15
txcurr_peds <- df_msd_combined %>% 
 # resolve_knownissues()  %>% 
  filter(fiscal_year >= 2017 & fiscal_year <= 2024,
         indicator == "TX_CURR",
         trendscoarse == "<15",
         standardizeddisaggregate %in% c("Age Aggregated/Sex/HIVStatus", "Age/Sex/HIVStatus")) %>% 
  group_by(fiscal_year, country, indicator, trendscoarse) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  select(-c(qtr2:qtr4)) %>% 
  mutate(indicator = "TX_CURR <15 Q1",
         table_name = metadata_msd$source) %>%
  rename(value = qtr1,
         year = fiscal_year) %>% 
  mutate(year = year - 1) %>% 
  select(country, year, indicator, value, table_name)
  
  #TX_CURR Q1 - all ages
 txcurr_all <- df_msd_combined %>% 
   # resolve_knownissues()  %>% 
    filter(fiscal_year >= 2017 & fiscal_year <= 2024,
           indicator == "TX_CURR",
         #  trendscoarse == "<15",
           standardizeddisaggregate %in% c("Total Numerator")) %>% 
    group_by(fiscal_year, country, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    select(-c(qtr2:qtr4)) %>% 
    mutate(indicator = "TX_CURR Q1",
           table_name = metadata_msd$source) %>%
    rename(value = qtr1,
           year = fiscal_year) %>% 
    mutate(year = year - 1) %>% 
    select(country, year, indicator, value, table_name)
 
 # TX_TB_N <15
 tx_tb_n_peds <- df_msd_combined %>% 
   clean_indicator() %>% 
   # resolve_knownissues()  %>% 
   filter(fiscal_year >= 2017,
          indicator == "TX_TB",
          numeratordenom == "N",
          trendscoarse == "<15",
          standardizeddisaggregate %in% c("Age Aggregated/Sex/NewExistingArt/HIVStatus", "Age Aggregated/Sex",
                                          "Age/Sex/NewExistingArt/HIVStatus", "Age Aggregated/Sex/HIVStatus")) %>% 
   group_by(fiscal_year, country, indicator, trendscoarse) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   select(-c(qtr1, qtr3)) %>% 
   mutate(total = qtr2 + qtr4) %>% 
   mutate(indicator = "TX_TB_N <15 Cumulative",
          table_name = metadata_msd$source) %>%
   rename(value = total,
          year = fiscal_year) %>% 
   select(country, year, indicator, value, table_name)
 
 # TX_TB_N All ages
 tx_tb_n_all <- df_msd_combined %>% 
   clean_indicator() %>% 
   # resolve_knownissues()  %>% 
   filter(fiscal_year >= 2017,
          indicator == "TX_TB",
          numeratordenom == "N",
        #  trendscoarse == "<15",
          standardizeddisaggregate %in% c("Total Numerator")) %>% 
   group_by(fiscal_year, country, indicator) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
   select(-c(qtr1, qtr3)) %>% 
   mutate(total = qtr2 + qtr4) %>% 
   mutate(indicator = "TX_TB_N All Cumulative",
          table_name = metadata_msd$source) %>%
   rename(value = total,
          year = fiscal_year) %>% 
   select(country, year, indicator, value, table_name)
 
 df_msd_clean <- bind_rows(txcurr_all, txcurr_peds, tx_tb_n_all, tx_tb_n_peds)
 

# BIND ALL TOGETHER --------------------------------------------------------

#join together
df_who_unaids_final <- rbind(df_cntry_clean, df_notif_clean) %>% 
  # filter(indicator %in% c("e_inc_tbhiv_num", "e_inc_tbhiv_num_hi", "e_inc_tbhiv_num_lo",
  #                         "e_mort_tbhiv_num", "e_mort_tbhiv_num_hi", "e_mort_tbhiv_num_lo",
  #                         "newrel_hivpos", "newrel_hivpos_014", "e_inc_num", "e_inc_num_hi",
  #                         "e_inc_num_lo")) %>%
  select(-c(iso3, g_whoregion)) %>% 
  rbind(df_est_final, df_tt_final, df_age_sex_clean, df_msd_clean)

 today <- lubridate::today()

write_csv(df_who_unaids_final, glue::glue("Dataout/2024_who_unaids_tb_dataset_v3_{today}.csv"))
  