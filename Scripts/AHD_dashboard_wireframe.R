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
metadata$curr_pd <- "FY24Q2"
metadata$curr_fy <- 2024

ref_id <- "f50503e6"

# IMPORT ------------------------------------------------------------------

#download MSD from Pano using Pano API
pano_extract_msd(operatingunit = NULL,
                 #version = "clean",
                 fiscal_year = 2024,
                 #quarter = 2,
                 level = "ou",
                 dest_path = si_path())


df <- si_path() %>% return_latest("PSNU_IM_FY22.*Mozambique") %>% 
  read_psd()

# MUNGE -------------------------------------------------------------------

df_lim <- df %>% 
  clean_indicator() %>% 
  filter(funding_agency == "USAID",
         fiscal_year == metadata$curr_fy,
         operatingunit == "Mozambique",
         indicator %in% c("TX_NEW", "TX_TB", "TX_TB_D"))

#TX_TB_D
df_tx_tb_d <- df_lim %>% 
  filter(indicator %in% c("TX_TB_D"),
         standardizeddisaggregate  %in% c("Age/Sex/TBScreen/NewExistingART/HIVStatus")) %>% 
  mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already on ART", "New on ART")) %>% 
  group_by(fiscal_year, indicator,
           otherdisaggregate, art_status) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  mutate(tb_status = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
  group_by(fiscal_year, indicator, art_status, tb_status) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd,
         art_status == 'New on ART')

df_cd4 <- df_lim %>% 
  filter(indicator == "TX_NEW",
         standardizeddisaggregate == "Age/Sex/CD4/HIVStatus") %>% 
  group_by(fiscal_year, indicator,
           otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  mutate(
    year = as.numeric(str_sub(period, 3, 4)),  # Extract the year as numeric
    quarter = as.numeric(str_sub(period, 6, 6))  # Extract the quarter as numeric
  ) %>% 
  mutate(
    semiannual = ifelse(quarter %in% c(1, 2), "FY24Q2", "FY24Q4")  # Group Q1 & Q2, and Q3 & Q4
  ) %>% 
  group_by(indicator, otherdisaggregate, year, semiannual) %>%
  summarise(semiannual_sum = sum(value)) %>%
  ungroup() %>% 
  filter(semiannual == "FY24Q2")


df_tx_tb_n <- df_lim %>% 
  filter(indicator %in% c("TX_TB"),
         standardizeddisaggregate  %in% c("Age/Sex/NewExistingArt/HIVStatus")) %>% 
  mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already on ART", "New on ART")) %>% 
  group_by(fiscal_year, indicator,
           otherdisaggregate, art_status) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd,
         art_status == 'New on ART')

## BY PSNU ---------------------------------------------------------------

df_psnu_cd4 <- df_lim %>% 
  filter(indicator == "TX_NEW",
         standardizeddisaggregate == "Age/Sex/CD4/HIVStatus") %>% 
  group_by(fiscal_year, indicator,
           otherdisaggregate, psnu) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  mutate(
    year = as.numeric(str_sub(period, 3, 4)),  # Extract the year as numeric
    quarter = as.numeric(str_sub(period, 6, 6))  # Extract the quarter as numeric
  ) %>% 
  mutate(
    semiannual = ifelse(quarter %in% c(1, 2), "FY24Q2", "FY24Q4")  # Group Q1 & Q2, and Q3 & Q4
  ) %>% 
  group_by(indicator, otherdisaggregate, year, semiannual, psnu) %>%
  summarise(semiannual_sum = sum(value)) %>%
  ungroup() %>% 
  filter(semiannual == "FY24Q2") %>% 
  pivot_wider(names_from = "otherdisaggregate", values_from = semiannual_sum) %>% 
  mutate(cd4_total = `<200 CD4` + `>=200 CD4` + `CD4 Unknown`,
         cd4_recorded = `<200 CD4` + `>=200 CD4`) %>% 
  mutate(share_ahd = `<200 CD4`/cd4_recorded)


df_psnu_cd4 %>% 
  ggplot(aes(x = fct_reorder(psnu, `<200 CD4`))) +
  geom_col(aes(y = cd4_recorded), fill = trolley_grey_light) +
  geom_col(aes(y = `<200 CD4`), fill = hw_viking) +
  coord_flip() +
  si_style_xgrid() +
  geom_text(aes(y = cd4_recorded,
                label = glue::glue("{cd4_recorded}"),
                hjust = -0.1,
                # vjust = -1,
                family = "Source Sans Pro"),  size = 12/.pt) +
  geom_text(aes(y = `<200 CD4`,
                label = glue::glue("{`<200 CD4`}
                                   ({percent(share_ahd, 1)})"),
                hjust = -0.1,
                # vjust = -1,
                family = "Source Sans Pro"),  size = 12/.pt) +
  labs(x = NULL, y= NULL,
       subtitle = glue("{metadata$curr_pd} TX_NEW CD4 <200 over CD4 recorded"))

si_save("AHD_01_CD4_psnu.svg")



#TX_TB_D
df_tx_tb_d_psnu <- df_lim %>% 
  filter(indicator %in% c("TX_TB_D"),
         standardizeddisaggregate  %in% c("Age/Sex/TBScreen/NewExistingART/HIVStatus")) %>% 
  mutate(art_status = ifelse(str_detect(otherdisaggregate, "Already"), "Already on ART", "New on ART")) %>% 
  group_by(fiscal_year, indicator,
           otherdisaggregate, art_status, psnu) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  mutate(tb_status = ifelse(str_detect(otherdisaggregate, "Positive"), "Positive", "Negative")) %>% 
  group_by(fiscal_year, indicator, art_status, tb_status, psnu) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd,
         art_status == 'New on ART') %>% 
  pivot_wider(names_from = "tb_status") %>% 
  mutate(total = Negative + Positive,
         share_tb_pos = Positive/total)

df_tx_tb_d_psnu %>% 
  ggplot(aes(x = fct_reorder(psnu, total))) +
  geom_col(aes(y = total), fill = trolley_grey_light) +
  geom_col(aes(y = Positive), fill = hw_lavender_haze) +
  coord_flip() +
  si_style_xgrid() +
  geom_text(aes(y = total,
                label = glue::glue("{total}"),
                hjust = -0.1,
                # vjust = -1,
                family = "Source Sans Pro"),  size = 12/.pt) +
  geom_text(aes(y = Positive,
                label = glue::glue("{Positive}
                                   ({percent(share_tb_pos, 1)})"),
                hjust = -0.1,
                # vjust = -1,
                family = "Source Sans Pro"),  size = 12/.pt) +
  labs(x = NULL, y= NULL,
       subtitle = glue("{metadata$curr_pd} TX_TB_D New on ART and Screened Positived for TB"))


si_save("Graphics/AHD_02_tx_tb_d_psnu.svg")




df_lim %>% 
  filter(indicator %in% c("TX_TB_D"),
         standardizeddisaggregate == "TB Screen Type/HIVStatus") %>% 
  group_by(fiscal_year, indicator,
           otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop")


